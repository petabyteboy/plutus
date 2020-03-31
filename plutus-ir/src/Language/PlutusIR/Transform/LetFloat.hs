{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
module Language.PlutusIR.Transform.LetFloat (floatTerm) where

import           Language.PlutusIR
import           Language.PlutusIR.Analysis.Dependencies

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.RWS
import           Control.Monad.State

import qualified Language.PlutusCore                     as PLC
import qualified Language.PlutusCore.Name                as PLC

import qualified Algebra.Graph.AdjacencyMap              as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm    as AM
import qualified Algebra.Graph.NonEmpty.AdjacencyMap     as AMN

import           Data.Function                           (on)
import qualified Data.IntMap                             as IM
import qualified Data.Map                                as M
import           Data.Maybe                              (fromMaybe)
import qualified Data.Set                                as S

import qualified Data.List.NonEmpty                      as NE

{- Note [Float algorithm]

The goal of this let transformation is to merge nearby lets into let-groups for
better readability of the program's IR (and potentially to reveal any later code optimizations).

Each let-binding is moved right underneath its closest-surrounding lambda (or at toplevel in case of no surrounding lambdas).
Then all let-bindings belonging to the same lambda are merged into 1 or more let-groups. The order and the cardinality of
the let-groups under the lambda is determined by the pre-calculated dependency-graph of the program's IR.

Specifically the algorithm is comprised of two passes:

1st-pass: Traverses an IR term/program and for each let, mark the location of its closest-surrounding lambda (or Top if no lambda),
and store it into a mapping of let-name=>lambda-location. We call the closest-surrounding lambda-location the "Rank" of a let.

2nd-pass: Cleans the term from all its lets (see `removeLets` function) and traverses
the clean-term to place them back again, this time  directly under their "Rank".
During traversal, when a lambda-location of interest is reached (i.e. Rank),
the algorithm consults the dependency graph to figure (a) the grouppings of lets into let-groups based on their (mutual) recursivity (by utilizing the SCCs of the depgraph)
and (b) the correct code-generation *order* of the let-groups (by utilizing the topsort of the SCCs).
As an optimization, the algorithm merges adjacent *nonrec* let-groups into a single-group for better readability; this is possible
because letnonrec in PlutusIR is linearly scoped.

Invariant: The algorithm never moves a let past its closest-surrounding lambda, but instead only places it directly underneath its Rank.
This has the effect of preserving the lazy/strict semantics of the original program.
-}

{- Note [Versus the "Let-floating" paper's algorithm]

Reference: Peyton Jones, Simon, Will Partain, and Andre Santos. "Let-Floating: Moving Bindings to Give Faster Programs."
In Proceedings of the First ACM SIGPLAN International Conference on Functional Programming, 1-12.
ICFP '96. New York, NY, USA: ACM, 1996. https://doi.org/10.1145/232627.232630.

The above algorithm is applied to a different source language (Haskell/GHC),
which only has 'letrec' and no support for letnonrec, type-synonym-lets, and datatype-lets.

The algorithm of the referenced paper can be summarized to:

1. The first pass of the term annotates all let-bindings with their ranks (level numbers).
A rank is the maximum depth of a free-variable dependency.
2. The second pass of the term uses those ranks "to float each binding outward to just
outside the lambda which has a rank (level number) one greater than that on the binding".

A rank in the referenced paper includes only the lambda depth and not the lambda-unique.
This is because the lambda-uniques are not needed by the original second pass because
the re-positions of lets are applied "locally" (versus globally in our case, more on that later).

The first-pass calculates for each let-binding, its deepest dependency on a free variable (can be imagined as a maximum rank).

The second pass is heavily altered. The referenced algorithm's second pass applies
"local transformations": when it stumbles upon a let, it looks up its (maximum) rank
and "locally" decides if it is worth to float the let-rhs (outwards based on the rank or even inwards).
It stops re-positioning / floating outwards the let-rhs, when it crosses over a lambda with depth
one larger than its rank (level number). The advantage is that the lets will be floated
exactly around the 'expr' that needs them, and *exactly outside a lambda definition*.
The downside is that there is a single merged let-group geneerated in every rank.
This is no problem for Haskell/GHC, because all lets are letrecs.

We use a different approach where we prior clean the AST PlutusIR term from all the lets (rec&nonrec)
and store them in a separate 'RhsTable' (see `removeLets` function). We use the information obtained by the dependency graph
to create let-(rec&nonrec) groups at the closest-surrounding lambda location (rank). During
the second pass, whenever we reach a lambda/Lambda definition we float all lets belonging to
this lambda (rank, i.e. lambda's depth and unique). This means that the lets
are floated *exactly inside a lambda definition, i.e at the lambda body*,
with the downside being that they are floated "a bit" more outside than
what would be optimal as in the paper's algorithm, i.e. floated exactly around the 'expr' that
needs them, and *exactly outside a lambda definition*).
The upside is that this approach is not local/ad-hoc, but more holistic.

-}

-- | During the first-pass we compute the  "rank" for every let-binding declared in the given PIR program.
-- A rank points to a surrounding lambda/Lambda location which is closest to that let, or Top if the let does not depend on any lambda/Lambda.
-- A rank can also be used as a pointer to a (lambda) location in the PIR program.
data Rank =
  -- | Signifies that a let has no lambda/Lambda/let free dependency and thus can be placed at the toplevel of the program.
     Top
  -- | a let is directly surrounded by the lamda signified by the location lamDepth :: 'Int', lamUnique :: 'PLC.Unique'
  -- NB: the lamDepth (Int) should be strictly positive (1,2..)
     | LamDep Int PLC.Unique
  deriving Eq

-- | The underlying classic getter function for depth
--
-- NB: Top is arbitrarily defined as having depth 0.
-- It could also be made 'minBound' or 'Word', but this is conceptually clearer and allows us to use 'IM.IntMap's.
_depth :: Rank -> Int
_depth = \case
  Top -> 0
  LamDep d _ -> d

-- | Lens-style getter of the depth of a rank
depth :: Getting r Rank Int
depth  = to _depth

-- | The underlying classic getter function for unique-name
--
-- NB: Top is arbitrarily defined as having unique: -1.
-- We need this because we use Ord Rank for maps, and we need total ordering for Ranks with same depth.
_uniq :: Rank -> PLC.Unique
_uniq = \case
  Top -> PLC.Unique (-1)
  LamDep _ u -> u

instance Ord Rank where
  -- try depth, then try unique
  compare = (compare `on` _depth) <> (compare `on` _uniq)

-- | During the first pass of the AST, a reader context holds the current in-scope lambda locations (as Ranks).
type P1Ctx = ( P1Data  --  the lambdas-in-scope that surround an expression
             , Rank     --  the surround lambda (its location)
             )


-- | During the first pass of the AST, we build an intermediate table to hold the ranks for the lets encountered so far.
-- This intermediate table will be transformed right at the end of 1st pass to 'FloatData'.
-- OPTIMIZE: use UniqueMap (aka IntMap) instead of `Map PLC.Unique`
type P1Data = M.Map
              PLC.Unique --  the identifier introduced by a let-binding
              ( Rank --  its calculated rank
              , PLC.Unique   --  a principal (single) unique for this binding (needed specifically because a let-Datatype-bind can introduce multiple identifiers)
              )

-- | Before we return from the 1st pass, we transform the acccumulated 'P1Data' to something that can be more easily consumed (by the second pass).
-- This 'FloatData' is a mapping of depth=>lambda=>{let_unique}. We consume the float-data left-to-right, sorted on depth.
--
type FloatData = IM.IntMap ( --  the depth (starting from 0, which is Top)
                 M.Map --  a mapping of lambda-locations at this depth => to all letidentifiers to float at that depth.
                  Rank    --  the location where the let should float to: location is a lambda unique or Top
                  (S.Set PLC.Unique) --  the let bindings that should be floated/placed under this lambda or Top
                 )

-- | First-pass: Traverses a Term to create a mapping of every let variable inside the term ==> to its corresponding rank.
p1Term ::  forall name tyname uni a
        . (PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique)
        => Term tyname name uni a -> FloatData
p1Term pir = toFloatData $ runReader
                           (goTerm pir)
                           (M.empty, Top) -- starting from toplevel depth: 0
  where
    goTerm :: Term tyname name uni a
        -> Reader P1Ctx P1Data
    goTerm = \case
      LamAbs _ n _ tBody  -> goBody n tBody
      TyAbs _ n _ tBody   -> goBody n tBody

      Let _ _ bs tIn    -> goLet bs tIn

      -- recurse and then accumulate the return values
      Apply _ t1 t2 -> (<>) <$> goTerm t1 <*> goTerm t2
      TyInst _ t _ -> goTerm t
      IWrap _ _ _ t -> goTerm t
      Unwrap _ t -> goTerm t

      -- "ground" terms (terms that do not contain other terms)
      _ -> asks fst -- the ctx's p1data is propagated back outwards as the return value

    goBody :: PLC.HasUnique b b' => b -> Term tyname name uni a -> Reader P1Ctx P1Data
    goBody n tBody =
      let lamU= n^.PLC.theUnique
      in M.delete lamU -- this trick removes any lamrank from the final value, since we don't care about lambdas in the final pass1 result (P1Data)
         <$> (local (\ (scope,oldRank) ->
                        let newDepth = oldRank^.depth + 1
                            newRank = LamDep newDepth lamU
                            newScope = M.insert lamU (newRank,lamU) scope -- adds a new lam rank to the current scope
                        in (newScope, newRank))
                    $ goTerm tBody)

    goLet :: NE.NonEmpty (Binding tyname name uni a) -> Term tyname name uni a -> Reader P1Ctx P1Data
    goLet bs tIn = do
        -- all bindings have the same rank
        newScope <- addRanks bs
        resBs <- mconcat <$> forM (bs^..traverse.bindingSubterms)
                                  (withScope newScope . goTerm)
        resIn <- withScope newScope $ goTerm tIn
        pure $ resBs <> resIn

    -- | Given a set of bindings and the union set of their free-variables,
    -- return a new scope ('P1Data') by inserting mappings of each new let-identifier to this maximum rank
    addRanks :: NE.NonEmpty (Binding tyname name uni a) -- ^ bindings
             -> Reader P1Ctx P1Data -- ^ the updated scope that includes the added ranks
    addRanks bs = do
      (scope, lamUp) <- ask
      let
          maxRank = lamUp
          scopeDelta = M.fromList [(i,(maxRank, b^.principal)) | b <- NE.toList bs, i <- bindingIds b]
          newScope = scope `M.union` scopeDelta
      pure newScope


    withScope :: P1Data -> Reader P1Ctx b -> Reader P1Ctx b
    withScope s = local (set _1 s) -- changes only the scope (i.e. p1data in our case)


    -- | Transform the 1st-pass accumulated data to something that is easier to be consumed by the 2nd pass (that does the actual floating).
    toFloatData :: P1Data -> FloatData
    toFloatData = foldr (\ (dep, letUPrinc) acc -> IM.insertWith (M.unionWith (<>)) (dep^.depth) (M.singleton dep (S.singleton letUPrinc)) acc) IM.empty



-- To make the 2nd pass easier, we prior "clean" the PIR term from all its let declarations and store them separataly in a 'RhsTable'.
-- The 2nd pass will later place all these table entries back inside the cleaned term, thus "floating" all the lets.

-- | A simple table holding a let-introduced identifier/unique to its RHS.
--
-- In case of a datatype-let (which has multiple identifiers&bindings), we add a table entry for each identifier of that datatype.
-- The multi-let-grouppings of the initial PIR program do not exist anymore in this representation.
type RhsTable tyname name uni a = M.Map
                              PLC.Unique
                              (Rhs tyname name uni a)

-- | An Rhs is quadruple of Annotation, Recursivity, Binding expr(s), and its depth (location) in the original program.
--
-- If the RHS was prior belonging to a larger let-group, its Recursivity&Annotation is copied from the let-group's Recursivity&Annotation.
-- In other words the same Recursivity&Annotation is shared among the let-identifiers that were beloning to the same let-group.
type Rhs tyname name uni ann = (ann, Recursivity, Binding tyname name uni ann, Int)

-- | This function takes a 'Term', cleans the 'Term' from all its 'Let'-bindings and stores those lets into a separate table.
-- The output term is most-likely not a valid PIR program anymore, because it contains free unbound variables.
--
-- OPTIMIZE: this traversal may potentially be included/combined with the 1st-pass.
removeLets :: forall name tyname uni a
            . (PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique)
           => Term tyname name uni a
           -> (Term tyname name uni a, RhsTable tyname name uni a)
removeLets t =
  runState
    -- keep track of the current depth, while we are searching for lets, starting from Top depth = 0
    (runReaderT (go t) $ Top^.depth)
    -- initial table is empty
    mempty
 where
   go :: Term tyname name uni a -> ReaderT Int (State (RhsTable tyname name uni a)) (Term tyname name uni a)
   go = \case
         -- this overrides the 'termSubterms' functionality only for the 'Let' constructor
         LamAbs a n ty tBody -> LamAbs a n ty <$> local (+1) (go tBody)
         TyAbs a n k tBody  -> TyAbs a n k <$> local (+1) (go tBody)

         Let a r bs tIn -> do
           curDepth <- ask
           forM_ bs $ \ b -> do
             b' <- bindingSubterms go b
             lift . modify $ M.insert (b'^.principal) (a, r, b',curDepth)
           go tIn
         t' -> termSubterms go t'

-- | Starts the 2nd pass from the 'Top' depth and the toplevel expression of the cleanedup term (devoid of any lets).
-- See Note [Float second-pass]
--
p2Term :: forall name tyname uni a
       . (PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique, Monoid a)
      => Term tyname name uni a --
      -> FloatData
      -> Term tyname name uni a
p2Term pir fd =
  -- the 2nd pass starts by trying to float any lets around the top-level expression (body)
  -- For optimization reasons, we keep a state of remaining SCCs that we need to scan when we are generating let-groups in their correct order.
  -- The initial state starts from the all sccs top.sorted; the invariant is that the remaining sccs state should always be top.sorted.
  case runState (goFloat Top pirClean fd) topSortedSccs of
    (res, []) -> res
    (_, remState) -> error ("The final term is missing some lets because of a problem. The lets that could not be floated back to the final term were:" ++ show remState)

 where
  -- | Prior to starting the second pass, we clean the term from all its let-declarations and store them separately in a table.
  -- The 2nd pass will later re-introduce these let-declarations, potentially placing them differently than before, thus essentially "floating the lets".
  (pirClean :: Term tyname name uni a, rhsTable :: RhsTable tyname name uni a) = removeLets pir

  -- 2nd-pass functions
  ---------------------

  -- | visit each term to apply the float transformation
  -- OPTIMIZE: stop descending when state==mempty?
  goTerm :: Int -- ^ current depth
         -> FloatData -- ^ the lambdas we are searching for (to float some lets inside them)
         -> Term tyname name uni a
         -> State [S.Set PLC.Unique] (Term tyname name uni a)
  goTerm curDepth searchTable = \case
      -- we are only interested in lambdas/Lambdas
      LamAbs a n ty tBody -> LamAbs a n ty <$> goAbs curDepth searchTable n tBody
      TyAbs a n k tBody  -> TyAbs a n k <$> goAbs curDepth searchTable n tBody
      -- descend otherwise to apply the transformations to subterms
      t -> termSubterms (goTerm curDepth searchTable) t

  -- | If a lambda/Lambda is found, the current location is updated (depth+lamUnique) and try to float its body
  goAbs :: PLC.HasUnique b b'
        => Int -- ^ current depth
        -> FloatData -- ^ the lambdas we are searching for (to float some lets inside them)
        -> b                  -- ^ lambda/Lambda's unique
        -> Term tyname name uni a -- ^ lambda/Lambda's body
        -> State [S.Set PLC.Unique] (Term tyname name uni a)
  goAbs oldDepth floatData n tBody =
    let newLam = LamDep (oldDepth+1) (n^.PLC.theUnique)
    in goFloat newLam tBody floatData

  -- | We are currently INSIDE (exactly under) a lambda/Lambda body/Top (Top if we are right at the start of the algorithm)
  -- We try to see if we have some lets to float here based on our 1st-pass-table data (searchTable).
  goFloat :: Rank -- ^ the rank/location of the lambda above that has this body
         -> Term tyname name uni a                           -- ^ the body term
         -> FloatData -- ^ the lambdas we are searching for (to float some lets inside them)
         -> State [S.Set PLC.Unique] (Term tyname name uni a)                           -- ^ the transformed body term
  goFloat aboveLamLoc tBody floatData  =
   -- look for the next smallest depth remaining to place
   case IM.minViewWithKey floatData of
     Nothing -> pure tBody -- nothing left to float
     Just ((searchingForDepth, searchingForLams_Lets), restFloatData) ->
      let curDepth = aboveLamLoc^.depth
      in case curDepth `compare` searchingForDepth of
      -- the minimum next depth we are looking for, is not this one, so just descend with the whole floatdata
      LT -> goTerm curDepth floatData tBody
      -- found depth, see if our lambda above is a lambda we are interested in (to float some lets)
      EQ ->
        -- transform the lam-body, passing the rest floatdata
        let tBody' = goTerm curDepth restFloatData tBody
        in case M.lookup aboveLamLoc searchingForLams_Lets of
             -- the lambda above-this-body has some let-rhses to insert (float) here.
             Just lets -> genLets lets restFloatData =<< tBody'
             -- nothing to do for the lambda above, i.e. no let-rhses to insert here
             _         -> tBody'
      GT -> error "This shouldn't happen, because the algorithm takes care to stop descending when EQ is reached."


  -- | the dependency graph (as the one used by the deadcode elimination)
  -- but w/o the root node and only uses the Var's Unique as the node id
  -- OPTIMIZE: by using AdjacencyIntMap
  depGraph :: AM.AdjacencyMap PLC.Unique
  depGraph = AM.gmap (\case Variable u -> u; _ -> error "just for completion")
             $ AM.removeVertex Root -- we remove Root because we do not care about it
             $ runTermDeps pir

  -- | the dependency graph as before, but with datatype-related nodes merged/reduced under a single node per datatypebind a, see 'principal'.
  reducedDepGraph :: AM.AdjacencyMap PLC.Unique
  reducedDepGraph = M.foldr (\ (_,_,b,_) accGraph ->
                               let ids = bindingIds b
                               in case ids of
                                 _:[] -> accGraph -- optimization to avoid an O(n) op. if there is nothing to merge
                                 _    ->  AM.mergeVertices (`S.member` S.fromList ids) (b^.principal) accGraph

                            ) depGraph rhsTable

  -- |take the strongly-connected components of the reduced dep graph, because it may contain loops (introduced by the LetRecs)
  -- topologically sort these sccs, since we rely on linear (sorted) scoping in our 'genLets' code generation
  topSortedSccs :: [S.Set PLC.Unique]
  topSortedSccs =
    let allLets = M.keysSet rhsTable in
    -- we remove lambda sccs because we are not interested in them
    -- for optimization, we just check one of the elements if it is a let unique. lambdas appear only by themselves.
    filter (\ scc-> S.findMin scc `S.member` allLets)
    . fmap AMN.vertexSet -- we are not interested in graph structure anymore
    . fromMaybe (error "Cycle detected in the depgraph. This shouldn't happen in the first place.") $ AM.topSort $ AM.scc reducedDepGraph

  -- | Groups a given set of lets into one or more multilets and wraps these multilets around a given term.
  -- The grouping is done through the strongly-connected components
  -- The input lets is not sorted w.r.t. linear scoping, so this function uses the topological-sort of these SCCs,
  -- to figure out the correct (dependend/linear) order in which to generate these new multilets.
  --
  -- The resulting term is wrapped with linear-scope-sorted LetRecs and LetNonRecs (interspersed between each other because letnonrec and letrec syntax cannot be combined)
  -- Example: `let {i = e, ...} in let rec {j = e, ...} in let rec {...} in let {...} in ..... in originalTerm`
  genLets :: S.Set PLC.Unique -- ^ all the let identifiers to wrap around this term
           -> FloatData -- ^ the remaining data to be floated
           -> Term tyname name uni a                           -- ^ the term to be wrapped
           -> State [S.Set PLC.Unique] (Term tyname name uni a)                           -- ^ the final wrapped term
  genLets lets restDepthTable t = do
    (hereSccs, restSccs) <- gets splitState
    put $! restSccs
    foldM genLetsFromScc t hereSccs
      where
        -- | given the lets to float here, it splits the SCC state to a pair of: sccs to process at this location
        -- and remainder sccs to use as the next state.
        splitState :: [S.Set PLC.Unique] -> ([S.Set PLC.Unique], [S.Set PLC.Unique])
        splitState = foldMap (\ scc ->
                                           let comScc = scc `S.intersection` lets
                                           in if S.null comScc
                                              then ([], [scc])
                                              else ([comScc], let remScc = scc S.\\ lets
                                                              in if S.null remScc
                                                                 then []
                                                                 else [remScc])
                                       )
        -- | given an SCC, it creates a new (rec or nonrec) let-group from it and wraps it around an accumulated term
        -- Special case: if the new group and the accumulated term are both letnonrecs,
        -- it merges them together into a single let-group (i.e. linear scoped).
        genLetsFromScc :: Term tyname name uni a
                       -> S.Set PLC.Unique
                       -> State [S.Set PLC.Unique] (Term tyname name uni a)
        genLetsFromScc accTerm scc = do
              let isSingletonScc = S.size scc == 1
              res <- forM (S.toList scc) (\ v ->
                                             case M.lookup v rhsTable of
                                               Just rhs@(_,_,_,oldDepth) ->
                                                 -- lift each resulting binding in a list monoid to accumulate them
                                                 -- visit the generated rhs-term as well for any potential floating
                                                 (_3.bindingSubterms) (goTerm
                                                                        -- inside the RHS we "pretend" that we are at the depth of the let in the original program,
                                                                        -- since the depths of lets in FloatData correspond to the original depths.
                                                                        oldDepth
                                                                        -- for optimization, we pass only a part of the floatdata that are larger than this RHS orig. depth.
                                                                        (snd $ IM.split oldDepth restDepthTable)
                                                                      ) rhs
                                               _ -> error "Something went wrong: no rhs was found for this let in the rhstable."
                                                       )
              let (mAnn, mRecurs, newBindings) = foldr1 (<>) $ fmap (\(x,y,z,_) -> (x
                                                                                     -- if the SCC is a single node then use its original 'Recursitivy';
                                                                                     -- otherwise, the SCC is  a group of nodes and we *have to* treat all of them in a 'letrec',
                                                                                     -- since we don't have any information on how to linearize those.
                                                                                   , if isSingletonScc then y else Rec
                                                                                     -- lift the binding into a monoid for accumulation
                                                                                   , pure z)
                                                                    ) res
              pure $ case accTerm of
                       -- merge current let-group with previous let-group iff both groups' recursivity is NonRec
                       Let accAnn NonRec accBs accIn | mRecurs == NonRec -> Let (mAnn <> accAnn) NonRec (newBindings <> accBs) accIn
                       -- never merge if the previous let-group is a Rec or the current let-group is Rec,
                       -- but instead create a nested current let-group under the previous let-group (above)
                       _ -> Let mAnn mRecurs newBindings accTerm


-- | The main transformation function (Term -> Term) that returns a term with all lets floated as outwards as possible (full laziness).
-- Is comprised of two AST "passes":
-- 1stpass: to collect the ranks (positions) of all lets
-- 2ndpass:  to remove all its lets and place them back (float them) in their ranks (positions).
-- See Note [Float algorithm]
--
-- NB: This transformation requires that the PLC.rename compiler-pass has prior been run.
floatTerm :: forall name tyname uni a
           . (PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique, Monoid a)
          => Term tyname name uni a -> Term tyname name uni a
floatTerm pir = p2Term pir
                -- give the floatdata of the 1st pass to the start of the 2nd pass
              $ p1Term pir


-- Helpers
----------

-- | A getter that returns a single 'Unique' for a particular binding.
-- We need this because let-datatypes introduce multiple identifiers, but in our 'RhsTable', we use a single Unique as the key. See also: 'bindingIds'
principal :: (PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique)
            => Getting r (Binding tyname name uni a) PLC.Unique
principal = to $ \case TermBind _ _ (VarDecl _ n _) _ -> n ^. PLC.theUnique
                       TypeBind _ (TyVarDecl _ n _) _ -> n ^. PLC.theUnique
                       -- arbitrary: uses the type construtors' unique as the principal unique of this data binding group
                       DatatypeBind _ (Datatype _ (TyVarDecl _ tConstr _) _ _ _) -> tConstr ^. PLC.theUnique