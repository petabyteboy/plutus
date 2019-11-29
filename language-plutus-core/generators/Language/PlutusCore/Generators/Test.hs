-- | This module defines functions useful for testing.

module Language.PlutusCore.Generators.Test
    ( TypeEvalCheckError (..)
    , TypeEvalCheckResult (..)
    , TypeEvalCheckM
    , typeEvalCheckBy
    , unsafeTypeEvalCheck
    , getSampleTermValue
    , getSampleProgramAndValue
    , printSampleProgramAndValue
    , sampleProgramValueGolden
    , propEvaluate
    ) where

import           Language.PlutusCore.Constant
import           Language.PlutusCore.Evaluation.Result
import           Language.PlutusCore.Generators.Interesting
import           Language.PlutusCore.Generators.Internal.TypedBuiltinGen
import           Language.PlutusCore.Generators.Internal.TypeEvalCheck
import           Language.PlutusCore.Generators.Internal.Utils
import           Language.PlutusCore.Lexer.Type                          hiding (name)
import           Language.PlutusCore.Name
import           Language.PlutusCore.Pretty
import           Language.PlutusCore.Type

import           Control.Monad.Except
import           Data.Functor                                            ((<&>))
import qualified Data.Text.IO                                            as Text
import           Hedgehog                                                hiding (Size, Var, eval)
import qualified Hedgehog.Gen                                            as Gen
import           System.FilePath                                         ((</>))

import Debug.Trace

-- | Generate a term using a given generator and check that it's well-typed and evaluates correctly.
getSampleTermValue :: KnownType a => TermGen a -> IO (TermOf EvaluationResultDef)
getSampleTermValue genTerm = Gen.sample $ unsafeTypeEvalCheck <$> genTerm

-- | Generate a program using a given generator and check that it's well-typed and evaluates correctly.
getSampleProgramAndValue
    :: KnownType a => TermGen a -> IO (Program TyName Name (), EvaluationResultDef)
getSampleProgramAndValue genTerm =
    getSampleTermValue genTerm <&> \(TermOf term result) ->
        (Program () (defaultVersion ()) term, result)

-- | Generate a program using a given generator, check that it's well-typed and evaluates correctly
-- and pretty-print it to stdout using the default pretty-printing mode.
printSampleProgramAndValue :: KnownType a => TermGen a -> IO ()
printSampleProgramAndValue =
    getSampleProgramAndValue >=> \(program, value) -> do
        putStrLn $ prettyPlcDefString program
        putStrLn ""
        putStrLn $ prettyPlcDefString value

-- | Generate a pair of files: @<folder>.<name>.plc@ and @<folder>.<name>.plc.golden@.
-- The first file contains a term generated by a term generator (wrapped in 'Program'),
-- the second file contains the result of evaluation of the term.
sampleProgramValueGolden
    :: KnownType a
    => String          -- ^ @folder@
    -> String          -- ^ @name@
    -> TermGen a       -- ^ A term generator.
    -> IO ()
sampleProgramValueGolden folder name genTerm = do
    let filePlc       = folder </> (name ++ ".plc")
        filePlcGolden = folder </> (name ++ ".plc.golden")
    (program, value) <- getSampleProgramAndValue genTerm
    Text.writeFile filePlc       $ prettyPlcDefText program
    Text.writeFile filePlcGolden $ prettyPlcDefText value

-- | A property-based testing procedure for evaluators.
-- Checks whether a term generated along with the value it's supposed to compute to
-- indeed computes to that value according to the provided evaluate.
propEvaluate
    :: KnownType a
    => (Term TyName Name () -> EvaluationResultDef)  -- ^ An evaluator.
    -> TermGen a                                     -- ^ A term/value generator.
    -> Property
propEvaluate eval genTermOfTbv = withTests 200 . property $ do
    termOfTbv <- forAllNoShow genTermOfTbv
    case typeEvalCheckBy eval termOfTbv of
        Left (TypeEvalCheckErrorIllFormed err)             -> error $ prettyPlcErrorString err
        Left (TypeEvalCheckErrorIllEvaled expected actual) ->
            expected === actual  -- We know that these two are distinct, but there is no nice way we
                                 -- can report this via 'hedgehog' except by comparing them here again.
        Right _                                            -> return ()
