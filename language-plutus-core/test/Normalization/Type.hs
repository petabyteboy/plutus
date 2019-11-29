{-# LANGUAGE OverloadedStrings #-}

module Normalization.Type
    ( test_typeNormalization
    ) where

import           Language.PlutusCore
import           Language.PlutusCore.Generators.AST
import           Language.PlutusCore.MkPlc
import           Language.PlutusCore.Normalize

import           Control.Monad.Morph                (hoist)

import           Hedgehog
import qualified Hedgehog.Gen                       as Gen
import           Hedgehog.Internal.Property         (forAllT)
import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit

test_appAppLamLam :: IO ()
test_appAppLamLam = do
    let integer2 = TyBuiltin () TyInteger
        Normalized integer2' = runQuote $ do
            x <- freshTyName () "x"
            y <- freshTyName () "y"
            normalizeTypeFull $ mkIterTyApp ()
                (TyLam () x (Type ()) (TyLam () y (Type ()) $ TyVar () y))
                [integer2, integer2]
    integer2 @?= integer2'

test_normalizeTypesInIdempotent :: Property
test_normalizeTypesInIdempotent = property . hoist (pure . runQuote) $ do
    termNormTypes <- forAllT . Gen.just $ runAstGen genTerm >>= normalizeTypesManaIn (Mana 1000)
    termNormTypes' <- normalizeTypesFullIn termNormTypes
    termNormTypes === termNormTypes'

test_typeNormalization :: TestTree
test_typeNormalization =
    testGroup "typeNormalization"
        [ testCase     "appAppLamLam"               test_appAppLamLam
        , testProperty "normalizeTypesInIdempotent" test_normalizeTypesInIdempotent
        ]
