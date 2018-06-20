{-# LANGUAGE TemplateHaskell #-}

module Test.Parrotbot.Language (
  tests
) where

import Hedgehog (
    MonadGen
  , Property
  , assert
  , checkParallel
  , discover
  , forAllWith
  , property
  )
import Parrotbot.Language (
    Term(A, I, K, S)
  , parseAllParrot
  , parseAllSKI
  , renderParrot
  , renderSKI
  )

import qualified Data.Text as Text
import qualified Hedgehog.Gen as Gen

tests :: IO Bool
tests = checkParallel $$(discover)

prop_parrotRoundtrip :: Property
prop_parrotRoundtrip = property $ do
  t <- forAllWith (Text.unpack . renderParrot) genTerm
  assert $ (parseAllParrot . renderParrot $ t) == Right t

prop_skiRoundtrip :: Property
prop_skiRoundtrip = property $ do
  t <- forAllWith (Text.unpack . renderSKI) genTerm
  assert $ (parseAllSKI . renderSKI $ t) == Right t

genTerm :: (MonadGen m) => m Term
genTerm =
  Gen.recursive
    Gen.choice
    [Gen.element [S, K, I]]
    [A <$> genTerm <*> genTerm]
