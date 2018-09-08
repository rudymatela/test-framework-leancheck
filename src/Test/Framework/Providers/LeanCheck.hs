{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Test.Framework.Providers.LeanCheck
-- Copyright   : (c) 2018 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- LeanCheck support for test-framework (Test.Framework).
--
-- Here's how your @test.hs@ might look like:
--
-- > TODO: TBA
--
-- The output for the above program is:
--
-- > TODO: TBA
--
-- Use @--...@ to configure the maximum number of tests for each property.
-- TODO: TBA
--
-- Please see the documentation of
-- "Test.LeanCheck" and "Framework.Providers"
-- for more details.
module Test.Framework.Providers.LeanCheck
  ( testProperty
  )
where

import Test.Framework.Providers.API
import Test.LeanCheck

-- | List of test results for a given property
newtype Results = Results [([String],Bool)]

-- | The ultimate test result for a given property
data Result = OK        Int
            | Falsified Int [String]
            | Exception Int [String] String
  deriving (Eq, Show)

testProperty :: Testable a => TestName -> a -> Test
testProperty name = Test name . property

property :: Testable a => a -> Results
property = undefined

instance Testlike Int Result Results where
  runTest topts (Results rs) = undefined
  testTypeName _ = "Properties"

instance TestResultlike Int Result where
  testSucceeded = undefined
