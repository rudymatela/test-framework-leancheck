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
import Control.Exception (SomeException, catch, evaluate)

-- | List of test results for a given property
newtype Results = Results [([String],Bool)]

-- | The ultimate test result for a given property
data Result = OK        Int
            | Falsified Int [String]
            | Exception Int [String] String
  deriving (Eq, Show)

-- | Given a 'Testable' property, returns a test-framework test.
--   For example, place the following in a 'TestGroup' list:
--
-- > testProperty "sort . sort == sort" $
-- >   \xs -> sort (sort xs :: [Int]) == sort xs
--
-- You may want to import this module qualified and use @LC.TestProperty@
-- if mixing "Test.LeanCheck" tests
-- with those of other property testing libraries.
testProperty :: Testable a => TestName -> a -> Test
testProperty name = Test name . Results . results

-- | The current version does not 'runImprovingIO'.
--   So progress is only seen between properties, but not within properties.
instance Testlike Int Result Results where
  runTest topts results = do
    let m = unK $ topt_maximum_generated_tests topts
    result <- resultIO m results
    return (Finished result, return ())
  testTypeName _ = "Properties"

instance TestResultlike Int Result where
  testSucceeded (OK _)  =  True
  testSucceeded _       =  False

resultsIO :: Int -> Results -> [IO Result]
resultsIO n (Results results) = zipWith torio [1..] $ take n results
  where
    tor i (_,True) = OK i
    tor i (as,False) = Falsified i as
    torio i r@(as,_) = evaluate (tor i r)
       `catch` \e -> let _ = e :: SomeException
                     in return (Exception i as (show e))

resultIO :: Int -> Results -> IO Result
resultIO n = computeResult . resultsIO n
  where
  computeResult []  = error "resultIO: no results, empty Listable enumeration?"
  computeResult [r] = r
  computeResult (r:rs) = r >>= \r -> case r of
                                     (OK _) -> computeResult rs
                                     _      -> return r

-- joins the counter-example arguments
joinArgs :: [String] -> String
joinArgs ce | any ('\n' `elem`) ce = unlines $ map chopBreak ce
            | otherwise            = unwords ce

-- chops a line break at the end if there is any
chopBreak :: String -> String
chopBreak [] = []
chopBreak ['\n'] = []
chopBreak (x:xs) = x:chopBreak xs
