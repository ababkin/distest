{-# LANGUAGE NamedFieldPuns #-}

module Test.TestTypes
  ( -- * Types of Tests
    TestTypes (..)
    -- * Reexports for convenience
  , withArgs
    -- * Test selectors
  , pruneNotRunnableTests, selectTestTypesToRun, withDefaultTypes
    -- * Specs runner
  , runSpecs
    -- * Specs data
  , Specs(..)
    -- * smart constructor for empty spec
  , defaultSpecs
  )where

import           Data.String
import           Protolude
import           System.Environment        (withArgs)
import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.Runners.AntXML (antXMLRunner)

-- * Test Types
data TestTypes = Unit
               | QuickCheck
               | System
               deriving (Eq, Show)

-- | Select elements from `tests` that are tagged with element in `types`
pruneNotRunnableTests :: [ ( TestTypes, a ) ]
                      -> [ TestTypes ]
                      -> [ a ]

pruneNotRunnableTests tests types =
  snd <$> filter ((`elem` types) . fst) tests

-- | Select types of test to run based on given list of arguments and returns the
-- list pruned of used arguments
selectTestTypesToRun :: [ String ]
                     -> ([ TestTypes ], [ String ])

selectTestTypesToRun []         = ([], [])
selectTestTypesToRun (arg:rest) = do
  let (tts, args) = selectTestTypesToRun rest
  thisClassify arg args tts
  where
    thisClassify "--unit"   args tts = (Unit:tts, args)
    thisClassify "--qc"     args tts = (QuickCheck:tts, args)
    thisClassify "--system" args tts = (System:tts, args)
    thisClassify other      args tts = (tts, other:args)

-- | Add default test types to run if none is selected
-- This function is intended to be used in conjunction with `selectTestTypesToRun` to
-- provide some sensible defaults if user does not select explicitly type of tests to
-- run with command-line flags.
withDefaultTypes :: [ TestTypes ]
                 -> ([ TestTypes ], [ String ])
                 -> ([ TestTypes ], [ String ])

withDefaultTypes defs ([], args)  = (defs, args)
withDefaultTypes _    (tts, args) = (tts, args)


data Specs = Specs {
    name   :: Text
  , unit   :: [Spec]
  , system :: [Spec]
  , qc     :: [Spec]
  }

defaultSpecs = Specs{ name = ""
                    , unit    = []
                    , system  = []
                    , qc      = []
                    }

runSpecs :: Specs -> IO ()
runSpecs Specs{ name, unit, system, qc} = do
  (testTypes, args) <- withDefaultTypes [ Unit ] . selectTestTypesToRun <$> getArgs

  unitSpecs   <- traverse (testSpec "Unit") unit
  systemSpecs <- traverse (testSpec "System") system
  qcSpecs     <- traverse (testSpec "QuickCheck") qc

  let classifiedTests = [ (Unit,        testGroup "Unit Tests"        unitSpecs)
                        , (System,      testGroup "System Tests"      systemSpecs)
                        , (QuickCheck,  testGroup "QuickCheck Tests"  qcSpecs)
                        ]
      testsToRun      = classifiedTests `pruneNotRunnableTests` testTypes

  withArgs args $ do
    defaultMainWithIngredients (antXMLRunner:defaultIngredients) $
      testGroup (toS name <> " Tests") testsToRun

