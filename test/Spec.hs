import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.Tasty.Runners.AntXML
import BannedFuncs (bannedFuncs)

{-
timeouts can be added to any instance of TestTree in microseconds. This makes timing individual tests, groups and even the entire suite possible.
Since the output of tests are IO () based, we need to parse the output to extract the grades.
Thus, in order to assign marks and identify which tests pass / fail we implement a naming convention
Test groups are prefaced by the name of the module being tested (e.g. HelloWorld)
each test case (whether a proprty or unit test) is specified by a description of the case and the number of marks in the format %s : %d (e.g. String length <= 5 : 10)
Tests are run on gradescope with output redirected to a file, which is then parsed under the naming conventions and converted to a JSON output file for reporting on gradescope.
-}
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [localOption (mkTimeout 1) smallCheckTests, quickCheckTests, unitTests, bannedFunctions]

smallCheckTests :: TestTree
smallCheckTests = testGroup "SmallCheck Tests"
  [ SC.testProperty "v : String length <= 3 : 5" $  -- should timeout
      \s -> length (take 3 (s :: String)) <= 3
  , SC.testProperty "v : String length <= 2 : 5" $  -- should fail
      \s -> length (take 3 (s :: String)) <= 2
  ]

quickCheckTests :: TestTree
quickCheckTests = testGroup "QuickCheck Tests"
  [ QC.testProperty "v : String length <= 5 : 10" $
      \s -> length (take 5 (s :: String)) <= 5
  , QC.testProperty "v : String length <= 4 : 10" $  -- should fail
      \s -> length (take 5 (s :: String)) <= 4
  ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "v : String comparison 1 : 1" $
      assertEqual "description" "OK" "OK"
  , testCase "Hidden Test : 10" $
      assertEqual "description" "OK" "OK"
  , localOption (mkTimeout 100) (testCase "v : String comparison 2 : 1" $  -- should fail
      assertEqual "description" "fail" "fail!")
  ]

bannedFunctions :: TestTree
bannedFunctions = testGroup "Banned Functions"  
    [ testCase "v : Banned functions : -10" $ do
        mapUse <- bannedFuncs "src/Lib.hs" "Lib" 
            ["map", "foldl", "foldr", "zip"]
        assertEqual "Banned function detection" True mapUse
    ]