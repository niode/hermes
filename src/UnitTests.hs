module UnitTests where

import Test.HUnit
import Parser.Internal
import Items.Internal
import Engine.Internal
import System.Random
import Story

--run all tests
main = runTestTT allTests

--list of all tests
allTests = TestList [
  TestLabel "parseCommandAVerbTest" parseCommandAVerbTest,
  TestLabel "parseCommandAVerbTest2" parseCommandAVerbTest2,
  TestLabel "combineTest" combineTest,
  TestLabel "nounifyTest" nounifyTest,
  TestLabel "describeTest" describeTest,
  TestLabel "describeTest2" describeTest2,
  TestLabel "describeTest3" describeTest3,
  TestLabel "describeTest4" describeTest4,
  TestLabel "describeTest5" describeTest5,
  TestLabel "describeTest6" describeTest6,
  TestLabel "describeTest7" describeTest7,
  TestLabel "describeTest8" describeTest8,
  TestLabel "describeTest8" describeTest9,
  TestLabel "describeTest8" describeTest10
  
  ]

--TestLabel "initStateTest" initStateTest
--TestLabel "eventNameTest" eventNameTest
--


--individual tests. Format is: assertEqual <Error message> <Expected> <Actual>

--parser tests
parseCommandAVerbTest = TestCase $ assertEqual
  "Should get a Verb Action from parseCommand" (AVerb (Do (NounConst "thing") POn (NounConst "that"))) (parseCommand "do thing on that")

parseCommandAVerbTest2 = TestCase $ assertEqual
  "Should get a Verb Action from parseCommand" (AVerb (UseTarget (NounConst "FlashDrive") POn (NounConst "self"))) (parseCommand "use FlashDrive on self")

--engine tests
runCommandTest = TestCase $ assertEqual
  "Should return State GameState UIResponse" (Nothing) (runCommand (AVerb (Do (NounConst "thing") POn (NounConst "that"))))


--uiDescriptionTest = TestCase $ assertEqual
--  "Should return a ui description" (Just "not correct") (uiDescription (UIDString "test"))

--getResponseTest = TestCase $ assertEqual
--  "Incorrect response" () (getResponse "do thing on that")

--eventNameTest = TestCase $ assertEqual
--  "Should return event's name" ("test") (eventName ItemPickup ([Fly]) ("test1") Item [Fly] "test")

--initStateTest = TestCase $ assertEqual
--  "Incorrect initState" (GameState [baseSystem, baseInventory] (mkStdGen 42) [StoryEvent "intro"]) (initState (mkStdGen 42))



--items tests
describeTest = TestCase $ assertEqual
  "Should return an Item description" (Just "grabber") (describe [Grasp])

describeTest2 = TestCase $ assertEqual
  "Should return an Item description" (Just "lighter-than-air fins") (describe [Fly])

describeTest3 = TestCase $ assertEqual
  "Should return an Item description" (Just "pouch") (describe [Carry])

describeTest4 = TestCase $ assertEqual
  "Should return an Item description" (Nothing) (describe [Explode])

describeTest5 = TestCase $ assertEqual
  "Should return an Item description" (Nothing) (describe [Fire])

describeTest6 = TestCase $ assertEqual
  "Should return an Item description" (Nothing) (describe [Illuminate])

describeTest7 = TestCase $ assertEqual
  "Should return an Item description" (Nothing) (describe [Upgrade])

describeTest8 = TestCase $ assertEqual
  "Should return an Item description" (Nothing) (describe [Expend 2])

describeTest9 = TestCase $ assertEqual
  "Should return an Item description" (Nothing) (describe [Break])

describeTest10 = TestCase $ assertEqual
  "Should return an Item description" (Nothing) (describe [System])

combineTest = TestCase $ assertEqual
  "Should return an Item" ([Magnify 2,Fly,Fly,Rad,Rad]) (combine [Fly, Rad] [Fly, Rad])

nounifyTest = TestCase $ assertEqual
  "Should return a list of Strings" (["fins", ""]) (nounify ([Fly], Fly, [Fly]))



{- not working
parseCommandATargetTest = TestCase $ assertEqual
  "Should get a Target Action from parseCommand" (ATarget (Use (NounConst "sword")) POn (NounConst "rock")) (parseCommand "use sword on rock")

parseCommandAConjunctionTest = TestCase $ assertEqual
  "Should get a Conjunction Action from parseCommand" (AConjunction (AVerb (VerbConst "run")) (AVerb (VerbConst "hide"))) (parseCommand "run and hide")

-}
