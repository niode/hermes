module UnitTests where

import Test.HUnit
import Parser
import Items
import Engine


--run all tests
main = runTestTT allTests

--list of all tests
allTests = TestList [
  TestLabel "parseCommandAVerbTest" parseCommandAVerbTest,
  TestLabel "parseCommandAVerbTest2" parseCommandAVerbTest2
  
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
--getResponseTest = TestCase $ assertEqual
--  "Incorrect response" () (getResponse "do thing on that")

--eventNameTest = TestCase $ assertEqual
--  "Should return event's name" ("test") (eventName ItemPickup ([Fly]) ("test1") Item [Fly] "test")

--initStateTest = TestCase $ assertEqual
--  "Incorrect initState" (GameState) (initState StdGen)



--items tests
--alternateTest = TestCase $ assertEqual
-- "Proper alternate" ([4,1,5,2,6,3]) (alternate [1,2,3] [4,5,6])

--combineTest = TestCase $ assertEqual
--  "Should return an Item" ([Fly,Rad,Fly,Rad]) (combine [Fly, Rad] [Fly, Rad])

--nounifyTest = TestCase $ assertEqual
--  "Should return a list of Strings" (["fins", ""]) (nounify ([Fly], Fly, [Fly]))



{- not working
parseCommandATargetTest = TestCase $ assertEqual
  "Should get a Target Action from parseCommand" (ATarget (Use (NounConst "sword")) POn (NounConst "rock")) (parseCommand "use sword on rock")

parseCommandAConjunctionTest = TestCase $ assertEqual
  "Should get a Conjunction Action from parseCommand" (AConjunction (AVerb (VerbConst "run")) (AVerb (VerbConst "hide"))) (parseCommand "run and hide")

-}
