module Engine.Internal where

import Control.Monad.State
import Data.List
import Data.Maybe
import System.Random
import Parser
import Items
import Format
import Story
import Data.Set as Set (fromList, toList)

data UIDescriptionResponse
  = UIDExit
  | UIDString String

data UIInventoryResponse
  = UIIString String

data UIResponse
  = UIResponse (Maybe UIDescriptionResponse) (Maybe UIInventoryResponse)

data GameState = GameState  { inventory :: [Item]
                            , rng :: StdGen
                            , events :: [Event]}

type CommandFunction = State GameState UIResponse
data Command = Command {names :: [String], function :: CommandFunction}

initState :: StdGen -> GameState
initState rng = GameState [baseSystem, baseInventory] rng [StoryEvent "intro"]

--------------------------------------------------------------------------------
-- Initial game state
--------------------------------------------------------------------------------
baseInventory = [Upgrade, Magnify 10, Carry]
baseSystem = [System]

-- This is exposed to the UI code ----------------------------------------------
getResponse::String->State GameState UIResponse
getResponse cmd = do
  let command = parseCommand cmd
  resp <- runCommand command
  processEvents resp

--------------------------------------------------------------------------------
-- Internal functions
--------------------------------------------------------------------------------

uiDescription::String->UIResponse
uiDescription s = UIResponse (Just (UIDString s)) Nothing

uiInventory::String->UIResponse
uiInventory s = UIResponse Nothing (Just (UIIString s))

uiResponse::String->String->UIResponse
uiResponse d i = UIResponse (Just (UIDString d)) (Just (UIIString i))

putInventory::Item->State GameState ()
putInventory item = state $
  \(GameState is rng events) -> ((), GameState (item : is) rng events)

setInventory::[Item]->State GameState ()
setInventory inv = state $
  \(GameState _ rng events) -> ((), GameState inv rng events)

getInventory::State GameState [Item]
getInventory = state $ \gs -> ((inventory gs), gs)

getRng::State GameState StdGen
getRng = state $ \gs -> ((rng gs), gs)

setRng::StdGen->State GameState ()
setRng rng = state $ \(GameState inv _ events) -> ((), GameState inv rng events)

-- Get an item with the given name.
-- (Actually gets the first item because we don't need this yet.)
getItem::String->State GameState (Maybe Item)
getItem name = do
  inv <- getInventory
  if (length inv) > 0
    then return $ Just (inv !! 0)
    else return Nothing

-- Get the nth item.
getItemI::Int->State GameState (Maybe Item)
getItemI n = do
  inv <- getInventory
  if n >= 0 && n < (length inv)
    then return $ Just $ inv !! n
    else return Nothing

setEvents::[Event] -> State GameState ()
setEvents ev = state $
  \(GameState is rng _) -> ((), GameState is rng ev)

putEvent::Event->State GameState ()
putEvent event = state $
  \(GameState is rng es) -> ((), GameState is rng (event:es))

getEvents::State GameState [Event]
getEvents = state $ \gs -> ((events gs), gs)

getNewItem::State GameState Item
getNewItem = do
  rng <- getRng
  let (item, rng') = runState newItem rng
  setRng rng'
  return item

-- Calculate the description of the events.
processEvents::UIResponse->State GameState UIResponse
processEvents old = do
  list <- getEvents
  let new = foldr Engine.Internal.appendDescription old $ mapMaybe story list
  setEvents [] -- Clear the events.
  return new

-- Append a description to an existing one.
appendDescription::String->UIResponse->UIResponse

-- Update existing description.
appendDescription new (UIResponse (Just (UIDString old)) inv) =
  UIResponse (Just (UIDString (Format.appendDescription old new))) inv

-- Add new description.
appendDescription new (UIResponse Nothing inv) =
  UIResponse (Just (UIDString new)) inv

appendDescription _ resp = resp

--------------------------------------------------------------------------------
-- Parse the syntax tree from the parser
--------------------------------------------------------------------------------
runCommand::Action->State GameState UIResponse

-- Null command.
runCommand ANull = nullFunction

-- Regular command.
runCommand (AVerb (VerbConst s)) = constFunction s
runCommand (AVerb (With string noun)) = withFunction string noun
runCommand (AVerb (Use noun)) = useFunction noun
runCommand (AVerb (UseTarget noun prep noun')) =
  useTargetFunction noun prep noun'

runCommand (AVerb (Do noun prep noun')) = doFunction noun prep noun'
runCommand (AVerb (Apply noun prep noun')) = applyFunction noun prep noun'

-- Parsing error.
runCommand (AError s) = unknownFunction s
runCommand _ = noneFunction

-- Null function : update the inventory.
nullFunction::State GameState UIResponse
nullFunction = do
  items <- getInventory
  return $ uiInventory $ printInventory items

-- "Constant" function (i.e. no arguments).
constFunction::String->State GameState UIResponse
constFunction s = constCommand s >>= id -- Magic!

-- "Constant" command. Find a command function that matches the string.
constCommand::String->State GameState CommandFunction
constCommand s = do
  inv <- getInventory
  let matches = filter (commandFilter s) $ listConstCommands inv
  case matches of
    []    -> return $ unknownFunction s
    (c:_) -> return $ function c

-- List all "constant" command names available.
listCommandNames = toList -- Pull out of set.
    . fromList -- Put in set to remove duplicates.
    . concat
    . (map (moduleFold ((foldr f []) . constItemCommands)))
  where f (Command names _) ls = names ++ ls

-- List all "constant" commands available.
listConstCommands::[Item]->[Command]
listConstCommands =  concat . (map (moduleFold constItemCommands))

-- Check if any names for a command match a string.
commandFilter::String->Command->Bool
commandFilter s c = any (\name -> name == s) (names c)

withFunction::String->Noun->State GameState UIResponse
withFunction string (NounConst noun) = return $
  uiDescription (string ++ " with " ++ noun)

useFunction::Noun->State GameState UIResponse
useFunction (NounConst noun) = do
  item <- getItem noun
  return $ uiDescription $ "using " ++ noun

useTargetFunction::Noun->Preposition->Noun->State GameState UIResponse
useTargetFunction (NounConst n) _ (NounConst m) = return $
  uiDescription "whatever, frig"

doFunction::Noun->Preposition->Noun->State GameState UIResponse
doFunction (NounConst n) _ (NounConst m) = return $
  uiDescription "holy hell too much to do"

applyFunction::Noun->Preposition->Noun->State GameState UIResponse
applyFunction (NounConst n) _ (NounConst m) = return $
  uiDescription "got a lot on my plate"

-- Gets a list of items whose description matches the string.
getNoun::Noun->State GameState [Item]
getNoun (NounConst s) = do
  inv <- getInventory
  return $ itemMatches s inv

-- Returns a list of items whose description matches the string.
itemMatches::String->[Item]->[Item]
itemMatches s = mapMaybe (\item ->
  do desc <- describe item; if (isInfixOf s desc) then Just item else Nothing)

--------------------------------------------------------------------------------
-- Item commands
--------------------------------------------------------------------------------

-- Collect all ("constant") commands available through the item.
constItemCommands::([Module], Module, [Module]) -> [Command]
constItemCommands (p, m, ms) = case (p, m, ms) of
  (_, Walk props, _) -> [walkCommand props]
  (_, Examine props, _) -> [examineCommand props]
  (_, Carry, _) -> [itemCommand, combineCommand]
  (_, System, _) -> [exitCommand]
  _ -> []

walkCommand::LWalkProp->Command
walkCommand props = Command ["walk", "go", "run"] (walkFunction props)

walkFunction::LWalkProp->CommandFunction
walkFunction props = return $ uiDescription "But where?"

examineCommand::LExamineProp->Command
examineCommand props = Command ["look"] (examineFunction props)

examineFunction::LExamineProp->CommandFunction
examineFunction props = return $ uiDescription "You take a look around."

--------------------------------------------------------------------------------
-- System commands
--------------------------------------------------------------------------------

noneFunction::CommandFunction
noneFunction = return $ uiDescription $ "not implemented"

unknownCommand::String->Command
unknownCommand s = Command [] $ unknownFunction s

unknownFunction::String->CommandFunction
unknownFunction s = do
  inv <- getInventory
  let list = (numberList . listCommandNames) inv
  return $ uiDescription $
    "You don't know how to \"" ++ s ++ "\". Try:\n" ++ list
  where names = concat . (map (\x -> case x of (Command ls _) -> ls))

exitCommand::Command
exitCommand = Command ["exit"] exitFunction

exitFunction::CommandFunction
exitFunction = return $ UIResponse (Just UIDExit) Nothing

itemCommand::Command
itemCommand = Command ["item"] itemFunction

-- Create a new random item.
itemFunction::CommandFunction
itemFunction = do
  item <- getNewItem
  putInventory item
  inv <- getInventory
  return $ uiInventory (printInventory inv)

printInventory::[Item]->String
printInventory = numberList . (mapMaybe describe)

combineCommand::Command
combineCommand = Command ["combine"] combineFunction

-- Combine the top two items in the inventory.
combineFunction::CommandFunction
combineFunction = do
  old <- getInventory
  case old of
    (a:b:inv) -> do
      setInventory $ (combine a b) : inv
      new <- getInventory
      return $ uiInventory $ printInventory new
    _ -> return $ uiDescription "You do not have enough items."
