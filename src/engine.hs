module Engine
  ( UIResponse (UIResponse)
  , UIDescriptionResponse (UIDExit, UIDString)
  , UIInventoryResponse (UIIString)
  , GameState
  , getResponse
  , initState) where

import Control.Monad.State
import Data.List
import System.Random
import Parser
import Items

data UIDescriptionResponse
  = UIDExit
  | UIDString String

data UIInventoryResponse
  = UIIString String

data UIResponse
  = UIResponse (Maybe UIDescriptionResponse) (Maybe UIInventoryResponse)

data Event
  = ItemPickup Item String
  | ItemDrop Item String

instance Eq Event where
  (ItemPickup _ s) == (ItemPickup _ t) = s == t
  (ItemDrop _ s) == (ItemDrop _ t) = s == t
  _ == _ = False

-- Get the event's name
eventName::Event->String
eventName (ItemPickup _ s) = s
eventName (ItemDrop _ s) = s

data GameState = GameState  { inventory :: [Item]
                            , rng :: StdGen
                            , events :: [Event]}

type CommandFunction = State GameState UIResponse
data Command = Command {names :: [String], function :: CommandFunction}

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

getItem::String->State GameState (Maybe Item)
getItem name = do
  inv <- getInventory
  if (length inv) > 0
    then return $ Just (inv !! 0)
    else return Nothing

getItemI::Int->State GameState (Maybe Item)
getItemI n = do
  inv <- getInventory
  if n >= 0 && n < (length inv)
    then return $ Just $ inv !! n
    else return Nothing

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

initState :: StdGen -> GameState
initState rng = GameState [baseSystem, baseInventory] rng []

baseInventory = [Upgrade, Magnify 10, Carry]
baseSystem = [System]

-- This is exposed to the UI code ----------------------------------------------
getResponse::String->State GameState UIResponse
getResponse cmd = do
  let command = parseCommand cmd
  runCommand command

-- Read the syntax tree
runCommand::Action->State GameState UIResponse
runCommand (AVerb (VerbConst s)) = constFunction s
runCommand (AVerb (With string noun)) = withFunction string noun
runCommand (AVerb (Use noun)) = useFunction noun
runCommand (AVerb (UseTarget noun prep noun')) =
  useTargetFunction noun prep noun'

runCommand (AVerb (Do noun prep noun')) = doFunction noun prep noun'
runCommand (AVerb (Apply noun prep noun')) = applyFunction noun prep noun'

constFunction::String->State GameState UIResponse
constFunction "exit" = exitFunction
constFunction "item" = itemFunction
constFunction "combine" = combineFunction
constFunction s = return $ uiDescription $ "command " ++ s

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

--------------------------------------------------------------------------------
-- Item commands
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- System commands
--------------------------------------------------------------------------------
exitCommand::Command
exitCommand = Command ["exit"] exitFunction

exitFunction::CommandFunction
exitFunction = return $ UIResponse (Just UIDExit) Nothing

itemCommand::Command
itemCommand = Command ["item"] itemFunction

itemFunction::CommandFunction
itemFunction = do
  item <- getNewItem
  putInventory item
  inv <- printInventory
  return $ uiInventory inv

printInventory::State GameState String
printInventory = do
  items <- getInventory
  let desc = descriptions items
  return $ (concat . (intersperse "\n")) desc

combineFunction::CommandFunction
combineFunction = do
  (a:b:inv) <- getInventory
  setInventory $ (combine a b) : inv
  str <- printInventory
  return $ uiInventory str
