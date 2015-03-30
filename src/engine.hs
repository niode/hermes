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
runCommand (AVerb (VerbConst "exit")) = exitFunction
runCommand (AVerb (VerbConst "item")) = itemFunction
runCommand (AVerb (VerbConst "combine")) = combineFunction

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
