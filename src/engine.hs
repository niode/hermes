module Engine
  ( UIResponse (UIResponse)
  , UIDescriptionResponse (UIDExit, UIDString)
  , UIInventoryResponse (UIIString)
  , GameState
  , getResponse
  , initState) where

import Control.Monad.State
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

data GameState = GameState {inventory :: [Item], rng :: StdGen}
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
  \(GameState is rng) -> ((), GameState (item : is) rng)

setInventory::[Item]->State GameState ()
setInventory inv = state $ \(GameState _ rng) -> ((), GameState inv rng)

getInventory::State GameState [Item]
getInventory = state $ \gs -> ((inventory gs), gs)

getRng::State GameState StdGen
getRng = state $ \gs -> ((rng gs), gs)

setRng::StdGen->State GameState ()
setRng rng = state $ \(GameState inv _) -> ((), GameState inv rng)

getNewItem::State GameState Item
getNewItem = do
  rng <- getRng
  let (item, rng') = runState newItem rng
  setRng rng'
  return item

initState :: StdGen -> GameState
initState rng = GameState [baseSystem, baseInventory] rng

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
  return $ sep "\n" desc

combineFunction::CommandFunction
combineFunction = do
  (a:b:inv) <- getInventory
  setInventory $ (combine a b) : inv
  str <- printInventory
  return $ uiInventory str

sep::String->[String]->String
sep s = foldr (\x result -> case result of
  [] -> x
  _  -> x ++ s ++ result) []
