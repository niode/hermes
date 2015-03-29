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
  = UIResponse GameState (Maybe UIDescriptionResponse) (Maybe UIInventoryResponse)

data GameState = GameState {inventory :: [Item], rng :: StdGen}
type CommandFunction = (GameState -> GameState)
data Command = Command {names :: [String], function :: CommandFunction}

putInventory::Item->State GameState ()
putInventory item = state $
  \(GameState is rng) -> ((), GameState (item : is) rng)

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

getResponse::GameState->String->UIResponse
getResponse st cmd = fst $ runState (getResponseFromState cmd) st

getResponseFromState::String->State GameState UIResponse
getResponseFromState cmd = do
  st <- get
  if cmd == "exit"
    then return $ UIResponse st (Just UIDExit) Nothing
    else return $ UIResponse st (Just (UIDString cmd)) Nothing


initState :: StdGen -> GameState
initState rng = GameState [] rng

baseInventory = Item [Upgrade, Magnify 10, Carry]
baseSystem = Item [System]

{-
matchCommand::String->Command
matchCommand = Command [] (CommandFunction (\x -> x))

collectCommand::[Item]->[(Item, Command)]
collectCommand items = []
-}
