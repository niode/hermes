module Engine
  ( UIResponse (UIResponse)
  , UIDescriptionResponse (UIDExit, UIDString)
  , UIInventoryResponse (UIIString)
  , GameState
  , getResponse
  , initState) where

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

getResponse::GameState->String->UIResponse
getResponse state "exit" = UIResponse state (Just UIDExit) Nothing
getResponse state cmd = UIResponse state (Just (UIDString cmd)) Nothing

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
