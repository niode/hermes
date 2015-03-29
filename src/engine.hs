module Engine
  ( UIResponse (UIResponse)
  , UIDescriptionResponse (UIDExit, UIDString)
  , UIInventoryResponse (UIIString)
  , GameState
  , getResponse
  , initState) where

import Parser
import Items

data UIDescriptionResponse
  = UIDExit
  | UIDString String

data UIInventoryResponse
  = UIIString String
data UIResponse
  = UIResponse GameState (Maybe UIDescriptionResponse) (Maybe UIInventoryResponse)

data GameState = GameState {inventory :: [Item]}
type CommandFunction = (GameState -> GameState)
data Command = Command {names :: [String], function :: CommandFunction}

getResponse::String->UIResponse
getResponse "exit" = UIResponse initState (Just UIDExit) Nothing
getResponse s = UIResponse initState (Just (UIDString s)) Nothing

initState = GameState []
baseInventory = Item [Upgrade, Magnify 10, Carry]
baseSystem = Item [System]

{-
matchCommand::String->Command
matchCommand = Command [] (CommandFunction (\x -> x))

collectCommand::[Item]->[(Item, Command)]
collectCommand items = []
-}
