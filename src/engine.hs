module Engine
  ( UIResponse (UIResponse)
  , UIDescriptionResponse (UIDExit, UIDString)
  , UIInventoryResponse (UIIString)
  , getResponse) where

import Parser
import Items

data UIDescriptionResponse
  = UIDExit
  | UIDString String

data UIInventoryResponse
  = UIIString String
data UIResponse
  = UIResponse (Maybe UIDescriptionResponse) (Maybe UIInventoryResponse)

getResponse::String->UIResponse
getResponse "exit" = UIResponse (Just UIDExit) Nothing
getResponse s = UIResponse (Just (UIDString s)) Nothing
