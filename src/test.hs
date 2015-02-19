import Graphics.Vty.Widgets.All
import qualified Data.Text as T

data UIDescriptionResponse = UIDExit | UIDString String
data UIInventoryResponse = UIIString String
data UIResponse = UIResponse (Maybe UIDescriptionResponse) (Maybe UIInventoryResponse)

main::IO ()
main = do
  -- The player types here.
  input <- editWidget
  binput <- bordered =<< (return input)

  -- The title of... something?
  title <- plainText (T.pack "Game time!")

  -- The responses will go in here.
  output <- plainText (T.pack "This is where the output goes.")
  boutput <- bordered =<< (return output)

  -- The inventory.
  inventory <- plainText (T.pack "This is where the inventory goes.")
  binventory <- bordered =<< (return inventory)

  -- The window where all the descriptive stuff will be.
  stateWindow <- hBox boutput binventory
  ui <- (return title) <--> hBorder <--> (return stateWindow) <--> (return binput)

  fg <- newFocusGroup
  addToFocusGroup fg input

  c <- newCollection
  addToCollection c ui fg

  -- Process input.
  input `onActivate` \this ->
    getEditText this >>= ((updateUI output inventory input) . getResponse . T.unpack)

  -- Loop.
  runUi c defaultContext

updateUI::Widget FormattedText -> Widget FormattedText -> Widget Edit -> UIResponse -> IO ()
updateUI desc inv input (UIResponse d i) = do
    updateDescription desc d
    updateInventory inv i
    setEditText input (T.pack "")
    return ()

updateDescription::Widget FormattedText -> Maybe UIDescriptionResponse -> IO ()
updateDescription desc Nothing = return ()
updateDescription desc (Just resp) = case resp of
  UIDExit -> error "Game exited."
  UIDString s -> setText desc (T.pack s)

updateInventory::Widget FormattedText -> Maybe UIInventoryResponse -> IO ()
updateInventory inv Nothing = return ()
updateInventory inv (Just resp) = case resp of
  UIIString s -> setText inv (T.pack s)

getResponse::String->UIResponse
getResponse "exit" = UIResponse (Just UIDExit) Nothing
getResponse s = UIResponse (Just (UIDString s)) Nothing
