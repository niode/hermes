import Graphics.Vty.Widgets.All
import qualified Data.Text as T

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
    getEditText this >>= ((printResponse output) . getResponse . T.unpack)

  -- Loop.
  runUi c defaultContext

printResponse::Widget FormattedText -> String ->IO ()
printResponse w "exit" = error "Game exited."
printResponse w s = setText w (T.pack s)

getResponse::String->String
getResponse _ = "You are a bad player."
