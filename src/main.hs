import Graphics.Vty.Widgets.All
import qualified Data.Text as T
import System.Exit ( exitSuccess )
import System.Random
import Engine

newtype GameStateW = GameStateW GameState

newGameState :: GameState -> IO (Widget GameStateW)
newGameState state = do
  let st = GameStateW state
  newWidget st $ \w -> w

setGameState :: Widget GameStateW -> GameState -> IO ()
setGameState wRef val = updateWidgetState wRef $ const $ GameStateW val

getGameState :: Widget GameStateW -> IO GameState
getGameState wRef = do
  GameStateW state <- getState wRef
  return state

main::IO ()
main = do
  -- The game's state.
  rng <- getStdGen
  gameState <- newGameState (initState rng)

  -- The player types here.
  input <- editWidget
  binput <- bordered =<< (return input)

  -- The title of... something?
  title <- plainText (T.pack "Game time!")

  -- The responses will go in here.
  output <- plainText (T.pack "This is where the output goes.")
  boutput <- bordered =<< ((return output) <++> (hFill ' ' 1) <--> (vFill ' '))

  -- The inventory.
  inventory <- plainText (T.pack "This is where the inventory goes.")
  binventory <- bordered =<< (return inventory)

  -- The window where all the descriptive stuff will be.
  stateWindow <- (return boutput) <++> (return binventory)
  ui <- (return title)
        <--> hBorder
        <--> (return stateWindow)
        <--> (return binput)

  fg <- newFocusGroup
  addToFocusGroup fg input

  c <- newCollection
  addToCollection c ui fg

  -- Process input.
  input `onActivate` \this ->
    getEditText this >>=
      ((updateUI output inventory input gameState) .
      (wrapResponse gameState) . 
      T.unpack)

  -- Loop.
  runUi c defaultContext

wrapResponse:: Widget GameStateW -> String -> IO UIResponse
wrapResponse gs cmd = do
  state <- getGameState gs
  return $ getResponse state cmd

updateUI:: Widget FormattedText
        -> Widget FormattedText
        -> Widget Edit
        -> Widget GameStateW
        -> IO UIResponse
        -> IO ()
updateUI desc inv input state response = do
  UIResponse newState dUpdate iUpdate <- response
  setGameState state newState
  updateDescription desc dUpdate
  updateInventory inv iUpdate
  setEditText input (T.pack "")
  return ()

updateDescription::Widget FormattedText -> Maybe UIDescriptionResponse -> IO ()
updateDescription desc Nothing = return ()
updateDescription desc (Just resp) = case resp of
  UIDExit -> shutdownUi
  UIDString s -> setText desc (T.pack s)

updateInventory::Widget FormattedText -> Maybe UIInventoryResponse -> IO ()
updateInventory inv Nothing = return ()
updateInventory inv (Just resp) = case resp of
  UIIString s -> setText inv (T.pack s)
