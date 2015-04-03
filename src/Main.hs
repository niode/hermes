import Graphics.Vty.Widgets.All
import qualified Data.Text as T
import System.Exit ( exitSuccess )
import System.Random
import Control.Monad.State
import Engine

newtype GameStateW = GameStateW GameState

-- Generate a new game state widget.
newGameState :: GameState -> IO (Widget GameStateW)
newGameState state = do
  let st = GameStateW state
  newWidget st $ \w -> w

-- Set the current game state.
setGameState :: Widget GameStateW -> GameState -> IO ()
setGameState wRef val = updateWidgetState wRef $ const $ GameStateW val

-- Get the current game state.
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
  output <- plainText (T.pack "")
  boutput <- bordered =<< ((return output) <++> (hFill ' ' 1) <--> (vFill ' '))
  setTextFormatter output wrap

  -- The inventory.
  inventory <- plainText (T.pack "")
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

  -- Update the interface with the initial state.
  updateUI output inventory input gameState $ wrapResponse gameState ""

  -- Process input.
  input `onActivate` \this ->
    getEditText this >>=
      ((updateUI output inventory input gameState) . -- Update UI.
      (wrapResponse gameState) .  -- Get the engine's response.
      T.unpack) -- Unpack Text.

  -- Loop.
  runUi c defaultContext

-- Wrap the response function in the IO monad.
wrapResponse:: Widget GameStateW -> String -> IO (UIResponse, GameState)
wrapResponse gs cmd = do
  st <- getGameState gs
  return $ runState (getResponse cmd) st -- Run in the State GameState monad.

-- Update the UI (requires the description, inventory, input and game state).
updateUI:: Widget FormattedText
        -> Widget FormattedText
        -> Widget Edit
        -> Widget GameStateW
        -> IO (UIResponse, GameState)
        -> IO ()
updateUI desc inv input state response = do
  (UIResponse dUpdate iUpdate, newState) <- response
  setGameState state newState
  updateDescription desc dUpdate
  updateInventory inv iUpdate
  setEditText input (T.pack "")
  return ()

-- Update the description widget with the UIDescriptionResponse.
updateDescription::Widget FormattedText -> Maybe UIDescriptionResponse -> IO ()
updateDescription desc Nothing = return ()
updateDescription desc (Just resp) = case resp of
  UIDExit -> shutdownUi
  UIDString s -> setText desc (T.pack s)

-- Update the inventory widget with the UIInventoryResponse.
updateInventory::Widget FormattedText -> Maybe UIInventoryResponse -> IO ()
updateInventory inv Nothing = return ()
updateInventory inv (Just resp) = case resp of
  UIIString s -> setText inv (T.pack s)
