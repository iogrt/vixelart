module Main (main, handleKeys, handleUpdate) where
import Graphics.Gloss
import Data.Word
import Graphics.Gloss.Interface.Pure.Game
import Data.Matrix
import Model
import Render
import Data.Bifunctor (first,second)

handleKeys :: Event -> AppState -> AppState
handleKeys (EventKey (Char k) keyState _ _) appState =
  appState
  { inputState = handlekeyboard (inputState appState) k
  }
  where
    activate = keyState == Down
    activeDir = if activate then 1 else -1
    handlekeyboard oldInputState k =
      case k of
        'h' -> oldInputState
          { moveDirection = first (-activeDir +) (moveDirection oldInputState ) }
        'j' -> oldInputState
          { moveDirection = second (-activeDir +) (moveDirection oldInputState )}
        'k' -> oldInputState
          { moveDirection = second (activeDir +) (moveDirection oldInputState )}
        'l' -> oldInputState
          { moveDirection = first (activeDir +) (moveDirection oldInputState )}
        'z' -> oldInputState
          { isPainting = activate}
        _   -> oldInputState

handleKeys _ b = b

tupleTransform :: (a -> b -> c) -> (a,a) -> (b,b) -> (c,c)
tupleTransform f (a1,a2) (b1,b2) =
  (f a1 b1, f a2 b2)

handleUpdate :: Float -> AppState -> AppState
handleUpdate _ appState@AppState{inputState=input} =
  appState
  { cursor = newCursor
  , painting = if not (isPainting input) then painting appState else
      paintPixels red [newCursor] (painting appState)
  }
  where
    newCursor = tupleTransform (+) (moveDirection input) (cursor appState)

main :: IO ()
main =
  let
    appState = initialState
    RenderOptions {canvasSize = windowSize} = renderOptions appState
    window = InWindow "float" windowSize (500,500)
  in play window white 8 appState renderFn handleKeys handleUpdate
