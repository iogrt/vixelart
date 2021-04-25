{-# LANGUAGE NamedFieldPuns #-}
module Main (main, handleKeys, handleUpdate) where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model
import Render
import Transfer(exportPainting, importPainting)
import qualified Data.ByteString as ByteStr(writeFile, readFile)
import Data.Bifunctor (first,second)
import System.Exit

  -- do
    -- print k
    -- return initialState
  --(Char k)
handleKeys :: Event -> AppState -> IO AppState
handleKeys (EventKey k keyState _ _) appState =
  case (inputState appState,keyState) of
    (InputInsert i,_) -> insertModeHandler i
    (InputEx i, Down) -> exModeHandler i
    _ -> return appState
  where
    activate = keyState == Down
    insertModeHandler input =
      return appState { inputState = inputState' }
      where
        activeDir = if activate then 1 else -1
        inputState' =case k of
          Char 'h' -> InputInsert input
            { moveDirection = first (-activeDir +) (moveDirection input ) }
          Char 'j' -> InputInsert input
            { moveDirection = second (-activeDir +) (moveDirection input )}
          Char 'k' -> InputInsert input
            { moveDirection = second (activeDir +) (moveDirection input )}
          Char 'l' -> InputInsert input
            { moveDirection = first (activeDir +) (moveDirection input )}
          Char 'z' -> InputInsert input
            { isPainting = activate}
          Char ':' -> InputEx (ExMode "")
          _   -> InputInsert input
    exModeHandler input =
      case k of
       SpecialKey KeyEnter -> do
         print "enter"
         executeEx appState
       -- backspace key
       Char '\b' ->
         return appState
           { inputState = InputEx $ ExMode $ popMsg msg
           }
         where
           popMsg "" = ""
           popMsg str = init str
       Char letter ->
         return appState
           { inputState = InputEx $ ExMode (msg ++ [letter])}
       _ -> return appState
         { inputState = InputEx input }
      where
        (ExMode msg) = input
handleKeys _ b = return b

executeEx :: AppState -> IO AppState
executeEx appState =
  case command of
    "w" -> do
      print (painting appState)
      ByteStr.writeFile "art.save" (exportPainting (painting appState))
      return appState {inputState = defaultInput}
    "e" -> do
      fileContents <- ByteStr.readFile "art.save"
      return appState
        {inputState = defaultInput
        , painting = importPainting paintingWidth paintingHeight fileContents
        }
    "q" -> exitSuccess
    _ -> return appState
  where
    (InputEx (ExMode command)) = inputState appState

tupleTransform :: (a -> b -> c) -> (a,a) -> (b,b) -> (c,c)
tupleTransform f (a1,a2) (b1,b2) =
  (f a1 b1, f a2 b2)

handleUpdate :: Float -> AppState -> AppState
handleUpdate _ appState@AppState{inputState} =
  case inputState of
    InputInsert input ->
      appState
      { cursor = newCursor
      , painting = if not (isPainting input) then painting appState else
          paintPixels red (painting appState) [newCursor]
      }
      where
        newCursor = tupleTransform (+) (moveDirection input) (cursor appState)
    _ -> appState

main :: IO ()
main =
  let
    appState = initialState
    RenderOptions {canvasSize = windowSize} = renderOptions appState
    window = InWindow "float" windowSize (500,500)
  in playIO window white 8 appState
     (return . renderFn)
     handleKeys
     (return ... handleUpdate)
