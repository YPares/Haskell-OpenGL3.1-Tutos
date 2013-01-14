import qualified Graphics.UI.GLFW as W
import Graphics.Rendering.OpenGL.Raw

import Control.Monad
import Control.Applicative

main = do
  W.initialize
  success <- W.openWindow $ W.defaultDisplayOptions
    { W.displayOptions_numFsaaSamples = Just 4 }
  if not success then close "GLFW couldn't open a window."
    else do W.enableKeyRepeat
            glClearColor 0 0 0.4 0
            mainLoop
  close "Normal termination."
  where close = (W.terminate >>) . putStrLn

(<&&>) = liftA2 (&&)

mainLoop = do
  glClear gl_COLOR_BUFFER_BIT
  W.swapBuffers
  continue <- W.windowIsOpen
              <&&> (not <$> W.keyIsPressed W.KeyEsc)
  when continue mainLoop

