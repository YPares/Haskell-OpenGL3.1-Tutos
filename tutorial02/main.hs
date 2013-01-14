{-# LANGUAGE ImplicitParams, RecordWildCards,
             NoMonomorphismRestriction #-}

module Main where

import qualified Graphics.UI.GLFW as W
import Graphics.Rendering.OpenGL.Raw

import Control.Monad
import Control.Applicative
import Data.Function
import Foreign
import Foreign.C.String

withNewPtr f = alloca (\p -> f p >> peek p)

checkStatus statusFlag glGetFn glInfoLogFn id = do
  let fetch info = withNewPtr (glGetFn id info)
  status <- toBool <$> fetch statusFlag
  logLength <- fetch gl_INFO_LOG_LENGTH
  when (logLength > 0) $
    allocaArray0 (fromIntegral logLength) $ \msgPtr -> do
       glInfoLogFn id logLength nullPtr msgPtr
       peekCString msgPtr >>=
         if status then ?log else ?err
  return status

loadShader shaderTypeFlag filePath = do
  code <- readFile filePath
  id <- glCreateShader shaderTypeFlag
  withCString code $ \codePtr ->
    with codePtr $ \codePtrPtr ->
      glShaderSource id 1 codePtrPtr nullPtr
  ?log $ "Compiling shader: " ++ filePath
  glCompileShader id
  checkStatus gl_COMPILE_STATUS glGetShaderiv glGetShaderInfoLog id
  return id

loadProgram vertFP fragFP = do
  shaderIds <- mapM (uncurry loadShader)
    [(gl_VERTEX_SHADER, vertFP)
    ,(gl_FRAGMENT_SHADER, fragFP)]
  progId <- glCreateProgram
  ?log "Linking program"
  mapM_ (glAttachShader progId) shaderIds
  glLinkProgram progId
  checkStatus gl_LINK_STATUS glGetProgramiv glGetProgramInfoLog progId
  mapM_ glDeleteShader shaderIds
  return progId

data GLIds = GLIds { progId :: !GLuint, vertexArrayId :: !GLuint
                   , vertexBufferId :: !GLuint }

initGLStuff = do
  glClearColor 0 0 0.4 0
  progId <- loadProgram "simple.vert" "simple.frag"
  vertexArrayId <- withNewPtr (glGenVertexArrays 1)
  glBindVertexArray vertexArrayId
  let vertexBufferData = [-1, -1, 0
                         , 1, -1, 0
                         , 0,  1, 0]
  vertexBufferId <- withNewPtr (glGenBuffers 1)
  glBindBuffer gl_ARRAY_BUFFER vertexBufferId
  withArrayLen vertexBufferData $ \length ptr ->
    glBufferData gl_ARRAY_BUFFER (fromIntegral (length *
                                  sizeOf (undefined :: GLfloat)))
                 (ptr :: Ptr GLfloat) gl_STATIC_DRAW
  return GLIds{..}

(<&&>) = liftA2 (&&)

mainLoop GLIds{..} = fix $ \loop -> do
  glClear gl_COLOR_BUFFER_BIT
  glUseProgram progId
  glEnableVertexAttribArray 0  -- 1st attribute: vertices
  glBindBuffer gl_ARRAY_BUFFER vertexBufferId
  glVertexAttribPointer 0  -- attribute 0 in the shader
                        3  -- we draw 3 vertices
                        gl_FLOAT  -- coordinates type
                        (fromBool False)  -- normalized?
                        0  -- stride
                        nullPtr  -- vertex buffer offset
  glDrawArrays gl_TRIANGLES 0 3  -- from 0, 3 vertices
  glDisableVertexAttribArray 0
  W.swapBuffers
  continue <- W.windowIsOpen
              <&&> (not <$> W.keyIsPressed W.KeyEsc)
  when continue loop

cleanUpGLStuff GLIds{..} = do
  with vertexBufferId $ glDeleteBuffers 1
  with vertexArrayId $ glDeleteVertexArrays 1

main = do
  let ?log = putStrLn
      ?err = error
  W.initialize
  success <- W.openWindow $ W.defaultDisplayOptions
    { W.displayOptions_numFsaaSamples = Just 4
    , W.displayOptions_openGLVersion = (3, 1)}
    --, W.displayOptions_openGLProfile = W.CoreProfile }
  when (not success) $ do
    W.terminate
    ?err "GLFW couldn't open a window."
  W.enableKeyRepeat
  ids <- initGLStuff
  mainLoop ids
  cleanUpGLStuff ids
  W.terminate
  ?log "Normal termination."

