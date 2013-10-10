{-# LANGUAGE ImplicitParams, RecordWildCards,
             NoMonomorphismRestriction #-}

module Main where

import qualified Graphics.UI.GLFW as W
import Graphics.Rendering.OpenGL.Raw
import Data.Vec

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
                   , vertexBufferId :: !GLuint, colorBufferId :: !GLuint
                   , mvpMatrixUniform :: !GLint
                   , vertexAttrib :: !GLuint, colorAttrib :: !GLuint}

initGLStuff = do
  glClearColor 0 0 0.4 0
  progId <- loadProgram "simple.vert" "simple.frag"
  v <- withCString "vertexPosition_modelspace" $ glGetAttribLocation progId
  c <- withCString "vertexColor" $ glGetAttribLocation progId
  m <- withCString "MVP" $ glGetUniformLocation progId
  (vertexAttrib, colorAttrib, mvpMatrixUniform) <- if v < 0 || c < 0 || m < 0
        then error "One of the attribs/uniforms can't be found!"
        else return (fromIntegral v, fromIntegral c, m)
  vertexArrayId <- withNewPtr (glGenVertexArrays 1)
  glBindVertexArray vertexArrayId
  let vertexBufferData = [-1, -1, 0
                         , 1, -1, 0
                         , 0,  1, 0]
      colorBufferData  = [1,0,0
                         ,0,1,0
                         ,0,0,1]
  vertexBufferId <- fillNewBuffer vertexBufferData
  colorBufferId <- fillNewBuffer colorBufferData
  return GLIds{..}

fillNewBuffer list = do
  bufId <- withNewPtr (glGenBuffers 1)
  glBindBuffer gl_ARRAY_BUFFER bufId
  withArrayLen list $ \length ptr ->
    glBufferData gl_ARRAY_BUFFER (fromIntegral (length *
                                  sizeOf (undefined :: GLfloat)))
                 (ptr :: Ptr GLfloat) gl_STATIC_DRAW
  return bufId

(<&&>) = liftA2 (&&)

bindBufferToAttrib bufId attribLoc = do
  glEnableVertexAttribArray attribLoc
  glBindBuffer gl_ARRAY_BUFFER bufId
  glVertexAttribPointer attribLoc  -- attribute location in the shader
                        3  -- 3 components per vertex
                        gl_FLOAT  -- coordinates type
                        (fromBool False)  -- normalize?
                        0  -- stride
                        nullPtr  -- vertex buffer offset

mainLoop GLIds{..} = fix $ \loop -> do
  glClear gl_COLOR_BUFFER_BIT
  glUseProgram progId
  with mvpMatrix $ glUniformMatrix4fv mvpMatrixUniform 1 (fromBool True) . castPtr
                                                -- We are ROW-first ^^^
  bindBufferToAttrib vertexBufferId vertexAttrib
  bindBufferToAttrib colorBufferId colorAttrib
  glDrawArrays gl_TRIANGLES 0 3  -- from 0, 3 vertices
  glDisableVertexAttribArray colorAttrib
  glDisableVertexAttribArray vertexAttrib
  W.swapBuffers
  continue <- W.windowIsOpen
              <&&> (not <$> W.keyIsPressed W.KeyEsc)
  when continue loop

cleanUpGLStuff GLIds{..} = do
  with vertexBufferId $ glDeleteBuffers 1
  with colorBufferId $ glDeleteBuffers 1
  with vertexArrayId $ glDeleteVertexArrays 1

vec3 x y z = x:.y:.z:.()

mvpMatrix :: Mat44 GLfloat
mvpMatrix = projection `multmm` view `multmm` model
  where projection = perspective 0.1 100 (pi/4) (4/3)
        view       = lookAt (vec3 0 1 0) (vec3 4 3 3) (vec3 0 0 0)
        model      = identity

-- Data.Vec rotationLookAt does not work exactly like I want,
-- temporary recoding
lookAt :: Floating a => Vec3 a -> Vec3 a -> Vec3 a -> Mat44 a
lookAt up eye target = x :. y :. (-z) :. h :. ()
  where
   f = normalize $ target - eye
   s = normalize $ f `cross` normalize up
   u = s `cross` f
   x = homVec s
   y = homVec u
   z = f `snoc` (-(f `dot` eye))
   h = vec3 (-(s `dot` eye))  (-(u `dot` eye)) 0 `snoc` 1

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

