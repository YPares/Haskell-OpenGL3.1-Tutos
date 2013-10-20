{-# LANGUAGE ImplicitParams, RecordWildCards,
             NoMonomorphismRestriction #-}

module Main where

import qualified Graphics.UI.GLFW as W
import Graphics.Rendering.OpenGL.Raw
import Data.Vec

import Codec.Picture
import qualified Data.Vector.Storable as V

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
                   , vertexBufferId :: !GLuint, uvBufferId :: !GLuint
                   , mvpMatrixUniform :: !GLint
                   , vertexAttrib :: !GLuint, uvAttrib :: !GLuint}

initGLStuff = do
  glClearColor 0 0 0.4 0
  progId <- loadProgram "simple.vert" "simple.frag"
  v <- withCString "vertexPosition_modelspace" $ glGetAttribLocation progId
  c <- withCString "vertexUV" $ glGetAttribLocation progId
  m <- withCString "MVP" $ glGetUniformLocation progId
  (vertexAttrib, uvAttrib, mvpMatrixUniform) <- if v < 0 || c < 0 || m < 0
        then error "One of the attribs/uniforms can't be found!"
        else return (fromIntegral v, fromIntegral c, m)
  vertexArrayId <- withNewPtr (glGenVertexArrays 1)
  glBindVertexArray vertexArrayId
  glEnable gl_DEPTH_TEST
  glDepthFunc gl_LESS
  -- Yeah, a cube takes an awful lot of triangles to be defined...
  -- Dirty to stick it right in the middle of the code, but I like my tutos self-contained
  -- (even if, yeah, I know, the shaders code is not in this file)
  let vertexBufferData = [-1.0,-1.0,-1.0, -- triangle 1 : begin
                        -1.0,-1.0, 1.0,
                        -1.0, 1.0, 1.0, -- triangle 1 : end
                        1.0, 1.0,-1.0, -- triangle 2 : begin
                        -1.0,-1.0,-1.0,
                        -1.0, 1.0,-1.0, -- triangle 2 : end
                        1.0,-1.0, 1.0,
                        -1.0,-1.0,-1.0,
                        1.0,-1.0,-1.0,
                        1.0, 1.0,-1.0,
                        1.0,-1.0,-1.0,
                        -1.0,-1.0,-1.0,
                        -1.0,-1.0,-1.0,
                        -1.0, 1.0, 1.0,
                        -1.0, 1.0,-1.0,
                        1.0,-1.0, 1.0,
                        -1.0,-1.0, 1.0,
                        -1.0,-1.0,-1.0,
                        -1.0, 1.0, 1.0,
                        -1.0,-1.0, 1.0,
                        1.0,-1.0, 1.0,
                        1.0, 1.0, 1.0,
                        1.0,-1.0,-1.0,
                        1.0, 1.0,-1.0,
                        1.0,-1.0,-1.0,
                        1.0, 1.0, 1.0,
                        1.0,-1.0, 1.0,
                        1.0, 1.0, 1.0,
                        1.0, 1.0,-1.0,
                        -1.0, 1.0,-1.0,
                        1.0, 1.0, 1.0,
                        -1.0, 1.0,-1.0,
                        -1.0, 1.0, 1.0,
                        1.0, 1.0, 1.0,
                        -1.0, 1.0, 1.0,
                        1.0,-1.0, 1.0]
      vertexUVData = [0.000059, 0.000004,
                      0.000103, 0.336048,
                      0.335973, 0.335903,
                      1.000023, 0.000013,
                      0.667979, 0.335851,
                      0.999958, 0.336064,
                      0.667979, 0.335851,
                      0.336024, 0.671877,
                      0.667969, 0.671889,
                      1.000023, 0.000013,
                      0.668104, 0.000013,
                      0.667979, 0.335851,
                      0.000059, 0.000004,
                      0.335973, 0.335903,
                      0.336098, 0.000071,
                      0.667979, 0.335851,
                      0.335973, 0.335903,
                      0.336024, 0.671877,
                      1.000004, 0.671847,
                      0.999958, 0.336064,
                      0.667979, 0.335851,
                      0.668104, 0.000013,
                      0.335973, 0.335903,
                      0.667979, 0.335851,
                      0.335973, 0.335903,
                      0.668104, 0.000013,
                      0.336098, 0.000071,
                      0.000103, 0.336048,
                      0.000004, 0.671870,
                      0.336024, 0.671877,
                      0.000103, 0.336048,
                      0.336024, 0.671877,
                      0.335973, 0.335903,
                      0.667969, 0.671889,
                      1.000004, 0.671847,
                      0.667979, 0.335851]
  vertexBufferId <- fillNewBuffer vertexBufferData
  uvBufferId <- fillNewBuffer vertexUVData
  textureId <- juicyImageToTexture "uvtemplate.png"
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

newTexture2D :: (Integral a2, Integral a1) => a1 -> a2 -> Ptr a -> IO GLuint
newTexture2D width height ptr = do
  texId <- withNewPtr (glGenTextures 1)
  glBindTexture gl_TEXTURE_2D texId
  glTexImage2D gl_TEXTURE_2D
               0  -- level of detail, used with mipmaps
               (fromIntegral gl_RGB)  -- internal format the
                                      -- texture will have
                                      -- once loaded
               (fromIntegral width)
               (fromIntegral height)
               0  -- border. "This value must be 0". Okay, then.
               gl_RGB  -- format of the image in the file
               gl_UNSIGNED_BYTE  -- Type of each component in
                                 -- the file
               ptr
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER
                  (fromIntegral gl_NEAREST)
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER
                  (fromIntegral gl_NEAREST)
  return texId

juicyImageToTexture filepath = do
  e <- readImage filepath
  case e of
    Left x -> error x
    Right (ImageRGB8 (Image w h v)) ->
      V.unsafeWith v $ newTexture2D w h
    _ -> error "Sorry, only RGB8 for now!"

bindBufferToAttrib bufId attribLoc numComponents = do
  glEnableVertexAttribArray attribLoc
  glBindBuffer gl_ARRAY_BUFFER bufId
  glVertexAttribPointer attribLoc  -- attribute location in the shader
                        numComponents
                        gl_FLOAT  -- coordinates type
                        (fromBool False)  -- normalize?
                        0  -- stride
                        nullPtr  -- vertex buffer offset

mainLoop GLIds{..} = fix $ \loop -> do
  glClear gl_COLOR_BUFFER_BIT
  glUseProgram progId
  with mvpMatrix $ glUniformMatrix4fv mvpMatrixUniform 1 (fromBool True) . castPtr
                                                -- We are ROW-first ^^^
  bindBufferToAttrib vertexBufferId vertexAttrib 3
  bindBufferToAttrib uvBufferId uvAttrib 2
  glDrawArrays gl_TRIANGLES 0 (12*3)
  glDisableVertexAttribArray uvAttrib
  glDisableVertexAttribArray vertexAttrib
  W.swapBuffers
  continue <- W.windowIsOpen
              <&&> (not <$> W.keyIsPressed W.KeyEsc)
  when continue loop

cleanUpGLStuff :: GLIds -> IO ()
cleanUpGLStuff GLIds{..} = do
  with vertexBufferId $ glDeleteBuffers 1
  with uvBufferId $ glDeleteBuffers 1
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

main :: IO ()
main = do
  let ?log = putStrLn
      ?err = error
  W.initialize
  success <- W.openWindow $ W.defaultDisplayOptions
    { W.displayOptions_numFsaaSamples = Just 4}
  when (not success) $ do
    W.terminate
    ?err "GLFW couldn't open a window."
  W.enableKeyRepeat
  ids <- initGLStuff
  mainLoop ids
  cleanUpGLStuff ids
  W.terminate
  ?log "Normal termination."

