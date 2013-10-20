{-# LANGUAGE ImplicitParams, RecordWildCards,
             NoMonomorphismRestriction #-}

module Main where

import qualified Graphics.UI.GLFW as W
import qualified Graphics.Rendering.OpenGL.Raw as G
import Data.Vec

import Codec.Picture
import qualified Data.Vector.Storable as V

import Control.Monad
import Control.Applicative
import Data.Function
import Foreign
import Foreign.C.String
import Foreign.C

withNewPtr :: Storable b => (Ptr b -> IO a) -> IO b
withNewPtr f = alloca (\p -> f p >> peek p)

checkStatus :: (?log::String -> IO (), ?err::String -> IO (), Integral a1, Storable a1) => G.GLenum -> (t -> G.GLenum -> Ptr a1 -> IO a) -> (t -> a1 -> Ptr a3 -> Ptr CChar -> IO a2) -> t -> IO Bool
checkStatus statusFlag glGetFn glInfoLogFn id = do
  let fetch info = withNewPtr (glGetFn id info)
  status <- toBool <$> fetch statusFlag
  logLength <- fetch G.gl_INFO_LOG_LENGTH
  when (logLength > 0) $
    allocaArray0 (fromIntegral logLength) $ \msgPtr -> do
       glInfoLogFn id logLength nullPtr msgPtr
       peekCString msgPtr >>=
         if status then ?log else ?err
  return status

loadShader :: (?log::String -> IO (), ?err::String -> IO ()) => G.GLenum -> FilePath -> IO G.GLuint
loadShader shaderTypeFlag filePath = do
  code <- readFile filePath
  id <- G.glCreateShader shaderTypeFlag
  withCString code $ \codePtr ->
    with codePtr $ \codePtrPtr ->
      G.glShaderSource id 1 codePtrPtr nullPtr
  ?log $ "Compiling shader: " ++ filePath
  G.glCompileShader id
  checkStatus G.gl_COMPILE_STATUS G.glGetShaderiv G.glGetShaderInfoLog id
  return id

loadProgram :: (?log::String -> IO (), ?err::String -> IO ()) => FilePath -> FilePath -> IO G.GLuint
loadProgram vertFP fragFP = do
  shaderIds <- mapM (uncurry loadShader)
    [(G.gl_VERTEX_SHADER, vertFP)
    ,(G.gl_FRAGMENT_SHADER, fragFP)]
  progId <- G.glCreateProgram
  ?log "Linking program"
  mapM_ (G.glAttachShader progId) shaderIds
  G.glLinkProgram progId
  checkStatus G.gl_LINK_STATUS G.glGetProgramiv G.glGetProgramInfoLog progId
  mapM_ G.glDeleteShader shaderIds
  return progId

data GLIds = GLIds { progId :: !G.GLuint, vertexArrayId :: !G.GLuint
                   , vertexBufferId :: !G.GLuint, uvBufferId :: !G.GLuint
                   , textureId :: !G.GLuint
                   , mvpMatrixUniform :: !G.GLint
                   , vertexAttrib :: !G.GLuint, uvAttrib :: !G.GLuint}

initGLStuff :: (?log::String -> IO (), ?err::String -> IO ()) => IO GLIds
initGLStuff = do
  G.glClearColor 0 0 0.4 0
  progId <- loadProgram "simple.vert" "simple.frag"
  v <- withCString "vertexPosition_modelspace" $ G.glGetAttribLocation progId
  c <- withCString "vertexUV" $ G.glGetAttribLocation progId
  m <- withCString "MVP" $ G.glGetUniformLocation progId
  (vertexAttrib, uvAttrib, mvpMatrixUniform) <- if v < 0 || c < 0 || m < 0
        then error "One of the attribs/uniforms can't be found!"
        else return (fromIntegral v, fromIntegral c, m)
  vertexArrayId <- withNewPtr (G.glGenVertexArrays 1)
  G.glBindVertexArray vertexArrayId
  G.glEnable G.gl_DEPTH_TEST
  G.glDepthFunc G.gl_LESS
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
  textureId <- newTexture2DFromFile "uvtemplate.png"
  return GLIds{..}

fillNewBuffer :: [G.GLfloat] -> IO G.GLuint
fillNewBuffer list = do
  bufId <- withNewPtr (G.glGenBuffers 1)
  G.glBindBuffer G.gl_ARRAY_BUFFER bufId
  withArrayLen list $ \length ptr ->
    G.glBufferData G.gl_ARRAY_BUFFER (fromIntegral (length *
                                  sizeOf (undefined :: G.GLfloat)))
                 (ptr :: Ptr G.GLfloat) G.gl_STATIC_DRAW
  return bufId

(<&&>) = liftA2 (&&)

newTexture2DFromPtr :: (Integral a2, Integral a1) => a1 -> a2 -> Ptr a -> IO G.GLuint
newTexture2DFromPtr width height ptr = do
  texId <- withNewPtr (G.glGenTextures 1)
  G.glBindTexture G.gl_TEXTURE_2D texId
  G.glTexImage2D G.gl_TEXTURE_2D
               0  -- level of detail, used with mipmaps
               (fromIntegral G.gl_RGB)  -- internal format the
                                      -- texture will have
                                      -- once loaded
               (fromIntegral width)
               (fromIntegral height)
               0  -- border. "This value must be 0". Okay, then.
               G.gl_RGB  -- format of the image in the file
               G.gl_UNSIGNED_BYTE  -- Type of each component in
                                 -- the file
               ptr
  G.glGenerateMipmap G.gl_TEXTURE_2D
--   G.glTexParameteri G.gl_TEXTURE_2D G.gl_TEXTURE_WRAP_S
--                     (fromIntegral G.gl_REPEAT)
--   G.glTexParameteri G.gl_TEXTURE_2D G.gl_TEXTURE_WRAP_T
--                     (fromIntegral G.gl_REPEAT)
  G.glTexParameteri G.gl_TEXTURE_2D G.gl_TEXTURE_MAG_FILTER
                  (fromIntegral G.gl_LINEAR)
                  -- ^ Filter for magnification
  G.glTexParameteri G.gl_TEXTURE_2D G.gl_TEXTURE_MIN_FILTER
                  (fromIntegral G.gl_LINEAR_MIPMAP_LINEAR)
                  -- ^ Filter for minification
  return texId

newTexture2DFromFile :: FilePath -> IO G.GLuint
newTexture2DFromFile filepath = do
  e <- readImage filepath
  case e of
    Left x -> error x
    Right (ImageRGB8 (Image w h v)) ->
      V.unsafeWith v $ newTexture2DFromPtr w h
    _ -> error "Sorry, only RGB8 for now!"

bindBufferToAttrib :: G.GLuint -> G.GLuint -> G.GLint -> IO ()
bindBufferToAttrib bufId attribLoc numComponents = do
  G.glEnableVertexAttribArray attribLoc
  G.glBindBuffer G.gl_ARRAY_BUFFER bufId
  G.glVertexAttribPointer attribLoc  -- attribute location in the shader
                        numComponents
                        G.gl_FLOAT  -- coordinates type
                        (fromBool False)  -- normalized?
                        0  -- stride
                        nullPtr  -- vertex buffer offset

mainLoop :: GLIds -> IO ()
mainLoop GLIds{..} = fix $ \loop -> do
  G.glClear G.gl_COLOR_BUFFER_BIT
  G.glUseProgram progId
  with mvpMatrix $ G.glUniformMatrix4fv mvpMatrixUniform 1 (fromBool True) . castPtr
                                                -- We are ROW-first ^^^
  G.glBindTexture G.gl_TEXTURE_2D textureId
  bindBufferToAttrib vertexBufferId vertexAttrib 3
  bindBufferToAttrib uvBufferId uvAttrib 2
  G.glDrawArrays G.gl_TRIANGLES 0 (12*3)
  G.glDisableVertexAttribArray uvAttrib
  G.glDisableVertexAttribArray vertexAttrib
  W.swapBuffers
  continue <- W.windowIsOpen
              <&&> (not <$> W.keyIsPressed W.KeyEsc)
  when continue loop

cleanUpGLStuff :: GLIds -> IO ()
cleanUpGLStuff GLIds{..} = do
  with vertexBufferId $ G.glDeleteBuffers 1
  with uvBufferId $ G.glDeleteBuffers 1
  with vertexArrayId $ G.glDeleteVertexArrays 1

vec3 x y z = x:.y:.z:.()

mvpMatrix :: Mat44 G.GLfloat
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

