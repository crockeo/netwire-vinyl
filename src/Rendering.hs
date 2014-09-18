{-# LANGUAGE DataKinds, TypeOperators #-}
module Rendering where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Control.Applicative
import Data.Vinyl.Universe
import Graphics.VinylGL
import Graphics.GLUtil
import Data.Foldable
import Linear.Matrix
import Data.Vinyl
import Linear.V2

----------
-- Code --

-- | The data -- specifically the Camera -- that is inputed to the rendering
--   function upon every call.
type AppInfo = PlainFieldRec '["cam" ::: M33 GLfloat]

-- | Defining a vertex coordinate to use in GLSL
type Pos = "vertexCoord" ::: V2 GLfloat

-- | Defining a texture coordinate to use in GLSL
type Tex = "texCoord"    ::: V2 GLfloat

-- | Defining the vertex coordinate instance.
pos :: SField Pos
pos = SField

-- | Defining the texture coordinate instance.
tex :: SField Tex
tex = SField

-- Compute a textured vertex record for each input vertex.
tileTex :: [[V2 GLfloat]] -> [PlainFieldRec [Pos,Tex]]
tileTex =
  foldMap (flip (zipWith (<+>)) (cycle coords) . map (pos =:))
  where coords = map (tex =:) $ V2 <$> [0,1] <*> [0,1]

-- | Loading a single texture from a file.
loadTexture' :: FilePath -> IO TextureObject
loadTexture' path = do
  img <- either error id <$> (readTexture $ "res/" ++ path)

  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  texture2DWrap $= (Repeated, ClampToEdge)

  return img

-- | Loading a number of textures from a number of files.
loadTextures :: [FilePath] -> IO [TextureObject]
loadTextures = mapM loadTexture'

-- | Generating a function to render the crate.
renderCrate :: IO (V2 Float -> AppInfo -> IO ())
renderCrate = do
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

  crate <- loadTexture' "crate.png"

  s <- simpleShaderProgram "res/game2d.vert" "res/game2d.frag"
  setUniforms s (texSampler =: 0)

  return $ \p ai -> do
    let (V2 x y) = fmap realToFrac p
    verts <- bufferVertices $ tileTex [ V2 <$> [x - size, x + size] <*> [y - size, y + size] ]
    eb    <- bufferIndices indices
    vao   <- makeVAO $ do
      enableVertices' s verts
      bindVertices verts
      bindBuffer ElementArrayBuffer $= Just eb

    currentProgram $= Just (program s)
    setUniforms s ai
    withVAO vao . withTextures2D [crate] $ drawIndexedTris 2

  where texSampler = SField :: SField ("tex" ::: GLint)
        indices    = take 6 $
                       foldMap (flip map [0,1,2,2,1,3] . (+)) [0,4..]
        size       = 0.1
