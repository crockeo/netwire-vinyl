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

-- | Rendering a textured quad.
renderTexturedQuad :: TextureObject -> ShaderProgram -> AppInfo -> V2 Float -> V2 Float -> IO ()
renderTexturedQuad t sh i p s = do
  let (V2 x y) = fmap realToFrac p
      (V2 w h) = fmap realToFrac s

  verts <- bufferVertices $ tileTex [ V2 <$> [x - w, x + w] <*> [y - h, y + h] ]
  eb    <- bufferIndices indices
  vao   <- makeVAO $ do
    enableVertices' sh verts
    bindVertices verts
    bindBuffer ElementArrayBuffer $= Just eb

  currentProgram $= Just (program sh)
  setUniforms sh i
  withVAO vao . withTextures2D [t] $ drawIndexedTris 2
  where indices = take 6 $ foldMap (flip map [0, 1, 2, 2, 1, 3] . (+)) [0, 4..]
