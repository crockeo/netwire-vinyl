{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Render where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL hiding (Render)
import Control.Applicative
import Data.Vinyl.Universe
import Graphics.VinylGL
import Graphics.GLUtil
import Data.Foldable
import Linear.Matrix
import Data.Vinyl
import Linear.V2

-------------------
-- Local Imports --
import Assets
import DoList

----------
-- Code --

-- | The matrix of the camera being used to observe the scene.
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

-- | A typeclass to define that a type can be rendered. Should only be used to
--   perform render calls.
class Renderable a where
  render :: AppInfo -> Assets -> a -> IO ()

-- | A generic render type.
data Render = forall a. Renderable a => Render a

-- | A @'Renderable'@ instance for @'Render'@ so that after a type has been
--   wrapped, it can still be used as a @'Renderable'@ elsewhere in the code.
instance Renderable Render where
  render appinfo assets (Render a) = render appinfo assets a

-- | Being able to concatenate @'Render'@ calls through do-notation.
type Renders = DoList [Render]
