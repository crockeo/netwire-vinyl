{-# LANGUAGE DataKinds, TypeOperators #-}
module Rendering where

import Control.Applicative
import Control.Lens ((+~))
import Data.Foldable (foldMap, traverse_)
import Data.Vinyl
import Data.Vinyl.Universe ((:::), SField(..))
import Graphics.GLUtil
import Graphics.Rendering.OpenGL
import Graphics.VinylGL
import Linear (V2(..), _x, M33)
import System.FilePath ((</>))

-- A record each drawing function will receive.
type AppInfo = PlainFieldRec '["cam" ::: M33 GLfloat]
-- Our vertices will have position and texture coordinates.
type Pos = "vertexCoord" ::: V2 GLfloat
type Tex = "texCoord"    ::: V2 GLfloat

pos :: SField Pos
pos = SField

tex :: SField Tex
tex = SField
-}

-- Compute a textured vertex record for each input vertex.
tileTex :: [[V2 GLfloat]] -> [PlainFieldRec [Pos,Tex]]
tileTex = foldMap (flip (zipWith (<+>)) (cycle coords) . map (pos =:))
  where coords = map (tex =:) $ V2 <$> [0,1] <*> [0,1]

-- Load a list of textures from the art directory. Set each texture to
-- use nearest-neighbor filtering.
loadTextures :: [FilePath] -> IO [TextureObject]
loadTextures = fmap (either error id . sequence) . mapM aux
  where aux f = do img <- readTexture ("res" </> f)
                   traverse_ (const texFilter) img
                   return img
        texFilter = do textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
                       texture2DWrap $= (Repeated, ClampToEdge)

renderCrate :: IO (V2 Float -> AppInfo -> IO ())
renderCrate = do
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

  [crate] <- loadTextures ["crate.png"]
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
