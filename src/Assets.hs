module Assets ( AssetLoad (..)
              , AssetLoads
              , Assets (..)
              , loadTexture
              , loadShaderProgram
              , performAssetLoads
              ) where

--------------------
-- Global Imports --
import Graphics.GLUtil hiding (loadTexture, loadShaderProgram)
import qualified Data.Map.Strict as Map
import Graphics.Rendering.OpenGL
import Control.Monad
import Data.Monoid

-------------------
-- Local Imports --
import DoList

----------
-- Code --

-- | A datatype representing the loading of an asset.
data AssetLoad = LoadTexture       FilePath
               | LoadShaderProgram FilePath

-- | A list of @'AssetLoad'@s. Composable through do-notation.
type AssetLoads = DoList [AssetLoad]

-- | A structure to hold all of the @'TextureObject'@s and @'ShaderProgram'@s.
data Assets = Assets { textures :: Map.Map FilePath TextureObject
                     , shaders  :: Map.Map FilePath ShaderProgram
                     }

-- | A @'Monoid'@ instance for @'Assets'@ so that it can be concatenated in
--   @'performAssetLoads'@.
instance Monoid Assets where
  mempty = Assets { textures = Map.fromList []
                  , shaders  = Map.fromList []
                  }

  mappend (Assets t s) (Assets t' s') =
    Assets (mappend t t') (mappend s s')

-- | Constructing an @'AssetLoads'@ for a @'LoadTexture'@.
loadTexture :: FilePath -> AssetLoads
loadTexture path = dlFromList [LoadTexture path]

-- | Constructing an @'AssetLoads'@ for a @'LoadShaderProgram'@.
loadShaderProgram :: FilePath -> AssetLoads
loadShaderProgram path = dlFromList [LoadShaderProgram path]

-- | Prepending the res/ folder name to a filepath.
prependPath :: FilePath -> FilePath
prependPath = (++) "res/"

-- | Loading a @'TextureObject'@.
loadTextureObject' :: FilePath -> IO TextureObject
loadTextureObject' path = do
  img <- fmap (either error id) $ readTexture $ prependPath path

  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  texture2DWrap           $= (Repeated, ClampToEdge)

  return img

-- | Loading a @'ShaderProgram'@.
loadShaderProgram' :: FilePath -> IO ShaderProgram
loadShaderProgram' path =
  simpleShaderProgram vertPath fragPath
  where prepPath = prependPath path
        vertPath = prepPath ++ ".vert"
        fragPath = prepPath ++ ".frag"

-- | Performing @'AssetLoads'@.
performAssetLoads :: AssetLoads -> IO Assets
performAssetLoads a =
  fmap mconcat $ mapM loadAsset $ dlToList a
  where loadAsset :: AssetLoad -> IO Assets
        loadAsset (LoadTexture       path) = liftM (\tex -> mempty { textures = Map.insert path tex mempty }) $ loadTextureObject' path
        loadAsset (LoadShaderProgram path) = liftM (\sha -> mempty { shaders  = Map.insert path sha mempty }) $ loadShaderProgram' path
