module State.Themes
  ( listThemes
  , setTheme
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick ( invalidateCache )
import           Brick.Themes ( themeToAttrMap )
import qualified Data.Text as T
import           Lens.Micro.Platform ( (.=) )

import           State.Common
import           Themes
import           Types


listThemes :: MH ()
listThemes = do
    let themeList = T.intercalate "\n" $
                    "Available built-in themes:\n" :
                    (("* " <>) <$> internalThemeName <$> internalThemes)
    postInfoMessage themeList

setTheme :: Text -> MH ()
setTheme name =
    case lookupTheme name of
        Nothing -> listThemes
        Just it -> do
            mh invalidateCache
            csResources.crTheme .= (themeToAttrMap $ internalTheme it)
            csResources.crThemeColorMode .= (internalThemeColorMode it)
