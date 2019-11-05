module Screenshot (takeScreenshots) where

import Prelude

import Control.Promise (Promise)
import Effect (Effect)

foreign import takeScreenshots :: String -> Promise String