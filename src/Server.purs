module Server where

import Prelude hiding (apply)

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Int (fromNumber, toNumber)
import Data.Maybe (fromMaybe, maybe)
import Effect (Effect)
import Effect.Aff (attempt)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error)
import Effect.Ref as Ref
import Global (readFloat)
import Node.Express.App (App, get, listenHttp)
import Node.Express.Request (getQueryParam)
import Node.Express.Response (redirect, send, setStatus)
import Node.HTTP (Server)
import ScreenshotServer (ScreenshotsCache, mkScreenshotsCache, screenshotAndUploadSync)

newtype AppState = AppState {
    screenhostsCache :: Ref.Ref ScreenshotsCache
}

app :: App
app = do 
    cache <- liftEffect $ Ref.new mkScreenshotsCache
    get "/" $ do 
        url <- (getQueryParam "url" >>= maybe (throwError (error "url parameter is mandatory")) pure )
        ttl <- (getQueryParam "ttl" >>= maybe (pure $ 10 * 60 * 1000) pure )
        redir <- (getQueryParam "redir" >>= maybe (pure false) (\s -> pure $ s == "true" || (readFloat s > toNumber 0)) )
        fresherThan <- (getQueryParam "ft" >>= maybe (pure $ 5 * 60 * 1000) (pure <<< (\x -> x * 1000) <<< fromMaybe 300 <<< fromNumber <<< readFloat) )

        r <- liftAff $ attempt $ screenshotAndUploadSync cache ttl fresherThan url url
        case r of
            Right result -> if redir then redirect result.imageUrl else send result
            Left ex -> do setStatus 500  
                          send {error: show ex}

main :: Effect Server
main = do
    listenHttp app 8080 \_ ->
        log $ "Listening on " <> show 8080
