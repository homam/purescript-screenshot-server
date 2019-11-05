module ScreenshotServer where

import Prelude

import Control.Promise as Promise
import Data.Either (Either, either)
import Data.JSDate (JSDate)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Fiber, attempt, error, forkAff, joinFiber, throwError)
import Effect.Class (liftEffect)
import Effect.Exception as X
import Effect.Ref (Ref)
import Screenshot (takeScreenshots)
import TTLCache (Cache, addCache, getCacheIfIsFresh, updateCache)
import Test.Unit.Console (log)
import Upload (resolveAndUpload)

data ScreenshotState a = 
    Running ( Fiber (Either X.Error a)) JSDate.JSDate
  | Error String
  | Done {imageUrl :: String, url :: String, filename :: String} JSDate.JSDate

isDone :: forall a. ScreenshotState a → Boolean
isDone (Done _ _) = true
isDone _ = false

isRunning :: forall a. ScreenshotState a → Boolean
isRunning (Running _ _) = true
isRunning _ = false

instance showScreenshotState ::  Show (ScreenshotState a)  where
  show (Running _ _) = "Running"
  show (Error e) = "Error " <> e
  show (Done _ _) = "Done"

type UploadResult = { 
    filename :: String
  , imageUrl :: String
  , url :: String
  }

-- |
time :: JSDate -> Number
time d = JSDate.getTime d

-- |
type ScreenshotsCache = Cache (ScreenshotState UploadResult)

-- | 
mkScreenshotsCache :: ScreenshotsCache
mkScreenshotsCache = mempty

-- |
screenshotAndUpload1 :: String -> Aff UploadResult
screenshotAndUpload1 url = do
    log "taking screenshot and upload"
    filename <- Promise.toAff $ takeScreenshots url
    imageUrl <- Promise.toAff $ resolveAndUpload {file: "./.screenshots/" <> filename, key: "./screenshots/" <> filename}
    pure {filename, imageUrl, url}

-- |
screenshotAndUploadAsync :: 
    Ref (Cache (ScreenshotState UploadResult))
  -> Int
  -> Int
  -> String
  -> String
  -> Aff (ScreenshotState UploadResult)
screenshotAndUploadAsync cache ttl lastUpdated qid q = do
  mcached <- getCacheIfIsFresh cache ttl lastUpdated qid
  case mcached of 
    Just cached -> 
      if (ttl > 0) && (isDone cached || isRunning cached)
        then pure cached
        else go
    Nothing -> go

  where 
  go = do
    let myAff = screenshotAndUpload1 q

    now <- liftEffect JSDate.now
    fiber <- forkAff $ do
      results <- attempt myAff
      liftEffect $ log $ "Done " <> qid
      liftEffect $ updateCache cache qid  (either (Error <<< show) (\r -> Done r now) results)
      pure results

    let st = Running fiber now
    liftEffect $ addCache cache ttl qid st
    pure st


-- |
screenshotAndUploadSync :: Ref (Cache (ScreenshotState UploadResult))
  -> Int
  -> Int
  -> String
  -> String
  -> Aff UploadResult
screenshotAndUploadSync cache ttl lastUpdated hash sqlTemplate = do
  qs <- screenshotAndUploadAsync cache ttl lastUpdated hash sqlTemplate
  case qs of
    Running fiber _ -> (joinFiber fiber) >>=
      either
        (throwError <<< error <<< show)
        pure
    Done result _ -> pure result
    Error e -> throwError $ error (show e)
    -- Cancelled -> throwError (error "Cancelled")