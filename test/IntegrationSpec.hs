{-# LANGUAGE OverloadedStrings #-}

module IntegrationSpec (spec) where

import Test.Hspec
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import Data.Time.Clock
import Data.Time.Calendar
import Prometheus.Client
import qualified Data.Text as T

getTestConfig :: IO (Maybe String)
getTestConfig = lookupEnv "PROMETHEUS_URL"

spec :: Spec
spec = do
  describe "Prometheus Integration Tests" $ do
    it "queries 'up' metric successfully" $ do
      mUrl <- getTestConfig
      case mUrl of
        Nothing -> pendingWith "PROMETHEUS_URL not set - skipping integration test"
        Just url -> do
          let client = newClient url
          result <- query client "up" Nothing
          case result of
            Left err -> expectationFailure $ "Query failed: " ++ err
            Right qr -> do
              resultStatus qr `shouldBe` "success"
    
    it "queries with specific time" $ do
      mUrl <- getTestConfig
      case mUrl of
        Nothing -> pendingWith "PROMETHEUS_URL not set - skipping integration test"
        Just url -> do
          let client = newClient url
          now <- getCurrentTime
          result <- query client "up{job=\"node-exporter\"}" (Just now)
          case result of
            Left err -> expectationFailure $ "Query failed: " ++ err
            Right qr -> do
              resultStatus qr `shouldBe` "success"
    
    it "performs range query" $ do
      mUrl <- getTestConfig
      case mUrl of
        Nothing -> pendingWith "PROMETHEUS_URL not set - skipping integration test"
        Just url -> do
          let client = newClient url
          now <- getCurrentTime
          let end = now
              start = addUTCTime (-300) now  -- 5 minutes ago
          result <- queryRange client "rate(up[1m])" start end "60s"
          case result of
            Left err -> expectationFailure $ "Range query failed: " ++ err
            Right qr -> do
              resultStatus qr `shouldBe` "success"
    
    it "handles invalid query gracefully" $ do
      mUrl <- getTestConfig
      case mUrl of
        Nothing -> pendingWith "PROMETHEUS_URL not set - skipping integration test"
        Just url -> do
          let client = newClient url
          result <- query client "invalid_metric_name{bad=\"label" Nothing
          case result of
            Left err -> return ()  -- Expected to fail
            Right qr -> 
              if resultStatus qr == "error"
                then return ()  -- Prometheus returned error status
                else expectationFailure "Expected query to fail"
    
    it "queries histogram quantiles" $ do
      mUrl <- getTestConfig
      case mUrl of
        Nothing -> pendingWith "PROMETHEUS_URL not set - skipping integration test"
        Just url -> do
          let client = newClient url
          result <- query client "histogram_quantile(0.95, sum(rate(prometheus_http_request_duration_seconds_bucket[5m])) by (le))" Nothing
          case result of
            Left err -> pendingWith $ "Histogram query not available: " ++ err
            Right qr -> do
              resultStatus qr `shouldBe` "success"
    
    it "queries series metadata" $ do
      mUrl <- getTestConfig
      case mUrl of
        Nothing -> pendingWith "PROMETHEUS_URL not set - skipping integration test"
        Just url -> do
          let client = newClient url
          now <- getCurrentTime
          let end = now
              start = addUTCTime (-3600) now  -- 1 hour ago
          result <- series client ["up{job=\"node-exporter\"}"] start end
          case result of
            Left err -> expectationFailure $ "Series query failed: " ++ err
            Right qr -> do
              resultStatus qr `shouldBe` "success"
    
    it "gets all metric names" $ do
      mUrl <- getTestConfig
      case mUrl of
        Nothing -> pendingWith "PROMETHEUS_URL not set - skipping integration test"
        Just url -> do
          let client = newClient url
          result <- labelValues client "__name__"
          case result of
            Left err -> expectationFailure $ "Metric names query failed: " ++ err
            Right qr -> do
              resultStatus qr `shouldBe` "success"
    
    it "gets all label names" $ do
      mUrl <- getTestConfig
      case mUrl of
        Nothing -> pendingWith "PROMETHEUS_URL not set - skipping integration test"
        Just url -> do
          let client = newClient url
          result <- labels client
          case result of
            Left err -> expectationFailure $ "Labels query failed: " ++ err
            Right qr -> do
              resultStatus qr `shouldBe` "success"