{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import MCP.Server

data Options = Options
  { optPrometheusUrl :: String
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> strOption
      ( long "prometheus-url"
     <> short 'p'
     <> metavar "URL"
     <> value "http://localhost:9090"
     <> help "Prometheus server URL"
      )

main :: IO ()
main = do
  opts <- execParser $ info (optionsParser <**> helper)
    ( fullDesc
   <> progDesc "MCP server for Prometheus queries"
   <> header "mcp-prometheus-server - Model Context Protocol server for Prometheus"
    )
  
  let config = ServerConfig
        { configPrometheusUrl = optPrometheusUrl opts
        }
  
  runServer config