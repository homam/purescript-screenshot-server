module Main where

import Prelude

import Effect (Effect)
import Server as Server

main :: Effect Unit
main = do 
  server <- Server.main
  pure unit