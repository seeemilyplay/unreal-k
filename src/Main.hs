module Main (main) where

import System.Environment

import qualified AcidServer as AS
import qualified WebServer as Web


main :: IO ()
main = do
  args <- getArgs
  acid <- AS.open (Just 30)
  Web.start
  case args of
    ["top"] -> do
      xs <- AS.top acid "country"
      putStrLn $ show xs
    ["all"] -> do
      xs <- AS.all acid
      putStrLn $ show xs
    ["save"] -> do
      c <- getContents
      mapM_ (\e -> AS.save acid "country" (e, 1, 11)) $ lines c
    _ -> do putStrLn "Use it properly!"
  AS.close acid
