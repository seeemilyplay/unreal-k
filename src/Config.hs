module Config (Config(..), defaultConfig, configFromArgs) where

import System.Environment

import TopK

data Config = Config {
  cfgpersist :: Bool
, cfguseboxtime :: Bool
, cfgslots :: Slots
, cfgk :: K
, cfgperiod :: (Time -> Time)
}

defaultPersist :: Bool
defaultPersist = False

defaultUseBoxTime :: Bool
defaultUseBoxTime = False

defaultSlots :: Slots
defaultSlots = 10000

defaultK :: K
defaultK = 10

defaultTime :: Time
defaultTime = 86400

mkPeriod :: Time -> Time -> Time
mkPeriod d t = t - d

defaultConfig :: Config
defaultConfig = Config {
    cfgpersist = False
  , cfguseboxtime = defaultUseBoxTime
  , cfgslots = defaultSlots
  , cfgk = defaultK
  , cfgperiod = mkPeriod defaultTime
  }

configFromArgs :: IO Config
configFromArgs = do
  args <- getArgs
  let p = argBool args "-p" defaultPersist
      b = argBool args "-b" defaultUseBoxTime
      s = argNumber args "-s" defaultSlots
      k = argNumber args "-k" defaultK
      t = argNumber args "-t" defaultTime

  putStrLn $ "Running with:"
  putStrLn $ "  (-p) Persist      => " ++ show p
  putStrLn $ "  (-b) Use box time => " ++ show b
  putStrLn $ "  (-s) Slots        => " ++ show s
  putStrLn $ "  (-k) K            => " ++ show k
  putStrLn $ "  (-t) Time frame   => " ++ show t
  return $ Config {
    cfgpersist = p
  , cfguseboxtime = b
  , cfgslots = s
  , cfgk = k
  , cfgperiod = mkPeriod t
  }

argBool :: [String] -> String -> Bool -> Bool
argBool [] _ d = d
argBool (x:(y:_)) f _ | x==f = read y
argBool (_:xs) f d = argBool xs f d

argNumber :: [String] -> String -> Int -> Int
argNumber [] _ d = d
argNumber (x:(y:_)) f _ | x==f = read y
argNumber (_:xs) f d = argNumber xs f d