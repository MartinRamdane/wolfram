{-
-- EPITECH PROJECT, 2023
-- wolfram.hs
-- File description:
-- Wolfram
-}

import System.Environment
import System.Exit
import Data.Maybe

data Conf = Conf {
  rule :: Maybe Int,
  start :: Int,
  nb_lines :: Maybe Int,
  window :: Int,
  move :: Int
} deriving (Show, Eq)

defaultConf :: Conf
defaultConf = Conf {
  rule = Nothing,
  start = 0,
  nb_lines = Nothing,
  window = 80,
  move = 0
}

getOpts :: Conf -> [String] -> Maybe Conf
getOpts conf [] = Just conf
getOpts conf ("--rule":xs) = getOpts (conf {rule = Just (read (head xs))}) (tail xs)
getOpts conf ("--start":xs) = getOpts (conf {start = read (head xs)}) (tail xs)
getOpts conf ("--lines":xs) = getOpts (conf {nb_lines = Just (read (head xs))}) (tail xs)
getOpts conf ("--window":xs) = getOpts (conf {window = read (head xs)}) (tail xs)
getOpts conf ("--move":xs) = getOpts (conf {move = read (head xs)}) (tail xs)
getOpts _ _ = Nothing

showConf :: Maybe Conf -> IO ()
showConf (Just conf) = print conf
showConf Nothing = putStrLn "Error"

checkErrors :: Maybe Conf -> IO ()
checkErrors Nothing = exitWith (ExitFailure 84)
checkErrors (Just conf) = do
  if (isNothing (rule conf))
    then exitWith (ExitFailure 84)
  else
    return ()

main :: IO ()
main = do
  args <- getArgs
  let conf = getOpts defaultConf args
  checkErrors conf
  showConf conf
  return ()
