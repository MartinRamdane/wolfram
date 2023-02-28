{-
-- EPITECH PROJECT, 2023
-- wolfram.hs
-- File description:
-- Wolfram
-}

import System.Environment
import System.Exit
import Data.Maybe
import System.Console.Terminal.Size

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
  start = 1,
  nb_lines = Nothing,
  window = 80,
  move = 0
}

getOpts :: Conf -> [String] -> Maybe Conf
getOpts conf [] = Just conf
getOpts conf ("--rule":xs) =
  getOpts (conf {rule = Just (read (head xs))}) (tail xs)
getOpts conf ("--start":xs) =
  getOpts (conf {start = read (head xs)}) (tail xs)
getOpts conf ("--lines":xs) =
  getOpts (conf {nb_lines = Just (read (head xs))}) (tail xs)
getOpts conf ("--window":xs) =
  getOpts (conf {window = read (head xs)}) (tail xs)
getOpts conf ("--move":xs) =
  getOpts (conf {move = read (head xs)}) (tail xs)
getOpts _ _ = Nothing

showConf :: Maybe Conf -> IO ()
showConf (Just conf) = print conf
showConf Nothing = putStrLn "Error"

checkErrors :: Maybe Conf -> IO ()
checkErrors Nothing = exitWith (ExitFailure 84)
checkErrors (Just conf) =
  if (isNothing (rule conf))
    then exitWith (ExitFailure 84)
  else
    return ()

toBin :: Int -> [Int]
toBin 0 = [0]
toBin 1 = [1]
toBin n = toBin (n `div` 2) ++ [n `mod` 2]

eightsBytes :: [Int] -> [Int]
eightsBytes [] = []
eightsBytes (x:xs) = if (length xs < 7)
  then (replicate (7 - length xs) 0) ++ (x:xs)
  else x:(eightsBytes xs)

getNextLine :: [Int] -> String -> Char -> String
getNextLine _ [_] _ = " "
getNextLine _ [_,_] _ = " "
getNextLine _ "" _ = " "
getNextLine bin ('*':'*':'*':xs) c =
  c : getNextLine bin ('*':'*':xs) (if (bin !! 0) == 1 then '*' else ' ')
getNextLine bin ('*':'*':' ':xs) c =
  c : getNextLine bin ('*':' ':xs) (if (bin !! 1) == 1 then '*' else ' ')
getNextLine bin ('*':' ':'*':xs) c =
  c : getNextLine bin (' ':'*':xs) (if (bin !! 2) == 1 then '*' else ' ')
getNextLine bin ('*':' ':' ':xs) c =
  c : getNextLine bin (' ':' ':xs) (if (bin !! 3) == 1 then '*' else ' ')
getNextLine bin (' ':'*':'*':xs) c =
  c : getNextLine bin ('*':'*':xs) (if (bin !! 4) == 1 then '*' else ' ')
getNextLine bin (' ':'*':' ':xs) c =
  c : getNextLine bin ('*':' ':xs) (if (bin !! 5) == 1 then '*' else ' ')
getNextLine bin (' ':' ':'*':xs) c =
  c : getNextLine bin (' ':'*':xs) (if (bin !! 6) == 1 then '*' else ' ')
getNextLine bin (' ':' ':' ':xs) c =
  c : getNextLine bin (' ':' ':xs) (if (bin !! 7) == 1 then '*' else ' ')
getNextLine bin (_:y:z:xs) c = ' ' : getNextLine bin (y:z:xs) c

wolfram :: Maybe Conf -> String -> [Int] -> Int -> IO ()
wolfram _ _ [] _ = return ()
wolfram arg li bin n
  | n >= fromJust (nb_lines (fromJust arg)) + (start (fromJust arg)) = return()
  | n >= (start (fromJust arg)) =
    putStrLn li >> wolfram arg (getNextLine bin li ' ') bin (n + 1)
  | otherwise = wolfram arg (getNextLine bin li ' ') bin (n + 1)

main :: IO ()
main = do
  args <- getArgs
  let conf = getOpts defaultConf args
  checkErrors conf
  size <- size
  let (Window height width) = fromMaybe (Window 80 24) size
  let first = replicate ((width `div` 2) + (move (fromJust conf))) ' ' ++ "*"
  let scd = replicate ((width `div` 2) - 1 - (move (fromJust conf))) ' '
  let line = first ++ scd
  wolfram conf line (eightsBytes (toBin (fromJust (rule (fromJust conf))))) 1
