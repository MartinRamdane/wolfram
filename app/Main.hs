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
import Data.List (unfoldr)

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

checkErrors :: Maybe Conf -> IO ()
checkErrors Nothing = exitWith (ExitFailure 84)
checkErrors (Just conf) =
  if (isNothing (rule conf))
    then exitWith (ExitFailure 84)
  else
    return ()

toBin :: Int -> [Int]
toBin n = padTo8Bits $ reverse $ unfoldr f n
  where
    padTo8Bits xs = replicate (8 - length xs) 0 ++ xs
    f 0 = Nothing
    f x = Just (x `mod` 2, x `div` 2)

fChar :: [Int] -> String -> Char -> Char
fChar _ [_] _ = ' '
fChar _ [_,_] _ = ' '
fChar _ "" _ = ' '
fChar bin ('*':'*':_) ' ' = (if (bin !! 4) == 1 then '*' else ' ')
fChar bin ('*':' ':_) ' ' = (if (bin !! 5) == 1 then '*' else ' ')
fChar bin (' ':'*':_) ' ' = (if (bin !! 6) == 1 then '*' else ' ')
fChar bin (' ':' ':_) ' ' = (if (bin !! 7) == 1 then '*' else ' ')
fChar _ (_:_:_) _ = ' '

lChar :: [Int] -> String -> Char -> Char
lChar _ [_] _ = ' '
lChar _ "" _ = ' '
lChar bin ['*','*'] ' ' = (if (bin !! 1) == 1 then '*' else ' ')
lChar bin ['*',' '] ' ' = (if (bin !! 3) == 1 then '*' else ' ')
lChar bin [' ','*'] ' ' = (if (bin !! 5) == 1 then '*' else ' ')
lChar bin [' ',' '] ' ' = (if (bin !! 7) == 1 then '*' else ' ')
lChar _ [_,_] _ = ' '
lChar _ _ _ = ' '

nextLine :: [Int] -> String -> Char -> String
nextLine _ [_] _ = " "
nextLine bin [x,y] c = c : [(lChar bin [x,y] ' ')]
nextLine _ "" _ = " "
nextLine bin ('*':'*':'*':xs) c =
  c : nextLine bin ('*':'*':xs) (if (bin !! 0) == 1 then '*' else ' ')
nextLine bin ('*':'*':' ':xs) c =
  c : nextLine bin ('*':' ':xs) (if (bin !! 1) == 1 then '*' else ' ')
nextLine bin ('*':' ':'*':xs) c =
  c : nextLine bin (' ':'*':xs) (if (bin !! 2) == 1 then '*' else ' ')
nextLine bin ('*':' ':' ':xs) c =
  c : nextLine bin (' ':' ':xs) (if (bin !! 3) == 1 then '*' else ' ')
nextLine bin (' ':'*':'*':xs) c =
  c : nextLine bin ('*':'*':xs) (if (bin !! 4) == 1 then '*' else ' ')
nextLine bin (' ':'*':' ':xs) c =
  c : nextLine bin ('*':' ':xs) (if (bin !! 5) == 1 then '*' else ' ')
nextLine bin (' ':' ':'*':xs) c =
  c : nextLine bin (' ':'*':xs) (if (bin !! 6) == 1 then '*' else ' ')
nextLine bin (' ':' ':' ':xs) c =
  c : nextLine bin (' ':' ':xs) (if (bin !! 7) == 1 then '*' else ' ')
nextLine bin (_:y:z:xs) c = c : nextLine bin (y:z:xs) c

wolfram :: Maybe Conf -> String -> [Int] -> Int -> IO ()
wolfram _ _ [] _ = return ()
wolfram arg li bin n
  | n >= fromJust (nb_lines (fromJust arg)) + (start (fromJust arg)) = return()
  | n >= (start (fromJust arg)) =
    putStrLn li >> wolfram arg (nextLine bin li (fChar bin li ' ')) bin (n + 1)
  | otherwise = wolfram arg (nextLine bin li (fChar bin li ' ')) bin (n + 1)

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
  wolfram conf line (toBin (fromJust (rule (fromJust conf)))) 1
