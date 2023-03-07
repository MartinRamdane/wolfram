{-
-- EPITECH PROJECT, 2023
-- wolfram.hs
-- File description:
-- wolfram
-}

import System.Environment
import System.Exit
import Data.Maybe

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

myIsDigit :: String -> Bool
myIsDigit [] = True
myIsDigit "Error" = False
myIsDigit (x:xs)
  | (x >= '0' && x <= '9') || x == '-' = myIsDigit xs
  | otherwise = False

getHead :: [String] -> String
getHead [] = "Error"
getHead [x] = x
getHead (x:_) = x

checkErrorsArgs :: [String] -> IO ()
checkErrorsArgs [] = return ()
checkErrorsArgs ("--rule":xs)
  | myIsDigit (getHead xs) = checkErrorsArgs (tail xs)
checkErrorsArgs ("--start":xs)
  | myIsDigit (getHead xs) = checkErrorsArgs (tail xs)
checkErrorsArgs ("--lines":xs)
  | myIsDigit (getHead xs) = checkErrorsArgs (tail xs)
checkErrorsArgs ("--window":xs)
  | myIsDigit (getHead xs) = checkErrorsArgs (tail xs)
checkErrorsArgs ("--move":xs)
  | myIsDigit (getHead xs) = checkErrorsArgs (tail xs)
checkErrorsArgs (_:xs)
  | myIsDigit (getHead xs) = checkErrorsArgs (tail xs)
  | otherwise = exitWith (ExitFailure 84)

getOpts :: Conf -> [String] -> Maybe Conf
getOpts conf [] = Just conf
getOpts conf ("--rule":xs) =
  getOpts (conf {rule = Just (read (head xs))}) (tail xs)
getOpts conf ("--start":xs) =
  getOpts (conf {start = (read (head xs)) + 1}) (tail xs)
getOpts conf ("--lines":xs) =
  getOpts (conf {nb_lines = Just (read (head xs))}) (tail xs)
getOpts conf ("--window":xs) =
  getOpts (conf {window = read (head xs)}) (tail xs)
getOpts conf ("--move":xs) =
  getOpts (conf {move = read (head xs)}) (tail xs)
getOpts _ _ = Nothing

checkConf :: Maybe Conf -> IO ()
checkConf Nothing = exitWith (ExitFailure 84)
checkConf (Just conf) =
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

print80char :: Maybe Conf -> String -> Int -> Int -> IO ()
print80char _ [] _ _= putChar '\n'
print80char arg (x:xs) n s
  | n >= s && n < s + (window (fromJust arg)) =
    putChar x >> print80char arg xs (n + 1) s
  | n < s + (window (fromJust arg)) = print80char arg xs (n + 1) s
  | otherwise = putChar '\n'

wolfram :: Maybe Conf -> String -> [Int] -> Int -> Int -> IO ()
wolfram _ _ [] _ _ = return ()
wolfram arg li bin n s
  | isNothing (nb_lines (fromJust arg)) =
    print80char arg li 1 s >>
    wolfram arg(" "++(nextLine bin li (fChar bin li ' '))++" ")bin (n+1) (s+1)
  | n >= fromJust (nb_lines (fromJust arg)) + (start (fromJust arg)) = return()
  | n >= (start (fromJust arg)) =
    print80char arg li 1 s >>
    wolfram arg (" "++(nextLine bin li (fChar bin li ' '))++" ")bin (n+1) (s+1)
  | otherwise =
    wolfram arg (" "++(nextLine bin li (fChar bin li ' '))++" ")bin (n+1) (s+1)

getFirst :: Maybe Conf -> String
getFirst arg = replicate (calc + (move (fromJust arg)) - cell) ' ' ++ "*"
  where
    calc = ((window (fromJust arg)) `div` 2)
    cell = if (window (fromJust arg)) `mod` 2 == 0 then 0 else 1

getScd :: Maybe Conf -> String
getScd arg = replicate (calc - cell - (move (fromJust arg))) ' '
  where
    calc = ((window (fromJust arg)) `div` 2)
    cell = if (window (fromJust arg)) `mod` 2 == 0 then 1 else -1

main :: IO ()
main = do
  args <- getArgs
  checkErrorsArgs args
  let conf = getOpts defaultConf args
  checkConf conf
  let line = getFirst conf ++ getScd conf
  wolfram conf line (toBin (fromJust (rule (fromJust conf)))) 1 1
