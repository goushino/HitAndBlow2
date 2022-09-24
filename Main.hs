import Data.Char
import Data.List
import Data.Time.Clock.POSIX

type Ans = [Char]
type Hint = (Int, Int)

shuffle :: Eq a => [a] -> Int -> [a]
shuffle [] _ = []
shuffle xs g = y : shuffle ys g
  where
    n = g `mod` (length xs)
    y = xs !! n
    ys = delete y xs

cpuAns :: IO Ans
cpuAns = do
  g <- round <$> getPOSIXTime
  return $ take 4 (shuffle ns g)
  where
    ns = ['0'..'9']

parseAns :: String -> Maybe Ans
parseAns s
  | not4Digit = Nothing
  | notNumber = Nothing
  | notUnique = Nothing
  | otherwise = Just s
  where
    not4Digit = length s /= 4
    notNumber = any (not . isDigit) s
    notUnique = s /= nub s

parseHint :: [String] -> Maybe Hint
parseHint ss
  | length ss /= 2 = Nothing
  | or [length x /= 1 | x <- ss] = Nothing
  | or [notElem x "01234" | x <- cs] = Nothing
  | otherwise = checkHint cs
  where cs = map head ss

checkHint :: [Char] -> Maybe Hint
checkHint cs = 
  let (h:b:_) = map digitToInt cs
  in if (h + b > 4) then Nothing
                    else Just (h, b)

playerAns :: IO Ans
playerAns = do
  s <- getLine
  case parseAns s of
    Nothing -> do
      putStrLn "Invalid input."
      playerAns
    Just a -> return a

playerHint :: IO Hint
playerHint = do
  putStr "HIT BLOW = "
  ss <- words <$> getLine
  case parseHint ss of
    Nothing -> do
      putStrLn "Invalid input."
      playerHint
    Just a -> return a

choiceAns :: [Ans] -> IO Ans
choiceAns xs = do
  g <- round <$> getPOSIXTime
  let n = g `mod` (length xs)
  return $ xs !! n

possibleAns :: [Ans]
possibleAns = permut 4 ['0'..'9']

permut :: Eq a => Int -> [a] -> [[a]]
permut 0 _ = [[]]
permut n xs = do
  y <- xs
  let ys = delete y xs
  map (y:) $ permut (n - 1) ys

testAns :: Ans -> Ans -> Hint
testAns xs ys = (hit, tmp - hit)
  where
    hit = length [x | (x, y) <- zip xs ys, x == y]
    tmp = length [x | x <- xs, y <- ys, x == y]

testHint :: (Ans, Hint) -> [Ans] -> [Ans]
testHint (y, h) xs = [x | x <- xs, testAns x y == h]

playRound :: Ans -> IO ()
playRound x = do
  putStrLn "\nInput 4 digit number..."
  y <- playerAns
  case testAns x y of
    (4, _) -> putStrLn "That's right!"
    (h, b) -> do
      putStr $ "HIT: " ++ show h
      putStrLn $ ", BLOW: " ++ show b
      putStrLn "Try again!"
      playRound x

cpuRound :: [Ans] -> IO ()
cpuRound xs = do
  x <- choiceAns xs
  putStrLn $ "\nI try " ++ x
  putStrLn "Tell me your hint."
  s <- playerHint
  case s of
    (4, _) -> putStrLn "I got it!"
    _ -> case testHint (x, s) xs of
      [] -> putStrLn "You told me a lie."
      xs' -> cpuRound xs'

main :: IO ()
main = do
  putStrLn "\nThe game is beginning."
  putStrLn "Choose game mode 1 or 2."
  putStrLn "1: Player challenge"
  putStrLn "2: Computer challenge"
  mode <- getLine
  case mode of
    "1" -> do
      x <- cpuAns
      playRound x    
    "2" -> do
      let xs = possibleAns
      cpuRound xs
    _ -> return ()
  putStrLn "New game? (y/n)"
  yn <- getLine
  if yn == "y" then main else return ()