import Data.Char
import Data.Time.Clock.POSIX

type Ans = [Char]
type Hint = (Int, Int)

cpuAns :: IO Ans
cpuAns = do
  ms <- (round . (* 1000)) <$> getPOSIXTime
  return $ take 4 (shuffle ['0'..'9'] ms)
  where
    shuffle [] _ = []
    shuffle xs g = 
      let n = g `mod` (length xs - 1)
          xs' = (take n xs) ++ drop (n + 1) xs
      in (xs !! n) : shuffle xs' g

ensureAns :: String -> Maybe Ans
ensureAns str
  | not4Digit str = Nothing
  | notNumber str = Nothing
  | notUnique str = Nothing
  | otherwise     = Just str
  where
    f x = notElem x ['0'..'9']
    not4Digit s = length s /= 4
    notNumber s = any f s
    notUnique s = 
      s /= [x | x <- s, y <- s, x == y]

playerAns :: IO Ans
playerAns = do
  s <- getLine
  case ensureAns s of
    Nothing -> do
      putStrLn "Invalid input."
      playerAns
    Just a -> return a

testAns :: Ans -> Ans -> Hint
testAns xs ys = (hit, temp - hit)
  where
    hit = length . (filter (== True)) $ zipWith (==) xs ys
    temp = length [x | x <- xs, y <- ys, x == y]

playRound :: Ans -> IO ()
playRound x = do
  putStrLn "\nInput 4 digit number..."
  y <- playerAns
  case testAns x y of
    (4, _) -> putStrLn "That's right!"
    (h, b) -> do
      putStr $ "HIT: " ++ show h ++ ", "
      putStrLn $ "BLOW: " ++ show b
      putStrLn "Try again!"
      playRound x

main :: IO ()
main = do
  putStrLn "The game is beginning."
  x <- cpuAns
  playRound x
  putStrLn "New game? (y/n)"
  yn <- getLine
  if yn == "y" then main else return ()