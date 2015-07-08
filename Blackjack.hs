--Author: Ryuichiro S
--Date: 2015/07/08

module Blackjack where
import System.Random

------------------------------↓https://wiki.haskell.org/Random_shuffleから借用
import Data.Array.IO
import Control.Monad
 
-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs
------------------------------------↑https://wiki.haskell.org/Random_shuffleから借用


type Card = Int     --タイプエイリアス
type Hand = [Card]
type Deck = [Card]

--手札の合計得点が21以下かどうかを調べる。Aは小さい方の値である1として計算すればよい(その手が21以下をとる可能性があるかだけを判定すればよい)
checkHand :: Hand -> Int
checkHand [] = 0
checkHand (x:xs) = (evalCardMin x) + (checkHand xs)

evalCardMin :: Card -> Int
evalCardMin x
  | x == 1 = 1
  | x >= 10 = 10
  | otherwise = x

--Aを正しく判定しさえすれば、21以下を満たすということがcheckHandにより保証されている。また、A以外の値は不変であって複数の候補があるわけではない。
--sumHandでAは11として計算して、合計値を求める。合計値が21を超えていたら、-10をリストに加えて相殺する（Aは１だったと判断）。
--Aを複数枚引く可能性も考慮し、21を下回るまで10を引き続ければよい。例えば、AAA67という手札であれば、sumHandの段階では46と判定される。これを-10ずつしていくと16になる。21以下になったのでこれが答えである。
calHand :: Hand -> Int
calHand hand
  | (sumHand hand) > 21 = calHand (-10:hand)
  | otherwise = (sumHand hand)

sumHand :: Hand -> Int
sumHand [] = 0
sumHand (x:xs) = (evalCardMax x) + (sumHand xs)

evalCardMax :: Card -> Int
evalCardMax x
  | x == 1 = 11
  | x >= 10 = 10
  | otherwise = x

--デッキを生成して、それをplayInteractiveにわたす。構造はじゃんけんとほぼ同じ
play :: IO ()
play =
  do deck <- shuffle([1..13]++[1..13]++[1..13]++[1..13])
     playInteractive deck []

playInteractive :: Deck -> Hand -> IO ()
playInteractive (next:deck) hand =
  do
    putStr("HIT or STAND. ")
    putStrLn (show hand) -- debug
    line <- getLine
    let ch = head line
    if not (elem ch "hH")
      then showResult deck hand
      else do let draw = next    ------ Monadic way, not using the let-construct.
              putStr ("You choose to HIT. Your draw is ")
              putStrLn (show next)
              if ((checkHand(draw:hand)) > 21)  --Playerの手札が21を超えてしまっていないかの判定。超えた場合はBust
                then putStrLn("You BUST. You Lose.")
                else do putStr ("Your current score is ")
                        putStrLn(show (calHand (draw:hand)))
                        playInteractive deck (draw:hand)

--PlayerがBustせず、Standできたときの勝利判定。
showResult :: Deck -> Hand -> IO()
showResult deck hand =
  do
    let playerScore = calHand hand
    putStr("You choose to STAND. Your final score is ")
    putStrLn(show (calHand hand))
    let compScore = getCompScore deck []
    if (compScore > 21)                        ------ComputerがBustしたときの処理
      then putStrLn("Computer BUST. You Win.")
      else do putStr("Computer's final score is ") -----ComputerがBustしなかったときの勝利判定
              putStrLn(show (compScore))
              if (playerScore > compScore)
                then putStrLn("You Win")
                else if (playerScore == compScore)
                  then putStrLn("Draw")
                else putStrLn("You Lose")

--Computerに山札からカードをひかせる。ただし手札が17以上になったらそこでStandする。
getCompScore :: Deck -> Hand -> Card
getCompScore (next:deck) hand
  | ((checkHand(next:hand)) > 21) = 99
  | ((checkHand(next:hand)) >= 17) = (calHand (next:hand))
  | otherwise = 0 + getCompScore deck (next:hand)
