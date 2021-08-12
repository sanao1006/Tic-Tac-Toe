import Data.Char
import Data.List
import System.IO
size :: Int
size = 3

type Grid = [[Player]]

data Player = O | B | X
              deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = notElem B . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
        where
            os = length (filter (== O) ps)
            xs = length (filter (== O) ps)
            ps = concat g

-------勝敗決定-----------------------------------------------------------
wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
           where
               line = all (== p)
               rows = g
               cols = transpose g
               dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n<-[0..size-1]]

won :: Grid -> Bool
won g = wins O g || wins X g
------------------------------------------------------------------------


--------格子表示---------------------------------------------------------
putGrid :: Grid -> IO()
putGrid =
    putStrLn  . unlines . concat . interleave bar . map showRow
    where bar = [replicate (size*4-1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
          where
              beside = foldl1  (zipWith (++))
              bar = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["  ", " O", "  "]
showPlayer B = ["  ", "  ", "  "]
showPlayer X = ["  ", " X", "  "]

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys
-----------------------------------------------------------------------

--------------------手の決定--------------------------------------------
valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p =
    [chop size (xs ++ [p] ++ ys) | valid g i]
    where (xs,B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                        return (read xs)
                   else
                        do putStrLn "不可能な動きや！"
                           getNat prompt
--------------------------------------------------------------------

------ターミナル上でプレイできるようにする------------------------------
cls :: IO()
cls = putStr "\ESC[2J"

type Pos =(Int, Int)

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")


prompt :: Player -> String
prompt p = "手を入力してくれ："
----------------------------------------------------------------

data Tree a = Node a [Tree a]
              deriving Show

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p
 |won g = []
 |full g = []
 |otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 9

--ミニマックス法---------------------------------------------
minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
   | wins O g = Node (g, O) []
   | wins X g = Node (g, O) []
   | otherwise  = Node (g, B) []
minimax (Node g ts)
   | turn g == O = Node (g, minimum ps) ts'
   | turn g == X = Node (g, minimum ps) ts'
                   where
                       ts' =  map minimax ts
                       ps  =  [p | Node(_,p) _ <- ts']
bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node(g',p') _ <- ts, p' == best]
               where
                   tree = prune depth (gametree g p)
                   Node (_,best) ts = minimax tree
----------------------------------------------------------


main :: IO ()
main = do hSetBuffering stdout NoBuffering
          play empty O

play :: Grid -> Player -> IO ()
play g p = do cls
              goto (1,1)
              putGrid g
              play' g p


play' :: Grid -> Player -> IO()
play' g p
    | wins O g = putStrLn "あなたの勝ちや！\n"
    | wins X g = putStrLn "CPUの勝ちや!\n"
    | full g   = putStrLn "引き分けや！\n"
    | p == O   = do i <- getNat (prompt p)
                    case move g i p of
                        [] -> do putStrLn "不可能な動きや！"
                                 play' g p
                        [g'] -> play g' (next p)
    | p == X   = do putStrLn "相手が考えチュー..."
                    (play $! bestmove g p) (next p)