import System.Random
import Data.List

data Direction = North | South | East | West
			   deriving (Show, Eq)

wrapper a = do
	return a

--main = do
--	g <- getStdGen
--	rest g
--	fail "abc" 

main = do 
	g <- getStdGen
	putStrLn $ show $ take 3 (listOfPermutations g)

listOfPermutations :: RandomGen t => t -> [[Direction]]
listOfPermutations g = 
	(shuffled:listOfPermutations newG)
 	where (shuffled, newG) = shuffle [North, South, East, West] g

shuffle :: RandomGen t => [a] -> t -> ([a], t)
shuffle xs g = 
	let (permNum,newGen) = randomR (0, fac (length xs) -1) g
	in ((permutations xs) !! permNum, newGen)

fac :: Int -> Int
fac 0 = 1
fac x = x * (fac (x-1))
