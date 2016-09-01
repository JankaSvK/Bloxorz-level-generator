import System.Random
import Data.List

data Direction = North | South | East | West
			   deriving (Show, Eq)

data Point2 = (Int, Int)

data Point = Point {corX, corY :: Int} 
		   deriving (Show, Eq)

--type Width = Int
--type Height = Int
--type Depth = Int
data BlockDimensions = BlockDimensions {width, height, depth :: Int}
					deriving (Show, Eq)

data Side = Top | Front | Right 
		   deriving (Show, Eq)

-- Orientation (Side on plane, Side on the North)
--type BottomSide = Side
--type NorthSide = Side
--data Orientation = BottomSide NorthSide 
--				 deriving (Show, Eq)

data Block = Block {blockDimensions :: BlockDimensions, point :: Point}
		   deriving (Show, Eq)

type UpperLeftCorner = Point
type BottomRightCorner = Point
type Rectangle = (UpperLeftCorner, BottomRightCorner)

--- funkcia co vrati ake vsetky policka zabera
-- lavy horny roh tej steny

-- funkcia co vezme suradnicu a vrati lavu dolnu po otoceni
--getBlockAfterRotation :: Block -> Direction -> Block
--getBlockAfterRotation (dim, Point p) dir
--	= (newDim, newP)
--	where
--	 newDim = rotateBlockDimensions dim dir	   
--	 -- musim pricitat k suradnici, no neviem ako sa to robi
--	 newP
--	 	| dir == North = Point {corX = (corX p), corY = (corY p + height dim)}

---- tu hore som a chcem rotovat orientaciu a to ako sa zmeni bod- --- chce to
 -- dalsie dve fukncie


rotateBlockDimensions :: BlockDimensions -> Direction -> BlockDimensions
rotateBlockDimensions (BlockDimensions (a b c)) dir
	| dir == North || dir == South = (BlockDimensions (a c b))
	| dir == East  || dir == West  = (BlockDimensions (b a c))


wrapper a = do
	return a

--main = do
--	g <- getStdGen
--	rest g
--	fail "abc" 

main = do 
	g <- getStdGen
	putStrLn $ show $ take 3 (listOfPermutations g)


-- riesime kreslenie mapy na zaklade vygenerovanej postupnosnti


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
