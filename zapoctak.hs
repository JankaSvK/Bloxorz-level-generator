import System.Random
import Data.List

data Direction = North | South | East | West
			   deriving (Show, Eq)

data Point = Point Int Int 
		   deriving (Show, Eq)

type Width = Int
type Height = Int
type Depth = Int
data BlockDimensions = BlockDimensions Width Height Depth
		   deriving (Show, Eq)

data Side = Top | Front | Right 
		   deriving (Show, Eq)

-- Orientation (Side on plane, Side on the North)
type BottomSide = Side
type NorthSide = Side
data Orientation = BottomSide NorthSide 
				 deriving (Show, Eq)

type Block = (BlockDimensions, Orientation, Point)

type UpperLeftCorner = Point
type BottomRightCorner = Point
data Rectangle = UpperLeftCorner BottomRightCorner 
			   deriving (Show, Eq)

--- funkcia co vrati ake vsetky policka zabera
-- lavy horny roh tej steny

-- funkcia co vezme suradnicu a vrati lavu dolnu po otoceni
getBlockAfterRotation :: Block -> Direction -> Block
getBlockAfterRotation (dimensions, orient, point) dir
	= (newDimensions, newOrientation, newPoint)
	where 
	   newDimensions = rotateBlockDimension dimensions dir
	   newOrientation = rotateBlockOrientation orient dir
	   

---- tu hore som a chcem rotovat orientaciu a to ako sa zmeni bod- --- chce to
 -- dalsie dve fukncie

-- bottomside northside
rotateBlockOrientation (bottom north) dir =
	| dir == North = Orientation north bottom
 	| dir == East  = Orientation 
---- zrusit orientation a pozerat as na rozmery ako usporiadana trojica

rotateBlockDimensions :: BlockDimensions -> Direction -> BlockDimensions
rotateBlockDimensions (BlockDimensions a b c) dir
	| dir == North || dir == South = (BlockDimensions a c b)
	| dir == East  || dir == West  = (BlockDimensions b a c)


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
