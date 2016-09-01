import System.Random
import Data.List

data Direction = North | South | East | West
			   deriving (Show, Eq)
data Side = Top | Front | Right 
		   deriving (Show, Eq)

--data Point = Point {corX, corY :: Int} 
--		   deriving (Show, Eq)

-- (x, y)
type Point = Pt (Int, Int)
-- (UpperLeftCorner, BottomRightCorner)
type Rectangle = Rec (Point, Point)
-- (width, height, depth)
type Dim = Dim (Int, Int, Int)

-- Block {Dim, LavyHornyRoh}
data Block = Block {dim :: BlockDimensions, point :: Point}
		   deriving (Show, Eq)

--- funkcia co vrati ake vsetky policka zabera
-- lavy horny roh tej steny

-- funkcia co vezme suradnicu a vrati lavu dolnu po otoceni
getBlockAfterRotation :: Block -> Direction -> Block
getBlockAfterRotation (Block (dim = dim, point = pt)) dir =
	Block (newDim, newPt)
	where 
		newDim = rotateBlockDimensions dim dir
		newPt = pointAfterRotation pt dim dir


--	= (newDim, newP)
--	where
--	 newDim = rotateBlockDimensions dim dir	   
--	 -- musim pricitat k suradnici, no neviem ako sa to robi
--	 newP
--	 	| dir == North = Point {corX = (corX p), corY = (corY p + height dim)}

---- tu hore som a chcem rotovat orientaciu a to ako sa zmeni bod- --- chce to
 -- dalsie dve fukncie

pointAfterRotation :: Point -> Dim -> Direction -> Point
pointAfterRotation (Point (x, y)) (Dim (a, b, c)) dir
	| dir == North = Pt (x, y - b)
	| dir == South = Pt (x, y + b)
	| dir == East  = Pt (x - a, y)
	| dir == West  = Pt (x + a, y)


rotateBlockDimensions :: Dim -> Direction -> Dim
rotateBlockDimensions (Dim (a, b, c)) dir
	| dir == North || dir == South = (Dim (a, c, b))
	| dir == East  || dir == West  = (Dim (b, a, c))


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
