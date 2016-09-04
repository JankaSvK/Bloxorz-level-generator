import System.Random
import Data.List

data Direction = North | South | East | West
			   deriving (Show, Eq)
data Side = Top | Front | Right 
		   deriving (Show, Eq)

-- (x, y)
data Point = Pt (Int, Int)
	deriving (Show, Eq)
-- (UpperLeftCorner, BottomRightCorner)
data Rectangle = Rec (Point, Point)
	deriving (Show, Eq)
-- (width, height, depth)
data Dim = Dim (Int, Int, Int)
	deriving (Show, Eq)

-- Block {Dim, LavyHornyRoh}
data Block = Block {dim :: Dim, point :: Point}
		   deriving (Show, Eq)

-- Vrati obdlznik pod spodnou podstavou
allBottomFields :: Block -> Rectangle
allBottomFields (Block {dim = Dim (a,b,c), point = Pt (x, y)}) =
	Rec (Pt (x,y), Pt (x + a, y + c))

--Vrati Blok aj so suradnicami po otoceni danym smerom
getBlockAfterRotation :: Block -> Direction -> Block
getBlockAfterRotation (Block {dim = dim, point = pt}) dir =
	Block {dim = newDim, point = newPt}
	where 
		newDim = rotateBlockDimensions dim dir
		newPt = pointAfterRotation pt dim dir

--Vrati novu suradnicu po otoceni danym smerom
pointAfterRotation :: Point -> Dim -> Direction -> Point
pointAfterRotation (Pt (x, y)) (Dim (a, b, c)) dir
	| dir == North = Pt (x, y - b)
	| dir == South = Pt (x, y + c)
	| dir == East  = Pt (x + a, y)
	| dir == West  = Pt (x - b, y)

--Vrati nove rozmery Bloku po otoceni danym smerom
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
