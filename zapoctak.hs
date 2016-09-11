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

type Map = [[MapData]]

type MapWithBlock = (Map, Block) 


--nacitavanie zo vstupu
--prva pozicia je false

test = do
	g <- getStdGen
	putStrLn $ show $ createNewMap (Pt (5,5)) (15,15) 200 g

createNewMap :: RandomGen t => Point -> (Int, Int) -> Int -> t -> [(Map, Block,Int)]
createNewMap pt (wid, hei) val g =
	take 1 $ filter (sufficientMetric val) mapsWithInfos
	where 
	 block = Block {dim = Dim (1,2,1), point = pt}
	 (maps, newGen) = mapGenerator [(grid wid hei False, block)] 30 g
	 mapsWithInfos = map (addInfoToMap block) maps

sufficientMetric :: Int -> (Map, Block, Int) -> Bool
sufficientMetric goal (mapa, block, m)=
	if m > goal then True
	else False

addInfoToMap :: Block -> MapWithBlock-> (Map, Block, Int)
addInfoToMap block (mapa, _) = (mapa, endBlock, metric)
	where
	 (endBlock, metric) = getMetric mapa [block] [] 0

-- ak je vacsia diera koniec tak moze spadnut ne

grid x y = replicate x . replicate y

getMetric :: Map -> [Block] -> [Block] -> Int -> (Block, Int)
getMetric _ [] (lastVisited:rest) val = (lastVisited, val)
getMetric mapa (current:queue) visited val =
	if elem current visited 
		then getMetric mapa queue visited val
		else
 		 getMetric mapa (queue ++ newQueue) (current:visited) newVal
	where
	 newQueue = 
	 	filter (areFieldsUnderBlockTrue mapa) $ map (getBlockAfterRotation current) [North, West, South, East]
	 newVal = val + (length newQueue - 1)^2




areFieldsUnderBlockTrue :: Map -> Block -> Bool
areFieldsUnderBlockTrue mapa block = 
	and [(isBlockInMap block mapa),(all (and) (mapForMove mapa block))]

--zislo by sa prepisat do jednej lebo podobna je dole
setIfThereIsNotBlock :: Block -> Point -> Bool -> Bool
setIfThereIsNotBlock block pt orig =
	if isFieldUnderBlock block pt then orig
	else True

mapForMove :: Map -> Block -> Map 
mapForMove mapa block = applyFunctionOnMap (setIfThereIsNotBlock block) mapa  



---

mapGenerator :: RandomGen t => [MapWithBlock] -> Int -> t -> ([MapWithBlock], t)
mapGenerator listOfMaps 0 g = (listOfMaps, g)
mapGenerator [] _ g = ([], g)
mapGenerator (mapa:listOfMaps) depth g = 
	(proccessedHead ++ proccessedTail, newGen)
	where
	 (proccessedTail, newGen) = mapGenerator listOfMaps depth tmpGen
	 (proccessedHead, tmpGen) = proccessMap mapa depth g

proccessMap :: RandomGen t => MapWithBlock -> Int -> t -> ([MapWithBlock], t)
proccessMap mapa depth g =
	let (generatedMaps, newG) = mapsAfterMove mapa g
 	in
		mapGenerator generatedMaps (depth-1) newG

--generatorThroughMaps :: RandomGen t => [MapWithBlock] -> Int -> t ->	([MapWithBlock], t)
--generatorThroughMaps [] _ g = ([], g)
--generatorThroughMaps (map:mapsRest) depth g = 



mapsAfterMove :: RandomGen t => MapWithBlock -> t -> ([MapWithBlock], t)
mapsAfterMove mapa g = 
	((allMapsOfMoves mapa dirs), newG)
	 where (dirs, newG) = shuffle [North, West, South, East] g

allMapsOfMoves :: MapWithBlock -> [Direction] -> [MapWithBlock]
allMapsOfMoves mapWithBlock dirs = 
	excludeNothing $ map (checkedMapWithBlockAfterMove mapWithBlock) dirs 

excludeNothing :: [Maybe a] -> [a]
excludeNothing [Nothing] = []
excludeNothing [Just x] = [x]
excludeNothing (x:xs) = (excludeNothing [x]) ++ (excludeNothing xs)

checkedMapWithBlockAfterMove :: MapWithBlock -> Direction -> Maybe MapWithBlock
checkedMapWithBlockAfterMove mapBlock dir =
	if isBlockInMap newBlock newMap then Just newMapWithBlock
	else Nothing
 	where
	  newMapWithBlock = mapWithBlockAfterMove mapBlock dir
	  (newMap, newBlock) = newMapWithBlock


mapWithBlockAfterMove :: MapWithBlock -> Direction -> MapWithBlock
mapWithBlockAfterMove (map, block) dir = (newMap, newBlock)
	where
	 newMap = updateMap map newBlock
	 newBlock = getBlockAfterRotation block dir

isBlockInMap :: Block -> Map -> Bool
isBlockInMap Block {dim = dim, point = Pt (x, y)} mapa =
	if x >= 0 && y >= 0  && x + a  < s && y + c < v then True
	else False 
 	where
	  (s,v) = sizeOfMap mapa
	  Dim (a,b,c) = dim

sizeOfMap :: Map -> (Int, Int)
sizeOfMap mapa@(head:rest) = (length head, length mapa)
-------

updateMap :: Map -> Block -> Map 
updateMap mapa block = applyFunctionOnMap (setIfThereIsBlock block) mapa  

applyFunctionOnMap :: (Point -> MapData -> MapData) -> Map -> Map
applyFunctionOnMap f mapa =
	functionOnCoorMap (f) (addCoordsToAMap mapa)

addLineNumber mapa = zip [0..] mapa
addColumnNumber (y, val) = (y, addLineNumber val)
addCoordsToAMap mapa = map addColumnNumber (addLineNumber mapa)

type CoorMap = [CoorRow]
type CoorRow = (Int, [CoorColumns])
type CoorColumns = (Int, MapData)
type MapData = Bool

functionOnCoorMap :: (Point -> MapData -> MapData) -> CoorMap -> [[MapData]]
functionOnCoorMap f coorMap = map (functionOnRow f) coorMap

functionOnRow :: (Point -> MapData -> MapData) -> CoorRow -> [MapData]
functionOnRow (f) (numOfRow, columns) = 
	map (wrapperForFunction f numOfRow) columns

wrapperForFunction :: (Point -> a -> a) -> Int -> (Int, a) -> a 
wrapperForFunction f y (x, val) = f (Pt (x,y)) val

--giveSum' :: Point -> Int -> Int
--giveSum' (Pt (x, y)) val = x + y + val

setIfThereIsBlock :: Block -> Point -> Bool -> Bool
setIfThereIsBlock block pt orig =
	if isFieldUnderBlock block pt then True
	else orig

isFieldUnderBlock :: Block -> Point -> Bool
isFieldUnderBlock (block) (pt) =
	if isPointInRectangle rec pt then True
	else False
	where
		rec = allBottomFields block

-- Odpovie ci dany bod sa nachadza v uzavretom obdlzniku
isPointInRectangle :: Rectangle -> Point -> Bool
isPointInRectangle (Rec (Pt (x1, y1), Pt (x2, y2))) (Pt (x, y)) =
	if isInRange x x1 x2 && isInRange y y1 y2 then True
 	else False

-- Odpovie ci dane cislo sa nachadza v uzavretom intervale
isInRange :: Int -> Int -> Int -> Bool
isInRange val start end =
	if val >= start && val <= end then  True
 	else False

-- Vrati obdlznik pod spodnou podstavou
allBottomFields :: Block -> Rectangle
allBottomFields (Block {dim = Dim (a,b,c), point = Pt (x, y)}) =
	Rec (Pt (x,y), Pt (x + a - 1, y + c - 1))

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
