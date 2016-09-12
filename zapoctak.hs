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

-- Pomocné typy pre pridávanie súradníc k mape
type CoorMap = [CoorRow]
type CoorRow = (Int, [CoorColumns])
type CoorColumns = (Int, MapData)
type MapData = Bool

main :: IO()
main = do
	g <- getStdGen
   	input <- proccessInput
	(resMap, end, val) <- return $ createNewMap input g
	putStrLn $ listListPretty $ resMap
	putStr $ "Koniec má tento tvar: "
	putStrLn $ show $ end
	putStr $ "Metrika mapy je: "
	putStrLn $ show $ val

-- Načítavanie vstupu
proccessInput :: IO (Point, (Int, Int), Dim, Int, Int)
proccessInput = do
	putStrLn $ "Zadajte rozmery bloku -- šírka výška hĺbka --"
	blockDim <- readInts
	putStrLn $ "Zadajte veľkosť mapy -- šírka výška --"
	mapSize <- readInts
	putStrLn $ "Zadajte počiatočné súradnice -- x y --"
	startPoint <- readInts 
	putStrLn $ "Zadajte minimálnu obtiažnosť mapy"
	metric <- readInt
	putStrLn $ "Zadajte hĺbku generovanéj mapy"
	depth <- readInt
	start <- return $ Pt (startPoint !! 0, startPoint !! 1)
  	dim <- return $ Dim (blockDim !! 0, blockDim !! 1, blockDim !! 2)
	map <- return $ (mapSize !! 0, mapSize !! 1)
  	return (start, map, dim, metric, depth)

-- Načíta číslo
readInt :: IO Int
readInt = do
  	input <- getLine
	return (read input :: Int)

-- Načíta čísla oddelené bielymi znakmi do listu
readInts :: IO [Int]
readInts = do
   	input <- getLine
	return (map read $ words input :: [Int])

-- Pomôcky pre pekný výpis mapy
pretty :: Bool -> String
pretty False = "."
pretty True = "#"

listPretty :: [Bool] -> String
listPretty (a:as) = pretty a ++ listPretty as
listPretty [] = ""

listListPretty :: [[Bool]] -> String
listListPretty (a:as) = listPretty a ++ "\n" ++ listListPretty as
listListPretty [] = ""

-- Vytvorí mapu zadaných parametrov
createNewMap :: RandomGen t => (Point, (Int, Int), Dim, Int, Int) -> t -> (Map, Block,Int)
createNewMap (pt, (wid, hei), dim, val, depth) g =
	(updateMap resMap (Block {dim = dim, point = pt}), resEndBlock, resVal)
	where 
	 block = Block {dim = dim, point = pt}
	 (maps, newGen) = mapGenerator [(grid wid hei False, block)] depth g
	 mapsWithInfo = map (addInfoToMap block) maps
	 result = (take 1 $ filter (sufficientMetric val) mapsWithInfo) !! 0
	 (resMap, resEndBlock, resVal) = result

-- Skontroluje, či metrika mapy je dostačujúca
sufficientMetric :: Int -> (Map, Block, Int) -> Bool
sufficientMetric goal (mapa, block, m) = m >= goal

-- Vytvorí trojicu mapy s metrikou a koncovou pozíciou bloku
addInfoToMap :: Block -> MapWithBlock -> (Map, Block, Int)
addInfoToMap block (mapa, _) = (mapa, endBlock, metric)
	where
	 (endBlock, metric) = getMetric mapa [block] [] 0

-- Vytvorí list listov daných rozmerov
grid x y = replicate x . replicate y

-- Pomocou BFS vyhodnotí metriku mapy
getMetric :: Map -> [Block] -> [Block] -> Int -> (Block, Int)
getMetric _ [] (lastVisited:rest) val = (lastVisited, val)
getMetric mapa (current:queue) visited val =
	if elem current visited 
		then getMetric mapa queue visited val
		else
 		 getMetric mapa (queue ++ newQueue) (current:visited) newVal
	where
	 newQueue = filter (areFieldsUnderBlockTrue mapa) $ map (getBlockAfterRotation current) [North, West, South, East]
	 newVal = val + (length newQueue - 1)^2

-- Skontroluje, či pod blokom sú existujúce políčka
areFieldsUnderBlockTrue :: Map -> Block -> Bool
areFieldsUnderBlockTrue mapa block = 
	and [(isBlockInMap block mapa),(all (and) (mapForMove mapa block))]


-- Pripraví mapu pre kontrolovanie možného ťahu
mapForMove :: Map -> Block -> Map 
mapForMove mapa block = applyFunctionOnMap (setIfThereIsNotBlock block) mapa  

-- Mapový generátor
mapGenerator :: RandomGen t => [MapWithBlock] -> Int -> t -> ([MapWithBlock], t)
mapGenerator listOfMaps 0 g = (listOfMaps, g)
mapGenerator [] _ g = ([], g)
mapGenerator (mapa:listOfMaps) depth g = 
	(proccessedHead ++ proccessedTail, newGen)
	where
	 (proccessedTail, newGen) = mapGenerator listOfMaps depth tmpGen
	 (proccessedHead, tmpGen) = proccessMap mapa depth g

-- Pomocná funkcia mapového generátoru 
proccessMap :: RandomGen t => MapWithBlock -> Int -> t -> ([MapWithBlock], t)
proccessMap mapa depth g =
	let (generatedMaps, newG) = mapsAfterMove mapa g
 	in
		mapGenerator generatedMaps (depth-1) newG

-- Vráti korektné mapy po otočení smermi náhodne generovanými
mapsAfterMove :: RandomGen t => MapWithBlock -> t -> ([MapWithBlock], t)
mapsAfterMove mapa g = ((allMapsOfMoves mapa dirs), newG)
	where (dirs, newG) = shuffle [North, West, South, East] g

-- Vráti korektné mapy po otočení bloku zadanými smermi
allMapsOfMoves :: MapWithBlock -> [Direction] -> [MapWithBlock]
allMapsOfMoves mapWithBlock dirs = 
	excludeNothing $ map (checkedMapWithBlockAfterMove mapWithBlock) dirs 

-- List skráti o výskyt všetkých Nothing a z Just a urobí a
excludeNothing :: [Maybe a] -> [a]
excludeNothing [Nothing] = []
excludeNothing [Just x] = [x]
excludeNothing (x:xs) = (excludeNothing [x]) ++ (excludeNothing xs)

-- Skontroluje, či nová mapa (po otočení) je koretná - či je blok vo vnútri
checkedMapWithBlockAfterMove :: MapWithBlock -> Direction -> Maybe MapWithBlock
checkedMapWithBlockAfterMove mapBlock dir =
	if isBlockInMap newBlock newMap then Just newMapWithBlock
	else Nothing
 	where
	 newMapWithBlock = mapWithBlockAfterMove mapBlock dir
	 (newMap, newBlock) = newMapWithBlock

-- Mapa s pridanými políčkami, po tom, čo sa blok nejaým smerom otočil
mapWithBlockAfterMove :: MapWithBlock -> Direction -> MapWithBlock
mapWithBlockAfterMove (map, block) dir = (newMap, newBlock)
	where
	 newMap = updateMap map newBlock
	 newBlock = getBlockAfterRotation block dir

-- Skontroluje, či sa blok nachádza v rozsahu mapy
isBlockInMap :: Block -> Map -> Bool
isBlockInMap Block {dim = dim, point = Pt (x, y)} mapa =
	if x >= 0 && y >= 0  && x + a  < s && y + c < v then True
	else False 
 	where
	 (s,v) = sizeOfMap mapa
	 Dim (a,b,c) = dim

-- Vráti dvojicu veľkosti mapy
sizeOfMap :: Map -> (Int, Int)
sizeOfMap mapa@(head:rest) = (length head, length mapa)

-- Aktualizuje mapu podľa umiestnenia bloku (políčkam pod ním da True0
updateMap :: Map -> Block -> Map 
updateMap mapa block = applyFunctionOnMap (setIfThereIsBlock block) mapa  

-- Aplikuje funkciu na obyčajnú mapu
applyFunctionOnMap :: (Point -> MapData -> MapData) -> Map -> Map
applyFunctionOnMap f mapa =
	functionOnCoorMap (f) (addCoordsToAMap mapa)

-- Postupne pridáva súradnice k mape
addLineNumber mapa = zip [0..] mapa
addColumnNumber (y, val) = (y, addLineNumber val)
addCoordsToAMap mapa = map addColumnNumber (addLineNumber mapa)

-- Aplikuje funkciu na list listov so súradnicami
functionOnCoorMap :: (Point -> MapData -> MapData) -> CoorMap -> [[MapData]]
functionOnCoorMap f coorMap = map (functionOnRow f) coorMap

-- Aplikuje funkciu na riadok so súradnicami
functionOnRow :: (Point -> MapData -> MapData) -> CoorRow -> [MapData]
functionOnRow (f) (numOfRow, columns) = 
	map (wrapperForFunction f numOfRow) columns

-- Upravuje parametre pre funkciu
wrapperForFunction :: (Point -> a -> a) -> Int -> (Int, a) -> a 
wrapperForFunction f y (x, val) = f (Pt (x,y)) val

-- Ak je políčko pod blokom, tak ho nastaví na True
setIfThereIsBlock :: Block -> Point -> Bool -> Bool
setIfThereIsBlock block pt orig =
	if isFieldUnderBlock block pt then True
	else orig

-- Ak políčko nie je pod blkom, nastaví na True
setIfThereIsNotBlock :: Block -> Point -> Bool -> Bool
setIfThereIsNotBlock block pt orig =
	if isFieldUnderBlock block pt then orig
	else True

-- Odpovie, či dané políčko sa nachádza pod blokom 
isFieldUnderBlock :: Block -> Point -> Bool
isFieldUnderBlock (block) (pt) = isPointInRectangle rec pt
	where
	 rec = allBottomFields block

-- Odpovie, či daný bod sa nachádza v uzavretom obdĺžniku
isPointInRectangle :: Rectangle -> Point -> Bool
isPointInRectangle (Rec (Pt (x1, y1), Pt (x2, y2))) (Pt (x, y)) = isInRange x x1 x2 && isInRange y y1 y2

-- Odpovie, či dané číslo sa nachádza v uzavretom intervale
isInRange :: Int -> Int -> Int -> Bool
isInRange val start end = val >= start && val <= end

-- Vráti obdĺžnik pod spodnou podstavou
allBottomFields :: Block -> Rectangle
allBottomFields (Block {dim = Dim (a,b,c), point = Pt (x, y)}) =
	Rec (Pt (x,y), Pt (x + a - 1, y + c - 1))

-- Vráti blok aj so súradnicami po otočení daným smerom
getBlockAfterRotation :: Block -> Direction -> Block
getBlockAfterRotation (Block {dim = dim, point = pt}) dir =
	Block {dim = newDim, point = newPt}
	where 
	 newDim = rotateBlockDimensions dim dir
	 newPt = pointAfterRotation pt dim dir

-- Vráti novú súradnicu po otočeni daným smerom
pointAfterRotation :: Point -> Dim -> Direction -> Point
pointAfterRotation (Pt (x, y)) (Dim (a, b, c)) dir
	| dir == North = Pt (x, y - b)
	| dir == South = Pt (x, y + c)
	| dir == East  = Pt (x + a, y)
	| dir == West  = Pt (x - b, y)

-- Vráti nové rozmery bloku po otočení daným smerom
rotateBlockDimensions :: Dim -> Direction -> Dim
rotateBlockDimensions (Dim (a, b, c)) dir
	| dir == North || dir == South = (Dim (a, c, b))
	| dir == East  || dir == West  = (Dim (b, a, c))

-- Vráti list permutovaných štvoríc svetových strán
listOfPermutations g = 
	(shuffled:listOfPermutations newG)
 	where (shuffled, newG) = shuffle [North, South, East, West] g

-- Spermutuje daný list a vráti nový generátor
shuffle :: RandomGen t => [a] -> t -> ([a], t)
shuffle xs g = 
	let (permNum,newGen) = randomR (0, fac (length xs) -1) g
	in ((permutations xs) !! permNum, newGen)

-- Faktoriál prirodzeného čísla
fac :: Int -> Int
fac 0 = 1
fac x = x * (fac (x-1))
