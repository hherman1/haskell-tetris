module Tetris
 where

import Data.List
import Data.Matrix
import Data.Maybe
import System.Random
test = matrix 10 10 $ const Nothing

testGame :: RandomGen r => r -> Game r
testGame g = let (s,g') = randomShape g in
            Game test Nothing s g'

data Shape = J 
           | L 
           | I 
           | S 
           | Z 
           | O 
           | T
           deriving (Eq, Show, Enum, Bounded)

type Hitbox = [[Bool]]

data Obj = Obj Shape Orientation Coord
         deriving (Show,Eq)

type Grid = Matrix (Maybe Shape)

type Coord = (Int,Int)

data Rotation = Clockwise 
              | CounterClockwise
              deriving (Eq, Show)

data Orientation = North
                 | East
                 | South
                 | West
                 deriving (Eq, Show, Enum,Bounded)

data Game g = Game Grid (Maybe Obj) Shape g
            deriving (Show,Eq)

hitbox :: Shape -> Hitbox
hitbox = go
    where
        (t,f) = (True,False)
        go J = [[f,t],
                [f,t],
                [t,t]]
        go L = [[t,f],
                [t,f],
                [t,t]]
        go I = [[t],
                [t],
                [t]]
        go S = [[f,t,t],
                [t,t,f]]
        go Z = [[t,t,f],
                [f,t,t]]
        go O = [[t,t],
                [t,t]]
        go T = [[t,t,t],
                [f,t,f],
                [f,t,f]]

toOrientation :: Int -> Orientation
toOrientation n = toEnum . mod n . fromEnum $ (maxBound :: Orientation)


rotate :: Rotation -> Orientation -> Orientation
rotate Clockwise = toOrientation . succ . fromEnum
rotate CounterClockwise = toOrientation . pred . fromEnum

rotateCoord :: Orientation -> Coord -> Coord
rotateCoord North = id
rotateCoord East = \(x,y) -> (y,-x)
rotateCoord South = rotateCoord East . rotateCoord East
rotateCoord West = rotateCoord East . rotateCoord South

deleteOutofBounds g = deleteUnderflow . deleteOverflow g

deleteUnderflow :: [Coord] -> [Coord]
deleteUnderflow c = filter (\(x,y) -> x >= 1
                                   && y >= 1)
                            c

deleteOverflow :: Matrix a -> [Coord] -> [Coord]
deleteOverflow m c = filter (\(x,y) -> x <= (ncols m)
                                    && y <= (nrows m))
                            c

renderHitbox :: Hitbox -> [Coord]
renderHitbox h = catMaybes
                $ [ if v 
                        then Just (x,y) 
                        else Nothing 
                    | (r,y) <- zip (reverse h) [0..]
                    , (v,x) <- zip r [0..]]


translate :: Coord -> Coord -> Coord
translate (tx,ty) (x,y) = (tx + x, ty + y)

writeToGrid :: Grid -> Obj -> Grid
writeToGrid g obj@(Obj s _ _) = foldr (\c m -> setElem (Just s) c m) g 
                            . deleteOutofBounds g
                            $ coords obj

coords :: Obj -> [Coord]
coords (Obj s o origin) = map (translate origin . rotateCoord o)
                        . renderHitbox 
                        $ hitbox s

randomShape :: RandomGen g => g -> (Shape, g)
randomShape g = (\(v,g') -> (toEnum v,g')) 
              $ randomR (fromEnum (minBound :: Shape)
                        ,fromEnum (maxBound :: Shape)) 
                        g

spawnCoord :: Int -> Int -> Coord
spawnCoord width height = (div width 2,height)

newObj :: Grid -> Shape -> Obj
newObj g s = Obj s North $ spawnCoord (nrows g) (ncols g)

gravitate :: Obj -> Obj
gravitate (Obj s o origin) = Obj s o . translate (0,-1) $ origin

left :: Obj -> Obj
left (Obj s o origin) = Obj s o . translate (-1,0) $ origin

right :: Obj -> Obj
right (Obj s o origin) = Obj s o . translate (1,0) $ origin

willCollide :: Obj -> Grid -> Bool
willCollide o g = let cs = coords . gravitate $ o in
    if any (\(_,y) -> y == 0) cs then
        True
    else
        any isJust
        . map ((flip getAtCoord) g) 
        $ deleteOutofBounds g cs

getAtCoord :: Coord -> Matrix a -> a
getAtCoord (x,y) = getElem x y

step :: RandomGen g => Game g -> Game g
step (Game gr Nothing s r) = let (s',r') = randomShape r in
                           Game gr 
                                (Just $ newObj gr s)
                                s'
                                r'
step (Game gr (Just o) s r) = 
    if willCollide o gr then
        Game (writeToGrid gr o)
             Nothing
             s
             r
    else
        Game gr
             (Just $ gravitate o)
             s
             r

{-
coords (Obj s rs o) = map (translate o . rotateN rs) 
                    . renderHitbox 
                    $ hitbox s
-}
{-
--Returns an empty Tetris grid
newGame :: Int -> Int -> Grid
newGame gridHeight gridWidth = replicate gridHeight (replicate gridWidth Nothing)

--Returns a tuple containing a random shape and a generator

--Updates the state of a Tetris grid by gravitating, clearing lines and
--stopping blocks
update :: Grid -> Shape -> Grid
update state = addBlock (gravitate (clearLines (freezeBlocks state)))

--Adds shaped blocks on top of the grid
addBlock :: Grid -> Shape -> Grid
addBlock rows shape | empty rows && not (gameOver rows) = createShape shape ++ tail (tail (tail (tail rows)))
                    | otherwise = rows

--Drops current shape to the bottom
dropBlock :: Grid -> Grid
dropBlock rows | gravitate rows /= rows = dropBlock (gravitate rows)
               | otherwise = rows

--Speeds up the gravity
speedUp :: Grid -> Grid
speedUp = gravitate

--Moves the moving blocks right
moveRight :: Grid -> Grid
moveRight rows | not(touchright rows) = transpose (gravitate (transpose rows))
               | otherwise = rows
        where
            touchright :: Grid -> Bool
            touchright rows = any moving (mapMaybe last rows)

--Moves the moving blocks left
moveLeft :: Grid -> Grid
moveLeft rows | not(touchleft rows) = map reverse (transpose (gravitate (transpose (map reverse rows))))
              | otherwise = rows
        where
            touchleft :: Grid -> Bool
            touchleft rows = any moving (mapMaybe head rows)

--rotates the moving blocks clockwise
rotate :: Grid -> Grid
rotate grid = insertRotated' (clearGrid grid) (rotateBlock grid) (map (getBlock grid) (movingCoordinates grid))
    where
        insertRotated':: Grid -> [(Int,Int)] -> [Maybe Block] -> Grid
        insertRotated' grid [] _ = grid
        insertRotated' grid (h:t) (val:valt) = insertRotated' (setBlock grid h val) t valt

        clearGrid :: Grid -> Grid
        clearGrid grid = clearGrid' grid (movingCoordinates grid)
            where
                clearGrid' :: Grid -> [(Int,Int)] -> Grid
                clearGrid' = foldl (\ grid h -> setBlock grid h Nothing)

        movingCoordinates :: Grid -> [(Int,Int)]
        movingCoordinates [] = []
        movingCoordinates (h:t) = movingCoordinates' h (25 - length t)  ++ movingCoordinates t
            where
                movingCoordinates' :: Row -> Int -> [(Int,Int)]
                movingCoordinates' [] _ = []
                movingCoordinates' (h:t) y | movingBlock h = (y,9- length t):movingCoordinates' t y
                                        | otherwise = movingCoordinates' t y

        getOrigin::Grid -> (Int,Int)
        getOrigin grid = head (origins grid)

        isOrigin:: Grid -> (Int,Int) -> Bool
        isOrigin grid (x,y) = isJust (getBlock grid (x,y)) && origin (fromJust (getBlock grid (x,y)))

        origins:: Grid -> [(Int,Int)]
        origins grid = filter (isOrigin grid) (movingCoordinates grid)

        rotateBlock:: Grid -> [(Int,Int)]
        rotateBlock grid | hasOrigin grid 
                        && all (unoccupied grid) (map (rotatePoint (getOrigin grid)) (movingCoordinates grid))
                        = map (rotatePoint (getOrigin grid)) (movingCoordinates grid)
                         | otherwise = movingCoordinates grid

        rotatePoint::(Int,Int) -> (Int,Int) -> (Int,Int)
        rotatePoint (originx,originy) (x,y) = (originx + originy - y, originy - originx + x)

        hasOrigin::Grid -> Bool
        hasOrigin grid = not (null (origins grid))

        unoccupied::Grid -> (Int,Int) -> Bool
        unoccupied grid (x,y) = (x > 0 && x < gridHeight && y > 0 && y < gridWidth) 
                     && not (stationaryBlock (getBlock grid (x,y)))

        getBlock :: Grid -> (Int,Int) -> Maybe Block
        getBlock grid (x,y) = (grid !! x) !! y

        setBlock :: Grid -> (Int,Int) -> Maybe Block -> Grid
        setBlock grid (x,y) val =
                fst (splitAt x grid) ++ setBlock' (head (snd(splitAt x grid))) y val:tail(snd (splitAt x grid))
            where
                setBlock' :: Row -> Int -> Maybe Block -> Row
                setBlock' row y val = fst (splitAt y row) ++ val:tail(snd (splitAt y row))

--Gives the score for current state
score :: Grid -> Int
score state = product (replicate 2 (length (filter (==True) (map fullLine state))))

--Indicates whether the given states results in a game over
gameOver :: Grid -> Bool
gameOver state = any (not . all moving . catMaybes) (take 4 state)

---Helpers


gravitate :: Grid -> Grid
gravitate rows | not(stopped rows) = transpose (gravitate_rows (transpose rows))
               | otherwise = rows
    where
        gravitate_row :: Row -> Row
        gravitate_row [] = []
        gravitate_row row | movingBlock (head row) = move_blocks row
        gravitate_row (h:t) = h : gravitate_row t

        gravitate_rows :: Grid -> Grid
        gravitate_rows [] = []
        gravitate_rows lis = gravitate_row (head lis) : gravitate_rows (tail lis)

        --Moves blocks downwards
        move_blocks :: Row -> Row
        move_blocks l | is_gap (gap l) = (Nothing:movingBlocks l) ++ tail (gap l) ++ ground l
            where
                is_gap :: Row -> Bool
                is_gap row = not (null (gap row)) && isNothing (head (gap row))

                movingBlocks :: Row -> Row
                movingBlocks (h:t) | movingBlock h = h:movingBlocks t
                movingBlocks _ = []

                gap:: Row -> Row
                gap (Nothing:t) = Nothing:gap' t
                    where
                        gap' (Nothing:t) = Nothing:gap' t
                        gap' _ = []

                gap (h:t) | movingBlock h = gap t
                gap _ = []

                ground :: Row -> Row
                ground [] = []
                ground (h:t) | stationaryBlock h = h:t
                             | otherwise = ground t

--Determines whether the moving blocks have stopped moving
stopped :: Grid -> Bool
stopped rows = any stopped' (transpose rows) || empty rows
    where
        stopped' :: Row -> Bool
        stopped' [] = False
        stopped' row | all movingBlock row = True
        stopped' (first:second:_) | movingBlock first && stationaryBlock second = True
        stopped' (_:t) = stopped' t

--Determines whether a given block is moving
movingBlock :: Maybe Block -> Bool
movingBlock block = isJust block && moving (fromJust block)

--Determines whether a given block is moving
stationaryBlock :: Maybe Block -> Bool
stationaryBlock block = isJust block && not (moving (fromJust block))

--Determines whether there are no moving blocks
empty :: Grid -> Bool
empty rows = all empty' rows
    where
        empty' :: Row -> Bool
        empty' l | not (any moving (catMaybes l)) = True
        empty' l = False

--Clears all full lines from the grid
clearLines :: Grid -> Grid
clearLines rows | empty rows = replicate (missing_rows rows) empty_row ++ remove_lines rows
                 | otherwise = rows
        where
              missing_rows :: Grid -> Int
              missing_rows rows = length rows - length (remove_lines rows)

              empty_row :: Row
              empty_row = replicate 10 Nothing

              remove_lines :: Grid -> Grid
              remove_lines = filter (not . fullLine)

--Determines whether a line is full
fullLine :: Row -> Bool
fullLine line = filter (/= Nothing) line == line

--Changes moving blocks that have stopped moving to stationary
freezeBlocks :: Grid -> Grid
freezeBlocks rows | stopped rows = map freezeBlocks' rows
                   | otherwise = rows
            where
                freezeBlocks' :: Row -> Row
                freezeBlocks' [] = []
                freezeBlocks' (Just (Block s True o):t) = Just (Block s False o): freezeBlocks' t
                freezeBlocks' b  = head b:freezeBlocks' (tail b)

--Creates a grid containing a given shape to put on top of a game grid
createShape = undefined

-}
