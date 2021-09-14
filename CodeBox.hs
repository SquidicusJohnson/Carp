module CodeBox (CodeBox
               , FishGrid (FishNode)
               , Direction (RIGHT, UP, DOWN, LEFT)
               , Coord
               , Puts
               , newCodeBox
               , reallocate
               , getOp
               , getLabel
               , go
               , goTo
               , zipCoords
               , mkFishGrid
               ) where

import qualified Data.Map as M
import Data.Char (chr)
import Fraction  (Fraction, truncate')

data Direction = LEFT | UP | RIGHT | DOWN

-- |The FishGrid is a graph representing a rectangular grid with nodes containing
-- a coordinate label, the character corresponding to the same location in the
-- fish source code, and pointers to the four adjacent nodes. Nodes on the edges
-- of the grid contain pointers which wrap around to the other side.
data FishGrid = FishNode (Int, Int) Char FishGrid FishGrid FishGrid FishGrid

type Puts = M.Map (Int, Int) (Integer, Integer)

type Coord = (Int, Int)

-- |The CodeBox consists of a pointer into the grid representing the source code,
-- the current direction for the instruction pointer, a Map storing changes
-- made by the 'p' command, and the dimensions of the code grid.
type CodeBox = (FishGrid, Direction, Puts, (Int, Int))

-- |Construct a new codebox from fish source code
newCodeBox :: [Char] -> CodeBox
newCodeBox src = let lines' = lines src
                     l      = foldr1 max $ map length lines'
                     h      = length lines'
                     padded = map (\xs -> xs ++ replicate (l - (length xs)) ' ') lines'
                     grid   = mkFishGrid (zipCoords padded) M.empty
                 in (grid, RIGHT, M.empty, (l,h))

-- |Resize a CodeBox. Changes made by the p command which can added as characters
--  in the FishGrid are placed and removed from the CodeBox's puts map. The metaPuts
--  map keeps cumulative track of changes made if the box is reallocated multiple times.
--  Two empty rows and/or columns are created to accomodate further uses of p in the
--  same area without reallocating every time.
reallocate :: [Char] -> Coord -> Coord -> CodeBox -> Puts -> (CodeBox, Puts)
reallocate src currentPosition@(x,y) newLocation@(m',n') box@(_, d, puts, oldSize@(m,n)) metaPuts =
    let newMetaPuts  = M.union puts metaPuts
        (asciiPuts, nonAsciiPuts) = M.partitionWithKey placeable newMetaPuts
        lines'       = lines src
        srcHeight    = length lines'
        l            = max m (m' + 3)
        h            = max n (n' + 3)
        extraRows    = lines' ++ (replicate (h - srcHeight) [])
        padded       = map (\xs -> xs ++ replicate (l - (length xs)) ' ') extraRows
        grid         = mkFishGrid (zipCoords padded) asciiPuts
    in (goTo (x,y) (grid, d, nonAsciiPuts, (l, h)), newMetaPuts)

-- |Determine whether a Coord Fraction pair can be placed as an ascii character
--  in the codebox.
placeable :: Coord -> Fraction -> Bool
placeable (x,y) (a,b)
    | x < 0 || y < 0 = False
    | b /= 1         = False
    | a > 255        = False
    | otherwise      = True

-- |Zip a list of lists with two dimensional coordinates for each element,
--  starting at (0,0).
zipCoords :: [[a]] -> [[ (Coord, a) ]]
zipCoords code = let coords = [ [(x,y) | x <- [0..] ] | y <- [0..] ]
                 in map (\i -> zip (coords!!i) (code!!i) ) [0..length code - 1]

mkFishGrid :: [[ (Coord, Char) ]] -> Puts -> FishGrid
mkFishGrid xss asciiPuts =
  let (firstRow, lastRow) = stitch lastRow xss firstRow
  in firstRow
    where stitch prevRow []       nextRow = (nextRow, prevRow)
          stitch prevRow (xs:xss) nextRow =
              let thisRow         = mkRow prevRow rest xs asciiPuts
                  (rest, lastRow) = stitch thisRow xss nextRow
              in  (thisRow, lastRow)

mkRow :: FishGrid -> FishGrid -> [(Coord, Char)] -> Puts -> FishGrid
mkRow aboveRow belowRow xs asciiPuts =
  let (first, end) = stitch xs aboveRow belowRow end first
  in first
    where stitch []     above below prev next = (next, prev)
          stitch (x:xs) above below prev next =
              let c           = if M.member (fst x) asciiPuts
                                    then chr . fromInteger . fst $ asciiPuts M.! (fst x)
                                    else snd x
                  this        = FishNode (fst x) c prev above rest below
                  (rest, end) = stitch xs (go' RIGHT above) (go' RIGHT below) this next
              in (this, end)


-- |Extracts the current character from a CodeBox. A value placed by the p
--  command throws an error if it is outside the ASCII range. Fractional values
--  are truncated.
getOp :: CodeBox -> Char
getOp (FishNode (x,y) c _ _ _ _, _, puts, _) =
    case M.lookup (x,y) puts of
      Just n  -> if snd n /= 1 || fst n > 255
                     then error "Something smells fishy..."
                     else chr $ truncate' n
      Nothing -> c

-- |Extracts the current label from a CodeBox
getLabel :: CodeBox -> (Int, Int)
getLabel (FishNode n _ _ _ _ _, _, _, _) = n


-- |Move one step along the FishGrid in a given direction.
go' :: Direction -> FishGrid -> FishGrid
go' d (FishNode _ _ left up right down) =
  case d of
    LEFT  -> left
    UP    -> up
    RIGHT -> right
    DOWN  -> down

-- |Move one step in the current direction of the CodeBox
go :: CodeBox -> CodeBox
go (FishNode _ _ left up right down, direction, puts, size) =
    ( case direction of
        LEFT  -> left
        UP    -> up
        RIGHT -> right
        DOWN  -> down
    , direction
    , puts
    , size )

-- |Jump to the given coordinates in the code area. If the coordinate
--  is outside the exisiting code area, either jump to the edge that would
--  wrap appropriately or generate infinite spaces.
goTo :: Coord -> CodeBox -> CodeBox
goTo (x, y) box@(grid, d, p, size@(m,n))
    | x < 0 || y < 0   = error "Something smells fishy..."
    | x >= m && y >= n = newCodeBox " "
    | x >= m           = case d of
                           UP    -> newCodeBox " "
                           DOWN  -> newCodeBox " "
                           RIGHT -> goTo (m - 1, y) box
                           LEFT  -> goTo (0    , y) box
    | y >= n           = case d of
                           UP    -> goTo (x, 0)     box
                           DOWN  -> goTo (x, n - 1) box
                           RIGHT -> newCodeBox " "
                           LEFT  -> newCodeBox " "
    | otherwise = let (u, v) = getLabel box
                      dx     = if x - u `mod` m < u - x `mod` m
                                   then   x - u `mod` m
                                   else -(u - x `mod` m)
                      dy     = if y - v `mod` n < v - y `mod` n
                                   then   y - v `mod` n
                                   else -(v - y `mod` n)
                      newPos = foldr go' grid ( (replicate dx RIGHT) ++ (replicate (-dx) LEFT)
                                              ++(replicate dy DOWN)  ++ (replicate (-dy) UP) )
                  in (newPos, d, p, size)
