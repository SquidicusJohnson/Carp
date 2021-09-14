module RunTime (startFish, exec, get, put, (&), (#)) where

import Data.Char (ord, chr)
import System.IO (isEOF)
import System.Random
import qualified Data.Map as M
import CodeBox
import Ops
import Stacks
import Fraction

-- |Initialize and call runFish
startFish :: FishSticks -> [Char] -> IO ()
startFish stacks source = runFish stacks (newCodeBox source) source M.empty

-- |Begin execution, reallocate the CodeBox and resume execution
--  when necessary
runFish :: FishSticks -> CodeBox -> [Char] -> Puts -> IO ()
runFish stacks codeBox src metaPuts =
    (exec codeBox stacks False)
    >>= (\(box, newStacks, (m',n')) ->
            if (m',n') == (-1,-1)
                then return ()
                else let (newBox, newMetaPuts) = (reallocate src (getLabel box) (m',n') box metaPuts)
                     in runFish newStacks newBox src newMetaPuts )

exec :: CodeBox -> FishSticks -> Bool -> IO (CodeBox, FishSticks, Coord)
exec codeBox stacks stringMode
    | op == '\'' || op == '\"' = exec next stacks                (not stringMode)
    | stringMode               = exec next (pushChar op stacks)  stringMode
    | op `elem` literals       = exec next (pushHex op stacks)   stringMode
    | op `elem` memoryOps      = exec next (op & stacks)         stringMode
    | op `elem` boxOps         = exec (go $ op # codeBox) stacks stringMode
    | otherwise = case op of

      -- Nop
      ' ' -> exec next stacks stringMode

      -- Char out
      'o' -> let n = abs $ truncate' $ getTop stacks
             in putChar (chr n)
                >> exec next (pop 1 stacks) stringMode

      -- Num out
      'n' -> (let n = getTop stacks
              in if snd n == 1 || n == (0,0)
                     then (putStr . show . fst) n
                     else (putStr . show . toFloat) n )
              >> exec next (pop 1 stacks) stringMode

      -- Char in
      'i' -> isEOF >>= (\eof ->
                 if eof
                     then exec next (push (-1,1) stacks) stringMode
                     else getChar >>= (\n ->
                          exec next (pushChar n stacks) stringMode ))

      -- Conditional Trampoline
      '?' -> if getTop stacks == (0,0)
                 then exec (go $ go codeBox) (pop 1 stacks) stringMode
                 else exec (go codeBox)      (pop 1 stacks) stringMode

      -- Random direction
      'x' -> (randomIO :: IO Int)
             >>= (\n -> let d' = case n `mod` 4 of
                                   0 -> '<'
                                   1 -> '^'
                                   2 -> '>'
                                   3 -> 'v'
                        in exec (go $ d' # codeBox) stacks stringMode )

      -- Jump
      '.' -> let y = truncate' $ getTop stacks
                 x = truncate' $ getTop (pop 1 stacks)
             in exec (go $ goTo (x,y) codeBox) (pop 2 stacks) stringMode

      -- Get command
      'g' -> let y        = truncate' $ getTop stacks
                 x        = truncate' $ getTop (pop 1 stacks)
                 stacks'  = pop 2 stacks
             in exec next (push (get (x,y) codeBox) stacks') stringMode

      -- Put command
      'p' -> let y        = truncate' $ getTop stacks
                 x        = truncate' $ getTop (pop 1 stacks)
                 val      = getTop    $ pop 2 stacks
                 stacks'  = pop 3 stacks
             in case put (x,y) val codeBox of
                    Right rightBox        -> exec (go $ rightBox) stacks' stringMode
                    Left (leftBox, (x,y)) -> return (leftBox, stacks, (x,y))

      -- End Program
      ';' -> return (codeBox, stacks, (-1,-1))

      -- Invalid op
      c   -> error "Something smells fishy..."

     where op   = getOp codeBox
           next = go codeBox

get :: Coord -> CodeBox -> Fraction
get (x,y) box@(grid, _, puts, (m,n))
    | x >= m || y >= n    = (0, 0)
    | M.member (x,y) puts = puts M.! (x,y)
    | c == ' '            = (0, 0)
    | otherwise           = (fromIntegral $ ord c, 1)
        where c = getOp $ goTo (x,y) box

put :: Coord -> Fraction -> CodeBox -> Either (CodeBox, Coord) CodeBox
put (x,y) value box@(grid, d, puts, size@(m,n)) =
    if x < m && y < n
        then Right (grid, d, M.insert (x,y) value puts, size)
        else Left ((grid, d, puts, size), (x,y))

-- |Updates the memory with a memory instruction
(&) :: Char -> FishSticks -> FishSticks
-- Duplicate
':' & ((x : xs, r) : stacks) = (x : x : xs, r) : stacks

-- Pop
'~' & stacks = pop 1 stacks

-- Swap 2
'$' & ((x : y : xs, r) : stacks) = (y : x : xs, r) : stacks

-- Swap 3
'@' & ((x : y : z : xs, r) : stacks) = (y : z : x : xs, r) : stacks

-- Reverse
'r' & ((xs, r) : stacks) = (reverse xs, r) : stacks

-- Push length
'l' & ((xs, r) : stacks) = let n = fromIntegral $ length xs
                           in if n == 0
                               then ((0, 0) : xs, r) : stacks
                               else ((n, 1) : xs, r) : stacks

-- Shift left
'{' & (([], r) : stacks) = ([]                , r) : stacks
'{' & ((xs, r) : stacks) = (last xs : init xs , r) : stacks

-- Shift right
'}' & (([], r) : stacks)   = ([]       , r) : stacks
'}' & ((x : xs, r) : stacks) = (xs ++ [x], r) : stacks

-- New stack
'[' & ((x : xs, r) : stacks) = let n = truncate' x
                             in (take n xs, Nothing) : (drop n xs, r) : stacks

-- Clear stack
']' & ((xs, r) : [])                = ([], r) : []
']' & ((xs, r) : (ys, r2) : stacks) = (xs ++ ys, r2) : stacks

-- Add
'+' & ((x : y : xs, r) : stacks) = (y `add` x : xs, r) : stacks

-- Subtract
'-' & ((x : y : xs, r) : stacks) = (y `sub` x : xs, r) : stacks

-- Multiply
'*' & ((x : y : xs, r) : stacks) = (y `mul` x : xs, r) : stacks

-- Divide
',' & ((x : y : xs, r) : stacks) = (y `div'` x : xs, r) : stacks

-- Modulo
'%' & ((x : y : xs, r) : stacks) = (y `mod'` x : xs, r) : stacks

-- Equality
'=' & ((x : y : xs, r) : stacks) =
    if compare' x y == EQ
        then ((1,1):xs, r) : stacks
        else ((0,0):xs, r) : stacks

-- Greater than
')' & ((x : y : xs, r) : stacks) =
    if compare' x y == GT
        then ((1,1):xs, r) : stacks
        else ((0,0):xs, r) : stacks

-- Less Than
'(' & (( x : y : xs, r) : stacks) =
    if compare' x y == LT
        then ((1,1):xs, r) : stacks
        else ((0,0):xs, r) : stacks

-- Register access
'&' & ((xs  , Just x ) : stacks) = (x:xs, Nothing) : stacks
'&' & ((x:xs, Nothing) : stacks) = (xs  , Just x ) : stacks

c & notEnoughElements = error "Something smells fishy..."


-- |Updates the CodeBox with a control flow instruction.
(#) :: Char -> CodeBox -> CodeBox
-- Up
'^' # (grid, _, puts, s) = (grid, UP,    puts, s)

-- Right
'>' # (grid, _, puts, s) = (grid, RIGHT, puts, s)

-- Left
'<' # (grid, _, puts, s) = (grid, LEFT,  puts, s)

-- Down
'v' # (grid, _, puts, s) = (grid, DOWN,  puts, s)

-- Trampoline
'!' # box = go box

-- Right diagonal mirror
'/' # (grid, d, puts, s) =
    let d' = case d of
               LEFT  -> DOWN
               UP    -> RIGHT
               RIGHT -> UP
               DOWN  -> LEFT
    in (grid, d', puts, s)

-- Left diagonal mirror
'\\' # (grid, d, puts, s) =
    let d' = case d of
               LEFT  -> UP
               UP    -> LEFT
               RIGHT -> DOWN
               DOWN  -> RIGHT
    in (grid, d', puts, s)

-- Vertical mirror
'|' # (grid, d, puts, s) =
    let d' = case d of
               LEFT  -> RIGHT
               UP    -> UP
               RIGHT -> LEFT
               DOWN  -> DOWN
    in (grid, d', puts, s)

-- Horizontal mirror
'_' # (grid, d, puts, s) =
    let d' = case d of
               LEFT  -> LEFT
               UP    -> DOWN
               RIGHT -> RIGHT
               DOWN  -> UP
    in (grid, d', puts, s)

-- Omnidirectional reflectorizer
'#' # (grid, d, puts, s) =
   let d' = case d of
              LEFT  -> RIGHT
              UP    -> DOWN
              RIGHT -> LEFT
              DOWN  -> UP
   in (grid, d', puts, s)

c # grid = error "Something smells fishy..."
