import System.Environment
import Stacks
import Data.List (elemIndex)
import RunTime (startFish)

main :: IO ()
main = getArgs              >>= (\args   ->
       readFile (head args) >>= (\source ->
       let stacks = if length args > 1
                        then prepopulate [] args
                        else ([], Nothing) : []
       in startFish stacks source))

prepopulate :: [(Int, Int)] -> String -> FishSticks
