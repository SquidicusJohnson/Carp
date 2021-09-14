module Ops (boxOps, memoryOps, literals) where

-- |Control flow instructions
boxOps :: [Char]
boxOps    = "<>^v/\\|_#!"

-- |Stack of stack instructions
memoryOps :: [Char]
memoryOps = "+-*,%=)(:~$@}{rl[]&"

-- |List of instructions which require IO or affect both the stack of stacks
--  and control flow
otherOps :: [Char]
otherOps  = "onix?.gp;"

-- |Hex literals
literals :: [Char]
literals  = "0123456789abcdef"
