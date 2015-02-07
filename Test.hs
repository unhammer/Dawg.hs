{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MagicHash, BangPatterns #-}
import Dawg hiding (main)
import qualified Data.Map as Map

-- * Debug, for checking that minimisation worked
import Foreign
import System.IO.Unsafe
x === y = System.IO.Unsafe.unsafePerformIO $
  do
    px <- newStablePtr x
    py <- newStablePtr y
    let ret = px == py
    freeStablePtr px
    freeStablePtr py
    return ret

-- * The actual tests
justtrue m =
  case m of
   Just True -> True
   _ -> False

testmin =
  let root = finish $ insert "c" $ insert "b" $ insert "a" emptyView in
  do
    a <- Map.lookup 'a' (edges root)
    b <- Map.lookup 'b' (edges root)
    c <- Map.lookup 'c' (edges root)
    return (a===b && b===c)

testmin2 =
  let root = finish $ insert "ba" $ insert "aa" emptyView in
   do
     a <- Map.lookup 'a' (edges root)
     b <- Map.lookup 'b' (edges root)
     return (a===b)
testmin3 =
  let root = finish $ insert "bb" $ insert "aa" emptyView in
   do
     a <- Map.lookup 'a' (edges root)
     b <- Map.lookup 'b' (edges root)
     return (not (a===b))
testmin4 =
  let root = finish $ insert "catnip" $ insert "cat" $ insert "blip" emptyView
  in
   do
     l <- childOf 'b' root >>= childOf 'l'
     n <- childOf 'c' root >>= childOf 'a' >>= childOf 't' >>= childOf 'n'
     return (l===n)

testall =
  foldr (\a b -> justtrue a && b) True [testmin,testmin2,testmin3,testmin4]

main :: IO ()
main =
  if testall then
    putStrLn "a-ok"
  else
    putStrLn "fail"

-- TODO real tests :)
