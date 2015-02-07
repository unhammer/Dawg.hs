{-# OPTIONS_GHC -Wall #-}

{-|
Module      : Dawg
Description : A directed acyclic word graph
Copyright   : (c) Kevin Brubeck Unhammer, 2015
License     : GPL-3
Maintainer  : unhammer@fsfe.org
Stability   : experimental
Portability : POSIX

A work in progress.
-}

module Dawg where

import qualified Data.Map as Map
import           Prelude  hiding (lookup)


-- * Types

-- | The main dawg type:
data Node = Node {
  edges :: Map.Map Char Node,
  final :: Bool
  } deriving (Show, Eq, Ord)


-- | A zipper-like view onto an *unfinished* Dawg (finished by calling finish)
data View = View {
  prevWord  :: String,
  nexus     :: Node,
  unchecked :: Unchecked,
  minimised :: Minimap
  } deriving (Show)

-- | Two nodes can be merged during minimisation if they have the same
-- finality, and if their children are the same.
type Repr = (Bool, [(Char,Node)])
type Minimap = Map.Map Repr Node

-- | Nodes that haven't been merged yet, with the letter pointing to the
-- next node in this list (or previous; this list is typically in
-- reverse order)
type Unchecked = [(Node,Char)]


-- * Pretty printing
walk :: Int -> Node -> Node -> String
walk i n needle =
  let indent = take i $ repeat '\t'
      subf l child acc = indent ++ [l] ++ "→\t" ++ (walk (i+1) child needle) ++ acc
      sub = Map.foldWithKey subf [] (edges n)
      dot = if final n then "⬤" else ""
      node = if n == needle then "[○]" else "○"
  in
   node ++ dot ++ "\n" ++ sub

walk' :: Int -> Node -> String
walk' i n = walk i n n

printNode :: Node -> IO ()
printNode = putStrLn . walk' 0
printView :: View -> IO ()
printView v =
  let root = fst . last $ unchecked v in
  putStrLn $ walk 0 root (nexus v)

-- * Empty data
empty :: Node
empty = Node {
  edges = Map.empty,
  final = False
  }

emptyView :: View
emptyView = View {
  prevWord = "",
  unchecked = [],
  nexus = empty,
  minimised = Map.empty
  }

-- * Minor helpers

commonPrefix :: Eq a => [a] -> [a] -> Int
commonPrefix s1 s2 =
  length $ takeWhile (\(a,b) -> a==b) $ zip s1 s2

nodeKey :: Node -> Repr
nodeKey n =
  (final n, edgeIds) where
    edgeIds = Map.foldWithKey (\k v b -> (k,v):b) [] (edges n)

addEdge :: Node -> Char -> Node -> Node
addEdge n l c =                 -- replace!
  n{ edges = Map.insert l c (edges n) }

addEdges :: Node -> Node -> Node
addEdges node newNode =         -- prefers edges from newNode
  node{ edges = Map.union (edges newNode) (edges node) }

childOf :: Char -> Node -> Maybe Node
childOf l n =
  Map.lookup l (edges n)

childOfExn :: Char -> Node -> Node
childOfExn l n =
  case childOf l n of
   Nothing -> error "Edge not found in node, shouldn't happen"
   Just c -> c

-- * Insertion and minimisation

{- |
Minimise checks the unchecked nodes for redundant nodes, starting from
last one down to the common prefix size. The new unchecked nodes are
the ones before the common prefix.

nexus is expected to be the real child of the last unchecked node, use
the minimise wrapper to ensure this, since the real child can change
from the `childOf parent letter` throughout the recursion.
-}
mindown :: Int -> View -> View
mindown 0 v = v
mindown i v =
  case unchecked v of
  [] -> v
  (parent,letter):newUnchecked ->
    let child = nexus v     -- may differ from childOf parent letter!
        childKey = nodeKey child
        m = minimised v
    in
     case Map.lookup childKey m of
     Just existingChild -> -- orphan the given child:
       mindown (i-1) v{ nexus = addEdge parent letter existingChild,
                      unchecked = newUnchecked
                    }
     Nothing -> -- use the given child (set by our caller):
       mindown (i-1) v{ nexus = addEdge parent letter child,
                      unchecked = newUnchecked,
                      minimised = Map.insert childKey child m
                    }

-- | Wrapper for mindown, so caller doesn't have to ensure nexus is set
-- to the last unchecked node
minimise :: Int -> View -> View
minimise common v =
  case unchecked v of
  [] -> v
  (p,l):_ ->
    let downTo = length (unchecked v) - common in
    mindown downTo v{ nexus = childOfExn l p }




-- | Returns the node list in the order of suf
suffixToNodes :: String -> Unchecked
suffixToNodes suf =
  let lastNode = Node { edges = Map.empty,
                        final = True }
  in
   add (reverse suf) lastNode []
  where
    add "" _ acc = acc
    add (letter:pref) nextNode acc =
      let node = Node { edges = Map.singleton letter nextNode,
                        final = False }
          acc' = (node, letter):acc
      in
       add pref node acc'

-- | First argument is in reverse order, second in the order of suf,
-- return argument is in the order of suf
joinU :: Unchecked -> Unchecked -> Unchecked
joinU [] u = u
joinU u [] = u
joinU ((p,l):rest) ((c,l'):rest') =
  let pChild = childOfExn l p
      cMerged = addEdges pChild c
      pNew = addEdge p l cMerged
  in
   joinU rest ((pNew,l):(cMerged,l'):rest')

addSuffix :: String -> View -> View
addSuffix suf v =
  let uRev = joinU (unchecked v) $ suffixToNodes suf
      uRev' = case uRev of
               [] -> []
               (r,l):rest -> ((addEdges (nexus v) r),l):rest
      root = case uRev of
              [] -> nexus v
              (r,_):_ -> r
  in v{ unchecked = reverse uRev',
        nexus = root
      }

insert :: String -> View -> View
insert word v =
  if word < (prevWord v) then
    error ("words should be inserted in alphabetical order, "++word++">="++(prevWord v))
  else
    let common = commonPrefix word (prevWord v)
        suffix = (drop common word)
        -- | Note: this may look like it happens in the wrong order, but
        -- we can't minimise the current unchecked nodes until the
        -- next word is added (which is why we need a call to finalise
        -- after adding all words)
        vNew = addSuffix suffix $ minimise common v
    in
     vNew { prevWord = word }

finish :: View -> Node
finish v =
  let vMin = minimise 0 v
      vAdded = addSuffix "" vMin
  in
   if length (unchecked vAdded) /= 0 then error "final minimisation didn't work" else
   nexus vAdded

fromList :: [String] -> Node
fromList _ = empty              -- TODO


-- * Lookup
data LookupResult = Final | Prefix | None deriving (Show, Eq)

lookup :: String -> Node -> LookupResult
lookup word n =
  if length word > 0 then
    case Map.lookup (head word) (edges n) of
      Just n' -> lookup (tail word) n'
      Nothing -> None
  else
    if final n then
      Final
    else
      Prefix


-- * Tests
test :: Bool
test =
  let n' = Node {
        edges = Map.empty,
        final = True
        }
      n = Node {
        edges = Map.singleton 'a' n',
        final = False
        }
      (p,_) = head $ suffixToNodes "ab"
  in
   if not (
     lookup "a" n == Final &&
     lookup "ab" n == None &&
     lookup "" n == Prefix &&
     lookup "x" n == None &&
     (Map.size $ edges p) == 1 &&
     (length $ suffixToNodes "") == 0 &&
     (length $ suffixToNodes "a") == 1 &&
     (length $ suffixToNodes "ab") == 2 &&
     (walk' 0 $ finish $ insert "abc" $ insert "ab" emptyView) == "\9675\na\8594\t\9675\n\tb\8594\t\9675\11044\n\t\tc\8594\t\9675\11044\n"
     ) then
     error "tests failed!"
   else True


main :: IO ()
main =
  putStrLn $ show $ commonPrefix "abc" "abd"
