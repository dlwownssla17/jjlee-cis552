{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -Werror #-}
{-# LANGUAGE NoImplicitPrelude #-} 

{-
  Names: Jae-Joon Lee (jjlee), Vivek A. Raj (vivekraj)
  CIS 552 HW2
-}

module Main where
import Prelude hiding (takeWhile,all)
import Data.Tuple (swap)
import Test.HUnit      -- unit test support

import XMLTypes        -- support file for XML problem (provided)
import Play            -- support file for XML problem (provided)

doTests :: IO ()
doTests = do 
  _ <- runTestTT $ TestList [ testFoldr, testTree, testXML ]
  return ()

main :: IO ()
main = do 
       doTests
       return ()

----------------------------------------------------------------------

testFoldr :: Test
testFoldr = TestList [tintersperse, tinvert, ttakeWhile, tfind, tall]

-- The intersperse function takes an element and a list 
-- and `intersperses' that element between the elements of the list. 
-- For example,
--    intersperse ',' "abcde" == "a,b,c,d,e"

intersperse ::  a -> [a] -> [a]
intersperse _   []       = []
intersperse sep (x : xs) = x : foldr (\a b -> sep : a : b) [] xs

tintersperse :: Test
tintersperse = "intersperse" ~: TestList [intersperse ' ' "CIS" ~?= "C I S",
                                          intersperse 'a' "b"   ~?= "b",
                                          intersperse '!' ""    ~?= "",
                                          intersperse Nothing [Just 1, Just 2]
                                              ~?= [Just 1, Nothing, Just 2] ]


-- invert lst returns a list with each pair reversed. 
-- for example:
--   invert [("a",1),("a",2)] returns [(1,"a"),(2,"a")] 

invert :: [(a,b)] -> [(b,a)]
invert = map swap

tinvert :: Test
tinvert = "invert" ~: 
  TestList [invert [("a", 1), ("a", 2)]    ~?= [(1, "a"), (2, "a")],
            invert [(True, True)]          ~?= [(True, True)],    
            invert ([] :: [(String, Int)]) ~?= ([] :: [(Int, String)]) ] 


-- takeWhile, applied to a predicate p and a list xs, 
-- returns the longest prefix (possibly empty) of xs of elements 
-- that satisfy p:
-- For example, 
--     takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
--     takeWhile (< 9) [1,2,3] == [1,2,3]
--     takeWhile (< 0) [1,2,3] == []

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f l = reverse . fst $ foldr takeWhile_aux ([], True) (reverse l) where
  takeWhile_aux x (acc, stillTaking) =
    if stillTaking && f x then (x : acc, stillTaking) else (acc, False)
                                                                  
ttakeWhile :: Test
ttakeWhile = "takeWhile" ~: 
  TestList [takeWhile (< 3) [1, 2, 3, 4, 1, 2, 3, 4] ~?= [1, 2],
            takeWhile (< 9) [1, 2, 3]                ~?= [1, 2, 3],
            takeWhile (< 0) [1, 2, 3]                ~?= [],
            takeWhile (> 13) []                      ~?= [],
            takeWhile (> 13) [16, 7, 14]             ~?= [16],
            takeWhile even [2, 4, 7]                 ~?= [2, 4],
            takeWhile (not) [False, True]            ~?= [False]]
 

-- find pred lst returns the first element of the list that 
-- satisfies the predicate. Because no element may do so, the 
-- answer is returned in a "Maybe".
-- for example: 
--     find odd [0,2,3,4] returns Just 3

find :: (a -> Bool) -> [a] -> Maybe a
find f = foldr (\x acc -> if f x then Just x else acc) Nothing
                  
tfind :: Test
tfind = "find" ~:
  TestList [find odd [0, 2, 3, 4]           ~?= Just 3,
            find even [0, 2, 3, 4]          ~?= Just 0,
            find (> 13) [0, 2, 3, 4]        ~?= Nothing,
            find (< 4) []                   ~?= Nothing,
            find (not) [True, False, False] ~?= Just False ]
 

-- all pred lst returns False if any element of lst 
-- fails to satisfy pred and True otherwise.
-- for example:
--    all odd [1,2,3] returns False

all  :: (a -> Bool) -> [a] -> Bool
all f = foldr (\x acc -> acc && f x) True

tall :: Test
tall = "all" ~: 
  TestList [all odd [0, 2, 3, 4]  ~?= False,
            all even [0, 2, 3, 4] ~?= False,
            all odd [1, 3, 7, 5]  ~?= True,
            all (> 13) []         ~?= True,
            all (not) [True]      ~?= False ]
 

----------------------------------------------------------------------

testTree :: Test
testTree = TestList [ tinvertTree, ttakeWhileTree, tallTree, tmap2Tree, tzipTree ]

-- | a basic tree data structure
data Tree a = Leaf | Branch a (Tree a) (Tree a) deriving (Show, Eq)

foldTree :: b -> (a -> b -> b -> b) -> Tree a -> b
foldTree e _ Leaf     = e
foldTree e n (Branch a n1 n2) = n a (foldTree e n n1) (foldTree e n n2)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f = foldTree Leaf (\x t1 t2 -> Branch (f x) t1 t2) 

-- The invertTree function takes a tree of pairs and returns a new tree 
-- with each pair reversed.  For example:
--     invertTree (Branch ("a",1) Leaf Leaf) returns Branch (1,"a") Leaf Leaf

invertTree :: Tree (a,b) -> Tree (b,a)
invertTree = mapTree swap

tinvertTree :: Test
tinvertTree = "invertTree" ~:
  TestList [invertTree (Branch ("a", 1) Leaf Leaf)
            ~?= Branch (1, "a") Leaf Leaf,
            invertTree (Branch (Just True, Just True) Leaf Leaf)
            ~?= Branch (Just True, Just True) Leaf Leaf,
            invertTree (Branch (2, "Vivek") (Branch (1, "JJ") Leaf Leaf) Leaf)
            ~?= Branch ("Vivek", 2) (Branch ("JJ", 1) Leaf Leaf) Leaf,
            invertTree (Leaf :: Tree (Int, [Int]))
            ~?= (Leaf :: Tree ([Int], Int))]
 

-- takeWhileTree, applied to a predicate p and a tree t, 
-- returns the largest prefix tree of t  (possibly empty) 
-- where all elements satisfy p. 
-- For example, given the following tree

tree1 :: Tree Int
tree1 = Branch 1 (Branch 2 Leaf Leaf) (Branch 3 Leaf Leaf)

tree2 :: Tree Bool
tree2 = Branch False (Branch False Leaf (Branch True Leaf Leaf))
        (Branch True Leaf Leaf)

--     takeWhileTree (< 3) tree1  returns Branch 1 (Branch 2 Leaf Leaf) Leaf
--     takeWhileTree (< 9) tree1  returns tree1
--     takeWhileTree (< 0) tree1  returns Leaf

takeWhileTree :: (a -> Bool) -> Tree a -> Tree a
takeWhileTree f = foldTree Leaf takeWhileTree_aux where
  takeWhileTree_aux x lt rt = if f x then Branch x lt rt else Leaf

ttakeWhileTree :: Test
ttakeWhileTree = "takeWhileTree" ~:
  TestList [takeWhileTree (< 3) tree1        ~?=
              Branch 1 (Branch 2 Leaf Leaf) Leaf,
            takeWhileTree (< 9) tree1        ~?= tree1,
            takeWhileTree (< 0) tree1        ~?= Leaf,
            takeWhileTree (odd) tree1        ~?=
              Branch 1 Leaf (Branch 3 Leaf Leaf),
            takeWhileTree (not) tree2        ~?=
              Branch False (Branch False Leaf Leaf) Leaf,
            takeWhileTree (\_ -> True) tree2 ~?= tree2,
            takeWhileTree (< 3) Leaf         ~?= Leaf]
 

-- allTree pred tree returns False if any element of tree 
-- fails to satisfy pred and True otherwise.
-- for example:
--    allTree odd tree1 returns False

allTree :: (a -> Bool) -> Tree a -> Bool
allTree f = foldTree True allTree_aux where
  allTree_aux x ltSatisfies rtSatisfies = ltSatisfies && rtSatisfies && f x

tallTree :: Test
tallTree = "allTree" ~:
  TestList [allTree odd tree1          ~?= False,
            allTree (> 0) tree1        ~?= True,
            allTree (> 10) tree1       ~?= False,
            allTree (\x -> x) tree2    ~?= False,
            allTree (\_ -> True) tree2 ~?= True,
            allTree (\_ -> True) Leaf  ~?= True,
            allTree (\_ -> False) Leaf ~?= True]
 

-- WARNING: This one is a bit tricky!  (Hint: the value
-- *returned* by foldTree can itself be a function.)

-- map2Tree f xs ys returns the tree obtained by applying f to 
-- to each pair of corresponding elements of xs and ys. If 
-- one branch is longer than the other, then the extra elements 
-- are ignored.
-- for example:
--    map2Tree (+) (Branch 1 Leaf (Branch 2 Leaf Leaf)) (Branch 3 Leaf Leaf)
--        should return (Branch 4 Leaf Leaf)

map2Tree :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
map2Tree f tA tB = map2TreeBToC tB where
  map2TreeBToC = foldTree fBase map2TreeBToC_aux tfBToC
  fBase _ = Leaf
  map2TreeBToC_aux _     _  _  Leaf                = Leaf
  map2TreeBToC_aux fBToC lt rt (Branch bx blt brt) = Branch (fBToC bx)
                                                     (lt blt) (rt brt)
  tfBToC = mapTree f tA

treeTestMap1, treeTestMap2, treeTestMap3, treeTestMap4, treeTestMap5 :: Tree Int
treeTestMap1 = Branch 1 Leaf (Branch 2 Leaf Leaf)
treeTestMap2 = Branch 3 Leaf Leaf
treeTestMap3 = Branch 3 Leaf (Branch 4 Leaf Leaf)
treeTestMap4 = Branch 4 Leaf Leaf
treeTestMap5 = Branch 4 Leaf (Branch 6 Leaf Leaf)

treeTestMap6, treeTestMap7 :: Tree Double
treeTestMap6 = Branch 1.2 (Branch 3.5 Leaf Leaf) (Branch 4.7 Leaf Leaf)
treeTestMap7 = Branch 2.6 (Branch 0.9 Leaf (Branch 4.7 Leaf Leaf)) Leaf

treeTestMap8, treeTestMap9 :: Tree Bool
treeTestMap8 = Branch True (Branch False Leaf Leaf) Leaf
treeTestMap9 = Leaf
               
tmap2Tree :: Test
tmap2Tree = "map2Tree" ~:
  TestList [map2Tree (+) treeTestMap1 treeTestMap2 ~?= treeTestMap4,
            map2Tree (+) treeTestMap1 treeTestMap3 ~?= treeTestMap5,
            map2Tree (<) treeTestMap6 treeTestMap7 ~?= treeTestMap8,
            map2Tree (<) treeTestMap8 treeTestMap9 ~?= treeTestMap9,
            map2Tree (<) treeTestMap9 treeTestMap9 ~?= treeTestMap9]

-- zipTree takes two trees and returns a tree of corresponding pairs. If
-- one input branch is smaller, excess elements of the longer branch are
-- discarded.
-- for example:  
--    zipTree (Branch 1 (Branch 2 Leaf Leaf) Leaf) (Branch True Leaf Leaf) returns 
--            (Branch (1,True) Leaf Leaf)

-- To use foldTree, you'll need to think about this one in
-- the same way as part (d).

zipTree :: Tree a -> Tree b -> Tree (a,b)
zipTree = map2Tree (\x y -> (x, y))

tzipTree :: Test
tzipTree = "zipTree" ~:
  TestList [zipTree (Branch 1 (Branch 2 Leaf Leaf) Leaf)
            (Branch True Leaf Leaf)           ~?= Branch (1, True) Leaf Leaf,
            zipTree treeTestMap1 treeTestMap2 ~?= Branch (1, 3) Leaf Leaf,
            zipTree treeTestMap1 treeTestMap3 ~?=
              Branch (1, 3) Leaf (Branch (2, 4) Leaf Leaf),
            zipTree treeTestMap1 treeTestMap7 ~?= Branch (1, 2.6) Leaf Leaf,
            zipTree treeTestMap6 treeTestMap9 ~?= (Leaf :: Tree (Double, Bool)),
            zipTree treeTestMap9 treeTestMap9 ~?= (Leaf :: Tree (Bool, Bool))]

----------------------------------------------------------------------

formatPlayHelper :: String -> SimpleXML -> [SimpleXML]
formatPlayHelper superTag (Element tag xml)
  | tag == "SPEAKER"  = elt "b" subPlays ++ pcData "<br/>"
  | tag == "PERSONAE" = elt "h2" (pcData "Dramatis Personae") ++ subPlays
  | tag == "TITLE" && superTag == "PLAY"  = elt "h1" subPlays
  | tag == "TITLE" && superTag == "ACT"   = elt "h2" subPlays
  | tag == "TITLE" && superTag == "SCENE" = elt "h3" subPlays
  | tag == "PERSONA" || tag == "LINE"     = subPlays ++ pcData "<br/>"
  | tag == "PLAY"    || tag == "ACT"    ||
    tag == "SCENE"   || tag == "SPEECH"   = subPlays
  | otherwise         = error "Invalid Input"
    where
      elt htmlTag sub  = [Element htmlTag sub]
      pcData s         = [PCDATA s]
      subPlays         = concat $ map (formatPlayHelper tag) xml
formatPlayHelper _ xml = [xml]
                         
formatPlay :: SimpleXML -> SimpleXML
formatPlay xml = Element "html" [Element "body" (formatPlayHelper "" xml)]

firstDiff :: Eq a => [a] -> [a] -> Maybe ([a],[a])
firstDiff [] [] = Nothing
firstDiff (c:cs) (d:ds) 
    | c==d = firstDiff cs ds 
    | otherwise = Just (c:cs, d:ds)
firstDiff cs ds = Just (cs,ds)

-- | Test the two files character by character, to determine whether
-- they match.
testResults :: String -> String -> IO ()
testResults file1 file2 = do 
  f1 <- readFile file1
  f2 <- readFile file2
  case firstDiff f1 f2 of
    Nothing -> return ()
    Just (cs,ds) -> assertFailure msg where
      msg  = "Results differ: '" ++ take 20 cs ++ 
            "' vs '" ++ take 20 ds

testXML :: Test
testXML = TestCase $ do 
  writeFile "dream.html" (xml2string (formatPlay play))
  testResults "dream.html" "sample.html"

