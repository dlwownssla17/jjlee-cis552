{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

-- I should add the option -Werror upon completing this assignment.

{-# LANGUAGE NoImplicitPrelude #-}

{-
Names: Jae Joon Lee (jjlee), Vivek A. Raj (vivekraj)
CIS 552 HW1
-}

module Main where
import Prelude hiding (all, reverse, takeWhile, zip, concat, concatMap)
import Data.List hiding (reverse, intersperse, takeWhile, find, all, zip,
                         transpose, concat, concatMap)
import Test.HUnit 

main :: IO ()
main = do 
   _ <- runTestTT $ TestList [ testStyle,
                               testLists,
                               testBowlingKata,
                               testLcs ]
   return ()

--------------------------------------------------------------------------------

testStyle :: Test
testStyle = "testStyle" ~:
   TestList [ tabc , tarithmetic, treverse, tzap ]

abc :: Bool -> Bool -> Bool -> Bool
abc x y z = x && (y || z)
 
tabc :: Test
tabc = "abc" ~: TestList [abc True False True ~?= True, 
                          abc True False False ~?= False,
                          abc False True True ~?= False]


arithmetic :: ((Int, Int), Int) -> ((Int,Int), Int) -> (Int, Int, Int)
arithmetic ((a, b), c) ((d, e), f) =
    (b * f - c * e, c * d - a * f, a * e - b * d)

tarithmetic :: Test
tarithmetic = "arithmetic" ~:
   TestList[ arithmetic ((1,2),3) ((4,5),6) ~?= (-3,6,-3), 
             arithmetic ((3,2),1) ((4,5),6) ~?= (7,-14,7) ]

 
reverse :: [a] -> [a]
reverse = foldl' reverse_aux [] where
  reverse_aux acc x = x:acc

treverse :: Test
treverse = "reverse" ~: TestList [reverse [3,2,1] ~?= [1,2,3],
                                  reverse [1]     ~?= [1] ]

zap :: [a -> b] -> [a] -> [b]
zap (f:fs) (a:as) = f a : zap fs as
zap _ _           = []

tzap :: Test
tzap = "zap" ~:
  TestList [ zap [ (+1), \n -> n - 1, (+1) ]
                   ([3, 4, 5] :: [Int]) ~?= [4,3,6],
             zap [ null, not . null ] [ [], "a" ] ~?= [True, True],
             zap [] "a" ~?=  "",
             zap [not] [] ~?= []]

-------------------------------------------------------------------------------- 
testLists :: Test
testLists = "testLists" ~: TestList [tintersperse, tinvert, ttakeWhile, tfind, tall, tmap2, tzip, ttranspose, tconcat, tconcatMap]

-- The intersperse function takes an element and a list 
-- and intersperses that element between the elements of the list. 
-- For example,
--    intersperse ',' "abcde" == "a,b,c,d,e"
{-
Previous version:
intersperse :: Char -> String -> String
intersperse c (x : xs) = x : c : intersperse c xs
intersperse _ _        = []
Changed to make it more literal and intuitive:
intersperse :: Char -> [Char] -> [Char]
...
Okay, nevermind, I'm making this more general, based on the
general description of the function "an element and a list".
-}
intersperse :: a -> [a] -> [a]
intersperse _ []       = []
intersperse _ (x : []) = [x]
intersperse c (x : xs) = x : c : intersperse c xs
                         
tintersperse :: Test
tintersperse = "intersperse" ~: TestList [intersperse ' ' "CIS" ~?= "C I S",
                                          intersperse 'a' "b"   ~?= "b",
                                          intersperse '!' ""    ~?= "",
                                          intersperse Nothing [Just 1, Just 2]
                                              ~?= [Just 1, Nothing, Just 2]]

-- invert lst returns a list with each pair reversed. 
-- for example:
--   invert [("a",1),("a",2)]     returns [(1,"a"),(2,"a")] 
--   invert ([] :: [(Int,Char)])  returns []

--   note, you need to add a type annotation to test invert with []
--
invert :: [(a, b)] -> [(b, a)]
{-
Previous version:
invert ((x1, x2) : xs) = (x2, x1) : invert xs     
invert _               = []

Note: Real World Haskell generally does not seem to recommend the use of
lambda functions, but I used it here for extra brevity.
-}
invert = map (\(x, y) -> (y, x))

tinvert :: Test
tinvert = "invert" ~: 
  TestList [invert [("a", 1), ("a", 2)]    ~?= [(1, "a"), (2, "a")],
            invert [(True, True)]          ~?= [(True, True)],    
            invert ([] :: [(String, Int)]) ~?= ([] :: [(Int, String)])] 

-- takeWhile, applied to a predicate p and a list xs, 
-- returns the longest prefix (possibly empty) of xs of elements 
-- that satisfy p:
-- For example, 
--     takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
--     takeWhile (< 9) [1,2,3] == [1,2,3]
--     takeWhile (< 0) [1,2,3] == []
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f (x : xs) = if f x then x : takeWhile f xs else []
takeWhile _ _        = []


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
find f (x : xs) = if f x then Just x else find f xs
find _ _        = Nothing

tfind :: Test
tfind = "find" ~:
  TestList [find odd [0, 2, 3, 4]           ~?= Just 3,
            find even [0, 2, 3, 4]          ~?= Just 0,
            find (> 13) [0, 2, 3, 4]        ~?= Nothing,
            find (< 4) []                   ~?= Nothing,
            find (not) [True, False, False] ~?= Just False]
 

-- all pred lst returns False if any element of lst fails to satisfy
-- pred and True otherwise.
-- for example:
--    all odd [1,2,3] returns False
all :: (a -> Bool) -> [a] -> Bool
{-
Previous version:
all f (x : xs) = f x && all f xs
all _ _        = True
-}
all f = foldl' all_aux True where
  all_aux acc x = f x && acc

tall :: Test
tall = "all" ~: 
  TestList [all odd [0, 2, 3, 4]  ~?= False,
            all even [0, 2, 3, 4] ~?= False,
            all odd [1, 3, 7, 5]  ~?= True,
            all (> 13) []         ~?= True,
            all (not) [True]      ~?= False]

-- map2 f xs ys returns the list obtained by applying f to 
-- to each pair of corresponding elements of xs and ys. If 
-- one list is longer than the other, then the extra elements 
-- are ignored.
-- i.e. 
--   map2 f [x1, x2, ..., xn] [y1, y2, ..., yn, yn+1] 
--        returns [f x1 y1, f x2 y2, ..., f xn yn]
--
-- NOTE: map2 is called zipWith in the standard library.
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f (x : xs) (y : ys) = f x y : map2 f xs ys
map2 _ _ _               = []

tmap2 :: Test
tmap2 = "map2" ~: 
  TestList [map2 (\x y -> x * x + y * y) [3, 12] [4, 5]   ~?= [25, 169],
            map2 (\x y -> 3 * x + 2 * y) [1, 0] [0, 1, 0] ~?= [3, 2],
            map2 (\x y -> 3 * x + 2 * y) [1, 0, 0] [0, 1] ~?= [3, 2],
            map2 (==) ([] :: [Char]) ([] :: [Char]) ~?= ([] :: [Bool])]

-- zip takes two lists and returns a list of corresponding pairs. If
-- one input list is shorter, excess elements of the longer list are
-- discarded.
-- for example:  
--    zip [1,2] [True] returns [(1,True)]
zip :: [a] -> [b] -> [(a, b)]
zip (x : xs) (y : ys) = (x, y) : zip xs ys
zip _ _               = []

tzip :: Test
tzip = "zip" ~: 
  TestList [zip [3, 12] [4, 5]                 ~?= [(3, 4), (12, 5)],
            zip [1, 0] [0, 1, 0]               ~?= [(1, 0), (0, 1)],
            zip [1, 0, 0] [0, 1]               ~?= [(1, 0), (0, 1)],
            zip [True] [Just True, Nothing]    ~?= [(True, Just True)],
            zip ([] :: [Bool]) ([] :: [[Int]]) ~?= ([] :: [(Bool, [Int])])]


-- transpose  (WARNING: this one is tricky!)

-- The transpose function transposes the rows and columns of its argument. 
-- If the inner lists are not all the same length, then the extra elements
-- are ignored.
-- for example:
--    transpose [[1,2,3],[4,5,6]] returns [[1,4],[2,5],[3,6]]
-- Previous version:
transpose :: [[a]] -> [[a]]
transpose l = transpose_aux l [] [] [] where
  transpose_aux :: [[a]] -> [[a]] -> [a] -> [[a]] -> [[a]]
  transpose_aux ([] : _) acc acc_elem _               = [[]]
  transpose_aux [] acc acc_elem []                    = acc ++ [acc_elem]
  transpose_aux [] acc acc_elem new_list              = 
    transpose_aux new_list (acc ++ [acc_elem]) [] []
  transpose_aux ((x : []) : tl) acc acc_elem new_list = 
    transpose_aux tl acc (acc_elem ++ [x]) []
  transpose_aux ((x : xs) : tl) acc acc_elem new_list = 
    transpose_aux tl acc (acc_elem ++ [x]) (new_list ++ [xs])
-- transpose :: [[a]] -> [[a]]
-- transpose [] = []
--    where
--  min_length = minimum (map length l)
  

ttranspose :: Test
ttranspose = "transpose" ~: 
  TestList [transpose [[1, 2, 3], [4, 5, 6]]         ~?= [[1, 4],[2, 5],[3, 6]],
            transpose [[1, 2, 3], [4]]               ~?= [[1, 4]],
            transpose [[1, 2, 3], [4], [5, 6]]       ~?= [[1, 4, 5]],
            transpose [[1, 2, 3], [4, 5], [6, 7, 8]] ~?= [[1, 4, 6], [2, 5, 7]],
            transpose [[1, 2, 3], []]                ~?= [([] :: [Int])],
            transpose [[1, 2, 3]]                    ~?= [[1], [2], [3]],
            transpose [([] :: [Int])]                ~?= [([] :: [Int])],
            transpose ([] :: [[Int]])                ~?= ([] :: [[Int]])]

-- concat
 
-- The concatenation of all of the elements of a list of lists
-- for example:
--    concat [[1,2,3],[4,5,6],[7,8,9]] returns [1,2,3,4,5,6,7,8,9]
{-
Previous version:
concat :: [[a]] -> [a]
concat []                = []
concat ([] : xs)         = concat xs
concat ((x11 : x1) : xs) = x11 : concat (x1 : xs)
-}
concat :: [[a]] -> [a]
concat = foldr concat_aux [] where
  concat_aux [] accum       = accum
  concat_aux (x:xs) accum   = x : concat_aux xs accum
                           
tconcat :: Test
tconcat = "concat" ~: 
  TestList [concat [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
                                              ~?= [1, 2, 3, 4, 5, 6, 7, 8, 9],
            concat [[1, 2, 3], [4, 5]]        ~?= [1, 2, 3, 4, 5],
            concat [[], [1], [2, 3]]          ~?= [1, 2, 3],
            concat ([[], [], []] :: [[Int]])  ~?= ([] :: [Int]),
            concat [[True]]                   ~?= [True]]

-- concatMap
 
-- Map a function over all the elements of the list and concatenate the results.
-- for example:
--    concatMap (\x -> [x,x+1,x+2]) [1,2,3]  returns [1,2,3,2,3,4,3,4,5]
{-
Previous version:
concatMap :: (a -> [a]) -> [a] -> [a]
concatMap f l = concat_aux f l [] where
  concat_aux :: (a -> [a]) -> [a] -> [a] -> [a]
  concat_aux f (x : xs) acc = concat_aux f xs (acc ++ (f x))
  concat_aux _ _ acc        = acc
-}
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f l = concat (map f l)
    
tconcatMap :: Test
tconcatMap = "concatMap" ~: 
  TestList [concatMap (\x -> [x, x + 1, x + 2]) [1, 2, 3]
                                              ~?= [1, 2, 3, 2, 3, 4, 3, 4, 5],
            concatMap (\x -> [x]) [1, 2, 3]   ~?= [1, 2, 3],
            concatMap (\_ -> ([] :: [Int])) [1, 2, 3] ~?= ([] :: [Int]),
            concatMap (\x -> [x, 2 * x, x * x]) []    ~?= ([] :: [Int]),
            concatMap (\_ -> [True]) [1, 2, 3]        ~?= [True, True, True]] 

--------------------------------------------------------------------------------

bowlingTest0 :: ([Int] -> Int) -> Test
bowlingTest0 score = "all gutter balls" ~: 0 ~=? score (replicate 20 0)

score0 :: [ Int ] -> Int
score0 _ = 0

bowlingTest1 :: ([Int] -> Int) -> Test
bowlingTest1 score = 
   "allOnes" ~: 20 ~=? score (replicate 20 1)

score1 :: [ Int ] -> Int
score1 = sum

bowlingTest2 :: ([ Int ] -> Int) -> Test
bowlingTest2 score =
   "spare" ~: 37 ~=? score (replicate 3 5 ++ replicate 17 1)
                
score2 :: [ Int ] -> Int
score2 = score where
   score (x0:x1:x2:xs) =
     case x0 + x1 of
       10 -> x0 + x1 + x2 + score (x2:xs)
       _  -> x0 + x1 + score (x2:xs)
   score l             = score1 l

-- Each score function should be independent of each other,
-- i.e. do not use another score function within this function!
score2a :: [ Int ] -> Int
score2a = score where
   score (x0:x1:x2:xs) | x0 + x1 == 10 = x0 + x1 + x2 + score (x2:xs)
                       | otherwise     = x0 + x1 + score (x2:xs)
   score l                             = sum l

bowlingTest3 :: ([ Int ] -> Int) -> Test
bowlingTest3 score =
   "strike" ~: 52 ~=? score ([10, 8, 2, 3, 2] ++ replicate 14 1)

score3 :: [ Int ] -> Int
score3 = score where
   score (x0:x1:x2:xs) | x0 == 10      = x0 + x1 + x2 + score (x1:x2:xs)
                       | x0 + x1 == 10 = x0 + x1 + x2 + score (x2:xs)
                       | otherwise     = x0 + x1 + score (x2:xs)
   score l                             = sum l

bowlingTest4 :: ([ Int ] -> Int) -> Test
bowlingTest4 score = "perfect game" ~: 300 ~=? score (replicate 12 10) 

score4 :: [ Int ] -> Int
score4 = score where
   score l@(x0:x1:x2:xs) | xs == []      = sum l
                         | x0 == 10      = x0 + x1 + x2 + score (x1:x2:xs)
                         | x0 + x1 == 10 = x0 + x1 + x2 + score (x2:xs)
                         | otherwise     = x0 + x1 + score (x2:xs)
   score l                               = sum l

testBowlingKata :: Test
testBowlingKata = TestList (map checkOutput scores) where
  -- the five test cases, in order 
  bowlingTests  = [bowlingTest0, bowlingTest1, bowlingTest2, 
                   bowlingTest3, bowlingTest4]
 
  -- the names of the score functions, the functions themselves, 
  -- and the expected number of passing tests
  scores = zip3 ['0' ..] [score0, score1, score2a, score3, score4] [1..]
 
  -- a way to run a unit test without printing output 
  testSilently = performTest (\ _ _ -> return ()) 
                   (\ _ _ _ -> return) (\ _ _ _ -> return) ()
 
  -- run each bowling test on the given score function, making sure that 
  -- the expected number of tests pass.
  checkOutput (name, score, pass) = " Testing score" ++ [name] ~: do 
    (s0,_) <- testSilently $ (TestList $ bowlingTests `zap` (repeat score))
    assert $ pass @=? cases s0 - (errors s0 + failures s0)

-------------------------------------------------------------------------------- 

lcs :: String -> String -> String 
lcs (x:xs) (y:ys) | x == y    = x : lcs xs ys
                  | otherwise = max (lcs xs (y:ys)) (lcs (x:xs) ys)
lcs _ _                       = ""

testLcs :: Test
testLcs = "Lcs" ~: TestList [ lcs "Advanced" "Advantaged" ~?= "Advaned",
    lcs "abcd" "acbd" ~?= "acd", lcs "a" "abc" ~?= "a",
    lcs "ab" "ba" ~?= "b", lcs "" "a" ~?= "",
    lcs "" "" ~?= "", lcs "abc" "def" ~?= "" ]

