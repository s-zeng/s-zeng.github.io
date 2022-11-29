-- reference paper: https://www.cl.cam.ac.uk/%7Esd601/papers/semirings.pdf
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.List
import Prelude hiding (fromInteger)

infixl 9 |*|

infixl 8 |+|

-- closed semiring = ring with the usual rules, but w/o additive inverse, and w/ a closure operator
-- a' is the closure operator such that a' = 1 + a*a'
-- for semirings w/ infinite sums, a' = 1 + a + a^2 + ...
-- for fields, we have a' = 1/(1-a) and 1' = \infty (gotta add an infinity element to your field)
-- for regex, multiplication = concat, addition = union, closure = regex kleene star

class ClosedSemiring r where
    zero, one :: r
    (|+|), (|*|) :: r -> r -> r
    closure :: r -> r
    fromInteger :: Integer -> r

-- trivial closed semiring w/ two elements
instance ClosedSemiring Bool where
    zero = False
    one = True
    closure x = True
    (|+|) = (||)
    (|*|) = (&&)
    fromInteger = even

data IntegersWithInfinity = I Integer | Infty deriving Show
instance ClosedSemiring IntegersWithInfinity where
    zero = I 0
    one = I 1
    closure x = Infty
    (I a) |+| (I b) = I (a + b)
    x |+| Infty = Infty
    Infty |+| x = Infty
    (I a) |*| (I b) = I (a * b)
    x |*| Infty = Infty
    Infty |*| x = Infty
    fromInteger = I

{- | Definition of matrices
 by having a dedicated scalar constructor, we can include 0 matrix and scaling matrices without needing size info
-}
data Matrix a = Scalar a | Matrix {matrix :: [[a]]} deriving (Show)

type BlockMatrix a = (Matrix a, Matrix a, Matrix a, Matrix a) -- block decomposition to make calculations easier

-- | Blockmatrix and Matrix are isomorphic
mjoin :: BlockMatrix a -> Matrix a
msplit :: Matrix a -> BlockMatrix a
mjoin (Matrix a, Matrix b, Matrix c, Matrix d) = Matrix (hcat a b ++ hcat c d) where hcat = zipWith (++)

msplit (Matrix (row : rs)) =
    ( Matrix [[first]]
    , Matrix [top]
    , Matrix left
    , Matrix rest
    )
  where
    (first : top) = row
    (left, rest) = unzip (map (\(x : xs) -> ([x], xs)) rs)

{- | A matrix whose elements are elements of a closed semiring is itself an
 element of a closed semiring
 Similar in principle to the usual matrices resulting from taking a vector
 space over a field or a module over a ring
-}
instance ClosedSemiring a => ClosedSemiring (Matrix a) where
    zero = Scalar zero
    one = Scalar one

    (Scalar a) |+| (Scalar b) = Scalar (a |+| b)
    (Matrix a) |+| (Matrix b) = Matrix (zipWith (zipWith (|+|)) a b)
    s@(Scalar a) |+| m = m |+| s
    (Matrix [[a]]) |+| (Scalar b) = Matrix [[a |+| b]]
    m |+| s =
        let (first, top, left, rest) = msplit m
         in mjoin
                ( first |+| s
                , top
                , left
                , rest |+| s
                )

    (Scalar a) |*| (Scalar b) = Scalar (a |*| b)
    (Scalar a) |*| (Matrix b) = Matrix (map (map (a |*|)) b)
    (Matrix a) |*| (Scalar b) = Matrix (map (map (|*| b)) a)
    (Matrix a) |*| (Matrix b) =
        let columns = transpose b
         in Matrix [[foldl1 (|+|) (zipWith (|*|) row col) | col <- columns] | row <- a]

    -- algorithm from the paper
    closure (Matrix [[x]]) = Matrix [[closure x]]
    closure m =
        mjoin
            ( first |+| top |*| rest |*| left
            , top |*| rest
            , rest |*| left
            , rest
            )
      where
        (f, t, l, r) = msplit m
        first = closure f
        top = first |*| t
        left = l |*| first
        rest = closure (r |+| left |*| t)

    fromInteger = undefined

expt :: ClosedSemiring r => Matrix r -> Integer -> Matrix r
expt mat 0 = Scalar one
expt mat 1 = mat
expt mat x | even x = expt (mat |*| mat) (quot x 2)
expt mat x = mat |*| (expt mat (x - 1))

{- | We can now start constructing some more useful closed semirings
 If we take Z, insert an infinity element, and use `min` as the addition
 operator and the usual integer addition as the multiplication operator,
 we have a closed semiring which is very useful for graph distance algorithms
 Also known as a tropical semiring
-}
data ShortestDistance = Distance Integer | Unreachable deriving (Show, Eq, Ord)

instance ClosedSemiring ShortestDistance where
    zero = Unreachable -- additive identity is infinity
    one = Distance 0 -- multiplicative identity is Z's usual additive identity
    closure x = one -- 0 = min(0, a+0) for all a, so satisfies definition

    (|+|) = min

    x |*| Unreachable = Unreachable
    Unreachable |*| x = Unreachable
    (Distance a) |*| (Distance b) = Distance (a + b)

    fromInteger = Distance

{- | We can also define a variant of the above that keeps track of a path taken
 The second argument of `Path` is a list of directed edges (v, v') starting from vertex v and ending at v'
-}
data ShortestPath n = Path Integer [(n, n)] | NoPath deriving (Show, Eq, Ord)

instance Ord n => ClosedSemiring (ShortestPath n) where
    zero = NoPath
    one = Path 0 []
    closure x = one

    (|+|) = min

    x |*| NoPath = NoPath
    NoPath |*| x = NoPath
    (Path w ps) |*| (Path w' ps') = Path (w + w') (ps ++ ps')
    fromInteger = undefined

{- | We can also model flow problems as a closed semiring!
 This time, we need a negative infinity as well as an infinity,
 and we use max/min as +/*
-}
data MaxFlow = Unflowable | Width Integer | Infinity deriving (Show, Eq, Ord)

instance ClosedSemiring MaxFlow where
    zero = Unflowable
    one = Infinity
    closure x = one
    (|+|) = max
    (|*|) = min
    fromInteger = Width

-- | Similar augmentation as in the shortest path case
data MaxFlowPath n = NoPipe | Pipe Integer [(n, n)] | Infinite deriving (Show, Eq, Ord)

instance Ord n => ClosedSemiring (MaxFlowPath n) where
    zero = NoPipe
    one = Infinite
    closure x = one

    (|+|) = max

    (Pipe w ps) |*| (Pipe w' ps') = Pipe (min w w') (ps ++ ps')
    a |*| b = min a b

    fromInteger = undefined

edgedToPaths :: Matrix ShortestDistance -> Matrix (ShortestPath Int)
edgedToPaths (Matrix x) = Matrix . zipWith (curry convert_row) [0 ..] . map (zip [0 ..]) $ x
  where
    convert_row (i, row) = [path w i j | (j, w) <- row]
    path Unreachable _ _ = NoPath
    path (Distance w) i j = Path w [(i, j)]

edgesToFlow :: Matrix ShortestDistance -> Matrix MaxFlow
edgesToFlow (Matrix x) = Matrix $ map (map flow) x
  where
    flow Unreachable = Unflowable
    flow (Distance w) = Width w

edgesToFlowPaths :: Matrix ShortestDistance -> Matrix (MaxFlowPath Int)
edgesToFlowPaths (Matrix x) = Matrix . zipWith (curry convert_row) [0 ..] . map (zip [0 ..]) $ x
  where
    convert_row (i, row) = [pipe w i j | (j, w) <- row]
    pipe Unreachable _ _ = NoPipe
    pipe (Distance w) i j = Pipe w [(i, j)]

-- graph from dijkstra wiki page, but with vertices indexed from 0 instead of 1
dijkstraSample :: Matrix ShortestDistance
dijkstraSample =
    Matrix
        [ [Distance 0, Distance 7, Distance 9, Unreachable, Unreachable, Distance 14]
        , [Distance 7, Distance 0, Distance (-10), Distance 15, Unreachable, Unreachable]
        , [Distance 9, Distance (-10), Distance 0, Distance 11, Unreachable, Distance 2]
        , [Unreachable, Distance 15, Distance 11, Distance 0, Distance 6, Unreachable]
        , [Unreachable, Unreachable, Unreachable, Distance 6, Distance 0, Distance 9]
        , [Distance 14, Unreachable, Distance 2, Unreachable, Distance 9, Distance 0]
        ]

sampleGraph :: ClosedSemiring r => Matrix r
sampleGraph =
    Matrix
        [ [one, fromInteger 3, fromInteger 5, fromInteger 2, fromInteger 4, zero, zero]
        , [fromInteger 3, one, zero, zero, zero, fromInteger 1, fromInteger 4]
        , [fromInteger 5, zero, one, zero, zero, fromInteger 0, zero]
        , [fromInteger 2, zero, zero, one, zero, fromInteger 1, zero]
        , [fromInteger 4, zero, zero, zero, one, fromInteger 2, zero]
        , [zero, fromInteger 1, fromInteger 0, fromInteger 1, fromInteger 2, one, fromInteger 5]
        , [zero, fromInteger 4, zero, zero, zero, fromInteger 5, fromInteger 1]
        ]

-- tested in ghci 9.0.2
main :: IO ()
main = do
    putStrLn "Paths from 0 to 4: "
    putStr "Shortest: "
    print . (!! 4) . (!! 0) . matrix . closure $ edgedToPaths dijkstraSample
    putStr "Widest: "
    print . (!! 4) . (!! 0) . matrix . closure $ edgesToFlowPaths dijkstraSample
