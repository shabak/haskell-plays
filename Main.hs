module Main where

import Data.Char
import Data.Function
import Data.Tuple
import Data.Functor

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if (isDigit x && isDigit y) then digitToInt x * 10 + digitToInt y else 100

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt ((fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2)

factorial :: Integer -> Integer
factorial n | n >=0 = helper 1 n
            | otherwise = error "Err"
  where
    helper acc 0 = acc
    helper acc n = helper (acc * n) (n - 1)

doubleFact :: Integer -> Integer
doubleFact 0 = 1
doubleFact 1 = 1
doubleFact n = n * doubleFact (n - 2)

fibonacci0 :: Integer -> Integer
fibonacci0 0 = 0
fibonacci0 1 = 1
-- fibonacci0 (-1) = 1
-- fibonacci0 (-2) = (-1)
fibonacci0 n = fibonacci0 (n - 1) + fibonacci0 (n - 2)

fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | n > 1 = fibonacci (n - 1) + fibonacci (n - 2)
            | n == (-1) = 1
            | n == (-2) = (-1)
            | n < (-2) = fibonacci (n + 2) - fibonacci (n + 1)

loop :: Integer -> Integer
loop n = loopHelper 0 n
loopHelper acc 0 = acc
loopHelper acc n = loopHelper (acc + 2) (n - 1)


integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = helper 1000 f a 0 ((b - a) / 1000.0)
helper 0 f x acc h = acc
helper n f x acc h = helper (n - 1) f (x + h) (acc + s) h
    where
        ya = f (x)
        yb = f (x + h)
        s = h * ((ya + yb) / 2)


fibonacciLinear :: Integer -> Integer
fibonacciLinear n | n >= 0 = helperPositive 0 1 n
                  | n < 0  = (helperNegative 0 (-1) n) * ((-1) ^ (abs n))

helperPositive acc1 acc2 0 = acc1
helperPositive acc1 acc2 n = helperPositive (acc1 + acc2) acc1 (n - 1)

helperNegative acc1 acc2 0 = acc1
helperNegative acc1 acc2 n = helperNegative (acc1 + acc2) acc1 (n + 1)

--helper 0 1 3 -> helper 1 0 2
--helper 1 0 2 -> helper 1 1 1
--helper 1 1 1 -> helper 2 1 0
-- 1 2 3 4 5  6  7  8   9   10
--[1,2,3,3,2,-1,-5,-10,-13,-13,-6,7,27,46,59,51,18,-49,-133,-218,-253]
seqA :: Integer -> Integer
seqA n = foo 3 2 1 n

foo _ _ c 0 = c
foo _ b _ 1 = b
foo a _ _ 2 = a
foo a b c n = foo (a + b - 2 * c) a b (n - 1)

--func :: (Num a) => a -> a -> a -> a
func 0 _ p = p
func 1 c _ = c
func n c p = func (n-1) (3*c-2*p+1) c

--f :: (Num a) => a -> a
--f n = func n 2 1

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | x == 0 = (0,1)
              | x > 0 = foo x
              | x < 0 = foo (-x)
    where
        digs 0 = []
        digs x = digs (x `div` 10) ++ [x `mod` 10]
        sum = foldl (+) 0
        foo p = (sum $ digs p, fromIntegral $ length $ digs p)

getSecondFrom :: t -> t2 -> t1 -> t2
getSecondFrom a b c = b

fn :: a -> a -> b -> a -> a
fn a b c d = d


sumSquares = (+) `on` (^2)

--multSecond = g `on` h
--g = (*)
--h = snd

--on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
--on op f x y = f x `op` f y

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)

--doItYourself = f . g . h
--f = logBase 2
--g = (^3)
--h = max 42

-- a -> (a,b) -> a -> (b,a,a)

-- x -> (y,z) -> u -> (x,y,z)
-- x -> (y,z) -> u -> ( , , )

qqq = curry snd `on` (^2)

-- curry uncurry flip (,) const
-- curry uncurry (,) const

swp = uncurry $ flip $ (,)

--uncurry flip (,)

class Printable a where
  toString :: a -> String

instance Printable Bool where
  toString x = if (x == True) then "true" else "false"

instance Printable () where
  toString _ = "unit type"

instance (Printable a, Printable b) => Printable (a,b) where
  toString x = "(" ++ toString (fst x) ++ "," ++ toString (snd x) ++ ")"

--

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab x | (doesEnrageMork x && doesEnrageGork x) = stomp $ stab x
                  | (doesEnrageMork x) = stomp x
                  | (doesEnrageGork x) = stab x
                  | otherwise = x

    stompOrStabAlias :: a -> a
    stompOrStabAlias x = if (doesEnrageMork x && doesEnrageGork x) then stomp $ stab x else
                            if (doesEnrageGork x) then stab x else
                                if (doesEnrageMork x) then stomp x else x

ip = show a ++ show b ++ show c ++ show d
a = 127.2
b = 24.1
c = 20.1
d = 2

class (Bounded a, Enum a, Eq a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x = if (x == maxBound) then minBound else succ x
  spred :: a -> a
  spred x = if (x == minBound) then maxBound else pred x

instance SafeEnum Bool

avg :: Int -> Int -> Int -> Double
avg x y z = (fromIntegral x + fromIntegral y + fromIntegral z) / 3.0

ololo = const $ const (4 + 5) $ max 42

--fog a = a
--bar = const fog
--baz x = const True
--quux = let x = x in x
--corge = "Sorry, my value was changed"
--grault x 0 = x
--grault x y = x
--garply = grault 'q'
--waldo = fog

-- yes
fog 0 x = x
fog n x = let x' = fog (n - 1) (x + 1)
          in x' `seq` x'
-- ?
bar 0 f = f
bar x f = let f' = \a -> f (x + a)
              x' = x - 1
          in f' `seq` x' `seq` bar x' f'
-- no
baz 0 (x, y) = x + y
baz n (x, y) = let x' = x + 1
                   y' = y - 1
                   p  = (x', y')
                   n' = n - 1
               in p `seq` n' `seq` baz n' p
-- no
quux 0 (x, y) = x + y
quux n (x, y) = let x' = x + 1
                    y' = y - 1
                    p  = (x', y')
                    n' = n - 1
                in x' `seq` y' `seq` n' `seq` quux n' p


-- (result + n, ()) - уже слабая головная нормальная форма поэтому внутренняя сумма не будет вычисляться
mySum acc 0 = acc
mySum (result, ()) n = (mySum $! (result + n, ())) $ n - 1

goSum = mySum (0, ())

addTwoElements :: a -> a -> [a] -> [a]
addTwoElements x y = ((x : [y]) ++ )

nTimes:: a -> Int -> [a]
nTimes x n = hlpr [] x n
    where
     hlpr acc x 0 = acc
     hlpr acc x n = hlpr (x : acc) x (n - 1)

--let test = [(11,22), (12, 34), (56, 78)]
sndHead = snd . head
sndHead' ((,) y x : z) = x  -- ok
sndHead'' ((,) ((:) _ _) x) = x
sndHead''' ((,) y z : x) = x
sndHead'''' ((:) ((,) _ x) y) = x -- ok
sndHead''''' ((,) x y : z) = x
sndHead'''''' ((_, x) : _) = x -- ok

lengthList :: [a] -> Int
lengthList = foldr f 0 where
    f x s = s + 1

sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if (odd x) then s + x else s ) 0

meanList :: [Double] -> Double
meanList xs = (sum xs) / (foldr (\x c -> c + 1) 0 xs)
--meanList = fst + snd

--ml xs = (sum (xs)) / (length (xs))

--evenOnly :: [a] -> [a]
----evenOnly = fst . foldl (\x (xs,c) -> if (even c) then (x:xs,c+1) else (xs,c+1)) ([],0)
--evenOnly = reverse . fst . foldl (\(xs,c) x -> if (odd c) then (x:xs,c+1) else (xs,c+1)) ([],0)

--evenOnly :: [a] -> ([a],Integer)
--evenOnly = foldr (\x (xs,c) -> if (even c) then (x:xs,c+1) else (xs,c+1)) ([],0)

evenOnly :: [a] -> [a]
evenOnly = fst . foldr (\x (xs,c) -> if (even c) then (x:xs,c+1) else (xs,c+1)) ([],0)

data Color = Red | Green | Blue

instance Show Color where
    show Red = "Red"
    show Green = "Green"
    show Blue = "Blue"

charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9

stringToColor :: String -> Color
stringToColor "Red" = Red
stringToColor "Green" = Green
stringToColor "Blue" = Blue

emptyOrSingleton :: Bool -> a -> [a]
emptyOrSingleton False _ = []
emptyOrSingleton True x = [x]

--emptyOrSingleton undefined 5
--emptyOrSingleton True undefined                            * *
--emptyOrSingleton False undefined                          ****

isEqual :: (Eq a, Eq b) => (a, b) -> (a, b) -> Bool
isEqual (a, b) (a', b') = a == a' && b == b'

--isEqual undefined undefined
--isEqual undefined (undefined, undefined)
--isEqual (undefined, undefined) (undefined, undefined)     *  *
--isEqual (undefined, undefined) undefined

data LogLevel = Error | Warning | Info
cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Warning = GT
cmp Error Info = GT
cmp Warning Info = GT
cmp Warning Error = LT
cmp Info Warning = LT
cmp Info Error = LT
cmp Error Error = EQ
cmp Warning Warning = EQ
cmp Info Info = EQ

data Result = Fail | Success
data SomeData = A | B

doSomeWork :: SomeData -> (Result,Int)
doSomeWork x = (Fail, 2)
--doSomeWork x = (Success, 0)

processData :: SomeData -> String
processData x =
  case doSomeWork x of
    (Fail,x) -> "Fail: " ++ show x
    (Success,_) -> "Success"

data Point3D a = Point3D a a a deriving Show

instance Functor Point3D where
    fmap f (Point3D x y z) = Point3D (f x) (f y) (f z)

data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a) deriving Show

instance Functor GeomPrimitive where
    fmap f (Point (Point3D x y z)) = Point (Point3D (f x) (f y) (f z))
    fmap f (LineSegment (Point3D x y z) (Point3D x2 y2 z2)) = LineSegment (Point3D (f x) (f y) (f z)) (Point3D (f x2) (f y2) (f z2))

data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

instance Functor Tree where
    fmap g (Leaf Nothing) = Leaf Nothing
    fmap g (Leaf (Just x)) = Leaf (Just (g x))

    fmap g (Branch l Nothing r) = Branch (fmap g l) Nothing (fmap g r)
    fmap g (Branch l (Just x) r) = Branch (fmap g l) (Just (g x)) (fmap g r)

--data Log a = Log [String] a deriving Show

data Log a = Log [String] a deriving Show

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f s x = Log [s] (f x)

add1Log = toLogger (+1) "added one"
mult2Log = toLogger (* 2) "multiplied by 2"

--execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
--execLoggers x (Log [s] a) (Log [d] b) = undefined

--execLoggers :: Log a -> ([String],a)
--execLoggers (Log [s] x) = ([s],x)

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = Log [q,w] v where
    where
      v = ...




