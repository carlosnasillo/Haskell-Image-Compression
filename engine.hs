{-|
+++
    Copyright (C) 2012 Carlos Nasillo / me@carlosnasillo.com.
    
    Permission is hereby granted, free of charge, to any person obtaining a copy of 
    this software and associated documentation files (the "Software"), to deal in the 
    Software without restriction, including without limitation the rights to use, copy,
    modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
    and to permit persons to whom the Software is furnished to do so, subject to the 
    following conditions:
    
    The above copyright notice and this permission notice shall be included in all copies
    or substantial portions of the Software.
    
    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
    INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR 
    PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
    FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR 
    OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
    DEALINGS IN THE SOFTWARE.
---
-}

import Array
import Utilities

spacing :: Dims
spacing = (45, 45)

temp = showImageBlocks domainBlocks (45,45)

-----------------------------------------------------------------------------------------
-- TRANSFORMATIONS
-----------------------------------------------------------------------------------------

transformations  ::  Int -> [Pixels -> Pixels]
transformations  n
  = [f n | f <-[notMove,flipH,half180,flipV,flipB,right90,flipF,left90]]

notMove, flipH, half180, flipV, flipB, right90, flipF, left90 :: Int-> Pixels -> Pixels
notMove n = id
flipH   n = horizFlip n
half180 n = halfTurn
flipV   n = halfTurn . (horizFlip n)
flipB   n = trans n
right90 n = (trans n) . (horizFlip n)
flipF   n = (trans n) . halfTurn
left90  n = (trans n). halfTurn . (horizFlip n)

also f xs = xs ++ map f xs
trans n = concat . transposeB . toRows n
halfTurn = reverse
horizFlip n = concat . reverse . toRows n
transposeB = foldr (zipWith (:)) (repeat [])

toRows :: Int -> [a] -> [[a]]
toRows w []	= []
toRows w xs	= take w xs : toRows w (drop w xs)


------------------------------------------------------------------
-- ROTATIONS
------------------------------------------------------------------

-- BlockSum = ((Point,Pixels), Int, Int) 

rotations :: Dims ->  BlockSum -> [(BlockSum,Int)]
rotations d ((p, pix), s, t) 
  = zip [((p, f pix), s, t) | f <- transformations (fst d)] [0..7]

generateRotations :: Dims -> [BlockSum] -> [(BlockSum,Int)]
generateRotations d  = undefined

-- Now we can extract all the domain blocks and their rotations from an image:

allDomainBlocks :: Image -> Dims -> [(BlockSum, Int)]
allDomainBlocks i d = generateRotations d (domainBlocks i d)

temp2  = showRotations generateRotations domainBlocks (45,45)

allTransforms :: Dims -> BlockSum -> [(BlockSum,Int)] -> [Trans]
allTransforms d b = undefined

rangeBlock :: BlockSum
rangeBlock = (((1,2),[1,2,3,4]), 10, 30) 

temp_b :: [Trans]
temp_b = allTransforms (2,2) rangeBlock (rotations (2,2) rangeBlock)

maxAll :: Ord a => [a] -> a
maxAll = undefined

minTrans :: [Trans] -> Trans
minTrans = undefined

minT :: Trans -> Trans -> Trans
minT x y = undefined

temp_c :: Trans
temp_c = minTrans temp_b

bestTransform :: Dims ->  [(BlockSum,Int)] -> BlockSum  -> Trans
bestTransform  dim doms ran = minTrans (allTransforms dim ran doms)

-- We are now ready to do the encode...

encode :: Image -> Dims -> [Trans]
encode i d = map (bestTransform d (allDomainBlocks i d)) (rangeBlocks i d) 

-----------------------------------------------------------------------------------
--  DECODE 
-----------------------------------------------------------------------------------

scale :: Float -> Float -> Int -> Int
scale c b p = greyRound (floor (c * fromIntegral p + b))

greyRound :: Int -> Int
greyRound n = min (max n 0) 255

------------------------------------------------------------------
-- SCALE + TRANSFORM
------------------------------------------------------------------

scalePixels :: Float -> Float -> Pixels -> Pixels
scalePixels c b = undefined

getPixels :: Image -> Trans -> Pixels
getPixels im (r,b,c,rp,dp,rot,sz) = snd (getBlock im sz dp)

-- this uses the list of transformations above

transformPixels :: Trans -> Pixels -> Pixels
transformPixels (r,b,c,rp,dp,rot,sz)  
    = ((transformations (fst sz))!! rot)  . (scalePixels c b) 

-- so we can get a list of pixels and transform it like this

applyTransform :: Image -> Trans -> Pixels
applyTransform im t = transformPixels t (getPixels im t)

addPoints :: Dims -> Block -> [(Point, Int)]
addPoints dims (pos, points) = undefined

getPositions :: Dims -> Point -> [Point]
getPositions (w,h) (x,y) = undefined 

addPositions :: Trans -> Pixels -> [(Point,Int)]
addPositions (r,b,c,rp,dp,rot,sz) ps = addPoints sz (rp, ps)

makeRangeBlock :: Image -> Trans -> [(Point, Int)]
makeRangeBlock im t = addPositions t (applyTransform im t)

-- NOTE: This decode assumes domain positions are from squashed image

combineBlocks ::  [[(Point, Int)]] -> Image
combineBlocks as = array ((0,0) , d) bs
	where bs = concat as
	      d  = maxAll (map fst bs)

decode :: [Trans] -> Image ->  Image
decode  ts im = combineBlocks  (map (makeRangeBlock (squash im)) ts)

-----------------------------------------------------------------------------------------

plainImage :: (Point,Point) -> Int -> Image
plainImage ((a,b),(c,d)) n = array ((a,b),(c,d)) [((x,y), n) | x <- [a..c], y <-[b..d]]

whiteImage :: Image
whiteImage = plainImage ((0,0), (200, 150)) 255

greyImage :: Image
greyImage = plainImage ((0,0), (200, 150)) 150

xImage :: (Point,Point) -> Image
xImage ((a,b),(c,d))  = array ((a,b),(c,d)) [((x,y), x) |  x <- [a..c], y <-[b..d]]

stripeyImage :: (Point,Point) -> Image
stripeyImage ((a,b),(c,d))  = array ((a,b),(c,d)) [((x,y), fix x) | x <- [a..c], y <-[b..d]]
                              where fix x = mod (30 * (div x 25)) 255

-- rotate 180

rotate :: Transformer
rotate(d,xs) = (d,reverse xs)

-- invert pixels

invert :: Transformer
invert(p,xs) = (p, map (255-) xs)

-- whiten pixels

blackwhite :: Int -> Int
blackwhite x
  | x <= 120  = 0
  | otherwise = 255

blackAndwhite :: Transformer
blackAndwhite (p,xs) = (p, map blackwhite xs)

-----------------------------------------------------------------------------------------

getBlock :: Image -> Dims -> Point -> Block
getBlock i (w,h) (x,y) = ((x,y),  [i!(u,v) |  v <-[y..y + h - 1], u <- [x..x + w - 1]])

getBlocks :: Image -> Dims -> [Point] -> [Block]
getBlocks i d ps = map (getBlock i d) ps

-----------------------------------------------------------------------------------------

sumSquares :: [Int] -> Int
sumSquares = sum . (map (^2))

makeBlock :: Block -> BlockSum
makeBlock (d,p) = ((d,p), sum p, sumSquares p)

-----------------------------------------------------------------------------------------

getBlockSums :: Image -> Dims -> [Point] -> [BlockSum]
getBlockSums i d ps = map makeBlock (getBlocks i d ps)

-----------------------------------------------------------------------------------------

blockPos :: Image -> Dims -> Dims -> [Point]
blockPos i (x,y) (w,h) = [(u,v) |  v <-[b, b + y..d-h+1], u <- [a, a + x..c-w+1]]
     where ((a,b),(c,d)) = bounds i
     
domPositions :: Image -> Dims -> [Point]
domPositions i size =  blockPos i spacing size

ranPositions :: Image -> Dims -> [Point]
ranPositions i size = blockPos i  size size

-----------------------------------------------------------------------------------------

rangeBlocks :: Image -> Dims ->  [BlockSum]
rangeBlocks i d = getBlockSums i d (ranPositions i d)


domainBlocks :: Image -> Dims ->  [BlockSum]
domainBlocks i d = getBlockSums (squash i) d (domPositions (squash i) d)

-----------------------------------------------------------------------------------------

                

			              




