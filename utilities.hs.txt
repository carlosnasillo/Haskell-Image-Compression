module Utilities where

import Char
import Array

---------------------------------------------------------------------------
-- TYPE DECLARATIONS
---------------------------------------------------------------------------
type Point = (Int, Int)
type Image = Array Point Int

type Pixels = [Int]
type Block = (Point,Pixels)

type BlockSum = (Block, Int, Int)

type Dims = (Int, Int)
type Transformer = (Dims,Pixels) -> (Dims, Pixels)

---------------------------------------------------------------------------
-- FILE I/O
---------------------------------------------------------------------------

convertTransform :: (Int -> Pixels -> Pixels) -> Transformer
convertTransform f ((x,y), pix) = ((x,y), f x pix)

testTransform :: (Int -> Pixels -> Pixels) -> IO ()
testTransform f = transform (convertTransform f)

testRotations g = do { putStr ("Input file name:");
		                  ifile <- getLine;
		 	                ys <- readFile (ifile++".pgm");
                      writePixels ( [f (snd (fromPgm ys)) | f <- g (fst(fst (fromPgm ys)))]) (fst (fromPgm ys)) }


-- writes several blocks of the same size
writePixels :: [Pixels] -> Dims -> IO ()
writePixels b (w,h) = stringToFile (toPgm ((w, paddedHeight (length b) h), joinWithSpace w b))


transform :: Transformer -> IO ()
transform f = do putStr ("Input file name:")
		 ifile <- getLine
		 putStr ("Output file name:")
		 ofile <- getLine
		 ys <- readFile (ifile++".pgm")
                 writeFile (ofile++".pgm") ( toPgm (f (fromPgm ys)))
                 
showBlocks :: (Image -> Dims -> [Point] -> [Block]) -> Dims -> [Point] -> IO ()
showBlocks f d ps = do { putStr ("Input file name:");
		                  ifile <- getLine;
		 	                ys <- readFile (ifile++".pgm");
                      writeBlocks (f (stringToArray(fromPgm ys)) d ps) d}
                 
                 
stripSums :: [BlockSum] -> [Block]
stripSums = map f
						where f (x,y,z) = x

showImageBlocks :: (Image -> Dims -> [BlockSum]) -> Dims -> IO ()
showImageBlocks f d = do { putStr ("Input file name:");
		                  ifile <- getLine;
		 	                ys <- readFile (ifile++".pgm");
                      writeBlocks (stripSums   (f (stringToArray(fromPgm ys)) d )) d}
                      
--showRotations :: (Image -> Dims -> [(BlockSum,Int)]) -> Dims -> IO ()
showRotations f  h = showImageBlocks g
											 where g i d = map fst (f d(h i d))
                      

stringToFile   ::  String -> IO ()
stringToFile d      = do 
		 putStr ("Output file name:")
		 ofile <- getLine
		 writeFile (ofile++".pgm") d

writeImage :: Image -> IO ()
writeImage i = stringToFile (arrayToPgm i)

writeBlock :: Block -> Dims -> IO ()
writeBlock b d = stringToFile (toPgm (d, snd b))

-- writes several blocks of the same size
writeBlocks :: [Block] -> Dims -> IO ()
writeBlocks b (w,h) = stringToFile (toPgm ((w, paddedHeight (length b) h), joinWithSpace w (map snd b)))


gap :: Int
gap = 20

blackspace :: Int -> [Int]
blackspace w = take (w*gap) (repeat 0)

joinWithSpace :: Int -> [[Int]] -> [Int]
joinWithSpace w xs = concat (map (++(blackspace w)) xs)

paddedHeight :: Int -> Int -> Int
paddedHeight length height = length * (height + gap)

-----------------------------------------------------------------------
-- PGM TO/FROM (Dims, Pixels) CONVERSION
-----------------------------------------------------------------------

----------  Convert (Dims, Pixels) to String  ----------

toPgm :: (Dims, Pixels)  -> String
toPgm (d,gs) = "P2"++"\n"++
        "# Created by me"++"\n"++
        (fromPair d)++"\n"++
        "255"++"\n"++(toString gs)

----------  convert  a pair of dimensions to a string ----------

fromPair :: Show a => (a,a) -> String
fromPair (x,y) = toString [x,y]


toString :: Show a => [a] -> String
toString = concat . (map f)
       where f x = show x ++ " "

---------- convert text from pgm file into (Dims, Pixels)

fromPgm :: String -> (Dims, Pixels)
fromPgm xs = ((fstPair . stringToInts) dimlist, stringToInts text)
         where ys = lines xs
               dimlist = ys !! 2
               text = unlines (drop 4 ys)

fstPair :: [a] -> (a,a)
fstPair (x:y:ys) = (x,y)

stringToInts :: String -> [Int]
stringToInts = map numToInt . words

numToInt :: String -> Int
numToInt = foldl f 0 . map digitToInt
        where f x y = 10 * x + y

-------------------------------------------------------------------
-- ARRAY STUFF
-------------------------------------------------------------------

----------  convert array into (Dims, Pixels) ----------

arrayToString :: Image -> (Dims,  Pixels)
arrayToString arr =  (dims,  newElems arr)
		where ((a,b),(c,d)) = bounds arr
		      dims	= (c-a+1, d-b+1)

newElems :: Image -> Pixels
newElems i =  [i!(x,y) | y <- [b..d], x <- [a..c]]
		where ((a,b),(c,d)) = bounds i
		     

arrayToPgm :: Array (Int, Int) Int -> String
arrayToPgm  = toPgm . arrayToString


------------------------------------------------------------
stringToArray :: (Dims, Pixels) -> Array Point Int
stringToArray (d,gs) = array ((0,0), sub1 d) (addIndices d ((0,0), gs))
	where sub1 (x,y) = (x-1,y-1)


addIndices :: Dims -> Block -> [(Point, Int)]
addIndices (w,h) ((x,y), pts) = zip (getCoords (w,h) (x,y)) pts


getCoords :: Dims -> Point -> [Point]
getCoords (w,h) (x,y) = [  (u,v) |  v <- [y..y+h-1], u <- [x..x+w-1]] 

newnumToInt xs
  | head xs == '-' 	= - numToInt (tail xs)
  | otherwise		= numToInt xs
  
pgmToArray :: String -> Array Point Int
pgmToArray = stringToArray . fromPgm

----------------------------------------------------------
-- magnify picture (just used to produce test pictures)
----------------------------------------------------------

shift :: Int -> Int -> Int -> Int
shift base coord factor = base + factor * (coord - base)

shiftUpperBound :: Point -> Point -> Int -> Point
shiftUpperBound (a,b) (u,v) factor = (shift a u factor + factor - 1, shift b v factor + factor - 1)

expandPixel :: Point -> Point -> Int -> b -> [(Point,b)]
expandPixel (a,b) (u,v) factor pix = [((w,z), pix) | w <- [c..c+factor-1], z <-[d..d+factor-1]] 
												where c = shift a u factor
												      d = shift b v factor

magnify :: Int -> Image -> Image
magnify factor i = array ((a,b), (shiftUpperBound (a,b) (c,d) factor)) 
                         (concat [ expandPixel (a,b) (u,v) factor (i!(u,v)) |  u <- [a..c], v <- [b..d]])
			              where ((a,b),(c,d)) = bounds i
			              

-----------------------------------------------------------------------------------------
-- SQUASH PICTURE
-----------------------------------------------------------------------------------------

half :: Point -> Point
half (x,y) = (div x 2, div y 2)

double :: Point -> Point
double (x,y) = (x* 2, y *2)

halfBounds :: Image -> Point
halfBounds  = half . snd . bounds 

averagePoints :: Image -> Point -> Int
averagePoints im (x,y) = div (sum [im!(u,v) | u <- [x..x +1], v <-[y..y + 1]]) 4

fillImage :: (Point,Point) -> (Point -> b) -> Array Point b
fillImage ((a,b),(c,d)) f 
  = array ((a,b),(c,d)) [((x,y),f (x,y)) | x <- [a..c], y <-[b..d]]
  
squash :: Image -> Image
squash im = fillImage ((0,0),halfBounds im) ((averagePoints im) . double)


------------------------------------------------------------------
-- RMS CALCULATION
------------------------------------------------------------------
-- rmsTransform takes a range & domain block & returns the rms & transform

type Trans = (Float, Float, Float, Point, Point, Int, Dims)          

-- RMSdata, RBlockpos, DBlockpos, Rotflip, BlockSize

rmsTransform :: Dims -> BlockSum -> (BlockSum, Int) -> Trans
rmsTransform sz ((rpos,rpix), rsum, rsumsq) (((dpos, dpix), dsum, dsumsq) , drot )
    						= (r, o, s, rpos, dpos, drot,sz)
           where  (r, o, s) = (rms (intsToFloats (fst sz) (snd sz) dsum dsumsq rsum rsumsq rpix dpix ))
               	    	   

rms :: (Float, Float, Float, Float, Float, Float) -> (Float, Float, Float)
rms (n, a, asq, b, bsq, ab) = (r,o,s)
     where  s = if d == 0 then 0 else (n * ab - a * b) /d
            d =  n * asq - a * a
            o = (b - s * a) / n
            r = (bsq + s * (s * asq - 2 * ab + 2 * o * a) + o * (n * o - 2 * b)) / n
            
------------------------------------------------------------------
           
-- intsToFloats converts integers to floats in order to do the RMS calculation
            
intsToFloats :: Int -> Int-> Int -> Int-> Int -> Int -> Pixels -> Pixels -> (Float, Float, Float, Float, Float, Float)
intsToFloats w h aI asqI bI bsqI rpix dpix = (n, a, asq, b, bsq, ab)
     where  a = fromIntegral aI
            asq = fromIntegral asqI
            b = fromIntegral bI
            bsq = fromIntegral bsqI
            ab = fromIntegral (pixelProd rpix dpix )
            n = fromIntegral (w*h)

pixelProd :: Pixels -> Pixels -> Int
pixelProd xs ys = sum (zipWith (*) xs ys)          


---------------------------------------------------------------------------
-- MAIN ENCODE FUNCTION
-- type "image encode" to use it
---------------------------------------------------------------------------

image ::  (Image -> Dims -> [Trans]) -> IO ()
image e     = do putStr ("Input file name:")
		 ifile <- getLine
		 xs <- readFile (ifile++".pgm")
		 putStr ("Output file name:")
		 ofile <- getLine
		 putStr ("Block width:")
		 w <- getLine
		 putStr ("Block height:")
		 h <- getLine
                 writeFile ofile ( transformsToString (e (pgmToArray xs) (numToInt w, numToInt h) ))
 
 -----------------------------------------------------------------------------------
--  transToString converts a transform to a string
-----------------------------------------------------------------------------------

transToString :: Trans -> String
transToString (a,b,c,d,e,f,g) = show a++" "++show b++" "++show c++" "++
                              show (fst d)++" "++show (snd d)++" "++
                              show (fst e)++" "++show (snd e)++" "++
                              show f++" "++show (fst g)++" "++show (snd g)++"\n"


transformsToString :: [Trans] -> String
transformsToString  = concat . map transToString 

	
-----------------------------------------------------------------------------------
-- MAIN DECODE FUNCTION
-----------------------------------------------------------------------------------
 


file   ::  ([Trans] -> Image ->  Image) -> IO ()
file d      = do putStr ("Input file name:")
		 ifile <- getLine
		 xs <- readFile ifile
		 putStr ("Output file name:")
		 ofile <- getLine
		 putStr ("Number of iterations:")
		 n <- getLine
		 putStr ("Image width:")
		 w <- getLine
		 putStr ("Image height:")
		 h <- getLine
		 writeFile (ofile++".pgm") ( arrayToPgm ((iter (numToInt n) (d (fileToTrans xs))) 
		                               (blankImage (numToInt w, numToInt h) 128)  ))
 
 

blankImage :: Dims -> Int -> Image
blankImage (width, height) shade = 
  array ((0,0), (width-1, height-1))  [ ((x,y), shade) | x <- [0 .. width-1], y <- [0..height-1]]
  


iter ::  Int -> (a->a) ->(a->a)
iter 0 f  = id
iter n f  = f . (iter (n-1) f)


-- fileToTrans converts the file text to a list of transforms

fileToTrans :: String -> [Trans]
fileToTrans = map lineToTrans . lines

lineToTrans :: String -> Trans
lineToTrans xs = makeTrans ys zs
            where ws = words xs
                  ys = map makeFloat (take 3 ws)
                  zs = map newnumToInt (drop 3 ws)
                  

makeFloat :: String -> Float
makeFloat f
  | elem 'e' f = body f * expo f
  | otherwise = toFloat f




body f = toFloat (takeWhile (/= 'e') f)

expo :: String -> Float
expo f =  10 ^^ newnumToInt (tail (dropWhile (/= 'e') f))
  

numToFloat :: String -> Float
numToFloat = fromIntegral . numToInt


makeTrans :: [Float] -> Pixels -> Trans
makeTrans [a,b,c] [d,e,f,g,h,i,j] = (a,b,c,(d,e),(f,g),h,(i,j))


toFloat :: String -> Float
toFloat ss
     | head ss == '-' = 0 - posFloat (tail ss)
     | otherwise      = posFloat ss


posFloat :: String -> Float
posFloat ss = intpart + (numToFloat rem) / (10^ (length rem))
            where  intpart = numToFloat (takeWhile (/= '.') ss)
                   rem = tail (dropWhile (/= '.') ss)
