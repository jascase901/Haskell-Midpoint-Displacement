
-- |Mountain range generater based off  
-- http://www.gameprogrammer.com/fractal.html
--
import Graphics.Gloss
import System.Random

type LineSegment = (Point, Point)

--Configuration Constants
edge = 2 :: Float
maxHeight = 2 :: Float
dampeningConstant = 0.9 :: Float

main = do  
    myPic <- displayPic 15 
    display (InWindow "Mountain" (500, 500) (20,  20)) black (Color blue myPic)

--Calculates the midpoint of a line segment then moves the y value up by
--displacement
midPoint :: LineSegment -> Float -> Point
midPoint ((x1,y1), (x2,y2)) displacement = ((x1+x2)/2,   ((y1+y2)/2 + displacement))

--Splits a line segment by its midpoint, and displays the Y value by
--displacement
splitSegment :: LineSegment -> Float -> [LineSegment]
splitSegment (v1, v2)  displacement = [(v1, e), (e ,v2)]
  where e = midPoint (v1,v2) displacement

splitSegmentRandomly ::  LineSegment -> Float -> IO [LineSegment]
splitSegmentRandomly (v1, v2)  randomRange = do
    factor <- randomRIO (-(randomRange),randomRange) :: IO Float
    let x = splitSegment (v1,v2) factor
    return x

splitSegmentsRandomly :: [LineSegment] ->Float -> IO [LineSegment]
splitSegmentsRandomly  [] randomRange= return []
splitSegmentsRandomly xs randomRange= do 
                             segments <- return xs
                             let head' =  case segments of
                                           (x:xs) -> x
                             let tail' =  case segments of
                                           (x:xs) -> xs
                             fu <- splitSegmentRandomly head' randomRange 
                             cker <- splitSegmentsRandomly tail' (randomRange)
                             return (fu ++ cker)

--Lessons the dampening range each time 
stepRandomly :: Float -> IO [LineSegment]
stepRandomly 0 =  do return ( [((-(edge),0) , (edge,0))])
stepRandomly n  = do
                    segments <- stepRandomly (n-1)
                    let  newRandomRange = (maxHeight /  2 ** ((n-1) * dampeningConstant )) 
                    randomSplit <- splitSegmentsRandomly segments  newRandomRange
                    return $ randomSplit 
                    
displayPic :: Float -> IO Picture
displayPic n = do
    segments <-stepRandomly n
    print "\n"
    return $ Line $ segmentsToPoints segments


segmentsToPoints :: [LineSegment] -> Path
segmentsToPoints [] = []
segmentsToPoints  (x:xs)  = segmentToPoints x ++ segmentsToPoints xs
  where segmentToPoints (v1, v2)  = [v1, v2]

--For displaying pictures
showSteps :: [Picture] -> Picture
showSteps xs 
  = let 
      n = length xs
      yPos = [(1.5 * maxHeight) * (fromIntegral x) | x <- [1..n]]
      in 
      Pictures (zipWith (moveUpN) xs yPos)

moveUpN :: Picture -> Float -> Picture
moveUpN xs n = translate 0 n xs



