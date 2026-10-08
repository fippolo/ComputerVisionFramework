module ComputerVisionFW where

import Data.List(sort)
import Data.Array (Array, (!), array)
import Data.Word (Word8)
import Codec.Picture (PixelRGB8 (PixelRGB8), convertRGB8, generateImage, imageHeight, imageWidth, pixelAt, readImage, writeBitmap)
import System.Random (randomRIO)
import Data.Matrix (Matrix, submatrix, fromLists, toLists, nrows, ncols, getElem, zero, (<|>), (<->))



convolve1d :: Num a => [a] -> [a] -> [a]
convolve1d signal kernel =
  [ sum (zipWith (*) window (reverse kernel))
  | window <- windows1d (length kernel) signal
  ]


windows1d :: Int -> [a] -> [[a]]
windows1d n xs
    | length xs < n = []
    | otherwise = take n xs : windows1d n (drop 1 xs)

windows2d :: Int -> Int-> Int -> Int -> Matrix a -> [Matrix a]
windows2d startcol startrow n m matrix
    | n <= 0 || m <= 0 = []
    | nrows matrix < n || ncols matrix < m = []
    | startcol + m - 1 > ncols matrix = windows2d 1 (startrow + 1) n m matrix
    | startrow + n - 1 > nrows matrix = []
    | otherwise = submatrix startrow (startrow + n - 1) startcol (startcol + m - 1) matrix
                  : windows2d (startcol + 1) startrow n m matrix

flip2dKernel :: Matrix a -> Matrix a
flip2dKernel kernel = fromLists (map reverse (reverse (toLists kernel)))

convolvedWindow :: Num a => Matrix a -> Matrix a -> Matrix a
convolvedWindow signal kernel = fromLists (zipWith (zipWith (*)) (toLists signal) (toLists kernel))

convolvedValue :: Num a => Matrix a -> a
convolvedValue cWindow = sum (concat (toLists cWindow))

rowsOf :: Int -> [a] -> [[a]]
rowsOf _ [] = []
rowsOf n xs = take n xs : rowsOf n (drop n xs)

convolve2d :: Num a => Matrix a -> Matrix a -> Matrix a
convolve2d signal kernel
    | nrows signal < nrows kernel || ncols signal < ncols kernel = fromLists []
    | otherwise = fromLists (rowsOf outputWidth convolvedValues)
    where
        flippedKernel = flip2dKernel kernel
        signalWindows = windows2d 1 1 (nrows kernel) (ncols kernel) signal
        convolvedValues = [ convolvedValue (convolvedWindow window flippedKernel) | window <- signalWindows ]
        outputWidth = ncols signal - ncols kernel + 1

correlate1d :: Num a => [a] -> [a] -> [a]
correlate1d signal kernel = convolve1d signal (reverse kernel)

correlate2d :: Num a => Matrix a -> Matrix a -> Matrix a
correlate2d signal kernel = convolve2d signal (flip2dKernel kernel)

-- | Standard Prewitt masks for estimating horizontal and vertical intensity
-- gradients.  They are used with 'correlate2d', so their orientation is not
-- reversed before being applied.
prewittHorizontalKernel :: Matrix Double
prewittHorizontalKernel =
  fromLists
    [ [-1, 0, 1]
    , [-1, 0, 1]
    , [-1, 0, 1]
    ]

prewittVerticalKernel :: Matrix Double
prewittVerticalKernel =
  fromLists
    [ [-1, -1, -1]
    , [ 0,  0,  0]
    , [ 1,  1,  1]
    ]

-- | Detect edges by combining the horizontal and vertical Prewitt responses.
-- The result has two fewer rows and columns than the input, because
-- 'correlate2d' performs valid (unpadded) correlation.
prewittEdgeDetection :: Matrix Double -> Matrix Double
prewittEdgeDetection signal =
  if nrows signal < 3 || ncols signal < 3
    then zero 0 0
    else fromLists
      (zipWith (zipWith gradientMagnitude)
        (toLists horizontalResponse)
        (toLists verticalResponse))
  where
    horizontalResponse = correlate2d signal prewittHorizontalKernel
    verticalResponse = correlate2d signal prewittVerticalKernel
    gradientMagnitude horizontal vertical =
      sqrt (horizontal * horizontal + vertical * vertical)

zeroPad :: Num a => Int -> Matrix a -> Matrix a
zeroPad padSize matrix
    | padSize <= 0 = matrix
    | otherwise = padRows <-> (padCols <|> matrix <|> padCols) <-> padRows
        where
            padCols = zero (nrows matrix) padSize
            padRows = zero padSize (ncols matrix + 2 * padSize)

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe [x] = Just x
lastMaybe (_ : xs) = lastMaybe xs

padRow :: Int -> [a] -> [a]
padRow _ [] = []
padRow padSize row@(left : _) =
  case lastMaybe row of
    Nothing -> []
    Just right ->
      replicate padSize left ++ row ++ replicate padSize right

replicatePad :: Int -> Matrix a -> Matrix a
replicatePad padSize matrix
  | padSize <= 0 || nrows matrix == 0 || ncols matrix == 0 = matrix
  | otherwise =
      case toLists matrix of
        [] -> matrix
        rows@(topRow : _) ->
          case lastMaybe rows of
            Nothing -> matrix
            Just bottomRow ->
              fromLists
                ( replicate padSize (padRow padSize topRow)
               ++ map (padRow padSize) rows
               ++ replicate padSize (padRow padSize bottomRow)
                )

mirrorPad :: Int -> Matrix a -> Matrix a
mirrorPad padSize matrix
  | padSize <= 0 || nrows matrix == 0 || ncols matrix == 0 = matrix
  | otherwise =
      fromLists
        [ [ row !! mirrorIndex width x
          | x <- [-padSize .. width + padSize - 1]
          ]
        | y <- [-padSize .. height + padSize - 1]
        , let row = originalRows !! mirrorIndex height y
        ]
  where
    originalRows = toLists matrix
    height = nrows matrix
    width = ncols matrix

mirrorIndex :: Int -> Int -> Int
mirrorIndex size index
  | size == 1 = 0
  | otherwise =
      let period = 2 * size - 2
          position = index `mod` period
       in if position < size
            then position
            else period - position

-- | Load a bitmap and convert its pixels to grayscale intensities in [0, 255].
-- The resulting matrix has one row per image row and one column per image column.
bitmapToGrayscaleMatrix :: FilePath -> IO (Either String (Matrix Double))
bitmapToGrayscaleMatrix path = do
  decoded <- readImage path
  pure $ do
    image <- decoded
    let rgbImage = convertRGB8 image
    pure . fromLists $
      [ [ pixelLuminance (pixelAt rgbImage x y)
        | x <- [0 .. imageWidth rgbImage - 1]
        ]
      | y <- [0 .. imageHeight rgbImage - 1]
      ]

pixelLuminance :: PixelRGB8 -> Double
pixelLuminance (PixelRGB8 red green blue) =
  0.2126 * fromIntegral red
    + 0.7152 * fromIntegral green
    + 0.0722 * fromIntegral blue

-- | Save a matrix of grayscale intensities as an 8-bit bitmap.
-- Values outside [0, 255] are clamped before writing.
grayscaleMatrixToBitmap :: FilePath -> Matrix Double -> IO ()
grayscaleMatrixToBitmap path matrix
  | nrows matrix == 0 || ncols matrix == 0 =
      ioError (userError "grayscaleMatrixToBitmap: matrix must not be empty")
  | otherwise =
      writeBitmap path image
  where
    image = generateImage pixel (ncols matrix) (nrows matrix)
    rows = toLists matrix
    pixel x y =
      let intensity = rows !! y !! x
          value = intensityToWord8 intensity
       in PixelRGB8 value value value

intensityToWord8 :: Double -> Word8
intensityToWord8 intensity
  | isNaN intensity || isInfinite intensity = 0
  | otherwise = fromIntegral (round (max 0 (min 255 intensity)) :: Integer)

impulseNoise :: Double -> Double -> Matrix Double -> IO (Matrix Double)
impulseNoise saltProbability pepperProbability signal
  | saltProbability < 0 || pepperProbability < 0
      || saltProbability + pepperProbability > 1 =
      error "impulseNoise: probabilities must be non-negative and sum to at most one"
  | otherwise =
      fromLists <$> traverse (traverse addImpulse) (toLists signal)
  where
    addImpulse intensity = do
      sample <- randomRIO (0.0, 1.0)
      pure $
        if sample < pepperProbability
          then 0.0
          else if sample < pepperProbability + saltProbability
            then 255.0
            else intensity

whiteGaussianNoise :: Double -> Double -> Matrix Double -> IO (Matrix Double)
whiteGaussianNoise mean stdDev signal
  | stdDev < 0 = error "whiteGaussianNoise: standard deviation must be non-negative"
  | otherwise =
      fromLists <$> traverse (traverse addNoise) (toLists signal)
  where
    addNoise x = do
      z <- standardNormal
      pure (x + mean + stdDev * z)

standardNormal :: IO Double
standardNormal = do
  u1 <- randomRIO (1.0e-12, 1.0)
  u2 <- randomRIO (0.0, 1.0)
  pure (sqrt (-2 * log u1) * cos (2 * pi * u2))

meanValue :: Fractional a => Matrix a -> a
meanValue window =
  sum (concat (toLists window))
    / fromIntegral (nrows window * ncols window)

denoiseMean :: Fractional a =>Int -> Matrix a -> Matrix a
denoiseMean k signal
    | nrows signal < k || ncols signal < k = fromLists []
    | k <= 0 = fromLists []
    | otherwise = fromLists (rowsOf outputWidth meanValues)
    where
        signalWindows = windows2d 1 1 k k signal
        meanValues = [ meanValue window | window <- signalWindows ]
        outputWidth = ncols signal - k + 1

gaussianKernel :: Int -> Double -> Matrix Double
gaussianKernel size sigma
  | size <= 0 || even size || sigma <= 0 = fromLists []
  | otherwise = fromLists normalizedWeights
  where
    radius = size `div` 2
    weights =
      [ [ exp (-(fromIntegral (x * x + y * y)) / (2 * sigma * sigma))
        | x <- [-radius .. radius]
        ]
      | y <- [-radius .. radius]
      ]
    totalWeight = sum (concat weights)
    normalizedWeights = map (map (/ totalWeight)) weights

gaussianFilter :: Int -> Double -> Matrix Double -> Matrix Double
gaussianFilter size sigma signal
  | size <= 0 || even size || sigma <= 0 = fromLists []
  | otherwise = convolve2d (mirrorPad (size `div` 2) signal) kernel
  where
    kernel = gaussianKernel size sigma

medianFilter :: Int -> Matrix Double -> Matrix Double
medianFilter k signal
    | nrows signal < k || ncols signal < k = fromLists []
    | k <= 0 = fromLists []
    | otherwise = fromLists (rowsOf outputWidth medianValues)
    where
        signalWindows = windows2d 1 1 k k signal
        medianValues = [ medianvlues window | window <- signalWindows ]
        outputWidth = ncols signal - k + 1

medianvlues :: Matrix Double -> Double
medianvlues window =
  let sortedValues = sort (concat (toLists window))
      n = length sortedValues
  in if odd n
     then sortedValues !! (n `div` 2)
     else let mid1 = sortedValues !! (n `div` 2 - 1)
              mid2 = sortedValues !! (n `div` 2)
          in (mid1 + mid2) / 2

bilateralFilter :: Int -> Double -> Double -> Matrix Double -> Matrix Double
bilateralFilter size spatialSigma rangeSigma signal
  | size <= 0 || even size || spatialSigma <= 0 || rangeSigma <= 0 = fromLists []
  | otherwise = fromLists (rowsOf outputWidth filteredValues)
  where
    radius = size `div` 2
    paddedSignal = mirrorPad radius signal
    signalWindows = windows2d 1 1 size size paddedSignal
    filteredValues = map (bilateralValue spatialSigma rangeSigma) signalWindows
    outputWidth = ncols signal

bilateralValue :: Double -> Double -> Matrix Double -> Double
bilateralValue spatialSigma rangeSigma window =
  weightedSum / totalWeight
  where
    rows = toLists window
    radius = nrows window `div` 2
    centerValue = rows !! radius !! radius
    weightedPixels =
      [ (pixel, spatialWeight * rangeWeight)
      | (y, row) <- zip [-radius .. radius] rows
      , (x, pixel) <- zip [-radius .. radius] row
      , let spatialWeight = exp (-(fromIntegral (x * x + y * y)) / (2 * spatialSigma * spatialSigma))
      , let difference = pixel - centerValue
      , let rangeWeight = exp (-(difference * difference) / (2 * rangeSigma * rangeSigma))
      ]
    weightedSum = sum [pixel * weight | (pixel, weight) <- weightedPixels]
    totalWeight = sum [weight | (_, weight) <- weightedPixels]

nonLocalMeansFilter :: Int -> Int -> Double -> Matrix Double -> Matrix Double
nonLocalMeansFilter patchSize searchSize filteringStrength signal
  | patchSize <= 0 || even patchSize || searchSize <= 0 || even searchSize
      || filteringStrength <= 0 || nrows signal == 0 || ncols signal == 0 =
      fromLists []
  | otherwise =
      fromLists
        [ [ filteredValue (x + paddingRadius) (y + paddingRadius)
          | x <- [0 .. ncols signal - 1]
          ]
        | y <- [0 .. nrows signal - 1]
        ]
  where
    patchRadius = patchSize `div` 2
    searchRadius = searchSize `div` 2
    paddingRadius = patchRadius + searchRadius
    paddedSignal = mirrorPad paddingRadius signal
    paddedHeight = nrows paddedSignal
    paddedWidth = ncols paddedSignal
    offsetIntegrals =
      [ (offsetX, offsetY, squaredDifferenceIntegral offsetX offsetY)
      | offsetY <- [-searchRadius .. searchRadius]
      , offsetX <- [-searchRadius .. searchRadius]
      ]
    filteredValue centerX centerY =
      weightedSum / totalWeight
      where
        weightedPixels =
          [ (matrixPixelAt (centerX + offsetX) (centerY + offsetY), weight)
          | (offsetX, offsetY, integral) <- offsetIntegrals
          , let distance = patchDistance integral centerX centerY
          , let weight = exp (-distance / (filteringStrength * filteringStrength))
          ]
        weightedSum = sum [pixel * weight | (pixel, weight) <- weightedPixels]
        totalWeight = sum [weight | (_, weight) <- weightedPixels]
    squaredDifferenceIntegral offsetX offsetY =
      integralImage paddedHeight paddedWidth $ \x y ->
        let difference = matrixPixelAt x y - matrixPixelAt (x + offsetX) (y + offsetY)
         in difference * difference
    patchDistance integral centerX centerY =
      rectangleSum integral
        (centerX - patchRadius) (centerY - patchRadius)
        (centerX + patchRadius) (centerY + patchRadius)
    matrixPixelAt x y =
      getElem (mirrorIndex paddedHeight y + 1) (mirrorIndex paddedWidth x + 1) paddedSignal

integralImage :: Int -> Int -> (Int -> Int -> Double) -> Array (Int, Int) Double
integralImage height width valueAt = integral
  where
    integral = array ((0, 0), (height, width)) entries
    entries =
      [ ((0, column), 0.0) | column <- [0 .. width] ]
        ++ [ ((row, 0), 0.0) | row <- [1 .. height] ]
        ++ [ ((row, column)
             , valueAt (column - 1) (row - 1)
                 + integral ! (row - 1, column)
                 + integral ! (row, column - 1)
                 - integral ! (row - 1, column - 1)
             )
           | row <- [1 .. height]
           , column <- [1 .. width]
           ]

rectangleSum :: Array (Int, Int) Double -> Int -> Int -> Int -> Int -> Double
rectangleSum integral left top right bottom =
  integral ! (bottom + 1, right + 1)
    - integral ! (top, right + 1)
    - integral ! (bottom + 1, left)
    + integral ! (top, left)
