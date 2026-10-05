module ComputerVisionFW where

import Data.List()
import Data.Word (Word8)
import Codec.Picture (PixelRGB8 (PixelRGB8), convertRGB8, generateImage, imageHeight, imageWidth, pixelAt, readImage, writeBitmap)
import System.Random (randomRIO)
import Data.Matrix (Matrix, submatrix,fromLists,toLists,nrows, ncols, zero, (<|>), (<->))



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