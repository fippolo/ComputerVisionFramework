module ComputerVisionFW where

import Data.List()
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