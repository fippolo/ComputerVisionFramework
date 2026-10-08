module Main where

import ComputerVisionFW
import Data.Matrix (fromLists, toLists, zero)
import Test.HUnit

tests :: Test
tests = TestList
  [ "simple convolution" ~:
        convolve1d [1,2,3,4] [1,1] ~?= [3,5,7]

    , "kernel reversal matters" ~:
        convolve1d [1,2,3] [1,2] ~?= [4,7]

    , "flipped 2D kernel" ~:
        flip2dKernel (fromLists [[1,2],[3,4]]) ~?= fromLists [[4,3],[2,1]]

    , "kernel longer than signal" ~:
        convolve1d [1,2] [1,2,3] ~?= []
    , "2D windows" ~:
        windows2d 1 1 3 3 (fromLists [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]) ~?=
        map fromLists
          [ [[1,2,3],[5,6,7],[9,10,11]]
          , [[2,3,4],[6,7,8],[10,11,12]]
          , [[5,6,7],[9,10,11],[13,14,15]]
          , [[6,7,8],[10,11,12],[14,15,16]]
          ]

    , "larger 2D windows" ~:
        windows2d 1 1 3 3
          (fromLists
            [ [ 1, 2, 3, 4, 5]
            , [ 6, 7, 8, 9,10]
            , [11,12,13,14,15]
            , [16,17,18,19,20]
            , [21,22,23,24,25]
            ]) ~?=
        map fromLists
          [ [[ 1, 2, 3], [ 6, 7, 8], [11,12,13]]
          , [[ 2, 3, 4], [ 7, 8, 9], [12,13,14]]
          , [[ 3, 4, 5], [ 8, 9,10], [13,14,15]]
          , [[ 6, 7, 8], [11,12,13], [16,17,18]]
          , [[ 7, 8, 9], [12,13,14], [17,18,19]]
          , [[ 8, 9,10], [13,14,15], [18,19,20]]
          , [[11,12,13], [16,17,18], [21,22,23]]
          , [[12,13,14], [17,18,19], [22,23,24]]
          , [[13,14,15], [18,19,20], [23,24,25]]
          ]
    , "flipped kernel" ~:
        flip2dKernel (fromLists [[1,2,3],[4,5,6],[7,8,9]]) ~?= fromLists [[9,8,7],[6,5,4],[3,2,1]]
    , "convolvedWindow" ~:
        convolvedWindow (fromLists [[1,2],[3,4]]) (fromLists [[5,6],[7,8]]) ~?= fromLists [[5,12],[21,32]]
    , "convolvedValue" ~:
        convolvedValue (fromLists [[5,12],[21,32]]) ~?= 70
    , "convolve2d" ~:
        convolve2d (fromLists [[1,2,3],[4,5,6],[7,8,9]]) (fromLists [[1,0],[0,-1]]) ~?= fromLists [[4,4],[4,4]]
    ,"correlate1d" ~:
        correlate1d [1,2,3,4] [1,1] ~?= [3,5,7]
    , "correlate2d" ~:
        correlate2d (fromLists [[1,2,3],[4,5,6],[7,8,9]]) (fromLists [[1,0],[0,-1]]) ~?= fromLists [[-4,-4],[-4,-4]]
    , "larger 3x3 correlation" ~:
        correlate2d
          (fromLists
            [ [ 1, 2, 3, 4, 5]
            , [ 6, 7, 8, 9,10]
            , [11,12,13,14,15]
            , [16,17,18,19,20]
            ])
          (fromLists
            [ [1,2,3]
            , [4,5,6]
            , [7,8,9]
            ]) ~?=
        fromLists
          [ [411,456,501]
          , [636,681,726]
          ]
    , "Prewitt detects a vertical edge" ~:
        prewittEdgeDetection
          (fromLists
            [ [0.0, 0.0, 10.0, 10.0]
            , [0.0, 0.0, 10.0, 10.0]
            , [0.0, 0.0, 10.0, 10.0]
            , [0.0, 0.0, 10.0, 10.0]
            ]) ~?=
        fromLists
          [ [10.0, 10.0]
          , [10.0, 10.0]
          ]
    , "Prewitt combines horizontal and vertical gradients" ~:
        prewittEdgeDetection
          (fromLists
            [ [ 0.0, 10.0, 20.0]
            , [10.0, 20.0, 30.0]
            , [20.0, 30.0, 40.0]
            ]) ~?=
        fromLists [[sqrt 800.0]]
    , "Prewitt returns no values for an input smaller than its masks" ~:
        prewittEdgeDetection (fromLists [[1.0, 2.0], [3.0, 4.0]]) ~?= zero 0 0
    , "2x2 mean denoising" ~:
        denoiseMean 2
          (fromLists
            [ [1.0,2.0,3.0]
            , [4.0,5.0,6.0]
            , [7.0,8.0,9.0]
            ]) ~?=
        fromLists
          [ [3.0,4.0]
          , [6.0,7.0]
          ]
    , "one-pixel zero padding" ~:
        zeroPad 1 (fromLists [[1,2,3],[4,5,6]]) ~?=
        fromLists
          [ [0,0,0,0,0]
          , [0,1,2,3,0]
          , [0,4,5,6,0]
          , [0,0,0,0,0]
          ]
    , "one-pixel replicate padding" ~:
        replicatePad 1 (fromLists [[1,2,3],[4,5,6],[7,8,9]]) ~?=
        fromLists
          [ [1,1,2,3,3]
          , [1,1,2,3,3]
          , [4,4,5,6,6]
          , [7,7,8,9,9]
          , [7,7,8,9,9]
          ]
    , "one-pixel mirror padding" ~:
        mirrorPad 1 (fromLists [[1,2,3],[4,5,6],[7,8,9]]) ~?=
        fromLists
          [ [5,4,5,6,5]
          , [2,1,2,3,2]
          , [5,4,5,6,5]
          , [8,7,8,9,8]
          , [5,4,5,6,5]
          ]
    , "one-pixel Gaussian filter is the identity" ~:
        gaussianFilter 1 1.0 (fromLists [[1.0,2.0],[3.0,4.0]]) ~?=
        fromLists [[1.0,2.0],[3.0,4.0]]
    , TestLabel "Gaussian kernel is normalized" $
        TestCase $
          assertBool "kernel weights should sum to one"
            (abs (sum (concat (toLists (gaussianKernel 3 1.0))) - 1.0) < 1.0e-12)
    , TestLabel "Gaussian filter preserves constant intensity" $
        TestCase $
          assertBool "a normalized filter should not change a constant image"
            ( all (\value -> abs (value - 50.0) < 1.0e-12)
                (concat (toLists (gaussianFilter 3 1.0 (fromLists (replicate 4 (replicate 4 50.0))))))
            )
    , TestLabel "zero impulse-noise probabilities preserve the signal" $
        TestCase $ do
          result <- impulseNoise 0.0 0.0 (fromLists [[10.0, 20.0], [30.0, 40.0]])
          result @?= fromLists [[10.0, 20.0], [30.0, 40.0]]
    , TestLabel "salt probability one produces white pixels" $
        TestCase $ do
          result <- impulseNoise 1.0 0.0 (fromLists [[10.0, 20.0], [30.0, 40.0]])
          result @?= fromLists [[255.0, 255.0], [255.0, 255.0]]
    , TestLabel "white Gaussian noise with zero deviation shifts by its mean" $
        TestCase $ do
          noisy <- whiteGaussianNoise 2.5 0.0
            (fromLists [[1.0, 2.0], [3.0, 4.0]])
          noisy @?= fromLists [[3.5, 4.5], [5.5, 6.5]]
    , TestLabel "median filter test" $
        TestCase $ do
          medianFilter 3
            (fromLists
              [ [10.0, 10.0, 10.0]
              , [10.0, 255.0, 10.0]
              , [10.0, 10.0, 10.0]
              ])
            @?= fromLists [[10.0]]
    , "one-pixel bilateral filter is the identity" ~:
        bilateralFilter 1 1.0 10.0 (fromLists [[1.0,2.0],[3.0,4.0]]) ~?=
        fromLists [[1.0,2.0],[3.0,4.0]]
    , TestLabel "bilateral filter preserves a sharp edge better than Gaussian filtering" $
        TestCase $
          let edge = fromLists
                [ [0.0, 0.0, 0.0, 100.0, 100.0]
                , [0.0, 0.0, 0.0, 100.0, 100.0]
                , [0.0, 0.0, 0.0, 100.0, 100.0]
                , [0.0, 0.0, 0.0, 100.0, 100.0]
                , [0.0, 0.0, 0.0, 100.0, 100.0]
                ]
              bilateralEdgeValue = toLists (bilateralFilter 3 1.0 5.0 edge) !! 2 !! 2
              gaussianEdgeValue = toLists (gaussianFilter 3 1.0 edge) !! 2 !! 2
           in assertBool "the bilateral result should stay closer to the zero-valued side of the edge"
                (bilateralEdgeValue < 1.0 && bilateralEdgeValue < gaussianEdgeValue)
    , "one-pixel non-local means filter is the identity" ~:
        nonLocalMeansFilter 1 1 10.0 (fromLists [[1.0,2.0],[3.0,4.0]]) ~?=
        fromLists [[1.0,2.0],[3.0,4.0]]
    , TestLabel "non-local means preserves constant intensity" $
        TestCase $
          assertBool "matching patches in a constant image should preserve its intensity"
            ( all (\value -> abs (value - 50.0) < 1.0e-12)
                (concat (toLists (nonLocalMeansFilter 3 3 10.0
                  (fromLists (replicate 4 (replicate 4 50.0))))))
            )
  ]

main :: IO ()
main = do
  result <- runTestTT tests
  if errors result + failures result == 0
    then pure ()
    else fail "Tests failed"
