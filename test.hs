module Main where
import LinearRegression
import PCA
import Numeric.LinearAlgebra
import Test.HUnit


testlinearRegressionNETrain = TestCase (do let x = fromLists [[1,4],[2,5],[5,1],[4,2]]
                                           let y = fromLists [[19],[26],[19],[20]]
                                           let theta = linearRegressionNETrain x y
                                           let y' = x <> theta
                                           assertBool "testlinearRegressionNETrain failed" ((abs $ sumElements (y-y')) < 0.1))


testlinearRegressionGDTrain = TestCase (do let x = fromLists [[1,4],[2,5],[5,1],[4,2]]
                                           let y = fromLists [[19],[26],[19],[20]]
                                           let theta0 = fromLists [[2],[5]]
                                           let alpha = 0.01
                                           let num_iters = 1000::Int
                                           let lambda = 0.1
                                           let theta = linearRegressionGDTrain x y theta0 alpha num_iters lambda
                                           let y'= x <> theta
                                           assertBool "testlinearRegressionGDTrain failed" ((abs $ sumElements (y-y')) < 0.1))


testPCA = TestCase (do m <- loadMatrix "mnist.txt" -- fromFile "mnist.txt" (5000,785)
                       let xs = takeColumns (cols m -1) m -- the last column is the digit type (class label)
                       let x = toRows xs !! 4  -- an arbitrary test Vec
                       let p = pca 10 xs
                       let y = encode p x
                       let y' = fromList [-357.7685710646392,-687.2472126248036,-43.89815727620881,272.3317498278406,-154.99902552369008,71.43126250099931,60.65183348252385,374.7689711144638,257.5547336560461,211.72211078077007]
                       assertEqual "testPCA failed" y y')

tests = TestList [TestLabel "testlinearRegressionNETrain" testlinearRegressionNETrain,
                  TestLabel "testlinearRegressionGDTrain" testlinearRegressionGDTrain,
                  TestLabel "testPCA" testPCA]

main = do
    runTestTT tests