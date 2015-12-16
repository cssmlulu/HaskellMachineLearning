module Main where
import LinearRegression
import Numeric.LinearAlgebra
import Test.HUnit

x = fromLists [[1,4],[2,5],[5,1],[4,2]]
y = fromLists [[19],[26],[19],[20]]

thetaLRNE = linearRegressionNETrain x y
yLRNE = x <> thetaLRNE
testlinearRegressionNETrain = TestCase (assertBool "testlinearRegressionNETrain failed" ((abs $ sumElements (y-yLRNE)) < 0.1))

theta0 = fromLists [[2],[5]]
alpha = 0.01
num_iters = 1000::Int
lambda = 0.1
thetaLRGD = linearRegressionGDTrain x y theta0 alpha num_iters lambda
yLRGD = x <> thetaLRGD
testlinearRegressionGDTrain = TestCase (assertBool "testlinearRegressionGDTrain failed" ((abs $ sumElements (y-yLRGD)) < 0.1))

tests = TestList [TestLabel "testlinearRegressionNETrain" testlinearRegressionNETrain,
                  TestLabel "testlinearRegressionGDTrain" testlinearRegressionGDTrain]

main = do
    runTestTT tests