module LogisticRegression where
import Numeric.LinearAlgebra
import MyMatrix
import Utils

data LogisticRegression = LogisticR MyMatrix deriving Show

instance Predicter LogisticRegression where
  predict (LogisticR theta) x = sigmoid $ x <> theta
  fit x y = LogisticR $ logisticRegressionGDTrain x y initial_theta alpha num_iters lambda
    where
      initial_theta = fromLists [[2],[5]]
      alpha = 0.01
      num_iters = 1000::Int
      lambda = 0.1



sigmoid x = 1.0/ ( 1 + exp(-x) )

computeCost :: Matrix Double -> Matrix Double -> Matrix Double -> Double -> Double
computeCost x y theta lambda = - (1/m) * sumElements ( y * (log prediction) + (1 - y) * (log (1 - prediction)) ) + (lambda/(2*m)) * sumElements (tempTheta ^ 2)
  where
      m = fromIntegral $ rows y
      prediction = sigmoid $ x <> theta
      tempTheta = accum theta (*) [((0,0),0)]

computeGradient :: Matrix Double -> Matrix Double -> Matrix Double -> Double -> Matrix Double
computeGradient x y theta lambda = ctrans ((scalar (1/m)) * ( sumColumns  ((prediction - y) * x) ) ) + (scalar (lambda/m)) * tempTheta
  where
      m = fromIntegral $ rows y
      prediction = sigmoid $ x <> theta
      tempTheta = accum theta (*) [((0,0),0)]

logisticRegressionGDTrain :: MyMatrix -> MyMatrix -> MyMatrix -> Double -> Int -> Double -> MyMatrix
logisticRegressionGDTrain x y initial_theta alpha num_iters lambda = gradientDescent gradFunc initial_theta alpha num_iters
    where
      gradFunc t = computeGradient x y t lambda