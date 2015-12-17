module LinearRegression(
    linearRegressionNETrain,
    linearRegressionGDTrain
    ) where
import Numeric.LinearAlgebra
import Utils

data LinearRegression = LinearR MyMatrix deriving Show

instance Predicter LinearRegression where
  predict (LinearR theta) x = x <> theta
  fit x y = LinearR $ linearRegressionNETrain x y


-- input xtrain and ytrain to learn the paramter theta
-- Normal Equation method: theta = (X^TX)^-1X^Ty 
linearRegressionNETrain :: MyMatrix -> MyMatrix -> MyMatrix
linearRegressionNETrain x y = theta
    where
        theta = xhat <> y
        xhat = (inv (xt <> x)) <> xt
        xt = trans x    



-- Compute regularized cost using theta as the parameter for linear regression
computeCost :: MyMatrix -> MyMatrix -> MyMatrix -> Double -> Double
computeCost x y theta lambda = ( 1/(2*m) ) * sumElements (((x <> theta) - y) ^2 ) + (lambda/(2*m)) * sumElements (tempTheta ^ 2)
    where
      m = fromIntegral $ rows y
      tempTheta = accum theta (*) [((0,0),0)]

-- Compute gredient using theta as the parameter for linear regression
computeGradient :: MyMatrix -> MyMatrix -> MyMatrix -> Double -> MyMatrix
computeGradient x y theta lambda = ctrans $ (scalar (1/m)) * ( sumColumns  ((x <> theta - y) * x) ) + (scalar (lambda/m)) * (ctrans tempTheta)
    where
      m = fromIntegral $ rows y
      tempTheta = accum theta (*) [((0,0),0)]

-- input xtrain and ytrain to learn the paramter theta
-- using gradient descent
linearRegressionGDTrain :: MyMatrix -> MyMatrix -> MyMatrix -> Double -> Int -> Double -> MyMatrix
linearRegressionGDTrain x y theta0 alpha num_iters lambda = gradientDescent gradFunc theta0 alpha num_iters
    where
      gradFunc t = computeGradient x y t lambda

