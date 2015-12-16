module GradientDescent where
import MyMatrix
import Numeric.LinearAlgebra
--  update theta by taking num_iters gradient steps with learning rate alpha
gradientDescent :: (MyMatrix -> MyMatrix) -> MyMatrix -> Double -> Int -> MyMatrix
gradientDescent gradFunc initialTheta alpha num_iters = iterate initialTheta num_iters
            where
                iterate theta 0 = theta
                iterate theta iters = iterate (updateTheta theta) (iters-1)
                updateTheta t = t - (scalar alpha) * (gradFunc t)