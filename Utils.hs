module Utils where
import Numeric.LinearAlgebra

--  update theta by taking num_iters gradient steps with learning rate alpha
gradientDescent :: (MyMatrix -> MyMatrix) -> MyMatrix -> Double -> Int -> MyMatrix
gradientDescent gradFunc initialTheta alpha num_iters = iterate initialTheta num_iters
            where
                iterate theta 0 = theta
                iterate theta iters = iterate (updateTheta theta) (iters-1)
                updateTheta t = t - (scalar alpha) * (gradFunc t)

class Predicter a where
    predict :: a -> MyMatrix -> MyMatrix
    fit :: MyMatrix -> MyMatrix -> a

type Sample = (Double, [Double])
type Samples = [Sample]
type MyMatrix = Matrix Double
type MyVector = Vector Double

sumRows m = fromColumns $ [ sum $ toColumns m ]
sumColumns m = fromRows $ [ sum $ toRows m ]

listToMatrix l = trans $ asColumn $ fromList l 
matrixTolist m = toList $ head $ toColumns m
buildSample x y = zip y' x'
  where
    x' = map toList (toRows x)
    y' = map (head.toList) (toRows y)

dist :: [Double]->[Double]->Double
dist p q = sqrt $   sum $ zipWith (\a b-> (a-b)*(a-b)) p q