module MyMatrix (
    module MyMatrix
    ) where


import Numeric.LinearAlgebra
type MyMatrix = Matrix Double
type MyVector = Vector Double

sumRows m = fromColumns $ [ sum $ toColumns m ]
sumColumns m = fromRows $ [ sum $ toRows m ]