module MyMatrix (
    MyMatrix,
    sumRows,
    sumColumns
    ) where


import Numeric.LinearAlgebra
type MyMatrix = Matrix Double

sumRows m = fromColumns $ [ sum $ toColumns m ]
sumColumns m = fromRows $ [ sum $ toRows m ]