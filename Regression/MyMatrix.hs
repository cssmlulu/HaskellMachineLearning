module MyMatrix (
    MyMatrix.Matrix,
    fromLists,
    toLists,
    (<>),
    inv,    -- invariant Matrix
    trans   -- transpose Matrix
    ) where

import Data.Packed.Matrix (trans) 
import Numeric.LinearAlgebra.HMatrix as HM (Matrix,fromLists,toLists, inv, (<>))

type Matrix = HM.Matrix Double