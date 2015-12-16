-- Principal component analysis
module PCA where
import Numeric.LinearAlgebra
import MyMatrix

data PCAData = PCAData {means:: MyVector,
                        reduceMatrix :: MyMatrix} deriving Show

-- compute mean value of each column
mean :: MyMatrix -> MyVector
mean a = constant (recip . fromIntegral . rows $ a) (rows a) <> a

-- compute covariance matrix
cov :: MyMatrix -> MyMatrix
cov x = (trans xc <> xc) / fromIntegral (rows x - 1)
    where xc = x - asRow (mean x)


-- execute pca on a matrix. reduce to n dims
pca :: Int -> MyMatrix -> PCAData
pca n rawMatrix = PCAData{means=m,reduceMatrix=vp}
  where
    m = mean rawMatrix
    c = cov rawMatrix
    (evalue,evector) = eigSH' c
    vp = takeRows n (trans evector)

-- use pca result to compress a vector
encode :: PCAData -> MyVector -> MyVector
encode p x = vp <> (x - m)
  where
    vp = reduceMatrix p
    m = means p 

--reconstruct the vector
decode:: PCAData -> MyVector -> MyVector
decode p x = x <> vp + m
  where
    vp = reduceMatrix p
    m = means p 


-- norm = pnorm PNorm2
-- print $ norm (x - decode p y) / norm x --reconstruction quality