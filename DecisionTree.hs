import MyMatrix

import qualified Data.List as L
import qualified Data.Map as M
import Control.Arrow
import Control.Monad

data DecisionTree = Node Int Double (DecisionTree) (DecisionTree) | Leaf [(Double, Double)]
    deriving Show

type Samples = [(Double, [Double])]

class Predicter a where
    predict :: a -> [Double] -> Double
    fit :: Samples -> a

instance Predicter DecisionTree where
    predict tree x_test = dtPredict tree x_test
    fit samples = buildTree samples

-- divide samples to two subtree by ith feature & its threshold
divide :: Samples -> Int -> Double -> (Samples, Samples)
divide samples featureIdx thr = L.partition ((>= thr) . (flip (!!) featureIdx) . snd) samples

-- Gini(p) = sum(pi*(1-pi)) = 1 - sum(pi^2)
gini :: [Double] -> Double
gini fr = 1 - (sum . map (**2) $ fr)

-- use Data.Map to count List elements
countElem :: (Ord a) => [a] -> [(a, Double)]
countElem input = M.toList $ M.fromListWith (+) [(c, 1) | c <- input]

-- compute freq 
computeFreq :: [Double] -> [Double]
computeFreq tags = map ((/s)) tags
    where s = sum $ tags


-- compute gini value for all features and choose the best one to split
findBestThr :: [Int] -> Samples -> (Double,Int,Double)
findBestThr featureIdxes samples = L.maximumBy (\(x,_,_) (y,_,_) -> compare x y) genProps
    where
        h = gini . computeFreq . map snd . countElem
        labels = map fst samples
        pairs = [(i, (snd s) !! i) | i <- featureIdxes, s <- samples]
        computeGainOfDivide i thr = uncurry (commonGain h labels) . join (***) (map fst) $ divide samples i thr
        genProps = map (\(i, thr) -> (computeGainOfDivide i thr, i, thr)) pairs
        commonGain h s s1 s2 = h s - (l1/l) * h s1 - (l2/l) * h s2
            where 
                l  = fromIntegral $ length s
                l1 = fromIntegral $ length s1
                l2 = fromIntegral $ length s2

--build decision tree
buildTree :: Samples-> DecisionTree
buildTree samples
    | bestGain > 0 = let (r, l) = divide samples featureIdx thr in Node featureIdx thr (buildTree l) (buildTree r)
    | otherwise = Leaf . countElem . map fst $ samples
    where 
        (bestGain, featureIdx, thr) = findBestThr [0..length (snd $ samples !! 0) - 1] samples

--sue decision tree to predict
dtPredict :: DecisionTree  -> [Double] -> Double
dtPredict (Leaf vals) _ = fst . L.maximumBy (\x y -> compare (snd x) (snd y)) $ vals
dtPredict (Node featureIdx thr l r) features
    | features !! featureIdx >= thr = dtPredict r features
    | otherwise               = dtPredict l features