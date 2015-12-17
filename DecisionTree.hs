module DecisionTree where
import Utils

import qualified Data.List as L
import qualified Data.Map as M
import Numeric.LinearAlgebra as LA
import Control.Arrow 
import qualified Control.Monad as Monad


data DecisionTree = Node Int Double (DecisionTree) (DecisionTree) | Leaf [(Double, Double)]
    deriving Show

instance Predicter DecisionTree where
    predict tree x_test = dtPredict tree x_test
    fit x y  = buildTree $ buildSample x y

-- divide samples to two subtree by ith feature & its threshold
treeDivide :: Samples -> Int -> Double -> (Samples, Samples)
treeDivide samples featureIdx thr = L.partition ((>= thr) . (flip (!!) featureIdx) . snd) samples

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
        computeGainOfDivide i thr = uncurry (commonGain h labels) . Monad.join (***) (map fst) $ treeDivide samples i thr
        genProps = map (\(i, thr) -> (computeGainOfDivide i thr, i, thr)) pairs
        commonGain h s s1 s2 = h s - (l1/l) * h s1 - (l2/l) * h s2
            where 
                l  = fromIntegral $ length s
                l1 = fromIntegral $ length s1
                l2 = fromIntegral $ length s2

--build decision tree
buildTree :: Samples-> DecisionTree
buildTree samples
    | bestGain > 0 = let (r, l) = treeDivide samples featureIdx thr in Node featureIdx thr (buildTree l) (buildTree r)
    | otherwise = Leaf . countElem . map fst $ samples
    where 
        (bestGain, featureIdx, thr) = findBestThr [0..length (snd $ samples !! 0) - 1] samples

--use decision tree to predict
dtPredict :: DecisionTree  -> MyMatrix -> MyMatrix
dtPredict tree x = listToMatrix $ map (dtPredictSingle tree) (LA.toLists x)

--predict for single sample
dtPredictSingle :: DecisionTree  -> [Double] -> Double
dtPredictSingle (Leaf vals) _ = fst . L.maximumBy (\x y -> compare (snd x) (snd y)) $ vals
dtPredictSingle (Node featureIdx thr l r) features
    | features !! featureIdx >= thr = dtPredictSingle r features
    | otherwise               = dtPredictSingle l features