module KNN where
import Utils
import Data.List

takeKNeighbours :: Int -> [(Double,Sample)] -> [(Double,Sample)]
takeKNeighbours k distances = take k $ sortBy (\x y -> compare (fst x) (fst y)) distances


findDistances :: Samples -> [Double] -> [(Double,Sample)]            
findDistances points p = map f points
    where
        f (a,b) = (dist p b, (a,b))
        
knn :: Samples -> [Double] -> Int -> Double
knn model test k =
    let
        distances = findDistances model test
        cluster = takeKNeighbours k distances
        points = map snd cluster
        labels = map fst points
        clusterLabels = group labels
        numLabels = map (\x -> (length x,head x)) clusterLabels
        bestClass = maximumBy (\x y -> compare (fst x) (fst y)) numLabels
        result = snd bestClass
    in result