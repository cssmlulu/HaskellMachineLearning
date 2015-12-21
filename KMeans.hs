import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Vector as V
import Control.Parallel.Strategies as Strategies


type Point = V.Vector Double
type Points = [Point]
type Clusters = [Cluster (V.Vector Double)]
type Center = Point
type Centers = [Point]

type Distance = V.Vector Double -> V.Vector Double -> Double
-- distance between two points
dist :: Distance
dist a b = sqrt $ V.sum (V.zipWith (\x y -> (x-y)^2) a b)

-- a cluster is a list of points
newtype Cluster a = Cluster { getPoints :: [a] } deriving (Eq, Show)


-- calculate a cluster's center
center :: Cluster (V.Vector Double) -> Center 
center cluster = V.fromList $ map (flip (/) r) l
    where (l,r) = foldl (\(x,n) y -> (zipWith (+) x y,n+1)) (replicate (length $ cluster' !! 0) 0.0, 0) cluster'
          cluster' = map V.toList $ getPoints cluster

-- input clusters and output centers
calCenters :: Clusters -> Centers
calCenters clusters = map center clusters

-- init k centers
initCenters :: Int -> Points -> Centers
initCenters k points = take k points

-- assgin each point to its nearest center
assignToCenters :: Centers -> Points -> M.Map Center (Cluster (V.Vector Double))
assignToCenters centers points =  M.map Cluster centerMapPoints
    where 
        pair = map nearest points
        centerMapPoints = M.fromListWith (++) $ map (\(k,v) -> (k,[v])) pair
        nearest p = snd $ minimum $ map (\x -> (dist p x , (x,p))) centers

-- get new centers
getCenters :: Centers -> Points -> Centers
getCenters centers points = M.keys $ assignToCenters centers points

-- get new clusters
getClusters :: Centers -> Points -> Clusters
getClusters centers points = M.elems $ assignToCenters centers points


-- one single iteration
lloydIter :: Centers -> Points -> (Centers, Clusters)
lloydIter centers points = (newCenters,newClusters)
    where
        newCenters = calCenters newClusters
        newClusters = getClusters centers points

-- kmeans main funciton
kmeans :: Int -> Points -> IO Clusters
kmeans k points =
    let
        loop n centers = do
            putStrLn ("iteration " ++ show n)
            let newPair = lloydIter centers points
            let newCenters = fst newPair
            let newClusters = snd newPair
            putStrLn ("New Centers: " ++ show newCenters)
            if newCenters == centers
                then return newClusters
                else loop (n+1) newCenters
        in
            loop 0 centers
        where
            centers = initCenters k points


---
split :: Int -> [a] -> [[a]]
split numChunks xs = chunk (length xs `quot` numChunks) xs

chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n xs = as : chunk n bs
  where (as,bs) = splitAt n xs

-- parallel version 
getClusters' :: Centers -> [Points] -> Clusters
getClusters' centers chunks = M.elems $ centerMapPoints
  where
    centerMapPoints = foldr1 (M.unionWith combine) $ (map (assignToCenters centers) chunks `using` parList rseq)
    combine c1 c2 = Cluster $ (getPoints c1) ++ (getPoints c2)

lloydIter' :: Centers -> [Points] -> (Centers, Clusters)
lloydIter' centers chunks = (newCenters,newClusters)
    where
        newCenters = calCenters newClusters
        newClusters = getClusters' centers chunks

kmeans' :: Int -> Int -> Points -> IO Clusters
kmeans' numChunks k points =
    let
        chunks = split numChunks points
        loop n centers = do
            putStrLn ("iteration " ++ show n)
            let newPair = lloydIter' centers chunks
            let newCenters = fst newPair
            let newClusters = snd newPair
            putStrLn ("New Centers: " ++ show newCenters)
            if newCenters == centers
                then return newClusters
                else loop (n+1) newCenters
        in
            loop 0 centers
        where
            centers = initCenters k points