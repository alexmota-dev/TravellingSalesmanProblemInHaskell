import System.Random (randomRIO)
import Data.List (delete, minimumBy)
import Data.Ord (comparing)
import Control.Monad (replicateM)

data Point = Point
  { pointId :: String
  , x       :: Int
  , y       :: Int
  } deriving (Show, Eq)

data Edge = Edge
  { pointA     :: Point
  , pointB     :: Point
  , edgeLength :: Float
  } deriving (Show)

data CartesianPlane = CartesianPlane
  { points :: [Point]
  , edges  :: [Edge]
  } deriving (Show)

main :: IO ()
main = do
  -- Definindo valores hardcoded
  let isMock = False  -- ou False para usar o modo REAL
  let vertexNumber = 5  -- número de vértices

  -- Gerando o plano cartesiano com base nos valores hardcoded
  cartesianPlane <- if isMock
    then generateCartesianPlaneMock vertexNumber
    else generateCartesianPlane vertexNumber

  print cartesianPlane
  let path = calculateWithNearestNeighbor cartesianPlane
  showResult cartesianPlane path

showResult :: CartesianPlane -> [Point] -> IO ()
showResult cartesianPlane path = do
  let totalDistance = calculateTotalDistance path (edges cartesianPlane)
  putStrLn "\n------------------------------------"
  putStrLn $ "Caminho: " ++ formatPath path
  putStrLn $ "Distância total: " ++ show totalDistance
  putStrLn "\n------------------------------------"

generateCartesianPlane :: Int -> IO CartesianPlane
generateCartesianPlane vertexNumber = do
  points <- generatePoints vertexNumber
  edges <- generateEdges points
  return $ CartesianPlane points edges

generateCartesianPlaneMock :: Int -> IO CartesianPlane
generateCartesianPlaneMock vertexNumber = do
  points <- generatePointsMock vertexNumber
  edges <- generateEdgesMock points
  return $ CartesianPlane points edges

generatePoints :: Int -> IO [Point]
generatePoints vertexNumber = replicateM vertexNumber generatePointNotRepeat

generatePointsMock :: Int -> IO [Point]
generatePointsMock vertexNumber = return [Point (show i) i i | i <- [0..vertexNumber-1]]

generateEdges :: [Point] -> IO [Edge]
generateEdges points = do
  let pairs = [(p1, p2) | (p1, i) <- zip points [0..], (p2, j) <- zip (drop (i+1) points) [i+1..]]
  edges <- mapM (\(p1, p2) -> return (createEdge p1 p2)) pairs
  return edges
  where
    createEdge p1 p2 = Edge p1 p2 (calculateDistance p1 p2)

generateEdgesMock :: [Point] -> IO [Edge]
generateEdgesMock points = return [Edge p1 p2 (calculateDistance p1 p2) | (p1, i) <- zip points [0..], (p2, j) <- zip (drop (i+1) points) [i+1..]]

generatePointNotRepeat :: IO Point
generatePointNotRepeat = do
  x <- randomRIO (0, 100)
  y <- randomRIO (0, 100)
  cityName <- generateNewCityName
  return $ Point cityName x y

generateNewCityName :: IO String
generateNewCityName = do
  city <- generateRandomString 3
  -- Aqui você pode adicionar lógica para verificar se o nome da cidade já existe
  return city

-- Gera uma string aleatória de comprimento n
generateRandomString :: Int -> IO String
generateRandomString n = replicateM n randomChar

-- Gera um caractere aleatório entre 'a' e 'z'
randomChar :: IO Char
randomChar = do
  index <- randomRIO (0, 25)
  return $ toEnum (97 + index)

calculateDistance :: Point -> Point -> Float
calculateDistance p1 p2 = sqrt $ fromIntegral ((x p2 - x p1)^2 + (y p2 - y p1)^2)

findNearestEdge :: Point -> [Point] -> [Edge] -> Maybe Edge
findNearestEdge from visited edges = 
    case filter (isUnvisitedEdge from visited) edges of
        [] -> Nothing
        filteredEdges -> Just (minimumBy (comparing edgeLength) filteredEdges)

isUnvisitedEdge :: Point -> [Point] -> Edge -> Bool
isUnvisitedEdge from visited edge = (pointA edge == from || pointB edge == from) && (not (pointA edge `elem` visited) || not (pointB edge `elem` visited))

nearestNeighbor :: Point -> CartesianPlane -> [Point]
nearestNeighbor start cartesianPlane = go [start] [start]
  where
    go path visited
      | length visited == length (points cartesianPlane) = reverse (start : path)
      | otherwise = case findNearestEdge (last visited) visited (edges cartesianPlane) of
          Just nearestEdge -> let nextPoint = if pointA nearestEdge == last visited then pointB nearestEdge else pointA nearestEdge
                               in go (nextPoint : path) (nextPoint : visited)
          Nothing -> []

calculateWithNearestNeighbor :: CartesianPlane -> [Point]
calculateWithNearestNeighbor cartesianPlane = nearestNeighbor (head (points cartesianPlane)) cartesianPlane

formatPath :: [Point] -> String
formatPath [] = ""
formatPath path = unwords $ init formattedPath ++ [last formattedPath]
  where
    formattedPath = map (<> " -> ") (map pointId path) ++ [pointId (head path)]

calculateTotalDistance :: [Point] -> [Edge] -> Float
calculateTotalDistance [] _ = 0
calculateTotalDistance [_] _ = 0
calculateTotalDistance (p1:p2:ps) edges = distance + calculateTotalDistance (p2:ps) edges
  where
    distance = edgeLength $ head $ filter (\e -> (pointA e == p1 && pointB e == p2) || (pointA e == p2 && pointB e == p1)) edges
