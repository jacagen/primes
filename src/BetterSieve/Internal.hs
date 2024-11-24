module BetterSieve.Internal (updateAppendMap) where
import Data.Map (Map, insertWith)


updateAppendMap :: Ord k => k -> v -> Map k [v] -> Map k [v]
updateAppendMap k v = 
    insertWith (++) k [v]