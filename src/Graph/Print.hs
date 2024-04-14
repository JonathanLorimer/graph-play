module Graph.Print where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Foldable
import Graph.Utils (tshow)
import Algebra.Graph 
import Data.List (intersperse, sort)

surround :: Char -> Char -> TB.Builder -> TB.Builder
surround c1 c2 b = TB.singleton c1 <> b <> TB.singleton c2

printEdgeList :: forall a . Show a => [(a,a)] -> Text
printEdgeList es = TL.toStrict . TB.toLazyText . surround '{' '}' 
  $ fold (intersperse (TB.singleton ',') (showEdge <$> es))
  where
    showEdge :: (a,a) -> TB.Builder
    showEdge (v1, v2) = surround '{' '}' 
      $ fold 
        [ TB.fromText $ tshow v1 
        , TB.singleton ','
        , TB.fromText $ tshow v2
        ]
    
printGraph :: (Show a, Ord a) => Graph a -> Text
printGraph = printEdgeList . sort . edgeList

printAdjList' :: forall a . Show a => [(a, [a])] -> Text
printAdjList' = 
    TL.toStrict 
  . TB.toLazyText
  . fold 
  . intersperse (TB.singleton '\n') 
  . fmap showAdj
  where
    showAdj :: (a, [a]) -> TB.Builder
    showAdj (v, vs) = fold . intersperse (TB.singleton ' ') $
      [ TB.fromText $ tshow v
      , TB.fromText "->"
      ] <> (TB.fromText . tshow <$> vs)
    
printAdjList :: (Show a, Ord a) => Graph a -> Text
printAdjList = printAdjList' . adjacencyList
