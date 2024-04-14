module Graph.G01ParsePrintSpec where

import Test.Hspec
import Algebra.Graph (Graph, edges)
import Text.Megaparsec 
import Graph.Parse
import Graph.Print
import Text.Megaparsec.Char.Lexer (decimal)
import TestUtils
import Data.Text (Text)
import GHC.Natural
import Data.Foldable
import Data.List (intersperse)
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range

vtx3Text :: Text
vtx3Text = "{{0,1},{1,2},{2,0}}"

vtx3Adj :: Text
vtx3Adj = fold . intersperse "\n" $ 
  [ "0 -> 1"
  , "1 -> 2"
  , "2 -> 0"
  ]

vtx3Graph :: Graph Natural
vtx3Graph = (0 * 1) + (1 * 2) + (2 * 0)

vtx4Text :: Text
vtx4Text = "{{0,1},{1,2},{1,3},{2,3},{3,0}}"

vtx4Adj :: Text
vtx4Adj = fold . intersperse "\n" $ 
  [ "0 -> 1"
  , "1 -> 2 3"
  , "2 -> 3"
  , "3 -> 0"
  ]

vtx4Graph :: Graph Natural
vtx4Graph = (0 * 1) + (1 * 2) + (1 * 3) + (2 * 3) + (3 * 0)

spec :: Spec
spec = do
  describe "Print Adjacency List for a Directed Graph" $ do
    it "prints {{0,1},{1,2},{2,0}}" $ do
      printAdjList vtx3Graph `shouldBe` vtx3Adj
    it "prints  {{0,1},{1,2},{1,3},{2,3},{3,0}}" $ do
      printAdjList vtx4Graph `shouldBe` vtx4Adj
  describe "parseGraph" $ do
    it "parses  {{0,1},{1,2},{2,0}}" $ do
      let parsedInput :: Maybe (Graph Natural) = parseMaybe (parseGraph decimal) vtx3Text
      input <- assertJust parsedInput 
      input `shouldBe` vtx3Graph
    it "parses  {{0,1},{1,2},{1,3},{2,3},{3,0}}" $ do
      let parsedInput :: Maybe (Graph Natural) = parseMaybe (parseGraph decimal) vtx4Text
      input <- assertJust parsedInput 
      input `shouldBe` vtx4Graph
  describe "printGraph" $ do
    it "prints  {{0,1},{1,2},{2,0}}" $ do
      printGraph vtx3Graph `shouldBe` vtx3Text
    it "prints  {{0,1},{1,2},{1,3},{2,3},{3,0}}" $ do
      printGraph vtx4Graph `shouldBe` vtx4Text
  describe "parse print properties" $ do
    it "parseGraph . printGraph == id" $ hedgehog $ do
      es <- forAll 
        $ Gen.list (linear 0 20) 
        $ liftA2 (,)
          (wordToNatural <$> Gen.word (linear 0 20))
          (wordToNatural <$> Gen.word (linear 0 20))
      let graph = edges es
      parseMaybe (parseGraph decimal) (printGraph graph) === Just graph
