module Graph.Parse where

import Text.Megaparsec
import Data.Void
import Data.Text (Text)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char
import Control.Monad
import Control.Applicative qualified as A
import Algebra.Graph.Class

type Parser = Parsec Void Text

sp :: Parser ()
sp = L.space space1 A.empty A.empty

comma :: Parser ()
comma = void $ L.symbol sp ","

openBracket :: Parser ()
openBracket = void $ L.symbol sp "{"

closeBracket :: Parser ()
closeBracket = void $ L.symbol sp "}"

brackets :: Parser a -> Parser a
brackets = between openBracket closeBracket

parseGraph :: Graph a => Parser (Vertex a) -> Parser a
parseGraph vp = 
  fmap overlays 
    $ brackets 
      $ (`sepBy` comma)  $ brackets $ do
        v1 <- vp
        comma
        v2 <- vp
        pure $ edge v1 v2
    
    
