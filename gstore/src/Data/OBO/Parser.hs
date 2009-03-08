module Data.OBO.Parser (parseOBO) where

{- http://www.geneontology.org/GO.format.obo-1_2.shtml -}

import Data.Char
import Data.OBO.Document
import Control.Monad
import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

-- Make parsec Applicative and Alternative.

instance Applicative (GenParser s a) where
  pure  = return
  (<*>) = ap

instance Alternative (GenParser s a) where
  empty = mzero
  (<|>) = mplus

-- OBO format parser.

parseOBO :: String -> Either ParseError Document
parseOBO = parse pDocument ""

pDocument :: GenParser Char st Document
pDocument = Document
  <$> pMap <* some pEmptyLine
  <*> many (pStanza <* some pEmptyLine)
  
pStanza :: GenParser Char st Stanza
pStanza = Stanza
  <$> pStanzaName <* pEmptyLine
  <*> pMap

pEmptyLine :: GenParser Char st ()
pEmptyLine = () <$ many (oneOf " \t") <* char '\n'

pStanzaName :: GenParser Char st String
pStanzaName = char '[' *> many (noneOf "\n]") <* char ']'

pMap :: GenParser Char st TagValues
pMap = many (pTagValue <* pEmptyLine)

pTagValue :: GenParser Char st (String, String)
pTagValue = (,) <$> pTag <*> pValue

pTag :: GenParser Char st String
pTag = many (noneOf ":\n") <* char ':' <* many (oneOf " \t")

pValue :: GenParser Char st String
pValue = concat <$> many (pQuoted <|> pString)

pString :: GenParser Char st String
pString = some (noneOf "\n")

pQuoted :: GenParser Char st String
pQuoted = char '"' *> many (noneOf "\\\"" <|> pEscape) <* char '"'

pEscape :: GenParser Char st Char
pEscape = char '\\' *> choice (map (\(a, b) -> a <$ char b) escapees)

escapees :: [(Char, Char)]
escapees = [
    ('\n', 'n')
  , (' ',  'W')
  , ('\t', 't')
  , (':',  ':')
  , (',',  ',')
  , ('"',  '"')
  , ('\\', '\\')
  , ('(',  '(')
  , (')',  ')')
  , ('[',  '[')
  , (']',  ']')
  , ('{',  '{')
  , ('}',  '}')
  , ('\n', '\n')
  ]

