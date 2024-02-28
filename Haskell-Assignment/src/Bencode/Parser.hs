module Bencode.Parser where

import Bencode.Value
import qualified Data.List as L
import Parsec (Parser, andThen, orElse, pMap, pThen)
import qualified Parsec as P
import Result

-- | Parse a bencode value
--
-- >>> P.runParser value "i10e"
-- Success (BencodeInt 10, "")
--
-- >>> P.runParser value "3:abc"
-- Success (BencodeString "abc", "")
--
-- >>> P.runParser value "l3:abc4:abcde"
-- Success (BencodeList [BencodeString "abc",BencodeString "abcd"], "")
--
-- >>> P.runParser value "d3:abci10ee"
-- Success (BencodeDict [("abc",BencodeInt 10)], "")
value :: Parser BencodeValue
value =
  (pMap BencodeString string)
    `orElse` (pMap BencodeInt int)
    `orElse` (pMap BencodeList list)
    `orElse` (pMap BencodeDict dict)

-- | Parse a bencode integer
--

-- >>> P.runParser int "i10e"
-- Success (10, "")
int :: Parser Int
int = P.parser inner
  where
    inner input = do
      (_, rest1) <- P.runParser (P.char 'i') input
      (nr, rest2) <- P.runParser P.number rest1
      (_, rest3) <- P.runParser (P.char 'e') rest2
      return (nr, rest3)
-- | Parse a bencode string
--
-- >>> P.runParser string "3:abc"
-- Success ("abc", "")
string :: Parser String
string = P.parser inner
  where
    inner input = do
      (nr, rest1) <- P.runParser P.number input
      (_, rest2) <- P.runParser (P.char ':') rest1
      (str, rest3) <- P.runParser (P.take nr) rest2
      return (str, rest3)

-- | Parse a bencode list
--
-- >>> P.runParser list "li1ei2ee"
-- Success ([BencodeInt 1,BencodeInt 2], "")
--

-- >>> P.runParser list "l1:a1:be"
-- Success ([BencodeString "a",BencodeString "b"], "")
list :: Parser [BencodeValue]
list = P.parser inner
  where
    inner input = do
      (_, rest1) <- P.runParser (P.char 'l') input
      (list, rest2) <- P.runParser (P.many value) rest1
      (_, rest3) <- P.runParser (P.char 'e') rest2
      return (list, rest3)
-- | Parse a bencode dict
--
-- >>> P.runParser dict "d1:ai1e1:bi2ee"
-- Success ([(BencodeString "a", BencodeInt 1),(BencodeString "b",BencodeInt 2)], "")
dict :: Parser [BencodeKW]
dict = P.parser inner
  where
    inner input = do
      (_, rest1) <- P.runParser (P.char 'd') input
      (list, rest2) <- P.runParser (P.many (P.andThen string value)) rest1
      (_, rest3) <- P.runParser (P.char 'e') rest2
      return (list, rest3)


-- | Convenience wrapper for `value`
--
-- >>> parse "i10e"
-- Success (BencodeInt 10)
parse :: String -> Result P.ParseError BencodeValue
parse input = fst <$> P.runParser value input