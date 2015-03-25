module Bencode
    (
        Bencode(BInt, BString, BList, BDict),
        bParse
    ) where

-- library that supports parsing bencode
import Text.ParserCombinators.Parsec
import Data.Map

-- TO DO: figure out why it won't work with latin1 encodings.

data Bencode = BInt Integer
             | BString String
             | BList [Bencode]
             | BDict (Map String Bencode)
             deriving (Show, Eq)

bParse :: String -> Maybe Bencode
bParse str = case parse bParseAny "" str of
                  Left _ -> Nothing
                  Right val -> Just val

bParseAny :: Parser Bencode
bParseAny = bParserInt <|> bParserStr <|> bParserDict <|> bParserList


number :: Parser Integer
number = do n_str <- many1 digit
            let n = read n_str
            return n

bParserStr :: Parser Bencode
bParserStr = do n <- number
                char ':'
                str <- count (fromIntegral n) anyChar
                return (BString str)

bParserInt :: Parser Bencode
bParserInt = do char 'i'
                n <- number
                char 'e'
                return (BInt n)

bParserList :: Parser Bencode
bParserList = do char 'l'
                 xs <- many bParseAny
                 char 'e'
                 return (BList xs)

bParserDict :: Parser Bencode
bParserDict = do char 'd'
                 xs <- many bParserDictContents
                 char 'e'
                 return (BDict (fromList xs))

bParserDictContents :: Parser (String, Bencode)
bParserDictContents = do (BString key) <- bParserStr
                         val <- bParseAny
                         return (key, val)

--main :: IO ()
--main =
    --getArgs >>= \args ->
    --readFile (head args) >>= \contents ->
    --putStrLn (show (fromJust (bParse contents)))
