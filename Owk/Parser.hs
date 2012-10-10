{-# LANGUAGE OverloadedStrings #-}
module Owk.Parser where

import Control.Applicative hiding ((<|>), many)
import Control.Monad.Identity (Identity)
import Data.Attoparsec.Number (Number(..))
import Data.Char (isHexDigit)
import Numeric (readHex)
import Text.Parsec as Parsec
import Text.Parsec.Expr
import Text.Parsec.Text

import Data.Word (Word8)

import qualified Data.Text as T
import qualified Data.Text.IO as TI

import Owk.AST

parseOwk :: SourceName -> T.Text -> Either String Program
parseOwk n t =
    case parse program n t of
        Left e  -> Left $ show e
        Right p -> Right p

parseOwkFile :: FilePath -> IO (Either String Program)
parseOwkFile fp = do
    s <- TI.readFile fp
    return $ parseOwk fp s

program :: Parser Program
program = Program <$> (whiteSpace *> expression `sepEndBy` semicolon <* eof <?> "owk program")

expression :: Parser Expression
expression = buildExpressionParser table term

symbol :: String -> Parser String
symbol name = lexeme (string name)

parens, braces, brackets :: Parser a -> Parser a
parens p   = between (symbol "(") (symbol ")") p
braces p   = between (symbol "{") (symbol "}") p
brackets p = between (symbol "[") (symbol "]") p

comma, colon, semicolon, dot :: Parser String
comma      = symbol ","
colon      = symbol ":"
--semicolon :: Parser T.Text
semicolon = symbol ";"
dot        = symbol "."

lexeme :: Parser a -> Parser a
lexeme p = p <* whiteSpace

whiteSpace :: Parser ()
whiteSpace = skipMany (simpleSpaces <|> oneLineComment <?> "white spaces")
  where
    simpleSpaces = oneOf " \t\r\n" >> return ()
    oneLineComment = do
        try (string "#")
        skipMany (satisfy (/= '\n'))
        return ()


table :: OperatorTable T.Text () Identity Expression
table = [ [unary "-" "__neg__", unary "+" "__num__"]
        , [binary "*" "__mul__" AssocLeft, binary "/" "__div__" AssocLeft, binary "%" "__mod__" AssocLeft]
        , [binary "+" "__add__" AssocLeft, binary "-" "__sub__" AssocLeft]
        , [binary ">" "__gt__" AssocLeft, binary "<" "__lt__" AssocLeft, binary ">=" "__ge__" AssocLeft, binary "<=" "__le__" AssocLeft]
        , [binary "==" "__eq__" AssocNone, binary "!=" "__nq__" AssocNone
          , binary "=~" "__match__" AssocNone, binary "!~" "__nmatch__" AssocNone]
        , [unary "!" "__not__"]
        , [binary "&&" "__and__" AssocNone, binary "||" "__or__" AssocNone]
        , [binary ":" "__app__" AssocRight, binary "?" "__if__" AssocLeft]
        , [binary ":=" "__wref__" AssocNone]
        ]

unary :: String -> T.Text -> Operator T.Text () Identity Expression
unary  name fun       = Prefix (do{ reservedOp name; return $ \x -> FuncCall (Variable fun) [x] })

binary :: String -> T.Text -> Assoc -> Operator T.Text () Identity Expression
binary  name fun assoc = Infix (do{ reservedOp name; return $ \x y -> FuncCall (Variable fun) [x, y] }) assoc

reservedOp :: String -> Parser ()
reservedOp name =
    lexeme $ try $ do
        string name
        notFollowedBy opLetter <?> ("end of " ++ show name)
  where
    opLetter = oneOf "!%&*+-/:<=>?|~"

term :: Parser Expression
term = flip label "term" $ lexeme $ do
    e <- term'
    params <- option [] (try $ lexeme term' `sepBy1` comma)
    case params of
        [] -> return e
        _  -> return $ FuncCall e params

term' :: Parser Expression
term' = flip label "expressions without function call" $ do
    e <- tryAll [ parens expression, unit, function, define, variable, string_, number, list, dict ]
    sub <- option [] $ try subscripts
    whiteSpace
    case sub of
        [] -> return e
        sub' -> return $ FuncCall (Variable "__get__") $ e : (map String sub')

unit :: Parser Expression
unit = symbol "(" >> symbol ")" >> return Unit

-- [ param, param -> ] { expression [; expression ...] }
function :: Parser Expression
function = do
    params <- funcParams
    es <- braces $ expression `sepEndBy` semicolon
    return $ Function params es
  <?> "function"

funcParams :: Parser [T.Text]
funcParams = option [] (try varName `sepBy1` comma <* symbol "->")
    <?> "function parameters"

define :: Parser Expression
define = Define <$> varName <* symbol "=" <*> expression
    <?> "define"

variable :: Parser Expression
variable = Variable <$> varName
    <?> "variable"

string_ :: Parser Expression
string_ = String . T.pack <$> p_string
    <?> "string"

number :: Parser Expression
number = Number <$> do
    d <- many1 digit
    mdot <- optionMaybe $ char '.'
    case mdot of
        Just _ -> do
            n <- many1 digit
            return $ D $ read $ d ++ "." ++ n
        Nothing -> return $ I $ read d
  <?> "number"

list :: Parser Expression
list = flip label "list" $ List <$> (brackets $ expression `sepEndBy` comma)

dict :: Parser Expression
dict = flip label "dict" $ Dict <$> (braces $ kv `sepEndBy` comma)
  where
    kv = do
        k <- varName
        symbol "=>"
        v <- expression
        return (k, v)

varName :: Parser T.Text
varName = lexeme $ do
    h <- oneOf $ "$_" ++ ['a'..'z'] ++ ['A'..'Z']
    t <- many $ oneOf "$_" <|> alphaNum
    return $ T.pack $ h : t

subscripts :: Parser [T.Text]
subscripts = do
    char '.'
    (T.pack <$> many (char '_' <|> alphaNum) <|> (T.pack <$> p_string)) `sepBy1` char '.'

tryAll :: [Parser a] -> Parser a
tryAll ps = foldl1 (<|>) $ map try ps

toWord :: Char -> Word8
toWord = toEnum . fromEnum

toChar :: Word8 -> Char
toChar = toEnum . fromEnum


-- XXX: copied from json-0.7:Text.JSON.Parsec
tok              :: Parser a -> Parser a
tok p             = p <* spaces

p_string         :: Parser String
p_string          = between (tok (char '"')) (tok (char '"')) (many p_char)
  where p_char    =  (char '\\' >> p_esc)
                 <|> (satisfy (\x -> x /= '"' && x /= '\\'))

        p_esc     =  ('"'   <$ char '"')
                 <|> ('\\'  <$ char '\\')
                 <|> ('/'   <$ char '/')
                 <|> ('\b'  <$ char 'b')
                 <|> ('\f'  <$ char 'f')
                 <|> ('\n'  <$ char 'n')
                 <|> ('\r'  <$ char 'r')
                 <|> ('\t'  <$ char 't')
                 <|> (char 'u' *> p_uni)
                 <?> "escape character"

        p_uni     = check =<< count 4 (satisfy isHexDigit)
          where check x | code <= max_char  = pure (toEnum code)
                        | otherwise         = empty
                  where code      = fst $ head $ readHex x
                        max_char  = fromEnum (maxBound :: Char)
