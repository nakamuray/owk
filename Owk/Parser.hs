{-# LANGUAGE OverloadedStrings #-}
module Owk.Parser where

import Control.Applicative hiding (many)
import Data.Attoparsec.Number (Number(..))
import Data.Char (isHexDigit)
import Numeric (readHex)
import Text.Parser.Expression
import Text.Trifecta hiding (symbol, whiteSpace, parens, braces, comma, brackets)
import Text.Trifecta.Delta (Delta(Directed))

import Data.Word (Word8)

import qualified Data.Text as T
import qualified Data.ByteString.UTF8 as BU

import Owk.AST

type SourceName = String

parseOwk :: SourceName -> T.Text -> Either String Program
parseOwk n t =
    case parseString program (Directed (BU.fromString n) 0 0 0 0) $ T.unpack t of
        Failure e  -> Left $ show e
        Success p -> Right p

parseOwkFile :: FilePath -> IO (Either String Program)
parseOwkFile fp = do
    r <- parseFromFileEx program fp
    case r of
        Failure e  -> return $ Left $ show e
        Success p -> return $ Right p

program :: Parser Program
program = Program <$> block <* eof

block, block1 :: Parser [Expression]
block = blankLines *> whiteSpace *> expression `sepEndBy` ((semicolon <|> symbol "\n") <* blankLines <* whiteSpace)
block1 = blankLines *> whiteSpace *> expression `sepEndBy1` ((semicolon <|> symbol "\n") <* blankLines <* whiteSpace)

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
semicolon = symbol ";"
dot        = symbol "."

lexeme :: Parser a -> Parser a
lexeme p = p <* whiteSpace

lexeme' :: Parser a -> Parser a
lexeme' p = p <* whiteSpace'

whiteSpace :: Parser ()
whiteSpace = skipMany (simpleSpaces <|> newlineFollowingBackspace <|> oneLineComment <?> "white spaces")
  where
    simpleSpaces = oneOf " \t\r" >> return ()
    newlineFollowingBackspace = string "\\\n" >> return ()
    oneLineComment = do
        char '#'
        skipMany (satisfy (/= '\n'))
        return ()

whiteSpace' :: Parser ()
whiteSpace' = blankLines >> whiteSpace

blankLines :: Parser ()
blankLines = do
    many $ try (whiteSpace >> newline)
    return ()


table :: OperatorTable Parser Expression
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

unary :: String -> T.Text -> Operator Parser Expression
unary  name fun       = Prefix (do{ reservedOp name; return $ \x -> FuncCall (Variable fun) x } <?> "operator")

binary :: String -> T.Text -> Assoc -> Operator Parser Expression
binary  name fun assoc = Infix (do{ reservedOp2 name; return $ \x y -> FuncCall (FuncCall (Variable fun) x) y } <?> "binary operator") assoc

reservedOp :: String -> Parser ()
reservedOp name = try $ lexeme $ reservedOp' name

reservedOp2 :: String -> Parser ()
reservedOp2 name = try $ lexeme' $ reservedOp' name

reservedOp' :: String -> Parser ()
reservedOp' name = do
    string name
    notFollowedBy opLetter <?> ("end of " ++ show name)
  where
    opLetter = oneOf "!%&*+-/:<=>?|~"


term :: Parser Expression
term = foldl1 FuncCall <$> many1 term'
     <?> "expression"

term' :: Parser Expression
term' = lexeme $ do
    e <- try function <|> try define <|> parensesEnclosed <|> list <|> dict <|> variable <|> string_ <|> number
    sub <- option [] subscripts
    whiteSpace
    case sub of
        [] -> return e
        sub' -> return $ FuncCall (Variable "__get__") $ List $ e : (map String sub')
  <?> "expression"

parensesEnclosed :: Parser Expression
parensesEnclosed = do
    t <- parens tupleBody
    case t of
        -- single is just a value
        Tuple [o] -> return o
        _         -> return t

tupleBody :: Parser Expression
tupleBody = Tuple <$> makeListParser expression

-- [ pattern -> ] { expression [; expression ...] } [ | [pattern -> ] { expression ...]
function :: Parser Expression
function = Function <$> ((try function' <|> function'') `sepBy1` funcSep)
  where
    funcSep = try $ do
        whiteSpace'
        lexeme' $ reservedOp' "|"

function' :: Parser (Pattern, [Expression])
function' = do
    pat <- funcPattern
    es <- braces block1
    return $ (pat, es)
  <?> "function"

funcPattern :: Parser Pattern
funcPattern = option (PVariable "_") (try pattern <* symbol "->")
    <?> "function parameters"

function'' :: Parser (Pattern, [Expression])
function'' = do
    pat <- try pattern
    symbol "->"
    e <- expression
    return $ (pat, [e])
  <?> "function"

define :: Parser Expression
define = Define <$> pattern <* symbol "=" <*> expression
    <?> "define"

variable :: Parser Expression
variable = Variable <$> varName
    <?> "variable"

string_ :: Parser Expression
string_ = String <$> string_'

string_' :: Parser T.Text
string_' = T.pack <$> p_string
    <?> "string"

number :: Parser Expression
number = Number <$> number'

number' :: Parser Number
number' = do
    d <- many1 digit
    mdot <- optional $ char '.'
    case mdot of
        Just _ -> do
            n <- many1 digit
            return $ D $ read $ d ++ "." ++ n
        Nothing -> return $ I $ read d
  <?> "number"

tuple :: Parser Expression
tuple = Tuple <$> parens (makeListParser expression)

list :: Parser Expression
list = List <$> brackets (makeListParser expression)

makeListParser :: Parser a -> Parser [a]
makeListParser p = (whiteSpace' *> lexeme' p `sepEndBy` lexeme' comma)
    <?> "list"

dict :: Parser Expression
dict = Dict <$> makeDictParser expression

makeDictParser :: Parser a -> Parser [(T.Text, a)]
makeDictParser p = (braces $ kv `sepEndBy` lexeme' comma)
    <?> "dict"
  where
    kv = do
        whiteSpace'
        k <- varName
        whiteSpace'
        symbol "=>"
        whiteSpace'
        v <- p
        whiteSpace'
        return (k, v)

pattern :: Parser Pattern
pattern = lexeme (pVariable <|> pString <|> pNumber <|> pTuple <|> pList <|> pDict)

pVariable :: Parser Pattern
pVariable = PVariable <$> varName

pString :: Parser Pattern
pString = PString <$> string_'

pNumber :: Parser Pattern
pNumber = PNumber <$> number'

pTuple :: Parser Pattern
pTuple = do
    t <- parens (makeListParser pattern)
    case t of
        -- single is just a value
        [o] -> return o
        _   -> return (PTuple t)

pList :: Parser Pattern
pList = PList <$> brackets (makeListParser pattern)

pDict :: Parser Pattern
pDict = PDict <$> makeDictParser pattern

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
--tok              :: Parser a -> Parser a
--tok p             = p <* spaces

p_string         :: Parser String
--p_string          = between (tok (char '"')) (tok (char '"')) (many p_char)
p_string          = between (char '"') (char '"') (many p_char)
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

many1 :: Parser a -> Parser [a]
many1 p = do
    r <- p
    rs <- many p
    return $ r : rs
