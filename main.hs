import Control.Monad.State
import Control.Monad (replicateM)
import Data.Char (toLower)

data Program = Program [CoolClass]
data CoolClass = ClassNoInherit (Int, String) [Feature] | ClassInherit (Int, String) (Int, String) [Feature]
data Feature 
    = Method (Int, String) [Formal] (Int, String) Exp
    | Attr (Int, String) (Int, String) (Maybe Exp)
data Formal = Formal (Int, String) (Int, String)
data Exp
    = Assign Int String Exp
    | StaticDispatch Int Exp String String [Exp]
    | DynamicDispatch Int Exp Int String [Exp]
    | SelfDispatch Int String [Exp]
    | Cond Int Exp Exp Exp
    | Loop Int Exp Exp
    | Block Int [Exp]
    | Let Int String String (Maybe Exp) Exp
    | Case Int Exp [CaseBranch]
    | New Int String
    | IsVoid Int Exp
    | Plus Int Exp Exp
    | Minus Int Exp Exp
    | Times Int Exp Exp
    | Divide Int Exp Exp
    | Neg Int Exp
    | CoolLT Int Exp Exp
    | LE Int Exp Exp
    | Eq Int Exp Exp
    | Not Int Exp
    | Var Int String
    | IntLiteral Int Int
    | StrLiteral Int String
    | Boolean Int Bool
    deriving Show

data CaseBranch = CaseBranch (Int, String) (Int, String) Exp
    deriving Show

data Token
    = TOKENAT Int | TOKENCASE Int | TOKENCLASS (Int, String)
    | TOKENCOLON Int | TOKENCOMMA Int | TOKENDIVIDE Int
    | TOKENDOT Int | TOKENELSE Int | TOKENEQUALS Int
    | TOKENESAC Int | TOKENFALSE Int | TOKENFI Int
    | TOKENIDENTIFIER (Int, String) | TOKENIF Int | TOKENIN Int
    | TOKENINHERITS Int | TOKENINTEGER (Int, Int) | TOKENISVOID Int
    | TOKENLARROW Int | TOKENLBRACE Int | TOKENLE Int
    | TOKENLET Int | TOKENLOOP Int | TOKENLPAREN Int
    | TOKENLT Int | TOKENMINUS Int | TOKENNEW Int
    | TOKENNOT Int | TOKENOF Int | TOKENPLUS Int
    | TOKENPOOL Int | TOKENRARROW Int | TOKENRBRACE Int
    | TOKENRPAREN Int | TOKENSEMI Int | TOKENSTRING (Int, String)
    | TOKENTHEN Int | TOKENTILDE Int | TOKENTIMES Int
    | TOKENTRUE Int | TOKENTYPE (Int, String) | TOKENWHILE Int
    deriving Show









type ParseState a = State [String] a

nextLine :: ParseState String
nextLine = do
    st <- get
    case st of
        [] -> error "Unexpected end of file"
        (x:xs) -> put xs >> return x

nextInt :: ParseState Int
nextInt = read <$> nextLine

readCoolList :: ParseState a -> ParseState [a]
readCoolList parser = do
    n <- nextInt
    replicateM n parser

readProgram :: ParseState Program
readProgram = Program <$> readCoolList readClass

readClass :: ParseState CoolClass
readClass = do
    ln <- nextInt
    name <- nextLine
    inheritance <- nextLine
    case inheritance of
        "no_inherits" -> 
            ClassNoInherit (ln, name) <$> readCoolList readFeature
        "inherits" -> do
            pLn <- nextInt
            pName <- nextLine
            ClassInherit (ln, name) (pLn, pName) <$> readCoolList readFeature
        _ -> error $ "Unknown inheritance type: " ++ inheritance

readFeature :: ParseState Feature
readFeature = do
    kind <- nextLine
    case kind of
        "method" -> do
            ln <- nextInt
            name <- nextLine
            formals <- readCoolList readFormal
            tLn <- nextInt
            tName <- nextLine
            Method (ln, name) formals (tLn, tName) <$> readExp
        "attribute_no_init" -> do
            ln <- nextInt
            name <- nextLine
            tLn <- nextInt
            tName <- nextLine
            return $ Attr (ln, name) (tLn, tName) Nothing
        "attribute_init" -> do
            ln <- nextInt
            name <- nextLine
            tLn <- nextInt
            tName <- nextLine
            Attr (ln, name) (tLn, tName) . Just <$> readExp
        _ -> error $ "Unknown feature kind: " ++ kind

readFormal :: ParseState Formal
readFormal = do
    ln <- nextInt
    name <- nextLine
    tLn <- nextInt
    tName <- nextLine
    return $ Formal (ln, name) (tLn, tName)

readExp :: ParseState Exp
readExp = do
    ln <- nextInt
    kind <- nextLine
    case kind of
        "assign" -> do
            _ <- nextInt
            name <- nextLine
            Assign ln name <$> readExp
        "static_dispatch" -> do
            obj <- readExp
            _ <- nextInt 
            typeN <- nextLine
            _ <- nextInt
            meth <- nextLine
            StaticDispatch ln obj typeN meth <$> readCoolList readExp
        "dynamic_dispatch" -> do
            obj <- readExp
            lm <- nextInt
            meth <- nextLine
            DynamicDispatch ln obj lm meth <$> readCoolList readExp
        "self_dispatch" -> do
            _ <- nextInt
            meth <- nextLine
            SelfDispatch ln meth <$> readCoolList readExp
        "if" -> Cond ln <$> readExp <*> readExp <*> readExp
        "while" -> Loop ln <$> readExp <*> readExp
        "block" -> Block ln <$> readCoolList readExp
        "let" -> readLet ln
        "case" -> Case ln <$> readExp <*> readCoolList readCaseBranch
        "new" -> do
            _ <- nextInt
            New ln <$> nextLine
        "isvoid" -> IsVoid ln <$> readExp
        "plus"   -> Plus ln <$> readExp <*> readExp
        "minus"  -> Minus ln <$> readExp <*> readExp
        "times"  -> Times ln <$> readExp <*> readExp
        "divide" -> Divide ln <$> readExp <*> readExp
        "lt"     -> CoolLT ln <$> readExp <*> readExp
        "le"     -> LE ln <$> readExp <*> readExp
        "eq"     -> Eq ln <$> readExp <*> readExp
        "negate" -> Neg ln <$> readExp
        "not"    -> Not ln <$> readExp
        "identifier" -> do
            _ <- nextInt
            Var ln <$> nextLine
        "integer" -> IntLiteral ln <$> nextInt
        "string"  -> StrLiteral ln <$> nextLine
        "true"    -> return $ Boolean ln True
        "false"   -> return $ Boolean ln False
        _ -> error $ "Unknown expression kind: " ++ kind

readLet :: Int -> ParseState Exp
readLet ln = do
    bindingType <- nextLine
    case bindingType of
        "let_no_init" -> do
            _ <- nextInt
            name <- nextLine
            _ <- nextInt
            tName <- nextLine
            Let ln name tName Nothing <$> readExp
        "1" -> do 
            _ <- nextLine
            _ <- nextInt
            name <- nextLine
            _ <- nextInt
            tName <- nextLine
            initExp <- readExp
            Let ln name tName (Just initExp) <$> readExp
        _ -> error "Invalid let binding"

readCaseBranch :: ParseState CaseBranch
readCaseBranch = do
    ln <- nextInt
    name <- nextLine
    tLn <- nextInt
    tName <- nextLine
    CaseBranch (ln, name) (tLn, tName) <$> readExp

parseASTFile :: String -> Program
parseASTFile content = evalState readProgram (lines content)



testClassCount :: FilePath -> IO ()
testClassCount path = do
    content <- readFile path
    
    let (Program classes) = parseASTFile content
    
    putStrLn $ "Successfully parsed " ++ path
    putStrLn $ "Number of classes found: " ++ show (length classes)
    
    let classNames = map getName classes
    putStrLn $ "Classes: " ++ show classNames

getName :: CoolClass -> String
getName (ClassNoInherit (_, name) _) = name
getName (ClassInherit (_, name) _ _) = name

main :: IO ()
main = do
    testClassCount "cool.cl-ast"
