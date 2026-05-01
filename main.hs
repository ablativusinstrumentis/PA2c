import Control.Monad.State
import Control.Monad (replicateM)
import Data.Char (toLower)
import System.Environment (getArgs)
import System.FilePath (replaceExtension)

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
    | Int Int Int
    | Str Int String
    | Boolean Int Bool
    deriving Show

predefinedClassList = [(ClassNoInherit (0, "Object") []),(ClassInherit (0, "Bool") (0, "Object") []),(ClassInherit (0, "IO") (0, "Object") []),(ClassInherit (0, "Int") (0, "Object") []),(ClassInherit (0, "String") (0, "Object") [])]

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

class HasL a where
    getL :: a -> Int
instance HasL Program where
    getL (Program _) = 0
instance HasL CoolClass where
    getL (ClassNoInherit (l, _) _)   = l
    getL (ClassInherit (l, _) _ _)   = l
instance HasL Feature where
    getL (Method (l, _) _ _ _) = l
    getL (Attr (l, _) _ _)     = l
instance HasL Formal where
    getL (Formal (l, _) _) = l
instance HasL Exp where
    getL e = case e of
        Assign l _ _              -> l
        StaticDispatch l _ _ _ _  -> l
        DynamicDispatch l _ _ _ _ -> l
        SelfDispatch l _ _        -> l
        Cond l _ _ _              -> l
        Loop l _ _                -> l
        Block l _                 -> l
        Let l _ _ _ _             -> l
        Case l _ _                -> l
        New l _                   -> l
        IsVoid l _                -> l
        Plus l _ _                -> l
        Minus l _ _               -> l
        Times l _ _               -> l
        Divide l _ _              -> l
        Neg l _                   -> l
        CoolLT l _ _              -> l
        LE l _ _                  -> l
        Eq l _ _                  -> l
        Not l _                   -> l
        Var l _                   -> l
        Int l _            -> l
        Str l _            -> l
        Boolean l _               -> l

type ParseState a = State [String] a

hasdups :: (Eq a) => [a] -> [a] -> Bool
hasdups newlist [] = False
hasdups l (x:xs) = if (x `elem` l) then True else hasdups (x:l) xs

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
        "integer" -> Int ln <$> nextInt
        "string"  -> Str ln <$> nextLine
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


-- =============================================================================
-- TYPE CHECKER HELPERS (Inheritance, LUB, SELF_TYPE)
-- =============================================================================

-- | InheritanceTable: [(ChildName, ParentName)]
type InheritanceTable = [(String, String)]

-- | TypeEnv: [(VariableName, TypeName)]
type TypeEnv = [(String, String)]

-- | SUBTYPING LOGIC
-- Returns True if t1 conforms to t2 (t1 <= t2)
isSubtype :: InheritanceTable -> String -> String -> Bool
isSubtype table t1 t2
    | t1 == t2 = True
    | t1 == "Object" = False 
    | otherwise = case lookup t1 table of
        Just parent -> isSubtype table parent t2
        Nothing     -> False

-- | LUB LOGIC: Find the Lowest Common Ancestor
-- Gets the full inheritance path from a class up to "Object"
getAncestry :: InheritanceTable -> String -> [String] --("Int", "Object"), ("String", "Object"), and ("Bool", "Object")
getAncestry table className 
    | className == "Object" = ["Object"]
    | otherwise = case lookup className table of
        Just parent -> className : getAncestry table parent
        Nothing     -> [className] 

-- | Calculates the Least Upper Bound between two types
getLUB :: InheritanceTable -> String -> String -> String
getLUB table t1 t2
    | t1 == t2 = t1
    | otherwise = findCommon (getAncestry table t1) (getAncestry table t2)
  where
    findCommon (x:xs) path2
        | x `elem` path2 = x
        | otherwise      = findCommon xs path2
    findCommon [] _ = "Object"

-- | SELF_TYPE LOGIC
-- Substitutes "SELF_TYPE" with the actual class being checked
subSelf :: String -> String -> String
subSelf currentClass "SELF_TYPE" = currentClass
subSelf _ t = t

-- | FORMATTING ERROR MESSAGES
-- Helper to format error as: Error: LINE_NUMBER: Type-Check: MESSAGE
typeError :: Int -> String -> Either String a
typeError ln msg = Left $ "Error: " ++ show ln ++ ": Type-Check: " ++ msg

-- =============================================================================
-- INTEGRATED TYPE CHECKING
-- =============================================================================

-- Context carries the Inheritance table and the name of the class currently being checked
type CheckContext = (InheritanceTable, String)

checkExp :: CheckContext -> TypeEnv -> Exp -> Either String String
checkExp (inh, currentClass) env expr = case expr of
    
    -- Variables: lookup in env
    Var _ name -> case lookup name env of
        Just t  -> Right (subSelf currentClass t)
        Nothing -> Left "Error: 0: Type-Check: Undefined variable"

    -- Assignment: Check if RHS conforms to LHS
    Assign ln name e2 -> do
        varT <- case lookup name env of
            Just t  -> Right (subSelf currentClass t)
            Nothing -> typeError ln "Variable not declared"
        
        valT <- checkExp (inh, currentClass) env e2
        
        if isSubtype inh valT varT
            then Right valT
            else typeError ln "Incompatible types in assignment"

    -- Let: Extend env, substitute SELF_TYPE for the declared type
    Let ln name typeName mInit body -> do
        let declaredT = subSelf currentClass typeName
        
        -- Check init expression
        case mInit of
            Just initExp -> do
                initT <- checkExp (inh, currentClass) env initExp
                if isSubtype inh initT declaredT
                    then Right ()
                    else typeError ln "Init type mismatch"
            Nothing -> Right ()
        
        -- Add to env and check body
        let newEnv = (name, declaredT) : env
        checkExp (inh, currentClass) newEnv body

    -- Cond (If): LUB calculation example
    Cond ln e1 e2 e3 -> do
        type1 <- checkExp (inh, currentClass) env e1
        if type1 /= "Bool" then typeError ln "If condition must be Bool"
        else do
            t2 <- checkExp (inh, currentClass) env e2
            t3 <- checkExp (inh, currentClass) env e3
            Right (getLUB inh t2 t3)

    -- Base cases (Literals)
    Int _ _    -> Right "Int"
    Boolean _ _       -> Right "Bool"
    Str _ _    -> Right "String"
    
    _ -> Left "Not implemented"
-------------------------------------------------
-- START TOPOSORT--------------------------------
start :: [(String,[String])] -> [String]
start d = [x | (x,[]) <- d]

tsort :: [String] -> [String] -> [(String,[String])] -> [String]
tsort starting sorted dep
	| null starting = sorted
	| otherwise = tsort ([x | (x,[]) <- ndep]) (sorted ++ [node]) (ndep)
  where ndep = [(a,b) | (a,b)<-[(a, [c | c<-b, c /= node]) | (a,b)<-dep], a /= node]
        node = head (qsort starting)

final :: [String] -> [String] -> [String]
final sorted tasks
	| length sorted < length tasks = ["cycle"]
	| otherwise = sorted
  
qsort :: (Ord a) => [a] -> [a]  
qsort [] = []  
qsort (x:xs) = 
    let smallerSorted = qsort [a | a <- xs, a <= x]  
        biggerSorted = qsort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted
    
rmdups :: (Eq a) => [a] -> [a]
rmdups [] = []
rmdups (x:xs)   
	| x `elem` xs   = rmdups xs
	| otherwise     = x : rmdups xs

listify :: [(String, String)] -> [(String, [String])]
listify [] = []
listify ((cname, "NoInherit"):xs) = (cname, []) : listify xs
listify ((cname,iname):xs) = (cname,[iname]) : listify xs
-- END TOPOSORT--------------------------
-----------------------------------------


inheritances :: CoolClass -> String
inheritances (ClassNoInherit _ _) = "NoInherit"
inheritances (ClassInherit _ (_, name) _) = name

findClass :: String -> [CoolClass] -> Maybe CoolClass
findClass s [] = Nothing
findClass s (c:cs)
    | s == className c = Just c
    | otherwise = findClass s cs

evilInherit :: [String] -> Bool
evilInherit [] = False
evilInherit (x:xs) = if (x == "Int" || x == "Bool" || x == "String") then True else evilInherit xs

fakeInherit :: [String] -> [String] -> Bool
fakeInherit [] _ = False
fakeInherit (x:xs) l = if ((x `elem` l) || x == "NoInherit") then fakeInherit xs l else True

checkeverything :: [CoolClass] -> String
checkeverything classes
    | hasdups [] classNames = "ERROR: 0: Type-Check: u messed up1" --Redefining a formal or class: (second) identifier location
    | evilInherit inheritlist = "ERROR: 0: Type-Check: u messed up2" --Other inheritance type problem: inherited type identifier location
    -- | fakeInherit inheritlist classNames = "ERROR: 0: Type-Check: u messed up3" --Other inheritance type problem: inherited type identifier location
    -- | (final (tsort (start deps) [] deps) tasks) == ["cycle"] = "ERROR: 0: Type-Check: inheritance cycle"
    --Redefining a feature: (second) feature location
    -- also like write the thing where it gives you (Bool, Int) and you guard based on the Bool and then write in the Int 
    -- let (a,b) = THING in | a = "ERROR: " ++ show b ++ ": Type-Check: u messed up"
    -- | dupfeatures classes = "ERROR: 0: Type-Check: u messed up" --ADD A SEPARATOR FOR ATTR AND METHOD BCS THEY CNA SHARE A NAME
    | not ("Main" `elem` classNames) = "ERROR: 0: Type-Check: u messed up4" --rev also THIS LINE MIGHT BE FUCKED UP BCS Nothing OR BCS guards bad
    -- | not ("main" `elem` getFeatures (findClass "Main" classes)) = "ERROR: 0: Type-Check: u messed up"
    -- | evilRedefine classinherits = "Error: 0: u messed up"
    | otherwise = "its chill"
    where classNames = map className classes
          inheritlist = map inheritances classes
          classinherits = listify (zip classNames inheritlist)
          tasks = rmdups classNames
          deps = classinherits

-- Errors related to parameter-less method main in class Main: always line 0
--Other inheritance type problem: inherited type identifier location
--self or SELF_TYPE used in wrong place: self (resp. SELF_TYPE) identifier (resp. type) location
--Redefining a feature: (second) feature location
--Redefining a formal or class: (second) identifier location
--Other attribute problems: attribute location
--Redefining a method and changing types: (second) type location
--Other problems with redefining a method: method location
--Method body type does not conform: method name identifier location
-- Attribute initializer does not conform: attribute name identifier location

className :: CoolClass -> String
className (ClassNoInherit (_, name) _) = name
className (ClassInherit (_, name) _ _) = name

classSort :: [CoolClass] -> [CoolClass]
classSort [] = []
classSort (x:xs) = let smallerSorted = classSort [c | c <-xs, className c <= className x]
                       biggerSorted = classSort [c | c<-xs, className c > className x]
                   in smallerSorted ++ [x] ++ biggerSorted

attrs :: [Feature] -> [Feature]
attrs [] = []
attrs ((Attr n t e):fs) = (Attr n t e) : attrs fs
attrs ((Method n f t e):fs) = attrs fs

classMapProgram :: Program -> String
classMapProgram (Program classes) = "class_map\n" ++ show (5 + length classes) ++ "\n" ++ (concatMap classMapClass (classSort (classes ++ predefinedClassList)))
-- add inherited feature stuff LATER HAHA
classMapClass :: CoolClass -> String
classMapClass (ClassNoInherit (_, name) features) = name ++ "\n" ++ show (length (attrs features)) ++ "\n" ++ (concatMap classMapAttributes features)
classMapClass (ClassInherit (_, name) _ features) = name ++ "\n" ++ show (length (attrs features)) ++ "\n" ++ (concatMap classMapAttributes features)

classMapAttributes :: Feature -> String
classMapAttributes (Attr (_, name) (_, typename) (Just expression)) = "initializer\n" ++ name ++ "\n" ++ typename ++ "\n" ++ serializeExp expression ++ "\n"
classMapAttributes (Attr (_, name) (_, typename) Nothing) = "no_initializer\n" ++ name ++ "\n" ++ typename ++ "\n"
classMapAttributes _ = ""

serializeExp :: Exp -> String
serializeExp (Int l val) = 
    show l ++ "\n" ++ 
    "integer\n" ++ 
    show val
serializeExp (Boolean l True) = 
    show l ++ "\n" ++ 
    "true"
serializeExp (Boolean l False) = 
    show l ++ "\n" ++ 
    "false"
serializeExp (Str l val) = 
    show l ++ "\n" ++ 
    "string\n" ++ 
    val
serializeExp other = show other ++ "\n"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> do
            content <- readFile path
            let (Program classes) = parseASTFile content
            --case checkeverything classes of
                --"its chill" -> do 
            writeFile (replaceExtension path ".cl-type") (classMapProgram (Program classes))
               -- _ -> do 
                --    putStrLn $ checkeverything classes
        _ -> do
            putStrLn $ "hey dont do that"
    -- IF SOMETHING IS WRONG ADD INT BOOL ETC
--Check for self and SELF_TYPE mistakes in classes and methods.
