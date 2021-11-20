{-
    This file defines our TAC (Three Address Code). It's an intermediate representation
    for optimization and further compilation processes. 

    Every TAC instruction is formed like this:
        operation (Maybe l-value) (Maybe r-value1) (Maybe r-value2)
    Where:
        -  operation : tells which operation will be performed 
        -  l-value (optional) : Where the result will be stored. Could be:
            - variable
        -  r-value1 (optional): First operation argument. Could be:
            - Variable 
            - Constant
        -  r-value2 (optional): Second operation argument 
            - Variable  
            - Constant
-}
{-# OPTIONS_GHC -Wall #-}
module TACTypes.TAC where
import Data.Char(isDigit, isSpace)
import qualified Data.List as L
import Data.Functor((<&>))
import Text.Read(readMaybe)
import Data.Maybe(fromJust, fromMaybe)
type Name = String

-- | Defines an object compatible with a symbol. In order to be compatible with a symbol, you need 
-- | to provide an identifier
class SymEntryCompatible a where
  -- | Return symbol ID
  getSymID :: a -> String

-- | Canonical program that every tac code generator should return 
newtype TACProgram = TACProgram [TACCode]

-- | Atomic operation for a Three Address Program. 'b' It's some custom type you can use for type information
data TACCode = TACCode
    {
        tacOperation :: Operation,          -- ^ tells which operation will be performed 
        tacLValue  :: Maybe Operand,      -- ^ Where the result will be stored
        tacRValue1 :: Maybe Operand,  -- ^ first operation argument 
        tacRValue2 :: Maybe Operand   -- ^ Second operation argument 
    } deriving (Eq)

instance Show TACCode where
    show (TACCode opr (Just lv) Nothing Nothing) = show opr ++ " " ++ show lv
    show (TACCode opr (Just lv) (Just rv1) Nothing) = show opr ++ " " ++ show lv ++ " " ++ show rv1
    show (TACCode opr (Just lv) (Just rv1) (Just rv2)) = show opr ++ " " ++ show lv ++ " " ++ show rv1 ++ " " ++ show rv2
    show (TACCode opr lv rv1 rv2 ) = error "Invalid configuration for tac code: " ++ show opr ++ " " ++ show lv ++ " " ++ show rv1 ++ " " ++ show rv2

-- | Possible variations for an r-value. 
data Operand =
    Id String    |       -- ^ A variable defined by its name. 
    Label String   |       -- ^ A label value, with a string as its name
    Constant ConstantValue -- ^ A constant with its corresponding value
    deriving(Eq)

-- | Constant values
data ConstantValue =
    Float Float |
    Int Int     |
    Char Char   |
    Bool Bool   |
    String String
    deriving(Eq)

instance Show Operand where
    show (Id sym) =  sym
    show (Label s) = s
    show (Constant cv) = show cv

-- | Possible operation you can perform with the given operands, describes the generated TACCode
data Operation =
    Assign      |

    -- Jumps & control flow
    Goto        |   -- ^ Go to specific Label
    Goif        |   -- ^ Go to specific Label when the given variable is true
    MetaLabel   |   -- ^ Meta instruction to define a label at some specific point in the program

    -- Relational operators
    Eq          |   -- ^ Equality
    Neq         |   -- ^ Non-equality
    Lt          |   -- ^ Less than
    Leq         |   -- ^ Less than or equal
    Gt          |   -- ^ Greater than
    Geq         |   -- ^ Greater than or equal

    -- Boolean (not bitwise)
    And         |   -- ^ a && b
    Or          |   -- ^ a || b
    Neg         |   -- ^ !a

    -- Arithmetic
    Add         | -- ^ Addition
    Sub         | -- ^ Substraction
    Mult        | -- ^ Multiplication
    Div         | -- ^ Divition
    Mod         | -- ^ Module 
    Minus       | -- ^ negative operator

    -- Memory management
    Malloc          | -- ^ Get n bytes of memory and return its start point 
    Free            | -- ^ Free the memory starting at the given location
    LDeref          | -- ^ assign value in this memory address with the given offset
    RDeref          | -- ^ retrieve value in this memory address with the given offset
    Ref             | -- ^ get memory address associated with a tac id
    MetaStaticv     | -- ^ Create a static variable named by a given name with the requested size in bytes and return its address
    MetaStaticStr   | -- ^ Create a static string named by a given name and return its address

    -- IO 
    Print           | -- ^ Print the given object to stdout
    Read            | -- ^ Read a string into memory 

    -- Function calling
    Call            | -- ^ Call a function with n stacked arguments
    Param           | -- ^ Stack a parameter for a function
    Return          | -- ^ end a function
    MetaBeginFunc   | -- ^ Mark start of a new function 
    MetaEndfunc     | -- ^ Mark end a function 

    -- Misc
    MetaComment       -- ^ Mark a comment string

    deriving (Eq)


-- | Get the current BASE id. 
-- | BASE value is updated implicitly.
-- | Will be implemented in target code.
base :: String
base = "BASE"

-- | convert from string representation to a tac program
parse :: String -> TACProgram
parse = TACProgram . map read . lines

-- < TAC Utility Functions > ------------------------------
-- | Shortcut function to create a tac code instance easier
newTAC :: Operation -> Operand -> [Operand] -> TACCode
newTAC opr lv [] = TACCode opr (Just lv) Nothing Nothing
newTAC opr lv [rv1] = TACCode opr (Just lv) (Just rv1) Nothing
newTAC opr lv [rv1, rv2] = TACCode opr (Just lv) (Just rv1) (Just rv2)
newTAC _ _ _ = error "Invalid configuration for tac code"

-- < Read & Show instances > ------------------------------

instance Show Operation where
    show Assign     = "assign"
    show Goto       = "goto"
    show Goif       = "goif"
    show MetaLabel  = "@label"
    show Eq         = "eq"
    show Neq        = "neq"
    show Lt         = "lt"
    show Leq        = "leq"
    show Gt         = "gt"
    show Geq        = "geq"
    show And        = "and"
    show Or         = "or"
    show Neg        = "neg"
    show Add        = "add"
    show Sub        = "sub"
    show Mult       = "mult"
    show Div        = "div"
    show Mod        = "mod"
    show Minus      = "minus"
    show Malloc     = "malloc"
    show Free       = "free"
    show LDeref     = "lderef"
    show RDeref     = "rderef"
    show Ref        = "ref"
    show MetaStaticv    = "@staticv"
    show MetaStaticStr  = "@stringd"
    show Read           = "read"
    show Print          = "Print"
    show Call       = "call"
    show Param      = "param"
    show Return     = "return"
    show MetaEndfunc = "@endfunction"
    show MetaBeginFunc = "@function"
    show MetaComment = "@comment"

instance Read Operation where
    readsPrec _ strOpr = res
        where
            -- split the command from the input
            (opr, rest) = _nextWord strOpr

            retOpr = case opr of
                        "assign"    -> Assign
                        "goto"      -> Goto
                        "goif"      -> Goif
                        "@label"    -> MetaLabel
                        "eq"        -> Eq
                        "neq"       -> Neq
                        "lt"        -> Lt
                        "leq"       -> Leq
                        "gt"        -> Gt
                        "geq"       -> Geq
                        "and"       -> And
                        "or"        -> Or
                        "neg"       -> Neg
                        "add"       -> Add
                        "sub"       -> Sub
                        "mult"      -> Mult
                        "div"       -> Div
                        "mod"       -> Mod
                        "minus"     -> Minus
                        "malloc"    -> Malloc
                        "free"      -> Free
                        "lderef"    -> LDeref
                        "rderef"    -> RDeref
                        "ref"       -> Ref
                        "@staticv"  -> MetaStaticv
                        "@stringd"  -> MetaStaticStr
                        "call"      -> Call
                        "param"     -> Param
                        _ -> error $ "unexpected operation: " ++ opr

            res = [(retOpr, rest)]

instance Show TACProgram where
    show (TACProgram l) = unlines . map show $ l

instance Show ConstantValue where
    show (Char c) = ['\'', c, '\'']
    show (Float f) = show f
    show (Int i) = show i
    show (Bool b) = show b
    show (String s) = s

instance Read ConstantValue where
    readsPrec _ ('\'' : c : '\'' : rest) = [(Char c, rest)]
    readsPrec _ ('\"':s) = [(String s', tail rest)]
        where
            (s',rest) = span (/= '\"') s
    readsPrec _ s =
        let
            (nums1, rest1) = span isDigit s
            (nums2, rest2) = span isDigit . tail $ rest1
            res
                | "False" `L.isPrefixOf` s = [(Bool False, fromJust . L.stripPrefix "False" $ s)]
                | "True"  `L.isPrefixOf` s = [(Bool True,  fromJust . L.stripPrefix "True"  $ s)]
                | not (null rest1) && head rest1 == '.' = [(Float $ read (nums1 ++ '.' : nums2) , rest2)]
                | otherwise  = [(Int (read nums1), rest1)]
        in res

instance Read Operand where
    readsPrec  _  s =
        let
            (word, rest) = _nextWord s
            mbConstant = (readMaybe word :: Maybe  ConstantValue ) <&> Constant
            opr = fromMaybe (Id word) mbConstant
        in [(opr, rest)]

instance Read TACCode where
    readsPrec _ s =
        let
            -- Split between next word and 
            (strOpr,rest1) = _nextWord s

        in [(_readTacCode (read strOpr) rest1, "")]

-- Utility function to read a tac code from a string of arguments knowing the operator
_readTacCode :: Operation -> String -> TACCode
_readTacCode op@Goif s =
    let
        (label, rval) = case words s of
            [labelStr, rvalStr] -> (labelStr,rvalStr)
            _ -> error $ "Error: invalid arguments for goif operation: " ++ s

    in
        TACCode op (Just $ Label label) (Just . read $ rval) Nothing

_readTacCode op@Goto s =
    let
        label = case words s of
            [label'] -> label'
            _ -> error $ "Error: invalid arguments for goto operation: " ++ s

    in
        TACCode op (Just $ Label label) Nothing Nothing

_readTacCode op@MetaLabel s =
    let 
        TACCode _ lb Nothing  Nothing = _readTacCode Goto s
    in 
        TACCode op lb Nothing  Nothing 

_readTacCode opr s = 
    let 
        lv:rvs = words s
        lvalue = read lv :: Operand
        rvalues = [read rv :: Operand | rv <- rvs]
    in newTAC opr lvalue rvalues


-- Utility function to get the next word
_nextWord :: String -> (String, String)
_nextWord s = (ans, _trim rest)
    where
        (ans, rest) = break isSpace . _trim $ s

-- Utility function to tell if a string starts with some unary operator
_startsWithUnary :: String -> Bool
_startsWithUnary s = (`L.isPrefixOf` s) `any`  ["goto", "goif", "free", "@staticv", "@string", "@label"]

-- Utility function to trim a string
_trim :: String -> String
_trim = L.dropWhileEnd isSpace . dropWhile isSpace

-- < Utility Functions > ---
_showThreeOps :: Operand -> String -> Operand -> String -> Operand -> String
_showThreeOps lvopr s1 rvopr1 s2 rvopr2 = show lvopr ++ s1 ++ show rvopr1 ++ s2 ++ show rvopr2

_showTwoOps :: Operand -> String -> Operand -> String
_showTwoOps lvopr s1 rvopr1 = show lvopr ++ s1 ++ show rvopr1

_showOneOps :: String -> Operand -> String
_showOneOps s lvopr = s ++ show lvopr

