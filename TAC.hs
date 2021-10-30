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
        tacLValue  :: Maybe LVOperand,      -- ^ Where the result will be stored
        tacRValue1 :: Maybe RVOperand,  -- ^ first operation argument 
        tacRValue2 :: Maybe RVOperand   -- ^ Second operation argument 
    } deriving (Eq)

instance Show TACCode where
    show TACCode {tacOperation=Goto,      tacLValue=Just lvoperand, tacRValue1=Nothing,        tacRValue2=Nothing } = "\t" ++ _showOneOps " goto " lvoperand                                  -- goto LABEL
    show TACCode {tacOperation=Goif,      tacLValue=Just lvoperand, tacRValue1=Just rvoperand, tacRValue2=Nothing } = "\tgoif " ++ show  lvoperand ++ " " ++ show rvoperand                   -- goto LABEL rvalue
    show TACCode {tacOperation=MetaLabel, tacLValue=Just lvoperand, tacRValue1=Nothing,        tacRValue2=Nothing}  = _showOneOps "@label " lvoperand                                         -- @label MyLabel

    show TACCode {tacOperation=Eq,  tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Just rvoperand2} = "\t" ++ _showThreeOps lvoperand " := " rvoperand1 " == " rvoperand2   -- x := y == z
    show TACCode {tacOperation=Neq, tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Just rvoperand2} = "\t" ++ _showThreeOps lvoperand " := " rvoperand1 " != " rvoperand2   -- x := y != z
    show TACCode {tacOperation=Lt,  tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Just rvoperand2} = "\t" ++ _showThreeOps lvoperand " := " rvoperand1 " < " rvoperand2    -- x := y < z
    show TACCode {tacOperation=Leq, tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Just rvoperand2} = "\t" ++ _showThreeOps lvoperand " := " rvoperand1 " <= " rvoperand2   -- x := y <= z
    show TACCode {tacOperation=Gt,  tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Just rvoperand2} = "\t" ++ _showThreeOps lvoperand " := " rvoperand1 " > " rvoperand2    -- x := y == z
    show TACCode {tacOperation=Geq, tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Just rvoperand2} = "\t" ++ _showThreeOps lvoperand " := " rvoperand1 " >= " rvoperand2   -- x := y == z

    show TACCode {tacOperation=And, tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Just rvoperand2}   = "\t" ++ _showThreeOps lvoperand " := " rvoperand1 " && " rvoperand2   -- x := y == z
    show TACCode {tacOperation=Or,  tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Just rvoperand2}   = "\t" ++ _showThreeOps lvoperand " := " rvoperand1 " || " rvoperand2   -- x := y == z
    show TACCode {tacOperation=Neg, tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Nothing} = "\t" ++ _showTwoOps lvoperand " := ! " rvoperand1                                -- x := !y

    show TACCode {tacOperation=Add,  tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Just rvoperand2} = "\t" ++ _showThreeOps lvoperand  " := " rvoperand1 " + " rvoperand2 -- lvalue := rvalue1 + ravlue2
    show TACCode {tacOperation=Sub,  tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Just rvoperand2} = "\t" ++ _showThreeOps lvoperand  " := " rvoperand1 " - " rvoperand2 -- lvalue := rvalue1 - ravlue2
    show TACCode {tacOperation=Mult, tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Just rvoperand2} = "\t" ++ _showThreeOps lvoperand  " := " rvoperand1 " * " rvoperand2 -- lvalue := rvalue1 * ravlue2
    show TACCode {tacOperation=Div,  tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Just rvoperand2} = "\t" ++ _showThreeOps lvoperand  " := " rvoperand1 " / " rvoperand2 -- lvalue := rvalue1 / ravlue2
    show TACCode {tacOperation=Mod,  tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Just rvoperand2} = "\t" ++ _showThreeOps lvoperand  " := " rvoperand1 " % " rvoperand2 -- lvalue := rvalue1 % ravlue2
    show TACCode {tacOperation=Minus,tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Nothing}   = "\t" ++ _showTwoOps lvoperand  " := - " rvoperand1                         -- lvalue := - rvalue1 

    show TACCode {tacOperation=Malloc, tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Nothing} = "\t" ++ _showTwoOps lvoperand  " := malloc( " rvoperand1 ++ " )"          -- lvalue := malloc(rvalue)
    show TACCode {tacOperation=Free,   tacLValue=Just lvoperand, tacRValue1=Nothing,         tacRValue2=Nothing} = "\t" ++ _showOneOps "free " lvoperand                                     -- free lvalue
    show TACCode {tacOperation=Deref,  tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Nothing} = "\t" ++ _showTwoOps lvoperand  " := * " rvoperand1                        -- lvalue := *rvalue
    show TACCode {tacOperation=MetaStaticv,  tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Nothing} = "@staticv " ++ show lvoperand ++ " " ++ show rvoperand1             -- @staticv lvalue rvalue


    show (TACCode _tacOperation _tacLValue _tacRValue1 _tacRValue2) = error $
        "Invalid configuration for TACCode."   ++
        "\n\tOperator: " ++ show _tacOperation ++
        "\n\tlvalue: "   ++ show _tacLValue    ++
        "\n\trvalue 1: " ++ show _tacRValue1   ++
        "\n\trvalue 2: " ++ show _tacRValue2

-- | Possible values for an operation. 'a' should be SymEntryCompatible 
data LVOperand =  
        LVId String    |
        LVLabel String 
        
        deriving(Eq)

instance Show LVOperand where
    show (LVId sym) = sym
    show (LVLabel l) = l

-- | Possible variations for an r-value. 
data RVOperand =
    RVId String    |       -- ^ A variable defined by its name. 
    RVLabel String   |       -- ^ A label value, with a string as its name
    Constant ConstantValue -- ^ A constant with its corresponding value
    deriving(Eq)

-- | Constant values
data ConstantValue =
    Float Float |
    Int Int     |
    Char Char   |
    Bool Bool   
    deriving(Eq)

instance Show RVOperand where
    show (RVId sym) =  sym
    show (RVLabel s) = s
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
    Deref           | -- ^ retrieve value in this memory address
    MetaStaticv       -- ^ Create a static variable named by a given name with the requested size in bytes and return its address

    deriving (Eq, Show, Read)

-- | convert from string representation to a tac program
parse :: String -> TACProgram
parse = TACProgram . map read . lines

-- < Read & Show instances > ------------------------------
instance Show TACProgram where
    show (TACProgram l) = unlines . map show $ l

instance Show ConstantValue where
    show (Char c) = ['\'', c, '\'']
    show (Float f) = show f
    show (Int i) = show i
    show (Bool b) = show b

instance Read ConstantValue where
    readsPrec _ ('\'' : c : '\'' : rest) = [(Char c, rest)]
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

instance Read RVOperand where
    readsPrec  _  s = 
        let 
            (word, rest) = _nextWord s
            mbConstant = (readMaybe word :: Maybe  ConstantValue ) <&> Constant 
            opr = fromMaybe (RVId word) mbConstant
        in [(opr, rest)]

instance Read LVOperand where
    readsPrec _ s = 
        let (word, rest) = _nextWord s;
        in [(LVId word, rest)]


-- Utility function to get the next word
_nextWord :: String -> (String, String)
_nextWord s = (ans, _trim rest)
    where
        (ans, rest) = break isSpace . _trim $ s

-- Utility function to tell if a string starts with some unary operator
_startsWithUnary :: String -> Bool
_startsWithUnary s = (`L.isPrefixOf` s) `any`  ["goto", "goif", "free", "@staticv", "@label"]

-- Utility function to trim a string
_trim :: String -> String
_trim = L.dropWhileEnd isSpace . dropWhile isSpace

-- Creat Read Instance to parse a tac program
instance Read TACCode where
    readsPrec _ s = 
        let
            (word0, rest0) = _nextWord s
            (word1, rest1) = _nextWord rest0
            (word2, rest2) = _nextWord rest1
            (word3, rest3) = _nextWord rest2
            (word4, rest4) = _nextWord rest3
            opr' = case word0 of 
                "goto" -> Goto
                "goif" -> Goif
                "@label" -> MetaLabel
                "free" -> Free
                "@staticv" -> MetaStaticv
                _ -> case word3 of
                    "==" -> Eq 
                    "!=" -> Neq
                    "<"  -> Lt
                    "<=" -> Leq
                    ">"  -> Gt
                    ">=" -> Geq
                    "&&" -> And
                    "||" -> Or
                    "+"  -> Add
                    "-"  -> Sub
                    "*"  -> Mult
                    "/"  -> Div
                    "%"  -> Mod
                    _ -> case word2 of
                        "malloc(" -> Malloc
                        "-" -> Minus
                        "!" -> Neg
                        "*" -> Deref 
                        _   -> error $ "Unknown operator: " ++ word2 ++ ". Original String: " ++ s
            -- In case of 3 addr instr
            ans3addrs = (TACCode opr' (Just . read $ word0) (Just . read $ word2) (Just . read $ word4), rest4)
            ans2addrs = (TACCode opr' (Just . read $ word0) (Just . read $ word3) Nothing, rest3)
            tcode 
                -- Examples: 
                -- word0 word1 word2 word3 word4
                -- x      :=    b     +     y
                -- free   x
                -- goif   x     z
                -- x      :=   -      y
                -- x      :=  malloc( y    )

                -- Unary operators
                | opr' == Goif = (TACCode opr' (Just $ LVLabel word1) (Just . read $ word2) Nothing, rest2)
                | opr' == Goto = (TACCode opr' (Just $ LVLabel word1) Nothing Nothing, rest1)
                | opr' == Free = (TACCode opr' (Just $ LVId word1) Nothing Nothing, rest1)
                | opr' == MetaLabel     = (TACCode opr' (Just $ LVId word1) Nothing Nothing, rest1)

                -- 3 addrs operators
                | opr' == Eq   = ans3addrs
                | opr' == Neq  = ans3addrs 
                | opr' == Lt   = ans3addrs 
                | opr' == Leq  = ans3addrs 
                | opr' == Gt   = ans3addrs 
                | opr' == Geq  = ans3addrs 
                | opr' == And  = ans3addrs 
                | opr' == Or   = ans3addrs 
                | opr' == Add  = ans3addrs 
                | opr' == Sub  = ans3addrs 
                | opr' == Mult = ans3addrs 
                | opr' == Div  = ans3addrs 
                | opr' == Mod  = ans3addrs 

                -- 2 addrs operators
                | opr' == MetaStaticv   = (TACCode opr' (Just $ LVId word1)(Just . read $ word2) Nothing, rest2)
                | opr' == Malloc = (TACCode opr' (Just . read $ word0) (Just . read $ word3) Nothing, rest4)
                | opr' == Minus  = ans2addrs 
                | opr' == Neg    = ans2addrs 
                | opr' == Deref  = ans2addrs 
                

                | otherwise  = error $ "Invalid operator: " ++ show opr'
        in [tcode]


-- < Utility Functions > ---
_showThreeOps :: LVOperand -> String -> RVOperand -> String -> RVOperand -> String
_showThreeOps lvopr s1 rvopr1 s2 rvopr2 = show lvopr ++ s1 ++ show rvopr1 ++ s2 ++ show rvopr2

_showTwoOps :: LVOperand -> String -> RVOperand -> String
_showTwoOps lvopr s1 rvopr1 = show lvopr ++ s1 ++ show rvopr1

_showOneOps :: String -> LVOperand -> String
_showOneOps s lvopr = s ++ show lvopr

