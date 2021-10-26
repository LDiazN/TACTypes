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

type Name = String

-- | Defines an object compatible with a symbol. In order to be compatible with a symbol, you need 
-- | to provide an identifier
class SymEntryCompatible a where
  -- | Return symbol ID
  getSymID :: a -> String

-- | Canonical program that every tac code generator should return 
type TACProgram = [TACCode]

-- | Atomic operation for a Three Address Program
data TACCode = TACCode
    {   
        tacOperation :: Operation,      -- ^ tells which operation will be performed 
        tacLValue  :: Maybe LVOperand,  -- ^ Where the result will be stored
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
    show TACCode {tacOperation=Neg, tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Nothing} = "\t" ++ _showTwoOps lvoperand " := !" rvoperand1                                -- x := !y

    show TACCode {tacOperation=Add,  tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Just rvoperand2} = "\t" ++ _showThreeOps lvoperand  " := " rvoperand1 " + " rvoperand2 -- lvalue := rvalue1 + ravlue2
    show TACCode {tacOperation=Sub,  tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Just rvoperand2} = "\t" ++ _showThreeOps lvoperand  " := " rvoperand1 " - " rvoperand2 -- lvalue := rvalue1 - ravlue2
    show TACCode {tacOperation=Mult, tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Just rvoperand2} = "\t" ++ _showThreeOps lvoperand  " := " rvoperand1 " * " rvoperand2 -- lvalue := rvalue1 * ravlue2
    show TACCode {tacOperation=Div,  tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Just rvoperand2} = "\t" ++ _showThreeOps lvoperand  " := " rvoperand1 " / " rvoperand2 -- lvalue := rvalue1 / ravlue2
    show TACCode {tacOperation=Mod,  tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Just rvoperand2} = "\t" ++ _showThreeOps lvoperand  " := " rvoperand1 " % " rvoperand2 -- lvalue := rvalue1 % ravlue2
    show TACCode {tacOperation=Minus,tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Nothing}   = "\t" ++ _showTwoOps lvoperand  " := -" rvoperand1                         -- lvalue := - rvalue1 

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
data LVOperand = LVOperand deriving(Eq, Show)
data RVOperand = RVOperand deriving(Eq, Show)

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

    deriving (Eq, Show)

-- < Utility Functions > ---
_showThreeOps :: LVOperand -> String -> RVOperand -> String -> RVOperand -> String
_showThreeOps lvopr s1 rvopr1 s2 rvopr2 = show lvopr ++ s1 ++ show rvopr1 ++ s2 ++ show rvopr2

_showTwoOps :: LVOperand -> String -> RVOperand -> String
_showTwoOps lvopr s1 rvopr1 = show lvopr ++ s1 ++ show rvopr1

_showOneOps :: String -> LVOperand -> String
_showOneOps s lvopr = s ++ show lvopr 
