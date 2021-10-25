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
    show TACCode {tacOperation=Add,  tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Just rvoperand2} = "\t" ++ _showThreeOps lvoperand  " := " rvoperand1 " + " rvoperand2 -- lvalue := rvalue1 + ravlue2
    show TACCode {tacOperation=Sub,  tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Just rvoperand2} = "\t" ++ _showThreeOps lvoperand  " := " rvoperand1 " - " rvoperand2 -- lvalue := rvalue1 - ravlue2
    show TACCode {tacOperation=Mult,  tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Just rvoperand2} = "\t" ++ _showThreeOps lvoperand  " := " rvoperand1 " * " rvoperand2 -- lvalue := rvalue1 * ravlue2
    show TACCode {tacOperation=Div,  tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Just rvoperand2} = "\t" ++ _showThreeOps lvoperand  " := " rvoperand1 " / " rvoperand2 -- lvalue := rvalue1 / ravlue2
    show TACCode {tacOperation=Mod,  tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Just rvoperand2} = "\t" ++ _showThreeOps lvoperand  " := " rvoperand1 " % " rvoperand2 -- lvalue := rvalue1 % ravlue2
    show TACCode {tacOperation=Minus,  tacLValue=Just lvoperand, tacRValue1=Just rvoperand1, tacRValue2=Nothing} = "\t" ++ _showTwoOps lvoperand  " := -" rvoperand1                         -- lvalue := - rvalue1 

-- add $resultado $opr1 $opr2

-- | Possible values for an operation. 'a' should be SymEntryCompatible 
data LVOperand = LVOperand deriving(Eq, Show)
data RVOperand = RVOperand deriving(Eq, Show)

-- | Possible operation you can perform with the given operands, describes the generated TACCode
data Operation =
    Assign        |
    -- Arithmetic
    Add            | -- ^ Addition
    Sub            | -- ^ Substraction
    Mult           | -- ^ Multiplication
    Div            | -- ^ Divition
    Mod            | -- ^ Module 
    Minus            -- ^ negative operator
    deriving (Eq, Show)

-- < Utility Functions > ---
_showThreeOps :: LVOperand -> String -> RVOperand -> String -> RVOperand -> String
_showThreeOps lvopr s1 rvopr1 s2 rvopr2 = show lvopr ++ s1 ++ show rvopr1 ++ s2 ++ show rvopr2

_showTwoOps :: LVOperand -> String -> RVOperand -> String
_showTwoOps lvopr s1 rvopr1 = show lvopr ++ s1 ++ show rvopr1
