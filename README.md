# TACTypes
Definiciones de tipos para el tipo de dato `TAC`, implementación común para los grupos de lenguajes de programación 3 sept-dic 2021

```
assign ADDR X

// Jumps
goto LABEL
goif LABEL X

// Comparators
eq  LVAL X Y
neq LVAL X Y
lt  LVAL X Y
leq LVAL X Y
gt  LVAL X Y
geq LVAL X Y

// Boolean operators
and LVAL X Y
or  LVAL X Y
neg LVAL X 

// Numeric operators
add   LVAL X Y
sub   LVAL X Y
mult  LVAL X Y
div   LVAL X Y
mod   LVAL X Y
minus LVAL X

// Memory management
malloc   LVAL size
free     X
LVAL [ X ] = Y
LVAL = X [ Y ]
ref      LVAL X
@staticv LABEL size // create a .data object named with LABEL and size 'size'

@string LABEL S     // create a .data literal string S named with LABEL 
@label LABEL        // create a label at this point in the program

// Functions?
param LVAL
call  LVAL X
```
Please note that instructions starting with the `@` prefix are actually meta instructions for the compiler.
# Grammar
You can use the following grammar to parse a tac file
```
S     -> Data Text
Data  -> *lambda*           // Data section
      |  D Data
D     -> @staticv ID INT \n
      |  @string ID STRING

Text  -> *lambda*
      | T \n Text
      
T     -> I
      -> F
I    -> *lambda*
      | @label ID
      | assignw Acc Val
      | assignw ID RVal
      | assignb Acc Val
      | assignb ID RVal
      | add     ID Val Val
      | sub     ID Val Val
      | mult    ID Val Val
      | div     ID Val Val
      | mod     ID Val Val
      | minus   ID RVal
      | eq      ID Val Val
      | neq     ID Val Val
      | lt      ID Val Val
      | leq     ID Val Val
      | gt      ID Val Val
      | geq     ID Val Val
      | goto    ID
      | goif    ID RVal
      | goifnot ID Rval
      | malloc  ID RVal
      | memcpy  ID ID INT
      | free    LVal
      | exit    RVal
      | return  RVal
      | param lvalue rval   # X_dir x_offset
      | call RETDIR FUNC
      | printi int_val
      | printf float_val
      | print   string_dir
      | printc  char_val
      | readi int_dir
      | readf float_dir
      | read   string_dir
      | readc  char_dir

F    -> @fun_begin ID SIZE \n
               Inst \n
        @fun_end SIZE \n

Acc  -> ID [ Val ] // memory access
LVal -> ID | Acc   // l-value
Val  -> TRUE | FALSE | CHAR | INT | FLOAT | ID 
RVal -> Val | Acc                     // r-value
ID   -> (Possible name for a variable)
```

# Serialization
In order to have some iteroperability between multiple compilers, we need serialization. And this is the serialization format:

## Constant serialization
```
'<char>'    :: character // for example: 'c'
<num>       :: int       // for example: 1
<num>.<num> :: float     // for example: 1.0
True | False :: Bool     // for example: False
```
## Instruction serialization
```
<instruction name> LVAL (Maybe RVAL) (Maybe RVAL) 
For example:
  add x 2 y
  free z
  @label foo
```

