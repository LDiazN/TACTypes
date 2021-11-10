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
deref    LVAL X
ref      LVAL X
@staticv LABEL size // create a .data object named with LABEL and size 'size'

@string LABEL S     // create a .data literal string S named with LABEL 
@label LABEL        // create a label at this point in the program

// Functions?
param LVAL
call  LVAL X
```
Please note that instructions starting with the `@` prefix are actually meta instructions for the compiler.

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

