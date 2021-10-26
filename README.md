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
@staticv LABEL size : create a .data object named with LABEL and size 'size'

// Functions?
param LVAL
call  LVAL X
```
Please note that instructions starting with the `@` prefix are actually meta instructions for the compiler.
