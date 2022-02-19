# Tutorial
Emu is a relatively simple language, so it shouldn't take long to learn as an end user.

## Types
Emu is a statically typed language. There are 3 main numeric types: unsigned integers, signed integers, and floating point numbers. Additionally, one can create variables of these types with different amounts of precision. The full list of numeric types is listed below:
* ```u8```
* ```u16```
* ```u32```
* ```u64```
* ```i8```
* ```i16```
* ```i32```
* ```i64```
* ```f32```
* ```f64```
Additionally, there is a ```bool``` type and a ```void``` type. The ```bool``` type is either ```true``` or ```false```. The ```void``` type can only be used as a function return type.
One can modify these types with ```*``` and ```[N]```. ```*``` denotes a pointer, while ```[N]``` denotes an array of size ```N```. Array sizes must be known at compile time.
For example, to create a pointer to a double precision floating point number, we would write ```*f64```. To create an array of k unsigned 8-bit integers, we would write ```u8[k]``` (this is how strings are represented in Emu).
One can also defined structure types. The following is an example of a struct:
```
struct S {
	x: f64,
	y: *bool
}
```
This struct contains a field named ```x```, which is a double precision floating point number, and a field named ```y```, which is a pointer to a boolean. We can use struct types just like any other type, so ```S[4]``` is an array of 4 structs of type S.

## Declarations
All Emu programs consist of a series of top-level declarations. A top level declaration is a function, a struct, or a variable. We have alread seen what a structure looks like; the following is an example function:
```
pure func fact(n: i64): i64 {
	if (n <= 0) return 1;
	else return n * fact(n - 1);
}
```
A function consists of a list of modifiers (```pure```), a name (```fact```), a list of parameters (```n: i64```), a return type (```i64```), and a body (everything between the braces).
Variables declared at the top level (outside of a function) are global variables. The following is an example of a global variable:
```
z: f64 = 0.0;
```
This variable is accessible by any function.

## Function bodies
Most code in Emu are in function bodies, as functions describe what our programs actually do. Every statement inside a function must be terminated with a semicolon. The following are all the constructs that can occur inside functions:

### If(Else)
```
if (bool_condition) {
	// something
}
else if (other_bool_condition) {
	// something else
}
else {
	// something else again
}
```
The conditions for if statements must be ```bool``` typed.

### While
```
while (bool_condition) {
	// something
}
```

### For
```
for (initializer; bool_condition, increment) {

}
```
```initializer``` is a statement run right before the first iteration of the for loop. ```increment``` is a statement run at the end of each loop iteration.

### Return
```
return expr;
```
```expr``` is the expression we are returning. It must have the same type as the return type of the function (or be able to be implicitly casted to that type).

### Block
```
{
	// Something
}
```
Blocks denote a statement that is actually comprised of multiple statements. In fact, every set of braces you've seen so far (except for structure declarations) were blocks. Blocks also enclose a local scope, so variables declared in a block are not accessible outside the block.

### Variable Declaration (local)
```
x: i64 = 0;
y: f64[2] = [1.0, -1.0];
z: bool = undefined;
```
The *only* time one can use ```undefined``` is when declaring a variable - this just says that the variable will be declared, but not initialized.

### Assign
```
x = 3
```
One can also use operations such as ```+=```, which operate as expected (```x += 3``` is equivalent to ```x = x + 3```).

### Increment/Decrement
```
x++
x--
++x
--x
```

### Binary Operations
```
x + y
z / w
```

### Unary Operations
```
~a
!b
```

### Arrays
```
[1, 2, 3]
```

### Cast 
```
(f64) x
```

### Dereference / Address of
```
*x
&y
```

### Function Call
```
my_func(x, y, z)
```

### Comptime
```
x = comptime expensive_func(a, b, c)
```
This expression says that, at compile-time, we will evaluate the call to the expensive function, and then use assign ```x``` to the value returned by the function. This allows us to avoid redundant calculations in users' programs.

## Modifiers
Emu has 4 modifiers which can be used to annotate certain parts of your program.
* ```pure```
* ```const```
* ```inline```
* ```restrict```
### Pure
Pure can be used to annotate functions that have no side effects. That is, calling a function with the same arguments *always* returns the same result, and that function has no affect on the rest of the program and system. Not every function can be marked as pure (namely, impure ones). The compiler will optimize away calls to the same pure function that have the same arguments as a previous occurrence.

### Const
Const can be used to annotate variable declarations for variables whose value should not change after initialization.

### Inline
Inline can be used to annotate functions that should be inlined. Inlining is the process of replacing a call to a function with the body of the function. This can allow the compiler to avoid unnecessary function calls and better optimize the resulting code. Not every function can be declared as inlinable - dependencies between inline functions must form an acyclic graph (no recursive inline functions, no 2 inline functions that refer to each other, etc).

### Restrict
Restrict can be used to annotate function parameters that are pointers and are known (by the programmer) to not refer to overlapping regions of memory. This can lead to more optimization opportunities for the compiler.

## Building Emu Code
Emu compiles Emu source files into object files. These object files must be linked by a linker to create an executable. Unfortunately, Emu does not have the capacity to mark a main function with the _start flag needed by the linker, so you will have to link Emu object files with object files from another language containing a main function. You can see some examples of how to build Emu programs in the ```tests/c-tests``` directory.
