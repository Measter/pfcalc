# pfcalc
A command line postfix calculator.

##Builtin Functions
Exposes the following `f64` functions from Rust's stdlib:

* abs
* ceil
* floor
* exp
* ln
* log10
* log
* sqrt
* to_radians (as d2rad)
* to_degrees (as r2deg)
* round
* cos
* cosh
* acos
* acosh
* sin
* sinh
* asin
* asinh
* tan
* tanh
* atan
* atanh
* atan2
* sum
* product

And the following constants:

* pi
* e

## Custom functions
Custom functions can be defined in the following manner:

`<args> <name> = <body>`

Arguments are given in the same order as they are supplied at the call site. Arguments are optional.

For example, a function to calculate the height of a triangle with the angle in degrees would look like this:

`len angle height = angle d2rad sin len *`

Built in functions are handled before custom functions, so a custom function with the same name as a builtin function will never be called.