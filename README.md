# Scheme Interpreter

A scheme interpreter that takes a "program" as input and returns the output. Example programs shown below:

( + ( - 10 2 ) 3 ) ---> 11

( + a 3 ) ---> undefined

( / 3 5 ) ---> 0

( block ( ( a 3 ) ( b 10 ) ) ( * a ( + b 1 ) ) ) ---> 33

( block ( ( a 3 ) ( b a ) ) ( + a b ) ) ---> undefined

( block ( ( x 3 ) ( y ( + 1 2) ) ( z ( block ( ( x 4 ) ) ( * x 2 ) ) ) ) ( + x ( + y z ) ) ) ----> 14

( block ( ( x 3 ) ) ( + ( block ( ( y x ) ) y ) 2 ) ) ---> 5
