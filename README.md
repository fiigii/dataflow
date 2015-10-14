Data Flow Analysis

This program is written in Haskell and depends on the Javascript
library called language-ecmascript 0.17.

The worklist algorithm is in the function `solveMFP' of file
MonotoneFramework.hs .

You can run the code with cabal and js file must be in "test/":

```
> cabal run while.js
Preprocessing executable 'dataflow' for dataflow-0.1.0.0...
Running dataflow...

Available Expression
Entry
1 : []
2 : []
3 : []
4 : []
5 : []
6 : []
Exit
1 : []
2 : []
3 : []
4 : []
5 : []
6 : []

Very Busy Expression
Entry
1 : []
2 : []
3 : []
4 : [y - 1]
5 : []
6 : []
Exits
1 : []
2 : []
3 : []
4 : [z * y,y - 1]
5 : [y - 1]
6 : []

> cabal run while2.js
Preprocessing executable 'dataflow' for dataflow-0.1.0.0...
Running dataflow...

Available Expression
Entry
1 : []
2 : [a + b]
3 : [a + b]
4 : [a + b]
5 : []
Exit
1 : [a + b]
2 : [a * b,a + b]
3 : [a + b]
4 : []
5 : [a + b]

Very Busy Expression
Entry
1 : [a * b,a + b]
2 : [a + b]
3 : []
4 : [a + b]
5 : [a + b]
Exits
1 : [a * b,a + b]
2 : [a * b,a + b]
3 : [a + b]
4 : [a + 1]
5 : [a + b]

```
