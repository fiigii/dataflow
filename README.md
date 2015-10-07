Data Flow Analysis

This program is written in Haskell and depends on the Javascript library called language-ecmascript 0.17.

You can run the code with cabal and js file must be in "test/":

$ cabal run while.js  
Preprocessing executable 'dataflow' for dataflow-0.1.0.0...  
Running dataflow...  

```
Kill Set:  
"1 : [z * y,y - 1]"  
"2 : [z * y]"  
"3 : []"
"4 : [z * y]"
"5 : [z * y,y - 1]"
"6 : [z * y,y - 1]"

Gen Set:
"1 : []"
"2 : []"
"3 : []"
"4 : []"
"5 : []"
"6 : []"

Entry
"1 : []"
"2 : []"
"3 : []"
"4 : []"
"5 : []"
"6 : []"

Exit
"1 : []"
"2 : []"
"3 : []"
"4 : []"
"5 : []"
"6 : []"
```
$ cabal run while2.js  
Preprocessing executable 'dataflow' for dataflow-0.1.0.0...  
Running dataflow...  

```
Kill Set:
"1 : []"
"2 : []"
"3 : []"
"4 : [a * b,a + 1,a + b]"
"5 : []"

Gen Set:
"1 : [a + b]"
"2 : [a * b]"
"3 : [a + b]"
"4 : []"
"5 : [a + b]"

Entry
"1 : []"
"2 : [a + b]"
"3 : [a + b]"
"4 : [a + b]"
"5 : []"

Exit
"1 : [a + b]"
"2 : [a * b,a + b]"
"3 : [a + b]"
"4 : []"
"5 : [a + b]"
```