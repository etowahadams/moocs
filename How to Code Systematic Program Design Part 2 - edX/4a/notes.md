## Self-Reference
##### List Mechanisms
Lists are the simplest form of arbitrary sized data. Uses `cons` notation
```
empty   ;empty list of values

(cons "Flames" empty)                ; a list of 1 element

(cons "Leafs" (cons "Flames" empty)) ; a list of 2 elements

(cons 1 (cons 2 (cons 3 empty)))     ; a list of 3 elements

(cons (string-append "C" "anucks") empty) ; expresssions which produce lists can be used 

```
- `cons` a two argument constructor
- `first` selects the first element of a list
- `rest` selects the elements after the first
- `empty?` produces true when argument is the empty list

Arbitrary data requires self-reference in type comment which incurrs a natural recursion

A well formed self-rederence:
- at least one base case
- at least one self reference case

Base case first in `check-expect`s 