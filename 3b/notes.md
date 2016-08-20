## Compound Data

##### define-struct
new kind of definition

```
(define-struct) pos (x y))

(define P1 (make-pos 3 6))    ; constructor

(pos-x P1)         ; selector, returns 3

pos?               ; predicate


```
- `pos` is the stucture name
- `(x y)` are the field names
- this would be a structure to help represent xy positions

###### Recipe
1. A possible structure defintion `(define-struct player (fn ln))`
2. A type comment ex. `;; Player is (make-player String String)
3. Interpretation
4. Example
5. Template 

``` 
(define (fn-for-player p)
	(... (player-fn p)   ;String
		 (plauer-ln p))) ;String
```


