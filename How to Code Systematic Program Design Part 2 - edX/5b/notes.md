## Helpers
Use function composition when two or more distinct operations have to be made on consumed data 
```
;<template as function composition>
(define (next-drops lod)
  (filter-drops (tick-drops lod)))
```
When to use helpers
- difference between quantities in a problem.
- arbitrary sized data a helper function must be used.
- special domain knowledge a helper function should be used.
- one task per function
