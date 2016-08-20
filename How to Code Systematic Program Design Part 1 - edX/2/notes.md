## How to Design Data

##### cond Expressions
Format: 
```
(cond [<expression> <expression>])]
	  ...)
```
where the first expression is the question, the second is an answer, and the last question can be `else`.

1. If no question/answer pairs, return error
2. Replace `cond` with new `cond` where first question is replaced by its value
3. If first question is true/else, replace entire `cond` with answer
4. If first question is false, drop the first q/a pair and replace with new `cond` 

##### Data Definitions
Data definitions explain how information is represented as data. This affects the design of functions that operate on that data.

Programs, through data, have to interpret information in the proplem domain. ex. "a light is red" represented by a "0" in the program data

Data definition tells us everything we need to know about what is being represented. If describes how to
- form a data of a new type
- represent info as data
- interpret data as information
- template for operating on data

It simplifies functions by
- restricting data consumed
- restricting data produced
- generating examples
- providing tempates

###### Recipe:

1. Possible structure
2. Type comment which defines new type name and describes how to form data of that type
3. Interpretation that describes relation between info and data
4. One or more examples
5. Template of a 1 argument function operation on data type

