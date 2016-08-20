## How to design functions

#### HtDF Recipe
Makes hard problem easier to solve, easy problems harder to solve. Every step of recipe is helps with all the steps after it. 

1. Signature, purpose, stub
2. Examples (wrapped in `check-expect`)
3. Inventory - template & constants
4. Code body
5. Test and debug

**Signature** - details the type of argument(s) and type of returned value(s). ex. `;; Number -> Number`

**Purpose** - explains the purpose of the function. ex. `;; produces 2 times the given number`

**Stub** - a mockup of the function where function body is simply the proper return type. Has correct function name, number of parameters, and produces dummy result of correct type. ex. `(define (double n) 0)`

**Template** - describes basic sturcture of the function, without the specific details (in code body). ex. `(define (double n) (... n))`

**Examples/tests** - helps understand what function does. Can also serve as unit tests because of `check-expect`. Tests should cover all code. ex. `(check-expect (double 3) 6)`



