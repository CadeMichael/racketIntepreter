# racketIntepreter
ProgrammingLanguageConceptsProject

## Members

- Cade 
- Xiangyi
- Zora 

## Part 1

### Goal

> write a function called interpret that takes a filename, calls parser with the filename, 
> evaluates the syntax tree returned by parser, and returns the proper value. You are to maintain a 
> state for the variables and return an error message if the user attempts to use a variable before it is 
> declared. You can use the Scheme function (error ...) to return the error.

### statements

1. **variable declaration**	(var variable) or (var variable value) : *cade*
2. **assignment**	(= variable expression) *cade*
3. **return**	(return expression) *Xiangyi*
4. **if statement**	(if conditional then-statement optional-else-statement) *Xiangyi/Zora*
5. **while statement**	(while conditional body-statement) *Xiangyi* 

### State 

- use **abstraction** to separate your state from the rest of your interpreter. As we increase the number of language features we have in future parts of the project, we will need to change how the state is implemented. If you correctly use abstraction, you will be able to redesign the state without changing the implementation of your interpreter. In this case, that means that the interpreter does not know about the structure of the state. Instead, you have generic functions that the interpreter can call to manipulate the state.

### Special notes for those who have not used github 

- **!!!!NEVER PUSH BEFORE PULLING, ALWAYS PULL FIRST OR YOU RISK OVERWRITING WHAT OTHERS HAVE DONE IN THE REPO!!!!**
- if you are using the terminal you will need 

```bash
# to clone the repo 
git clone https://github.com/CadeMichael/racketIntepreter/

# to add changes, commit, and push
git add . && git commit -m '[your commit message, it can be anything]' && git push
```
