# Haskell Interpreter for a Simple Imperative Language

## Overview
This repository contains an interpreter for a simple imperative programming language implemented in Haskell. The interpreter consists of three primary modules:

1. **Machine.hs**: Implements a virtual machine with a stack-based execution model.
2. **Interpreter.hs**: Defines the syntax and semantics of the imperative language.
3. **Main.hs**: Provides a user interface to interact with the interpreter.

## Files and Modules

### 1. Machine.hs
This module defines the fundamental components of the virtual machine:
- **State**: A map representing variable storage.
- **Stack**: A stack-based execution model for handling arithmetic computations.
- **Instructions**: A set of possible instructions such as `LOADI`, `LOAD`, `ADD`, `STORE`, and conditional jumps.
- **Execution Functions**:
  - `iexec`: Executes a single instruction.
  - `exec`: Executes a list of instructions sequentially.
  - Helper functions for stack operations (`push`, `pop`, `add`, etc.).

### 2. Interpreter.hs
This module defines the syntax and evaluation rules for the imperative language:
- **Arithmetic Expressions (`AExp`)**:
  - `N Val` (Integer values)
  - `V Vname` (Variables)
  - `Plus AExp AExp` (Addition)
- **Boolean Expressions (`BExp`)**:
  - `Less AExp AExp` (Comparison)
  - `And BExp BExp` (Logical AND)
  - `Not BExp` (Logical NOT)
- **Commands (`Com`)**:
  - `Assign Vname AExp`: Variable assignment
  - `Seq Com Com`: Sequential execution
  - `If BExp Com Com`: Conditional branching
  - `While BExp Com`: Looping construct
  - `SKIP`: No-op statement
- **Evaluation Functions**:
  - `aval`: Evaluates arithmetic expressions.
  - `bval`: Evaluates boolean expressions.
  - `eval`: Executes commands within a given state.

### 3. Main.hs
This module serves as the entry point, allowing users to:
- Load and parse imperative programs.
- Execute programs step-by-step or in full.
- View the state of variables after execution.

## Installation and Usage
### Prerequisites
Ensure you have the **GHC compiler** and **Stack** installed.

### Running the Interpreter
1. Clone the repository:
   ```sh
   git clone <repo-url>
   cd <repo-folder>
   ```
2. Compile the program:
   ```sh
   ghc --make Main.hs
   ```
3. Run the interpreter:
   ```sh
   ./Main
   ```

## Example Program
Consider the following example program:
```haskell
x := 5;
y := x + 3;
while (y > 0) do
  y := y - 1
end
```
The interpreter will evaluate this program and update the state accordingly.

## Future Enhancements
- Add support for more complex expressions and control structures.
- Implement a REPL mode for interactive execution.
- Improve error handling and debugging messages.

## License
This project is open-source under the MIT License.

