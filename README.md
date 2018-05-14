# Interpreter
## Type definitions
```hs
data Var = ...   -- variable value
type Loc = ...   -- variable location
```

## Monad `Reader`
Used to store the environment - a mapping from an _identifier_ to a _location_.

```hs
import qualified Data.Map as DataMap

type Env = DataMap.Map Ident Loc
```

## Monad `State`
Used as a store - a mapping from a _location_ to the actual _value_. Both
functions and variables are stored in the same store.

```hs
import qualified Data.Map as DataMap

type Store = DataMap.Map Loc Var
```

## Monad `Except`
Used to handle runtime errors in the interpreter.

# Static type checking
## Monad `Reader`
Used to store the environment - a mapping from an _identifier_ to a _type_.

```hs
import qualified Data.Map as DataMap

type Env = DataMap.Map Ident Type
```

## Monad `Except`
Used to handle runtime errors in the interpreter.

# Files structure
## General
- __Main.hs__ mostly copied from a BNFC generated source code with small tweaks
 to start the interpreter on a given file.
- __Util.hs__ contains helper functions used in both interpreter and the type
 checker.
- __IInterpreter.hs__ defines types used in the interpreter.
- __TCheck.hs__ defines types used in the tupe checker.

## Interpreter
- __IExec.hs__ implements `exec` function, which execs the given program.
- __IUtil.hs__ contains helper functions used in the interpreter.

## Type checker
- __TExec.hs__ implements `execType` function, which runs the type check on
 the given program.
- __TUtil.hs__ contains helper functions used in the type checker.

## Methods
- __MArray.hs__ implements methods, which can be called on an _Array_ variable:
  - `Append` appends an element at the end of an array.
  - `At` returns an element at the given position.
  - `Put` inserts an element at the given position to an array.
  - `Length` returns the length of an array.
- __MError.hs__ implements methods, which can be called on an _Error_ variable.
  - `HaveOccurred` returns whether an error is not empty.
- __MInt.hs__ implements methods, which can be called on an _Int_ variable.
  - `ToString` converts an integer to its string representation.
- __MString.hs__ implements methods, which can be called on a _String_ variable.
  - `ToInt` converts a string to an integer, if the given string does not
   represent a decimal number calling this method will result in an
   `RuntimeError`.
- __MTuple.hs__ implements methods, which can be called on a _Tuple_ variable.
  - `Extract` extracts a single value from a tuple.
  - `Exchange` replaces a single element in a tuple.

All methods have a corresponding `*Type` function used within the type checker
to validate the type.
