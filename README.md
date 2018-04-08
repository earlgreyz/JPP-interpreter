# Program
program ::= instr

# Typy
## `void`

## `int`
```
var intVar int = 0
intVar.ToString() -> str
```

## `bool`
```
var boolVar bool = false
boolVar.ToString() -> str
```
## `error`
```
var errorVar error = ``
errorVar.ToString() -> str
```

## `string`
```
var stringVar string = ""
stringVar.ParseToInt() -> int, err
```

## `array`
```
var arrayVar [type] = [1, 2]
arrayVar.ToString() -> string
arrayVar.Length() -> int
arrayVar.Insert(value type) -> void
arrayVar.Get(index int) -> type, err
```

## `map`
```
var mapVar {typeA, typeB} = {1: 1, 2: 2}
mapVar.ToString() -> string
mapVar.Length() -> int
mapVar.Insert(key typeA, value typeB) -> void
mapVar.Get(key typeA) -> typeB, err
```

## `tuple`
```
var tupleVar <typeA, typeB, ...> = <1, 2, ...>
tupleVar.ToString() -> string
x, y, z = tupleVar
```

## `function`
```
type -> type
var funcVar int -> int = (x int) => int { return x**2 }
funcVar(2)
```

# Deklaracja zmiennych
```
var k int = 2
```

# Przypisanie
```
x = 2
y = 3
```

# Instrukcje warunkowe
```
if b then {

} elif {

} else {

}
```

```
if b then {

}
```

# Pętle

```
while b {
  break
  continue
}
```

```
for x = 1 to 50 {

}
```

# Wyrażenia arytmetyczne
```
1 + 2 * (3 - 4) / 2 + 2**7
```

# Wyrażenia bool
```
true
false
p and q
p or q
not p
e < f
e == f
e <= f
e > f
e >= f
fun(3, 4, fun2(3))
```

# Funkcje
```
func f(a int) -> <int, error> {
  return <42, _>
}
```

# Wbudowane funkcje
```
print(s string)
int.ToString() -> string
string.ToInt() -> <int, error>
```
