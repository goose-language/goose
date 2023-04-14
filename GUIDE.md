# A fast tour of Goose language
Discover a new programming language, with a syntax inspired by Ruby, that is both functional and imperative. Discover the syntax, its features and how to use it.

Once you have installed Goose, you can start by writing your first "Hello world" program with the following code:
```ruby
def main() do
  IO::println("Hello, world!")
end
```
And then compile it with:
```bash
$ goose <your_file>.goose
```
You should see two files created:
  - One for the compiled CLang output which is basically a C file containing the compiled Goose code which is specific to your machine (due to imports and other things).
  - One for the compiled Goose output which is a binary file that you can run.

## Variables, scopes and types

Goose is a statically typed language, which means that you have to specify the type of your variables. You can do so by using the `:` operator, like so:
```rb
def x: int = 10
def y: float = 10.0
def z: string = "Hello"
```

But you can also let the compiler infer the type of your variables, like so:
```rb
def x = 10 // Will be inferred as int
def y = 10.0 // Will be inferred as float
def z = "Hello" // Will be inferred as string
```

A variable is scoped, meaning that it is only accessible inside the scope it was declared in. For example:
```rb
def x = 10
def main() do
  def x = 20
  IO::println(x) // Will print 20 and not 10
  do 
    def x = 30
    IO::println(x) // Will print 30 and not 20
  end
  IO::println(x) // Will print 20 and not 30
end
```

Do-end block is used to create a new block of code and so a new variable scope. 

## Functions, higher order functions and lambdas
A function is basically a declaration that binds a name to a higher order function. To make things clearer, a function is a list of instructions that relies on a list of arguments defined by the user. 

A higher order function is a function that takes a function as an argument or returns a function.

And finally, a lambda is an anonymous function that can be passed as an argument to another function.

Here is an example of a function:
```rb
def add(x: int, y: int) do
  return x + y
end
```

Here is an example of a higher order function:
```rb
def map[a, b](list: [a], f: fun(a): b): [b] do
  ...
end
```
> Where `a` and `b` are generics and `f` is a function that takes an `a` and returns a `b`.

Here is an example of a lambda:
```rb
def main() do
  def f = fun(x: int): int do
    return x + 1
  end
  IO::println(f(10)) // Will print 11
end
```

## Imperative programming
Goose is both a functional and imperative programming language. This means that you can use both paradigms in your code.

### Imperative-style loops
Goose has two types of loops: `while` and `for`. Here is an example of a `while` loop:
```rb
def main() do
  def x = mutable 0
  while *x < 10 do
    IO::println(x)
    x = *x + 1
  end
end
```
> The `*` operator is used to dereference a mutable variable. It is used to get the value of a mutable variable.

Here is an example of a `for` loop:
```rb
def main() do
  for i in [0, 1, 2, 3] do
    IO::println(i)
  end
end
```

### Imperative-style conditionals

Goose has two types of conditionals: `if` and `match`. Here is an example of an `if` conditional:
```rb
def main() do
  def x = 10
  if x < 10 do
    IO::println("x is less than 10")
  else if x > 10 do
    IO::println("x is greater than 10")
  else
    IO::println("x is equal to 10")
  end
end
```

Here is an example of a `match` conditional:
```rb
def main() do
  def x = 10
  match x do
    0 -> IO::println("x is equal to 0")
    10 -> IO::println("x is equal to 10")
    _ -> IO::println("x is not equal to 0 or 10")
  end
end
```
Match is a instruction that takes a value and a list of patterns, meaning that we can match a value against a pattern. If the pattern matches, the instruction will execute the code associated with the pattern. If the pattern does not match, the instruction will try to match the value against the next pattern. If no pattern matches, the instruction will execute the code associated with the `_` pattern.

At least one pattern must match, otherwise the program will crash because of a non-exhaustive match.

### Mutability

Goose has two types of variables: `mutable` and `immutable`. Mutable variables can be changed, while immutable variables cannot. Here is an example of a mutable variable:
```rb
def main() do
  def x = mutable 10
  IO::println(*x) // Will print 10
  x = 20
  IO::println(*x) // Will print 20
end
```
To access a mutable varibale value, you have to dereference it with the `*` operator.

Here is an example of an immutable variable:
```rb
def main() do
  def x = 10
  IO::println(x) // Will print 10
  x = 20 // Will crash because x is immutable
end
```

## Functional programming

Goose is both a functional and imperative programming language. This means that you can use both paradigms in your code.

### Functional-style loops

Goose has two types of loops: `map` and `reduce`. Here is an example of a `map` loop:
```rb
def main() do
  def x = Array::map([0, 1, 2, 3], fun(x: int): int do
    return x + 1
  end)
  IO::println(x) // Will print [1, 2, 3, 4]
end
```

Here is an example of a `reduce` loop:
```rb
def main() do
  def x = Array::reduce([0, 1, 2, 3], 0, fun(acc: int, x: int): int do
    return acc + x
  end)
  IO::println(x) // Will print 6
end
```

### Functional-style conditionals

Goose has two types of conditionals: `if` and `match` that can this time be used as expression instead of statements. Here is an example of an `if` conditional:
```rb
def main() do
  def x = 10
  def y = if x < 10 do
    0
  else if x > 10 do
    1
  else
    2
  end
  IO::println(y) // Will print 2
end
```

Here is an example of a `match` conditional:
```rb
def main() do
  def x = 10
  def y = match x do
    0 -> 0
    10 -> 1
    _ -> 2
  end
  IO::println(y) // Will print 1
end
```

## Modules

Goose has modules, which are basically namespaces. A module is a file that contains a list of declarations. A declaration is a variable, a function, a type... Here is an example of a module:
```rb
module Addition 
  def x = 10
  def y = 20
  def add(x: int, y: int): int do
    return x + y
  end
end
```
> The `module` keyword is used to declare a module. We can access this module by pointing by the name like so: `Addition::x` or `Addition::add`.

## Types

Goose has types, which are basically a set of values. A type is a declaration that binds a name to a type. Here is an example of a type:
```rb
type string = [char]
```
> Type is used as a type alias system. It's used to give a name to a type, as a shortcut.

## Surpowered enumerations

Goose has enumerations, which are basically a set of values. An enumeration is a declaration that binds a name to a list of values. Here is an example of an enumeration:
```rb
enum Color 
  Red
  Green
  Blue
end
```

But you can also specify some arguments to each variant of the enumeration, like so:
```rb
enum Color 
  Red
  Green
  Blue
  Custom(r: int, g: int, b: int)
end
```

You can then use them like that in your code:
```rb
def main() do
  def x = Red
  def y = Custom(255, 0, 0)
end
```

## Ditionaries

Goose has dictionaries, which are basically a set of key-value pairs. In Goose, a dictionnary can be anonymous, like so
```rb
def main() do
  def user = {
    username: "Thomas",
    age: 17
  }
  IO::println(user)
end
```
> Here user will have the following type `{ username: string, age: int }`.
