import "std/modules/array.goose"
import "std/modules/IO.goose"
import "std/modules/regex.goose"

def not(b) return if b then false else true

def factorial(n) do
  def acc = 1
  while n > 0 do
    acc = acc * n
    n = n - 1
  end
  return acc
end

module Option
  def Some(value) do
    return { type: "Some", value: value }
  end
  def None() do
    return { type: "None" }
  end
end

module Option 
  def map(option, f) do
    if option.type == "Some" then return Option::Some(f(option.value))
    else return Option::None()
  end
end

def main() do
  IO::println(Option::map(Option::Some(1), (x) -> x + 1))
  IO::println(Regex::match("hello world", "[[:alpha:]]+"))
  return 0
end
