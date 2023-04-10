import "std/modules/IO.goose"
import "std/modules/string.goose"

enum Option 
  Some(value)
  None
end

def main() do
  def factorial = fun (n) do
    if n == 0
      then return 1
      else return n * factorial(n - 1)
  end
  def user = IO::input("What is your name? ")
  IO::println("Hello {user}")
  IO::putsln("5! = {factorial(5)}")
end