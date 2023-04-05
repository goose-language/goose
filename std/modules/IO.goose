public module IO 
  declare print[a](input: a): nil

  def println(input) do
    IO::print(input);
    IO::print('\n')
  end

  declare clone[a](value: a): a
  declare readFile(filename: [char]): [char]
  declare writeFile(filename: [char], content: [char]): nil
  declare input([char]): [char]
  declare fileExists(filename: [char]): bool
  declare readDirectory(path: [char]): [[char]]

  def puts(input) do
    for i in input do
      IO::print(i)
    end
  end

  def putsln(input) do
    IO::puts(input)
    IO::print('\n')
  end

  declare exit[a](code: int): a

  def throw(message) do
    IO::println(message)
    IO::exit(1)
  end
end