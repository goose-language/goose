import "std/modules/parser/lexer.goose"
import "std/modules/parser/parser.goose"
import "std/modules/map.goose"
import "std/modules/IO.goose"
module JSON 
  def initTokens() do
    return [
      { type_: "number", regex: "^[[:digit:]]+" },
      { type_: "lbrace", regex: "^\\\{" },
      { type_: "rbrace", regex: "^\\}" },
      { type_: "rbracket", regex: "^\\]" },
      { type_: "lbrakcet", regex: "^\\[" },
      { type_: "bool", regex: "^(true|false)" },
      { type_: "separator", regex: "^(:)" },
      { type_: "null", regex: "^(null)" },
      { type_: "whitespace", regex: "^([[:space:]]+)", ignore: true },
      { type_: "string", regex: "^\"([^']*)\"" },
      { type_: "comma", regex: "^," },
      { type_: "operator", regex: "^([[:punct:]]+)" }
    ]
  end

  enum JSON 
    Number(string)
    String(string)
    Bool(bool)
    Null
    Array([JSON::JSON])
    Object(Map[string, JSON::JSON])
  end

  declare parser(): Parser[JSON::JSON]

  def array() do
    return Parser::bind(
      Parser::andThen(
        Parser::token("lbracket"),
        Parser::andThenBound(
          Parser::lazyMany(fun() JSON::parser()),
          Parser::token("rbracket")
        )
      ),
      fun(x) do
        IO::println(x)
        return Parser::return_(JSON::Null())
      end
    )
  end

  def object() do
    return Parser::bind(
      Parser::andThen(
        Parser::token("lbrace"),
        Parser::andThenBound(
          Parser::sepBy(
            Parser::andThenBound(
              Parser::token("string"),
              Parser::andThenBoundLazy(
                fun() Parser::token("separator"),
                fun() JSON::parser()
              )
            ),
            Parser::token("comma")
          ),
          Parser::token("rbrace")
        )
      ),
      fun(x) do
        match x do
          Tuple(exprs, _) -> do
            IO::println(exprs)
            def map = Empty()
            for expr in exprs do
              match expr do
                Tuple({ value: key }, Tuple(_, value)) -> do
                  map = Map::insert(Array::sliceFromTo(key, 1, Array::length(key) - 1), value, map)
                end
              end
            end
            return Parser::return_(JSON::Object(map))
          end
        end
      end
    )
  end

  def number() do
    return Parser::bind(
      Parser::token("number"),
      fun(x) do
        return Parser::return_(JSON::Number(x.value))
      end
    )
  end

  def string() do
    return Parser::bind(
      Parser::token("string"),
      fun(x) do
        return Parser::return_(JSON::String(Array::sliceFromTo(x.value, 1, Array::length(x.value) - 1)))
      end
    )
  end

  def bool() do
    return Parser::bind(
      Parser::token("bool"),
      fun(x) do
        return Parser::return_(JSON::Bool(x.value == "true"))
      end
    )
  end

  def parser() do
    return Parser::choice([
      JSON::number(),
      JSON::string(),
      JSON::bool(),
      JSON::object(),
      JSON::array()
    ])
  end

  def parse(input: string) do
    def rules = JSON::initTokens()
    def tokens = Lexer::tokenize(rules, input)
    // IO::println(tokens)
    def ast = runParser(JSON::parser(), tokens)
    return ast
  end
end