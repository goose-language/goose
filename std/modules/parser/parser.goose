import "std/modules/datatypes/either.goose"
import "std/modules/datatypes/tuple.goose"
import "std/modules/array.goose"

type Token = { type_: string, value: string }
type ParserResult[a] = Tuple[Either[string, a], [Token]]
type Parser[a] = fun([Token]): ParserResult[a] 

def runParser[b](p: Parser[b], tokens: [Token]): Either[string, b] do
  match p(tokens) do
    Tuple(Left(err), _) -> return Left(err),
    Tuple(Right(result), _) -> return Right(result)
  end
end

module Parser 
  def satisfy(pred: fun(Token): bool): Parser[Token] do
    return fun (tokens) do
      if Array::empty(tokens)
        then return Tuple(Left("Unexpected end of input"), tokens)
        else
          if pred(tokens[0]) then
            return Tuple(Right(tokens[0]), Array::slice(tokens, 1))
          else
            return Tuple(Left("satisfy failed"), tokens)
    end
  end

  def return_[a](result: a): Parser[a] do
    return fun (tokens) do
      return Tuple(Right(result), tokens)
    end
  end

  def bind[a, b](p: Parser[a], f: fun(a): Parser[b]): Parser[b] do
    return fun (tokens) do
      match p(tokens) do
        Tuple(Right(result), rest) -> return f(result)(rest),
        Tuple(Left(err), _) -> return Tuple(Left(err), tokens)
      end
    end
  end
  
  def between[a, b, c](open: Parser[a], close: Parser[b], p: Parser[c]): Parser[c] do
    return Parser::bind(open, fun (_) do
      return Parser::bind(p, fun (result) do
        return Parser::bind(close, fun (_) do
          return Parser::return_(result)
        end)
      end)
    end)
  end

  def lazyBetween[a, b, c](open: Parser[a], close: Parser[b], p: fun(): Parser[c]): Parser[c] do
    return fun(tokens) do 
      return Parser::between(open, close, p())(tokens)
    end
  end

  def token(type_: string): Parser[Token] do
    return Parser::satisfy(fun (token) do
      return token.type_ == type_
    end)
  end

  def string(value: string): Parser[Token] do
    return Parser::satisfy(fun (token) do
      return token.value == value
    end)
  end

  def reserved(value: string): Parser[Token] do
    return Parser::satisfy(fun (token: Token) do
      return token.type_ == "reserved" && token.value == value
    end)
  end

  def or[a](p1: Parser[a], p2: Parser[a]): Parser[a] do
    return fun (tokens) do
      match p1(tokens) do
        Tuple(Right(result), rest) -> return Tuple(Right(result), rest),
        Tuple(Left(_), _) -> return p2(tokens)
      end
    end
  end

  def choice[a](ps: [Parser[a]]): Parser[a] do
    return fun (tokens) do
      if Array::empty(ps) then
        return Tuple(Left("choice failed"), tokens)
      else
        match ps[0](tokens) do
          Tuple(Right(result), rest) -> return Tuple(Right(result), rest),
          Tuple(Left(_), _) -> return Parser::choice(Array::slice(ps, 1))(tokens)
        end
    end
  end

  def andThen[a, b](p1: Parser[a], p2: Parser[b]): Parser[b] do
    return Parser::bind(p1, fun (_) do
      return p2
    end)
  end
  
  def andThenLazy[a, b](p1: fun(): Parser[a], p2: fun(): Parser[b]): Parser[b] do
    return fun (tokens) do
      return Parser::andThen(p1(), p2())(tokens)
    end
  end

  def andThenBound[a, b](a: Parser[a], b: Parser[b]): Parser[Tuple[a, b]] do
    return return Parser::bind(a, fun (x) do
      return Parser::bind(b, fun (y) do
        return Parser::return_(Tuple(x, y))
      end)
    end)
  end

  def andThenBoundLazy[a, b](a: fun(): Parser[a], b: fun(): Parser[b]): Parser[Tuple[a, b]] do
    return fun (tokens) do
      return Parser::andThenBound(a(), b())(tokens)
    end
  end

  def many[a](p: Parser[a]): Parser[[a]] do
    return fun (tokens) do
      match p(tokens) do
        Tuple(Right(result), rest) -> 
          match Parser::many(p)(rest) do
            Tuple(Right(results), rest_) -> return Tuple(Right(Array::concat([result], results)), rest_),
            Tuple(Left(err), _) -> return Tuple(Left(err), rest)
          end,
        Tuple(Left(_), _) -> return Tuple(Right([]), tokens)
      end
    end
  end

  def sepBy1[a, b](p: Parser[a], sep: Parser[b]): Parser[[a]] do
    return Parser::bind(p, fun (result) do
      return Parser::bind(Parser::many(Parser::andThen(sep, p)), fun (results) do
        return Parser::return_(Array::concat([result], results))
      end)
    end)
  end

  def sepBy[a, b](p: Parser[a], sep: Parser[b]): Parser[[a]] do
    return Parser::or(Parser::sepBy1(p, sep), Parser::return_([]))
  end

  def lazySepBy[a, b](p: fun(): Parser[a], sep: fun(): Parser[b]): Parser[[a]] do
    return fun (tokens) do
      return Parser::sepBy(p(), sep())(tokens)
    end
  end

  def try[a](p: Parser[a]): Parser[a] do
    return fun (tokens) do
      match p(tokens) do
        Tuple(Right(result), rest) -> return Tuple(Right(result), tokens),
        Tuple(Left(err), _) -> return Tuple(Left(err), tokens)
      end
    end
  end

  def lazyMany[a](p: fun(): Parser[a]): Parser[[a]] do
    return fun (tokens) do
      match p()(tokens) do
        Tuple(Right(result), rest) -> 
          match Parser::lazyMany(p)(rest) do
            Tuple(Right(results), rest_) -> return Tuple(Right(Array::concat([result], results)), rest_),
            Tuple(Left(err), _) -> return Tuple(Left(err), rest)
          end,
        Tuple(Left(_), _) -> return Tuple(Right([]), tokens)
      end
    end
  end

  def option[a](p: Parser[a], default: a): Parser[a] do
    return Parser::or(p, Parser::return_(default))
  end

  def lazyOption[a](p: fun(): Parser[a], default: a): Parser[a] do
    return fun (tokens) do
      return Parser::option(p(), default)(tokens)
    end
  end
end