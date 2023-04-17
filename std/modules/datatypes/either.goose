enum Either[a, b] 
  Left(a)
  Right(b)
end

// Either instances

module Either 
  def map[a, b, c](eit: Either[a, b], f: fun(b): c): Either[a, c] do
    return match eit do
      Left(a) -> Left(a),
      Right(b) -> Right(f(b))
    end
  end
end