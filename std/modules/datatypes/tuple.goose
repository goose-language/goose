enum Tuple[a, b] 
  Tuple(a, b)
end

module Tuple 
  def fst[t, y](tup: Tuple[t, y]): t do
    match tup do
      Tuple(fst, _) -> return fst
    end
  end

  def snd[t, y](tup: Tuple[t, y]): y do
    match tup do
      Tuple(_, snd) -> return snd
    end
  end

  def map[a, b, c](tup: Tuple[a, b], f: fun(a): c): Tuple[c, b] do
    return Tuple(f(Tuple::fst(tup)), Tuple::snd(tup))
  end

  def bimap[a, b, c, d](tup: Tuple[a, b], f: fun(a): c, g: fun(b): d): Tuple[c, d] do
    return Tuple(f(Tuple::fst(tup)), g(Tuple::snd(tup)))
  end
end