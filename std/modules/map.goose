
enum Map[k, a] 
  Empty
  Cons(k, a, Map[k, a])
end

module Map
  def insert[k, a](key: k, value: a, map: Map[k, a]): Map[k, a] do
    match map do
      Empty() -> return Cons(key, value, Empty()),
      Cons(k, v, m) ->
        if key == k
          then return Cons(key, value, m)
          else return Cons(k, v, Map::insert(key, value, m))
    end
  end
end