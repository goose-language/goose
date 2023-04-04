import "std/modules/io.goose"

public module Array 
  declare slice[a](array: [a], idx: int): [a]
  declare length[a](array: [a]): int
  declare create[a](length: int, value: a): [a]
  declare next[a](array: [a]): [a]

  def empty(arr) return Array::length(arr) == 0

  def concat(arr1, arr2) do
    def acc = Array::create(Array::length(arr1) + Array::length(arr2), if Array::length(arr1) > 0 then arr1[0] else arr2[0])
    def i = 0
    for item in arr1 do
      acc[i] = item
      i = i + 1
    end
    for item in arr2 do
      acc[i] = item
      i = i + 1
    end
    return acc
  end
  
  def map(arr, f) do
    if Array::empty(arr) then return []
    def acc = Array::create(Array::length(arr), arr[0])
    def i = 0
    for item in arr do
      acc[i] = f(item)
      i = i + 1
    end
    return acc
  end
  
  def push(arr, value) do
    arr = Array::concat(arr, [value])
    return ()
  end

  def slice(array, idx) do
    def i = 0
    def arr = IO::clone(array)
    while i < idx do
      if Array::length(arr) == 1 then return []
      arr = Array::next(arr)
      i = i + 1
    end
    return arr
  end
end