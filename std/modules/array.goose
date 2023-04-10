import "std/modules/io.goose"

public module Array 
  declare slice[a](array: [a], idx: int): [a]
  declare length[a](array: [a]): int
  declare create[a](length: int, value: a): [a]
  declare next[a](array: [a]): [a]

  def empty[a](arr: [a]): bool do return Array::length(arr) == 0 end

  def concat[a](arr1: [a], arr2: [a]): [a] do
    def acc: [a] = Array::create(Array::length(arr1) + Array::length(arr2), if Array::length(arr1) > 0 then arr1[0] else arr2[0])
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

  def push[a](arr: [a], value: a): nil do
    arr = Array::concat(arr, [value])
    return nil
  end
  
  def map[a, b](arr: [a], f: fun(a): b): [b] do
    if Array::empty(arr) then return []
    def acc = []
    for item in arr do
      Array::push(acc, f(item))
    end
    return acc
  end

  def slice[a](array: [a], idx: int): [a] do
    def i = 0
    def arr = IO::clone(array)
    while i < idx do
      if Array::length(arr) == 1 then return []
      arr = Array::next(arr)
      i = i + 1
    end
    return arr
  end

  def startsWith[a](arr: [a], prefix: [a]): bool do
    if Array::length(arr) < Array::length(prefix) then return false
    def i = 0
    for item in prefix do
      if item != arr[i] then return false
      i = i + 1
    end
    return true
  end

  def sort[a](array: [a], f: fun(a, b): bool): [a] do
    def arr = IO::clone(array)
    def i = 0
    def j = 0
    def len = Array::length(arr)
    while i < len do
      j = i
      while j > 0 do
        if f(arr[j], arr[j - 1]) then do
          def tmp = arr[j]
          arr[j] = IO::clone(arr[j - 1])
          arr[j - 1] = tmp
        end
        j = j - 1
      end
      i = i + 1
    end
    return arr
  end

  def reverse[a](arr: [a]): [a] do
    def i = 0
    def j = Array::length(arr) - 1
    def tmp = 0
    while i < j do
      tmp = arr[i]
      arr[i] = arr[j]
      arr[j] = tmp
      i = i + 1
      j = j - 1
    end
    return arr
  end
  
  declare has[a](dict: a, key: string): bool
end