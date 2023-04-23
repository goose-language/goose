import "std/modules/io.goose"

public module Array 
  declare slice[a](array: [a], idx: int): [a]
  declare length[a](array: [a]): int
  declare create[a](length: int, value: a): [a]
  declare push[a](array: [a], value: a): nil

  def empty[a](arr: [a]): bool do return Array::length(arr) == 0 end

  def concat[a](arr1: [a], arr2: [a]): [a] do
    return arr1 + arr2
  end
  
  def map[a, b](arr: [a], f: fun(a): b): [b] do
    if Array::empty(arr) then return []
    def acc2: [b] = []
    for item in arr do
      acc2 = acc2 + [f(item)]
    end
    return acc2
  end

  def slice[a](array: [a], idx: int): [a] do
    def i = 0
    def arr: [a] = []
    while i < Array::length(array) do
      if i >= idx then arr = arr + [array[i]]
      i = i + 1
    end
    return arr
  end

  def sliceFromTo[a](array: [a], from: int, to: int): [a] do
    def i = 0
    def arr: [a] = []
    while i < Array::length(array) do
      if i >= from && i < to then arr = arr + [array[i]]
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

  def reverse[a](arr: [a]): [a] do
    def i = Array::length(arr) - 1
    def acc1: [a] = []
    while i >= 0 do
      acc1 = acc1 + [arr[i]]
      i = i - 1
    end
    return acc1
  end
  
  declare has[a](dict: a, key: string): bool
end