import "std/modules/io.goose"

public module Array 
  declare slice[a](array: [a], idx: int): [a]
  declare length[a](array: [a]): int
  declare create[a](length: int, value: a): [a]
  declare push[a](array: Mutable[[a]], value: a): nil

  def empty[a](arr: [a]): bool do return Array::length(arr) == 0 end

  def concat[a](arr1: [a], arr2: [a]): [a] do
    def arr: Mutable[[a]] = mutable []
    for item in arr1 do
      Array::push(arr, item)
    end
    for item in arr2 do
      Array::push(arr, item)
    end
    return *arr
  end
  
  def map[a, b](arr: [a], f: fun(a): b): [b] do
    if Array::empty(arr) then return []
    def acc: Mutable[[b]] = mutable []
    for item in arr do
      Array::push(acc, f(item))
    end
    return *acc
  end

  def slice[a](array: [a], idx: int): [a] do
    def i = mutable 0
    def arr: Mutable[[a]] = mutable []
    while *i < Array::length(array) do
      if *i >= idx then Array::push(arr, array[*i])
      i = *i + 1
    end
    return *arr
  end

  def startsWith[a](arr: [a], prefix: [a]): bool do
    if Array::length(arr) < Array::length(prefix) then return false
    def i = mutable 0
    for item in prefix do
      if item != arr[*i] then return false
      i = *i + 1
    end
    return true
  end

  def reverse[a](arr: [a]): [a] do
    def i = mutable Array::length(arr) - 1
    def acc: Mutable[[a]] = mutable []
    while *i >= 0 do
      Array::push(acc, arr[*i])
      i = *i - 1
    end
    return *acc
  end
  
  declare has[a](dict: a, key: string): bool
end