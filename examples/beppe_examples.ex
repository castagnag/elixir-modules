defmodtype StackT do
  $param item
  $param other
  $opaque s
  callback new : list(item) -> s
  callback push : item -> s -> s
  callback pop : s -> opt(item)
end

defmodule StackInt do
  @behaviour StackT
  $param item = int
  $param other = bool
  $type s = list(int)
  def new (l : list(item)) : s = l
  def push(x : item, st : s) : s = List.cons(x, st)
  def pop(st : s) : opt(item) = List.head(st)
end

defmodule StackIntCopy do
  @behaviour StackT
  $param item = int
  $param other = bool
  $type s = list(int)
  def new (l : list(item)) : s = l
  def push(x : item, st : s) : s = List.cons(x, st)
  def pop(st : s) : opt(item) = List.head(st)
end

defmodule Test do
  def push_zero(m : StackT(%{item = int; other = bool}), st : m.s) : m.s =
    m.push(0, st)
  def test_ok() : StackInt.s = push_zero(StackInt, StackInt.new([1]))
  def test2_ok() : opt(int) = StackInt.pop(test_ok())
  def test3_ok() : StackInt.s = push_zero
  def test1_ko() : opt(int) = StackIntCopy.pop(test3_ok())
  def test3_ko() : StackIntCopy.s = push_zero(StackIntCopy,test_ok())
end
