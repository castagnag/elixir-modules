defmodtype StackT do
  $param item
  $opaque s
  callback new : list(item) -> s
  callback push : item -> s -> s
end

defmodule StackInt do
  @behaviour StackT
  $param item = int
  $type s = list(int)
  def new (l : list(item)) : s = l
  def push(x : item, st : s) : s = List.cons(x, st)
end

defmodule Test do
  defp push_zero(m : StackT(%{item = int}), st : m.s) : m.s =
    m.push(0, st)
end