defmodtype StackT do
  $param item
  $opaque s
  callback new : list(item) -> s
end

defmodule StackInt do
  @behaviour StackT
  $param item = int
  $type s = list(int)
  def new (l : list(item)) : s = l
end

defmodule StackGeneric do
  $param a
  @behaviour StackT
  $param item = a
  $type s = list(item)
  def new (l : list(item)) : s = l
end