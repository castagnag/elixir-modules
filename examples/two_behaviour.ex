defmodtype StackT do
  $param item
  $opaque s
  callback new : list(item) -> s
  callback push : item -> s -> s
end

defmodtype GenServerT do
  $param state
  $param request
  callback handle_call : request -> state -> state
end

defmodule StackServer do
  $param a

  @behaviour StackT
  $param item = a
  $type s = list(a)
  def new (l : list(item)) : s = l
  def push (x : item, l : s) : s = cons(x, l)

  @behaviour GenServerT
  $param state = s
  $param request = a
  def handle_call(x : item, s : state) : state =
    push(x, s)
end
  