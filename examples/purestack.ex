defmodule PureStack do
  $param elt
  def new(list_of_elements : list(elt)) : list(elt) = list_of_elements
  def push(s : list(elt), x : elt) : list(elt) =
    List.cons(x, s)
end

defmodtype PureStackType do
   $param elt
   callback new : list(elt) -> list(elt)
end