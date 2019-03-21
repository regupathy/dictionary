defmodule DictSearchTest do
  use ExUnit.Case
  doctest DictSearch


  test "greets the world" do
    tree = Dictionary.add(DictTree.new(),[1,2,4,5],"dfcg")
    tree = Dictionary.add(tree,[1,2,4,5,9],"jhhdd")
    tree = Dictionary.add(tree,[1,2,4,5,9],"aaaa")
    tree = Dictionary.add(tree,[5,2,3,6],"ddsc")
    IO.puts("#{inspect tree}")
    assertTrue is_map(tree)
  end
end
