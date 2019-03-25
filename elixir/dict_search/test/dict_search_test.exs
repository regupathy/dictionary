defmodule DictSearchTest do
  use ExUnit.Case
  doctest Dictionary


  setup_all do
    {:ok,pid} =  Dictionary.start
    Dictionary.load("dictionary.txt")
    {:ok,pid: pid}
  end

  test "dictionary search for 6686787825", _context do
    {time,words} = :timer.tc(fn-> Dictionary.search 6686787825 end )
     assert  div(time,1000) < 1000
     IO.puts("Dictionary search for 6686787825  : \n #{inspect words} \n")
     assert ["MOTORTRUCK"] == hd(words)
     assert( Enum.member?(words,["NOUNS", "USUAL"]))
     assert( Enum.member?(words,["ONTO", "STRUCK"]))
     assert( Enum.member?(words,["MOTOR", "USUAL"]))
  end

  test "dictionary search for 2282668687", _context do
    {time,words} = :timer.tc(fn-> Dictionary.search 2282668687 end )
    assert  div(time,1000) < 1000
    IO.puts("Dictionary search for 2282668687  : \n #{inspect words} \n")
    assert(["CATAMOUNTS"] == hd(words))
    assert( Enum.member?(words,["ACTA", "MOUNTS"]))
    assert( Enum.member?(words,["BAT", "CONTOUR"]))
    assert( Enum.member?(words,["ACT", "AMOUNTS"]))
  end

  test "adding words to dictionary" do
    tree = Dictionary.add(DictTree.new(),[1,2,4,5],"dfcg")
    tree = Dictionary.add(tree,[1,2,4,5,9],"jhhdd")
    tree = Dictionary.add(tree,[1,2,4,5,9],"aaaa")
    tree = Dictionary.add(tree,[5,2,3,6],"ddsc")
    assert is_map(tree)
  end



end
