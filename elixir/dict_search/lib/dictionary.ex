defmodule Dictionary do
  @moduledoc """
  Dictionary is a storage mechanism to store all given words in a Tree format.

  I have used Map to represent the Tree Structure


"""

  import DictTree

  alias __MODULE__ Dictionary

  def start(), do: Agent.start(fn -> DictTree.new end,name: Dictionary )

  def getDict(), do: Agent.get(Dictionary,& &1)


  def search(number,tree), do: number_to_digits(number) |> search(tree,[])

  defp search([digit|[]],tree,acc), do: DictTree.takeNode(digit,tree) |> search1([],tree,acc)
  defp search([digit|rest],tree,acc), do: DictTree.takeNode(digit,tree) |> search1(rest,tree,acc)

  defp search1(nil,[],_tree,_acc), do: []
  defp search1(dictNode,[],_tree,acc), do: dictNode |> DictNode.getWord() |> fn [] -> [];words -> zip(words,acc) end
  defp search1(nil,_rest,_tree,_acc), do: []
  defp search1(dictNode,rest,tree,acc), do: dictNode |> DictNode.isEmptyNode()
                                            |> fn true -> []; false -> search2(dictNode,rest,tree,acc) end
  defp search2(dictNode,keys,tree,acc), do: dictNode |> DictNode.isPerfectNode()
                                            |> fn true -> search(keys,getDict(),[DictNode.getWord(dictNode)|acc])
                                                          ++ search(keys,tree,acc)
                                                  false -> search3(dictNode,keys,tree,acc)  end
  defp search3(dictNode,keys,tree,acc), do: dictNode |>  DictNode.getWord() == []
                                            |> fn true -> search(keys,tree,acc)
                                                  false -> search(keys,getDict(),[DictNode.getWord(dictNode)|acc]) end

  def add(tree,[],_), do:  tree
  def add(tree,[lastKey],word), do: addNode(tree,lastKey,word)
  def add(tree,[key|rest],word) do
    dictTree = getNode(tree,key)
    dictTree |> DictNode.getSubTree |> add(rest,word) |> update_subTree(dictTree) |> replace(tree,key)
  end

  defp update_subTree(subTree,dictNode), do: %{dictNode | subTree: subTree}


  def load(path) do
    case File.read(path) do
      {:ok,content} -> upload(content,[])
      _ -> IO.puts("Error in loading File #{path}")
    end end

  defp upload("",_), do: :ok
  defp upload(<<" ",rest/:binary>>,temp), do: upload(rest,temp)
  defp upload(<<"\r",rest/:binary>>,temp), do: upload(rest,temp)
  defp upload(<<"\n">>,rest/:binary,temp)when size(temp) >= 3 do update(temp)
                                                                 upload(rest,Enum.reverse(temp)) end
  defp upload(<<"\n">>,rest/:binary,temp), do: upload(rest,[])
  defp upload(<<a,rest/:binary>>,temp), do: upload(rest,[a|temp])

  defp update(word), do: Agent.update(Dictionary,fn(tree) -> add(tree,gen_key(word),word) end)

  defp gen_key(word), do: gen_key(String.upcase(word),[])
  defp gen_key([],acc), do: :ok
  defp gen_key([char|rest],acc)when(char>=?A and Char<=?C),do: gen_key(rest,[2|acc])
  defp gen_key([char|rest],acc)when(char>=?D and Char<=?F),do: gen_key(rest,[3|acc])
  defp gen_key([char|rest],acc)when(char>=?G and Char<=?I),do: gen_key(rest,[4|acc])
  defp gen_key([char|rest],acc)when(char>=?J and Char<=?L),do: gen_key(rest,[5|acc])
  defp gen_key([char|rest],acc)when(char>=?M and Char<=?O),do: gen_key(rest,[6|acc])
  defp gen_key([char|rest],acc)when(char>=?P and Char<=?S),do: gen_key(rest,[7|acc])
  defp gen_key([char|rest],acc)when(char>=?T and Char<=?V),do: gen_key(rest,[8|acc])
  defp gen_key([char|rest],acc)when(char>=?W and Char<=?Z),do: gen_key(rest,[9|acc])




  defp  number_to_digits(int), do: int_to_int_list(int,[])
  defp  number_to_digits(0,acc), do: acc
  defp  number_to_digits(n,acc), do: int_to_int_list(n div 10,[n rem 10 | acc])


  defp zip(), do: :ok



end
