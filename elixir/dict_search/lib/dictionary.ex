defmodule Dictionary do
  @moduledoc """
  Dictionary is a storage mechanism to store all given words in a Tree format.

  I have used Map to represent the Tree Structure


"""

  import DictTree
  ####################################################################
  #
  #                  Dictionary Stoarge
  #
  ####################################################################
  use Agent

  def start(), do: Agent.start(fn -> DictTree.new end,name: :dict )

  def getDict(), do: Agent.get( :dict,& &1)

  def update(word), do: Agent.update( :dict,fn(tree) -> add(tree,gen_key(word),word) end)

  def update_list(words) do
    newDictTree = List.foldl(words,getDict(),fn word,tree -> add(tree,gen_key(word),word)  end)
    Agent.update(:dict,fn _ -> newDictTree end)
  end

  ####################################################################
  #
  #                  Dictionary APIs
  #
  ####################################################################

  @doc """
        Dictionary search is performed through given number
  """
  def search(number), do: search(number,getDict()) |> Enum.reverse
  def search(number,tree), do: number_to_digits(number) |> search(tree,[],tree)

  def search_timing(number) do
    {time,result} = :timer.tc(fn -> search(number) end)
    IO.puts("Time taken for search #{time/1000} ms\n\n")
    result
  end
  @doc """
  Adding a word in Dictionary
  """
  def add(tree,[],_), do:  tree
  def add(tree,[lastKey],word), do: addNode(tree,lastKey,word)
  def add(tree,[key|rest],word) do
    dictTree = getNode(tree,key)
    dictTree |> DictNode.getSubTree |> add(rest,word) |> DictNode.update_subTree(dictTree) |> replace(tree,key)
  end

  @doc """
      Loading the Dictionary text files into Dictionary stoarge
  """
  def load(path) do
    fun = fn ->
      case File.read(path) do
        {:ok,content} -> parse(content,<<>>,[]) |> update_list
        _ -> IO.puts("Error in loading File #{path}")
      end
    end
    {time,_} = :timer.tc(fun)
    IO.puts("Time taken for loading text #{time/1000}ms")
  end

  ####################################################################
  #
  #                  Local Methods
  #
  ####################################################################

  defp search([digit|[]],tree,acc,dictTree), do: DictTree.takeNode(digit,tree) |> search1([],acc,dictTree)
  defp search([digit|rest],tree,acc,dictTree), do: DictTree.takeNode(digit,tree) |> search1(rest,acc,dictTree)

  defp search1(nil,[],_acc,_), do: []
  defp search1(dictNode,[],acc,_), do: dictNode |> DictNode.getWord |> fn [] -> [];words -> zip([words|acc]) end.()
  defp search1(nil,_rest,_acc,_), do: []
  defp search1(dictNode,rest,acc,dictTree), do: dictNode |> DictNode.isEmptyNode()
                                       |> fn true -> []; false -> search2(dictNode,rest,acc,dictTree) end.()
  defp search2(dictNode,keys,acc,dictTree), do: dictNode |> DictNode.isPerfectNode()
                                       |> fn true -> search(keys,dictTree,[DictNode.getWord(dictNode)|acc],dictTree)
                                                     ++ search(keys,DictNode.getSubTree(dictNode),acc,dictTree)
                                            false -> search3(dictNode,keys,acc,dictTree)  end.()
  defp search3(dictNode,keys,acc,dictTree), do: DictNode.getWord(dictNode) |> fn [] -> search(keys,DictNode.getSubTree(dictNode),acc,dictTree)
                                                                       words -> search(keys,dictTree,[words|acc],dictTree) end.()



  #### Parse the text binary into list of words
  defp parse("",_,acc), do: acc
  defp parse(<<" ",rest::binary>>,temp,acc), do: parse(rest,temp,acc)
  defp parse(<<"\r",rest::binary>>,temp,acc), do: parse(rest,temp,acc)
  defp parse(<<"\n",rest::binary>>,temp,acc)when byte_size(temp) >= 3, do: parse(rest,<<>>,[temp|acc])
  defp parse(<<"\n",rest::binary>>,_,acc), do: parse(rest,<<>>,acc)
  defp parse(<<a,rest::binary>>,temp,acc), do: parse(rest,<<temp::binary,a>>,acc)


  ### generate a key based on the given word

  defp gen_key(word), do: gen_key(String.upcase(word),[])

  defp gen_key(<<>>,acc), do: Enum.reverse acc
  defp gen_key(<<char,rest::binary>>,acc)when(char>=?A and char<=?C),do: gen_key(rest,[2|acc])
  defp gen_key(<<char,rest::binary>>,acc)when(char>=?D and char<=?F),do: gen_key(rest,[3|acc])
  defp gen_key(<<char,rest::binary>>,acc)when(char>=?G and char<=?I),do: gen_key(rest,[4|acc])
  defp gen_key(<<char,rest::binary>>,acc)when(char>=?J and char<=?L),do: gen_key(rest,[5|acc])
  defp gen_key(<<char,rest::binary>>,acc)when(char>=?M and char<=?O),do: gen_key(rest,[6|acc])
  defp gen_key(<<char,rest::binary>>,acc)when(char>=?P and char<=?S),do: gen_key(rest,[7|acc])
  defp gen_key(<<char,rest::binary>>,acc)when(char>=?T and char<=?V),do: gen_key(rest,[8|acc])
  defp gen_key(<<char,rest::binary>>,acc)when(char>=?W and char<=?Z),do: gen_key(rest,[9|acc])

  ### convert number into list of digits
  defp  number_to_digits(int), do: number_to_digits(int,[])
  defp  number_to_digits(0,acc), do: acc
  defp  number_to_digits(num,acc), do: number_to_digits(div(num,10) ,[rem(num,10) | acc])

  ### concatenating the given list by grouping

  defp zip(list), do: zip(list,[])
  defp zip([hd|tail],[]), do: zip(tail,Enum.map(hd,fn x -> [x]  end))
  defp zip([hd|tail],acc), do: zip(tail,(for x <- acc, y <- hd, do: [y|x]))
  defp zip([],acc), do: acc

end
