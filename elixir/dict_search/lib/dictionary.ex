defmodule Dictionary do
  @moduledoc """
  Dictionary is a storage mechanism to store all given words in a Tree format.

  I have used Map to represent the Tree Structure


"""

  import DictTree



  def add(tree,[],_), do:  tree
  def add(tree,[lastKey],word), do: addNode(tree,lastKey,word)
  def add(tree,[key|rest],word) do
    dictTree = getNode(tree,key)
               dictTree |> DictNode.getSubTree |> add(rest,word)
                                     |> update_subTree(dictTree) |> replace(tree,key)
  end

  defp update_subTree(subTree,dictNode), do: %{dictNode | subTree: subTree}






end
