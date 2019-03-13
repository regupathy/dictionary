defmodule Dictionary do
  @moduledoc """
  Dictionary is a storage mechanism to store all given words in a Tree format.

  I have used Map to represent the Tree Structure


"""

  def add(map,[],_), do:  map
  def add(map,[lastKey],word), do: Map.update(map,lastKey,treeNode(lastKey),fn x -> update_val(x,word) end)
  def add(map,[key|rest],word), do: Map.update(map,key,treeNode(key),
                                                       fn x -> IO.puts "s", x.inner |> add(rest,word) |> assign(x,:inner)  end)



  defp assign(newValue,map,position), do: IO.puts "asad asdad #{ inspect newValue}", Map.put(map,position,newValue)

  defp update_val(map,word), do: map.put(map,:val,map.get(map,:val) |> MapSet.put(word))

  defp treeNode(key), do: Map.new(key: key,val: MapSet.new(),inner: Map.new())







end
