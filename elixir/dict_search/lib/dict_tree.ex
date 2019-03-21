defmodule DictTree do
  @moduledoc false

  def new(), do: %{}

  # It will add or update the node to the Tree
  def addNode(tree,key,element), do:
  condition(fn -> Map.has_key?(tree,key) end,
    fn -> Map.put(tree,key,DictNode.add_word(tree[key],element)) end,
    fn -> Map.put_new(tree,key,DictNode.new(key,[element])) end)

  def takeNode(key,tree), do: Map.get_lazy(tree,key,& :nil)

  # it will return node if present in tree. otherwise return a new node
  def getNode(tree,key), do: condition(fn -> Map.has_key?(tree,key) end,
    fn -> Map.get(tree,key)end,
    fn -> DictNode.new(key)end)

  # replace node in the specified key of the Tree
  def replace(dictNode,tree,key),do:
     condition(fn -> Map.has_key?(tree,key) end,
       fn -> Map.put(tree,key,dictNode) end,
       fn -> Map.put_new(tree,key,dictNode) end)

  defp condition(conditionFun,successFun,failureFun) do
    if (conditionFun.() == true),do: successFun.(), else: failureFun.()
  end

end
