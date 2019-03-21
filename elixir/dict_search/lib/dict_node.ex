defmodule DictNode do
  @moduledoc false

  defstruct key: nil,  word: [], subTree: Map.new()

#  initalize the Dictionary Node
  def new(key,word \\ []), do: %DictNode{key: key, word: word}

  def add_word(dictNode,word)when word == [], do: dictNode
  def add_word(dictNode,word), do: %{dictNode | word: [word|dictNode.word]}

  def getSubTree(dictNode), do: dictNode.subTree

  def getWord(dictNode), do: dictNode[:word]

  def isEmptyNode(dictNode), do: dictNode[:word] == [] and dictNode[:subTree] == %{}

  def isPerfectNode(dictNode), do: dictNode[:word] != [] and dictNode[:subTree] != %{}

end
