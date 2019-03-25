defmodule DictNode do
  @moduledoc """

"""

  defstruct key: nil,  word: [], subTree: Map.new()

  ###############################################################################
  #                         API Methods
  ##############################################################################

  @doc """
  new method will create new Node with three elements key, word and subTree.
  """
  def new(key,word \\ []), do: %DictNode{key: key, word: word}

  @doc """
  add new entry in word element in the Dictionary Node
  """
  def add_word(dictNode,word)when word == [], do: dictNode
  def add_word(dictNode,word), do: %{dictNode | word: [word|dictNode.word]}

  @doc """
  getSubTree/1 will return the value of the 'subTree' element from the given Node
  """
  def getSubTree(dictNode), do: dictNode.subTree

  @doc """
  getWord/1 will return the value of the 'word' element from the given Node
  """
  def getWord(dictNode), do: dictNode.word

  @doc """
        isEmptyNode/1 is return true only if 'word' and 'subTree' elements are Empty.
  """
  def isEmptyNode(dictNode), do: dictNode.word == [] and dictNode.subTree == %{}

  @doc """
      isPerfectNode/1 is return true only if 'word' and 'subTree' elements are non Empty
  """
  def isPerfectNode(dictNode), do: dictNode.word != [] and dictNode.subTree != %{}

  @doc """
  update_subTree/2 is replace the SubTree in the given Node
  """
  def update_subTree(subTree,dictNode), do: %{dictNode | subTree: subTree}
  ###############################################################################
  #                         Local Methods
  ##############################################################################

end
