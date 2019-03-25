# DictSearch

**DictSearch is a implementation of the Dictionary. This dictionary used digits for indexing. 
 Digits are generated for each word based on the given condition in the problem statement.
 The Dictionary are stored using Map and Struct**

## Unit Test

    mix test
    
## Run 

    iex -S mix
    
    iex(1)> Dictionary.start
    {:ok, #PID<0.141.0>}
    iex(2)> Dictionary.load "dictionary.txt"
    Time taken for loading text 6409.542ms
    iex(3)> Dictionary.search 6686787825
    [
      ["MOTORTRUCK"],
      ["NOUNS", "USUAL"],
      ["MOTOR", "USUAL"],
      ["NOUNS", "TRUCK"],
      ...
    ]        

To know Execution Time taken for search

    iex(4)> Dictionary.search_timing 6686787825
    Time taken for search 1298.012 ms
    
    [
      ["MOTORTRUCK"],
      ["NOUNS", "USUAL"],
      ["MOTOR", "USUAL"],
      ["NOUNS", "TRUCK"],
      ...
    ] 