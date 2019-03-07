# dictionary

I have been using the tree structure to represent the Dictionary. 
Tree structure is good for finding a word in Dictionary

Tree Structure :- 

    {Key,Value,NestedTree}   
    

where,

Key - single digit of the phone number
Value - list of word
NestedTree - if the NODE of the tree has a subtree


To perform unit test

    make eunit
    
    
To compile and run 

    make run
    
    
Dictionary will be loaded once the application is up and running.

    Erlang/OTP 21 [erts-10.0] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1]
    
    Loading Dictionary is started......
    Loading Dictionary is completed with in 1354.159 ms
    Eshell V10.0  (abort with ^G)
    (dictionary_search@127.0.0.1)1> 

time taken for Loading Dictionary is mentioned in millisecond

Loading Dictionary will allow perform the duplicate checking in the dictionary.


To perform phone number conversation

    >  dictionary_api:conversation(6686787825).    