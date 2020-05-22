# Tries.jl Documentation

Implemented of a Trie data structure.  
This is an associative data structure with keys of type `NTuple{N,K} where N` and values of type `V`.
	
## Package Features

- General trie data structure building on `Dict`.
- Generalizes [DataStructures Trie](https://juliacollections.github.io/DataStructures.jl/latest/trie/) from `AbstractString` to arbitrary key types.

!!! note
    Future versions might switch backend to Andy Ferris [Dictionaries.jl](https://github.com/andyferris/Dictionaries.jl).
	
## Using Tries

```@repl
using Tries
x=Trie((:a,)=>"a", 
       (:a,:b)=>"c", 
	   (:a,:c,:d)=>"z", 
	   (:a,:b,:d)=>1)
eltype(x)
x[:a,:b]
x[:a,:b].path
get(x[:a,:b])
get(x[:a][:b,:d])

# 
get(x,[:a,:b])

x[:z]="added"
get(x[:z])
x[:z,:n]="n"
x[:z]
x[:z,:n]="m"
x[:z]
x
```

```@repl
using Tries
x=Trie{Int,Int}(0)
subtrie!(x, 1,2,3,4,5) do x
   x[end]+1
end
x

collect(keys(x))
```

## Library Outline

```@contents
Pages = [ "lib/public.md" ]
Depth = 5
```

```@index
Pages = ["lib/public.md"]
```

