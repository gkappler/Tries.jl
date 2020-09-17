"""
Implemented of a Trie data structure.  
This is an associative data structure with keys of type `NTuple{N,K} where N` and values of type `V`.
"""
module Tries

import Base: get!, show, get, isempty, haskey, setindex!, getindex, pairs, keys, values, keytype, eltype, valtype
import AbstractTrees
import AbstractTrees: children, printnode, PreOrderDFS, print_tree
##using VectorDicts

export AbstractTrie, Trie, SubTrie, nodes, subtrie

abstract type AbstractTrie{K,T} end

export SortedTrie
"""
    SortedTrie{T,S<:Function}

    tree::T
    by::S
"""
struct SortedTrie{K,V,T,S<:Function} <: AbstractTrie{K,V}
    tree::T
    by::S
    SortedTrie(tree,by) =
        new{eltype(keytype(tree)),valtype(tree),typeof(tree),typeof(by)}(tree,by)
end
function Base.show(io::IO, x::SortedTrie)
    print_tree(IOContext(io, :compact=>false),x)
end
AbstractTrees.children(x::SortedTrie) =
    map(s->SortedTrie(s,x.by), sort(children(x.tree); by=x.by))

function AbstractTrees.printnode(io::IO, x::SortedTrie)
    printnode(io,x.tree)
end

"""
    Base.length(x::Tries.AbstractTrie)

Cumulative count of all nodes.
"""
Base.length(x::AbstractTrie) =
    1+_length(x)

_length(x::AbstractTrie) =
    isempty(nodes(x)) ? 0 : (0+length(nodes(x)) + (sum)(_length.(values(nodes(x)))))::Int

"""
    Base.iterate(x::Tries.AbstractTrie, a...)

`iterate(pairs(x), a...)`.
"""
Base.iterate(x::AbstractTrie, a...) =
    iterate(pairs(x), a...)

struct Trie{K,T} <: AbstractTrie{K,T}
    value::Union{Missing,T}
    nodes::Dict{K,Trie{K,T}}
end

"""
    nodes(x::AbstractTrie{K,T})

Getter for node dictionary.
"""
nodes(x::Trie) = x.nodes
"""
A Trie with a path.
"""
struct SubTrie{K,T} <: AbstractTrie{K,T}
    path::NTuple{N,K} where N
    value::Trie{K,T}
    SubTrie(path::NTuple{N,K} where N, t::Trie{K,V}) where {K,V} =
        new{K,V}(path, t)
    SubTrie(path::NTuple{N,K} where N, st::SubTrie{K,V}) where {K,V} =
        new{K,V}((path..., st.path...), st.value)
end
nodes(x::SubTrie) = x.value.nodes

subtrie(x::SubTrie, a...) =
    SubTrie(x.path,subtrie(x.value,a...))

subtrie!(x::SubTrie, a...) =
    SubTrie(x.path,subtrie!(x.value,a...))

subtrie!(f::Function, x::SubTrie, a...) =
    SubTrie(x.path,subtrie!(f,x.value,a...))

"""
    Trie{K,T}()

Construct an empty `Trie{K,T}` with root value `missing`.
"""
Trie{K,T}() where {K,T} = Trie{K,T}(missing, Dict{K,Trie{K,T}}())

"""
    Trie{K,T}(value)

Construct an empty `Trie{K,T}` with root value is `value`.
"""
Trie{K,T}(value) where {K,T} = Trie{K,T}(value, Dict{K,Trie{K,T}}())

"""
    Trie(values::Vararg{Pair{NTuple{N,K},T} where N}) where {K,T}
    Trie(values::Vararg{Pair{Vector{K},T}}) where {K,T}
    Trie(values::Vararg{Pair{NTuple{N,K},<:Any} where N}) where {K}
    Trie(values::Base.Generator)

Construct a `Trie{K,T}` and populate it with `r[k...]=v`.

```jldoctest
julia> Trie((:a,)=>"a", (:a,:b)=>"c", (:a,:c,:d)=>"z", (:a,:b,:d)=>1)
Trie{Symbol,Any}
└─ :a => "a"
   ├─ :b => "c"
   │  └─ :d => 1
   └─ :c
      └─ :d => "z"


julia> Trie((:a,)=>"a", (:a,:b)=>"c", (:a,:c,:d)=>"z", (:a,:b,:d)=>"y")
Trie{Symbol,String}
└─ :a => "a"
   ├─ :b => "c"
   │  └─ :d => "y"
   └─ :c
      └─ :d => "z"

```

See also [`setindex!`](@ref).
"""
function Trie(values::Vararg{Pair{NTuple{N,K},T} where N}) where {K,T}
    r = Trie{K,T}(missing, Dict{K,Trie{K,T}}())
    for (k,v) in values
        r[k...]=v
    end
    r
end

function Trie(values::Vararg{Pair{Vector{K},T}}) where {K,T}
    r = Trie{K,T}(missing, Dict{K,Trie{K,T}}())
    for (k,v) in values
        r[k...]=v
    end
    r
end

function Trie(values::Vararg{Pair{NTuple{N,K},<:Any} where N}) where {K}
    r = Trie{K,Any}(missing, Dict{K,Trie{K,Any}}())
    for (k,v) in values
        r[k...]=v
    end
    r
end

Trie(values::Base.Generator) = Trie(values...)

# struct ETrie{K,T,D<:AbstractDict}
#     value::Union{Missing,T}
#     nodes::D{K,ETrie{K,T,D}}
# end
# ETrie{K,T}() where {K,T} =
#     ETrie{K,T,}(missing, @show VectorDict{K,ETrie{K,T,VectorDict}}())    
"""
    Base.get(x::Trie)
    Base.get(x::SubTrie)

Return `value::Union{Missing,valtype(x)}` of `x`.
"""
function Base.get(x::Trie)
    x.value
end

Base.get(x::SubTrie) = get(x.value)


"""
    Base.show(x::Trie)
    Base.show(x::SubTrie)

Display `x` with `AbstractTrees.print_tree`.
"""
function Base.show(io::IO, x::Trie)
    print(io,"Trie{$(keytype(x)),$(valtype(x))}") ## error("should print key")
    print_tree(io,x)
end

function Base.show(io::IO, x::SubTrie)
    print(io,"SubTrie{$(keytype(x)),$(valtype(x))} @ ") ## error("should print key")
    if length(x.path)>1
        for p in x.path[1:end-1]
            show(io,p)
            print(io,", ")
        end
    end
    print_tree(io,x)
end


AbstractTrees.children(x::Trie{K,V}) where {K,V} =
    [ SubTrie(tuple(k), v) for (k,v) in pairs(x.nodes) ]

function AbstractTrees.printnode(io::IO, x::Trie)
    if get(x) !== missing
        print(io, " => ")
        show(io, get(x))
    end
end

AbstractTrees.children(x::SubTrie{K,V}) where {K,V} =
    [ SubTrie(tuple(x.path..., k), v)
      for (k,v) in pairs(x.value.nodes) ]

function AbstractTrees.printnode(io::IO, x::SubTrie)
    !isempty(x.path) && show(io,x.path[end])
    if get(x) !== missing
        print(io, " => ")
        show(io, get(x))
    end
end



"""
    Base.keytype(::Type{Trie{K,V}}) where {K,V}
    Base.keytype(::Trie{K,V}) where {K,V}

Returns `K`.
!!! warning
    please review: should this return `NTuple{N,K} where N`?
"""
Base.keytype(::Type{<:AbstractTrie{K,V}}) where {K,V} = K
Base.keytype(x::AbstractTrie) = keytype(typeof(x))


"""
    Base.eltype(::Type{<:AbstractTrie{K,V}}) where {K,V}
    Base.etype(::AbstractTrie{K,V}) where {K,V}

Returns `Pair{Tuple{Vararg{K,N} where N},Union{Missing,V}}` for `iterate` and `collect`.
"""
Base.eltype(::Type{<:AbstractTrie{K,V}}) where {K,V} = Pair{Tuple{Vararg{K,N} where N},Union{Missing,V}}
Base.eltype(x::AbstractTrie) = eltype(typeof(x))


"""
    Base.valtype(::Type{AbstractTrie{K,V}}) where {K,V}
    Base.valtype(::AbstractTrie{K,V}) where {K,V}

Returns `V`.
"""
Base.valtype(::Type{<:AbstractTrie{K,V}}) where {K,V} = V
Base.valtype(x::AbstractTrie) = valtype(typeof(x))


"""
    Base.get!(x::Trie,k)

Returns `subtrie!(x,k).value`.

See also [`subtrie!`](@ref)
"""
Base.get!(x::Trie{K,T},k) where {K,T} =
    get(subtrie!(x, k...))


"""
    Base.get(x::Trie,k)

Returns `subtrie(x,k).value`.

See also [`subtrie`](@ref)
"""
Base.get(x::Trie{K,T}, k) where {K,T} =
    get(subtrie(x, k...))

"""
    Base.get!(x::Trie,k)

Returns `subtrie!(x,k).value`.

See also [`subtrie!`](@ref)
"""
Base.get!(f::Function, x::Trie{K,T}, k) where {K,T} =
    get(subtrie!(f, x, k...))

"""
    Base.isempty(x::Trie)

Returns `true` iif x has no nodes.
"""
Base.isempty(x::Trie) =
    isempty(x.nodes)


"""
    Base.haskey(x::Trie,path)

Returns `true` iif x has nodes along `path`.
"""
Base.haskey(x::Trie,path) =
    isempty(path) || ( haskey(x.nodes,path[1]) && ( length(path)==1 || haskey(x[path[1]],path[2:end]) ) )

export subtrie!
"""
    subtrie!(x::Trie,path...)

Return a subtree at `path`.
Nodes missing in `x` along path are created and populated with values `missing`.
"""
subtrie!(x::Trie{K,V},path::K...) where {K,V} =
    subtrie!((_,_)->missing, x,path...)

"""
    subtrie!(f::Function,x::Trie,path...)

Return a subtree at `path`.
Nodes missing in `x` along path are created and populated with values `f(path, index)`.

```jldoctest
julia> a = Trie{Int,Int}(0)
Trie{Int64,Int64} => 0

julia> subtrie!((p,i)->i, a, 4,3,2,1)
SubTrie{Int64,Int64} @ 4, 3, 2, 1 => 4

julia> a
Trie{Int64,Int64} => 0
└─ 4 => 1
   └─ 3 => 2
      └─ 2 => 3
         └─ 1 => 4

```

"""
function subtrie!(f::Function,x::Trie{K,T},path::K...) where {K,T}
    isempty(path) && return SubTrie(tuple(),x)
    x_::Trie{K,T} = x
    for i in 1:(lastindex(path)-1)
        k = path[i]
        x_ = get!(() -> Trie{K,T}(f(path,i)),
                  nodes(x_), k)
    end
    ##if length(path) >= 1
    x_ = get!(() -> Trie{K,T}(f(path,lastindex(path))),
              nodes(x_), path[end])
    ##end
    SubTrie(path, x_)
end


"""
    subtrie(x::Trie{K,T},path...)

Return a subtree at `path`.

```jldoctest
julia> a = Trie((:a,)=>"a", (:a,:b)=>"c", (:a,:c,:d)=>"z", (:a,:b,:d)=>"y")
Trie{Symbol,String}
└─ :a => "a"
   ├─ :b => "c"
   │  └─ :d => "y"
   └─ :c
      └─ :d => "z"

julia> subtrie(a, :a, :b)
SubTrie{Symbol,String} @ :a, :b => "c"
└─ :d => "y"

julia> subtrie(a, :a, :d, :b)
ERROR: KeyError: key (:d, :b) not found
Stacktrace:
 [1] (::Tries.var"#41#42")(::Tuple{Symbol,Symbol,Symbol}, ::Int64) at /home/gregor/dev/julia/Tries/src/Tries.jl:334
 [2] subtrie(::Tries.var"#41#42", ::Trie{Symbol,String}, ::Symbol, ::Vararg{Symbol,N} where N) at /home/gregor/dev/julia/Tries/src/Tries.jl:386
 [3] subtrie(::Trie{Symbol,String}, ::Symbol, ::Symbol, ::Vararg{Symbol,N} where N) at /home/gregor/dev/julia/Tries/src/Tries.jl:334
 [4] top-level scope at REPL[12]:1

```
"""
function subtrie(x::AbstractTrie,path...)
    subtrie((path,i)->throw(KeyError(path[i:end])),x,path...)
end

"""
    subtrie(::Nothing,x::Trie{K,T},path...)

Return a subtree at `path`, or `nothing`, if `path` does not exist in `x`.
Does not modify `x`.

```jldoctest
julia> a = Trie((:a,)=>"a", (:a,:b)=>"c", (:a,:c,:d)=>"z", (:a,:b,:d)=>"y")
Trie{Symbol,String}
└─ :a => "a"
   ├─ :b => "c"
   │  └─ :d => "y"
   └─ :c
      └─ :d => "z"

julia> subtrie(nothing, a, :a, :d)

```
"""
function subtrie(::Nothing,x::AbstractTrie{K,T},path::K...) where {K,T}
    subtrie((p,i)->nothing,x,path...)
end

"""
    subtrie(notfound::Function,x::Trie{K,T},path...)

Return a subtree at `path`, or `notfound(path,error_index)`, if `path` does not exist in `x`
(default `(path,i)->throw(KeyError(path[i:end]))`).
Does not modify `x`.

```jldoctest
julia> a = Trie((:a,)=>"a", (:a,:b)=>"c", (:a,:c,:d)=>"z", (:a,:b,:d)=>"y")
Trie{Symbol,String}
└─ :a => "a"
   ├─ :b => "c"
   │  └─ :d => "y"
   └─ :c
      └─ :d => "z"


julia> subtrie((x...) -> x, a, :a, :d)
((:a, :d), 2)


```
"""
function subtrie(f::Function,x::AbstractTrie{K,T},path::K...) where {K,T}
    x_ = x
    for (i,k) in enumerate(path)
        !(haskey(nodes(x_),k)) && return f(path,i)
        # &&  @warn "no key $k" collect(keys(x_.nodes)) # k haskey(x_.nodes,k) x_.nodes
        x_ = nodes(x_)[k]
    end
    SubTrie(path, x_)
end


import Base.setindex!
"""
    Base.setindex!(x::Trie{K,T}, v::T, path...) where {K,T}

Set value at `path` to `v and return previous value or missing.

!!! note
    To retrieve last value you need to call `setindex!` explicitly.


```jldoctest
julia> x = Trie((:a,)=>"a", (:a,:b)=>"c", (:a,:c,:d)=>"z", (:a,:b,:d)=>"y")
Trie{Symbol,String}
└─ :a => "a"
   ├─ :b => "c"
   │  └─ :d => "y"
   └─ :c
      └─ :d => "z"

julia> x[:a,:b,:z]="node added"
"node added"

julia> setindex!(x,"value set",:a,:c)
Trie{Symbol,String}
└─ :d => "z"


julia> x
Trie{Symbol,String}
└─ :a => "a"
   ├─ :b => "c"
   │  ├─ :d => "y"
   │  └─ :z => "node added"
   └─ :c => "value set"
      └─ :d => "z"

```

See also [`subtrie!`](@ref)
"""
function Base.setindex!(x::Trie{K,T}, v::T, path::K...) where {K,T}
    x_=subtrie!(x,path[1:end-1]...)
    leaf=subtrie!(x_,path[end])
    x_.value.nodes[path[end]] = Trie{K,T}(v,leaf.value.nodes)
    leaf.value
end


"""
    Base.getindex(x::Trie{K,T}, path...) where {K,T}

Get `SubTrie` at `path`.

See also [`SubTrie`](@ref).
"""
function Base.getindex(x::Trie{K,T}, path::K...) where {K,T}
    subtrie(x,path...)
end

"""
    Base.getindex(x::SubTrie, path...)

Get `SubTrie` at `(x.path...,path...)`.

See also [`SubTrie`](@ref).
"""
function Base.getindex(x::SubTrie{K,V}, path::K...) where {K,V}
    SubTrie(tuple(x.path...,path...),subtrie(x,path...))
end

"""
    Base.pairs(x::Trie{K,V}) where {K,V}
    Base.pairs(x::SubTrie)

Generator returning `path => value` pairs.

See also [`AbstractTrees.PreOrderDFS`](https://juliacollections.github.io/AbstractTrees.jl/stable/api/#AbstractTrees.PreOrderDFS)
"""
Base.pairs(x::Trie) =
    pairs(SubTrie(tuple(),x))

function Base.pairs(x::SubTrie{K,V}) where {K,V}
    ( Pair{Tuple{Vararg{K,N} where N},Union{Missing,V}}(x.path, get(x))
      for x in PreOrderDFS(x) )
end


"""
    Base.keys(x::AbstractTrie)

Generator returning `path`s as `first` fields from `pairs(x)`.

See also [`pairs`](@ref)
"""
Base.keys(x::AbstractTrie) =
    (  kv.first for kv in pairs(x) )


"""
    Base.values(x::Union{Trie,SubTrie})

Generator returning `value`s as `second` fields from `pairs(x)`.

See also [`pairs`](@ref)
"""
Base.values(x::AbstractTrie) =
    ( kv.second for kv in pairs(x) )

end # module
