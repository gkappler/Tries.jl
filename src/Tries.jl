module Tries

import Base: get!, show, get, isempty, haskey, setindex!, getindex, pairs, keys, values, keytype, eltype
import AbstractTrees
import AbstractTrees: children, printnode, PreOrderDFS, print_tree

export Trie, SubTrie, subtrie
struct Trie{K,T}
    value::Union{Missing,T}
    nodes::Dict{K,Trie{K,T}}
end
function Base.get(x::Trie{K,T})::Union{Missing,T} where {K,T}
    x.value
end
Trie{K,T}() where {K,T} = Trie{K,T}(missing, Dict{K,Trie{K,T}}())
Trie{K,T}(value) where {K,T} = Trie{K,T}(value, Dict{K,Trie{K,T}}())
function Trie(values::Vararg{Pair{NTuple{N,K},T} where N}) where {K,T}
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

Base.keytype(::Type{Trie{K,V}}) where {K,V} = K
Base.keytype(x::Trie) = keytype(typeof(x))
Base.eltype(::Type{Trie{K,V}}) where {K,V} = V
Base.eltype(x::Trie) = eltype(typeof(x))

Base.get!(x::Trie{K,T},k) where {K,T} =
    subtrie!(x, k).value
Base.show(io::IO, x::Trie) =
    print_tree(io,x)
Base.get!(f::Function, x::Trie{K,T}, k) where {K,T} =
    subtrie!(f, x, k...).value
Base.get(x::Trie{K,T}, k) where {K,T} =
    subtrie(x, k...).value
Base.isempty(x::Trie) =
    isempty(x.nodes)

Base.haskey(x::Trie,k) =
    isempty(k) || ( haskey(x.nodes,k[1]) && ( length(k)==1 || haskey(x[k[1]],k[2:end]) ) )

subtrie!(x::Trie{K,T},path...) where {K,T} =
    subtrie!(()->missing, x,path...)
function subtrie!(f::Function,x::Trie{K,T},path...) where {K,T}
    isempty(path) && return x
    x_::Trie{K,T} = x
    for k::K in path[1:end-1]
        x_ = get!(() -> Trie{K,T}(),
                  x_.nodes, k)
    end
    ##if length(path) >= 1
    x_ = get!(() -> Trie{K,T}( f()), x_.nodes, path[end])
    ##end
    x_
end
function subtrie(x::Trie{K,T},path::Vararg) where {K,T}
    x_::Trie{K,T} = x
    for k in path
        !(haskey(x_.nodes,k)) &&  @warn "no key $k" collect(keys(x_.nodes)) # k haskey(x_.nodes,k) x_.nodes
        x_ = x_.nodes[k]
    end
    x_
end

import Base.setindex!
"returns previous value or missing"
function Base.setindex!(x::Trie{K,T}, v, path...) where {K,T}
    x_=subtrie!(x,path[1:end-1]...)
    leaf=subtrie!(x_,path[end])
    x_.nodes[path[end]] = Trie{K,T}(v,leaf.nodes)
    leaf.value
end

function Base.getindex(x::Trie{K,T}, path::Vararg) where {K,T}
    SubTrie(path,subtrie(x,path...))
end

struct SubTrie{K,T}
    path::NTuple{N,K} where N
    value::Trie{K,T}
end
function Base.getindex(x::SubTrie{K,T}, path::Vararg) where {K,T}
    SubTrie(tuple(x.path...,path...),subtrie(x,path...))
end
subtrie(x::SubTrie, a...) =
    subtrie(x.value,a...)

Base.get(x::SubTrie) = x.value === missing ? missing : get(x.value)
AbstractTrees.children(x::SubTrie{K,V}) where {K,V} =
    [ SubTrie{K,V}(tuple(x.path..., k), v) for (k,v) in pairs(x.value.nodes) ]
function AbstractTrees.printnode(io::IO, x::SubTrie{K,V}) where {K,V}
    print(io,x.path[end])
    get(x) !== missing && print(io, " => ", get(x))
end
AbstractTrees.children(x::Trie{K,V}) where {K,V} =
    [ SubTrie{K,V}(tuple(k), v) for (k,v) in pairs(x.nodes) ]
AbstractTrees.printnode(io::IO, x::Trie) = print(io,"Trie{$(keytype(x)),$(eltype(x))}") ## error("should print key")


function Base.pairs(x::Trie{K,V}; self=true) where {K,V}
    @assert self
    ( x.path => x.value for x in PreOrderDFS(SubTrie{K,V}(tuple(),x)) )
end

Base.keys(x::Trie{K,V}) where {K,V} =
    (  kv.first for kv in pairs(x)
       if get(kv.second) !== missing )
Base.values(x::Trie{K,V}) where {K,V} =
    ( get(kv.second) for kv in pairs(x)
      if get(kv.second) !== missing )

## duplicated code!
function subtrie_key(x::Trie{Pair{Symbol, Any}}, key::Symbol)
    for (k,v) in x.nodes
        k.first==key && return v
    end
    error("key $key not found")
end
function subtrie_value(x::Trie{Pair{Symbol, Any}}, key::Symbol)
    for (k,v) in x.nodes
        k.second==key && return v
    end
    error("key $key not found")
end

end # module
