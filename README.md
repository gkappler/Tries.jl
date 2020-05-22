# Tries
<!-- [![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://gkappler.github.io/Tries.jl/stable) -->
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://gkappler.github.io/Tries.jl/dev)
[![Build Status](https://travis-ci.org/gkappler/Tries.jl.svg?branch=master)](https://travis-ci.com/github/gkappler/Tries.jl)
[![Codecov](https://codecov.io/gh/gkappler/Tries.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/gkappler/Tries.jl)

[Trie](https://en.wikipedia.org/wiki/Trie) is a small package providing a tree-like data structure implemented on a `Dict` backend and using [AbstractTrees](https://github.com/JuliaCollections/AbstractTrees.jl) for printing and traversal.
`Trie` generalizes [DataStructures.Trie](https://juliacollections.github.io/DataStructures.jl/latest/trie/) from `AbstractString` keys to arbitrary `NTuple{N,K} where N` key types.

Some design decisions for a `trie::Trie{K,V}` regarding `keytype` and `getindex` might change in future versions based on discussions with the community.
:
- `getindex(trie, ks::K...)` and `setindex!(trie, v, ks::K...)` consider `Trie` as sparse representation of a mathematical object with values`::Union{V,Missing}` referenced by any finite `N`-dimensional key `ks::NTuple{N,K} where N`. 
- `keytype(trie)` currently is `K`, should it be `ks::NTuple{N,K} where N`?
- Future versions might switch backend to Andy Ferris [Dictionaries.jl](https://github.com/andyferris/Dictionaries.jl).
Contributions, thoughts and suggestions very welcome!

<pre><code class="language-julia-repl">
julia&gt; using Tries

julia&gt; x=Trie((:a,)=&gt;&quot;a&quot;,
              (:a,:b)=&gt;&quot;c&quot;,
       	   (:a,:c,:d)=&gt;&quot;z&quot;,
       	   (:a,:b,:d)=&gt;1)
Trie{Symbol,Any}
└─ :a =&gt; &quot;a&quot;
   ├─ :b =&gt; &quot;c&quot;
   │  └─ :d =&gt; 1
   └─ :c
      └─ :d =&gt; &quot;z&quot;

julia&gt; eltype(x)
Any

julia&gt; x[:a,:b]
SubTrie{Symbol,Any} @ :a, :b =&gt; &quot;c&quot;
└─ :d =&gt; 1

julia&gt; x[:a,:b].path
(:a, :b)

julia&gt; get(x[:a,:b])
&quot;c&quot;

julia&gt; get(x[:a][:b,:d])
1

julia&gt; #
       get(x,[:a,:b])
&quot;c&quot;

julia&gt; x[:z]=&quot;added&quot;
&quot;added&quot;

julia&gt; get(x[:z])
&quot;added&quot;

julia&gt; x[:z,:n]=&quot;n&quot;
&quot;n&quot;

julia&gt; x[:z]
SubTrie{Symbol,Any} @ :z =&gt; &quot;added&quot;
└─ :n =&gt; &quot;n&quot;

julia&gt; x[:z,:n]=&quot;m&quot;
&quot;m&quot;

julia&gt; x[:z]
SubTrie{Symbol,Any} @ :z =&gt; &quot;added&quot;
└─ :n =&gt; &quot;m&quot;

julia&gt; x
Trie{Symbol,Any}
├─ :a =&gt; &quot;a&quot;
│  ├─ :b =&gt; &quot;c&quot;
│  │  └─ :d =&gt; 1
│  └─ :c
│     └─ :d =&gt; &quot;z&quot;
└─ :z =&gt; &quot;added&quot;
   └─ :n =&gt; &quot;m&quot;</code></pre><pre><code class="language-julia-repl">julia&gt; using Tries

julia&gt; x=Trie{Int,Int}(0)
Trie{Int64,Int64} =&gt; 0

julia&gt; subtrie!(x, 1,2,3,4,5) do x
          x[end]+1
       end
SubTrie{Int64,Int64} @ 1, 2, 3, 4, 5 =&gt; 6

julia&gt; x
Trie{Int64,Int64} =&gt; 0
└─ 1 =&gt; 2
   └─ 2 =&gt; 3
      └─ 3 =&gt; 4
         └─ 4 =&gt; 5
            └─ 5 =&gt; 6
               ⋮

julia&gt; collect(keys(x))
6-element Array{Tuple{Vararg{Int64,N} where N},1}:
 ()
 (1,)
 (1, 2)
 (1, 2, 3)
 (1, 2, 3, 4)
 (1, 2, 3, 4, 5)
 </code></pre>
