using Tries
using Test

@testset "Tries.jl" begin
    x=Trie((:a,)=>"a", (:a,:b)=>"c", (:a,:c,:d)=>"z", (:a,:b,:d)=>1)
    x=Trie((:a,)=>"a", (:a,:b)=>"c", (:a,:c,:d)=>"z", (:a,:b,:d)=>"y")
    @test length(x) == 6
    @test [ n for n in x if n.second!==missing ] == [ (:a,)=>"a", (:a,:b)=>"c", (:a,:b,:d)=>"y", (:a,:c,:d)=>"z" ]
    @test isempty(x) == false
    @test haskey(x,(:a,)) == true
    @test collect(keys(nodes(x[:a]))) == [:b,:c]
    @testset "eltype, keytype" begin
        @test eltype(x) == Pair{Tuple{Vararg{Symbol,N} where N},Union{Missing,String}}
        @test eltype(typeof(x)) == Pair{Tuple{Vararg{Symbol,N} where N},Union{Missing,String}}
        @test eltype(x[:a]) == Pair{Tuple{Vararg{Symbol,N} where N},Union{Missing,String}}
        @test eltype(typeof(x[:a])) == Pair{Tuple{Vararg{Symbol,N} where N},Union{Missing,String}}

        @test valtype(x) == String
        @test valtype(typeof(x)) == String
        @test valtype(x[:a]) == String
        @test valtype(typeof(x[:a])) == String

        @test keytype(x) == Symbol
        @test keytype(typeof(x)) == Symbol
        @test keytype(x[:a]) == Symbol
        @test keytype(typeof(x[:a])) == Symbol
        @test_broken keytype(x) == Vararg{Symbol}
    end
    @testset "getting subtries and values" begin
        @test x[:a] isa SubTrie
        @test x[:a].path == (:a,)  ## method?
        @test get(x[:a])=="a"
        @test get(x[:a,:b])=="c"
        @test get(x[:a,:b,:d])=="y"
        @test get(x[:a][:b,:d])=="y"
    end

    @testset "keys as vectors" begin
        x=Trie([:a]=>"a", [:a,:b]=>"c", [:a,:c,:d]=>"z", [:a,:b,:d]=>"y")
        @test get(x[:a][:b,:d])=="y"
    end

    @testset "Generator constructor" begin
        x=Trie(kv for kv in ([:a]=>"a", [:a,:b]=>"c", [:a,:c,:d]=>"z", [:a,:b,:d]=>"y"))
        @test_throws KeyError subtrie(x,:z,:y)
        @test subtrie((p,i)->1,x,:z,:y)==1
        @test subtrie(nothing,x,:z,:y)===nothing
        @test get(x[:a][:b,:d])=="y"
        subtrie!(x,:z,:y)
        @test get(x[:z][:y])===missing
        get!(x,(:z2,:y2))
        @test get(x,(:z2,:y2)) === missing
        @test get!(x,(:z3,:y3)) do k
            string(k)
        end == "(:z3, :y3)"
    end

    @testset "setting values" begin
        x[:z]="added"
        @test get(x[:z])=="added"
        x[:z,:n]="n"
        @test get(x[:z,:n])=="n"
        x[:z,:n]="m"
        @test get(x[:z,:n])=="m"
        @testset "changing value preserves subtrie" begin
            x[:z]="changed"
            @test get(x[:z,:n])=="m"
        end
    end
    show(x[:a,:b])
    @testset "populating path" begin
        x=Trie{Int,Int}()
        leaf = subtrie!(x, 1,2,3,4,5) do x
            x[end]+1
        end
        @test get(leaf)==6
        @test get(x[1])==2
        @test get(x[1,2])==3
        leaf2 = subtrie!(x[1,2],9,10) do x
            x[end]-1
        end
        @test get(x[1,2,9,10])==9
    end
    show(Trie{Symbol,Int}(1))
    @testset "pairs, keys, values" begin
        x=Trie(tuple(:a,:b) => 1, (:a,) => 2)
        @test collect(values(x))[2:3] == [2,1]
        @test collect(keys(x)) == [tuple(), (:a,),(:a,:b)]
        ## @test collect(pairs(x))[1] == ( tuple()=>missing )
        @test collect(pairs(x))[2] == ( (:a,)=>2 )
        @test collect(pairs(x))[3] == ( (:a,:b)=>1 )
    end
end
