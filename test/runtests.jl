using Tries
using Test

@testset "Tries.jl" begin
    x=Trie((:a,)=>"a", (:a,:b)=>"c", (:a,:c,:d)=>"z", (:a,:b,:d)=>"y")
    @test eltype(x) == String
    @testset "getindex get splatted path, maybe a getindex of Vararg?" begin
        @test keytype(x) == Symbol
        @test_broken keytype(x) == Vararg{Symbol}
    end
    @test "getting subtries and values" begin
        @test x[:a] isa SubTrie
        @test x[:a].path == (:a,)  ## method?
        @test get(x[:a])=="a"
        @test get(x[:a,:b])=="c"
        @test get(x[:a,:b,:d])=="y"
        @test get(x[:a][:b,:d])=="y"
    end
    @test "setting values" begin
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
end
