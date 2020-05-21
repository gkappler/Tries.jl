push!(LOAD_PATH,"../src")
using Documenter: Documenter, makedocs, deploydocs, doctest, DocMeta
using Tries
using Test

DocMeta.setdocmeta!(Tries, :DocTestSetup, quote
    using Tries
end; recursive=true)

## doctest(Tries; fix=true)

makedocs(;
    modules=[Tries],
    authors="Gregor Kappler",
    repo="https://github.com/gkappler/Tries.jl/blob/{commit}{path}#L{line}",
    sitename="Tries.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://gkappler.github.io/Tries.jl",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
        "Library" => Any[
            "Public" => "lib/public.md",
        ],
        # "Developer Guide" => "developer.md"
    ],
)

deploydocs(;
    repo="github.com/gkappler/Tries.jl",
)
