struct test
    n::Int64
    b::Bool
	id::String
end

println(test(42, true, "yo").b)
x= test(42,true,"yo");print(x.n, "\n")
println(x.id)

