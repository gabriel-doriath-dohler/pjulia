function verif(n::Int64,f)
	if n < 0
        return true
    elseif f(n)
        return verif(n-1,f)
    else
        return false
    end
end

function f(x) true end
function g(x) false end
function h(x) x <= 43 end

println(verif(314151692, f))
println(verif(1, f))
println(verif(0, f))
println(verif(-42, f))
println(verif(314151692, g))
println(verif(1, g))
println(verif(0, g))
println(verif(-42, g))
println(verif(314151692, h))
println(verif(1, h))
println(verif(0, h))
println(verif(-42, h))
println(verif(42, h))
println(verif(43, h))

