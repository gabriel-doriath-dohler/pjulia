function division(a, b)
	k=0
    while a*k<b
        k = k+1
    end
    return k
end

function syra(n::Int64)
	if n==1
        return 0
    elseif n%2 == 0
        return 1 + syra(division(2,n))
    else
        return 1 + syra(3*n+1)
    end
end

println(syra(42))

