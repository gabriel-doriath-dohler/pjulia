mutable struct ref x end

k = 0
x = ref(0)
while (y = "yo"; k <= 20)
	x.x = x.x + 2
end
println(x.x)
println(y)
