mutable struct ref x end

k = 0
c = ref(0)
while (k <= 13)
	c.x = c.x + 1
	x = 7
	while (c.x % 3 != 0)
		y = x
		z = y * 9
		c.x = c.x + 1
	end
	k = k + 1
end
println(c.x)
