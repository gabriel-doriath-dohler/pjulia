k = 0
c = 0
while (k <= 13)
	c = c + 1
	x = 7
	while (c % 3 != 0)
		y = x
		z = y * 9
		c = c + 1
	end
	k = k + 1
end
println(c)
