# Compute binomial coefficient mod m (inefficiently).
#function binomial(k, n, m)
#	if k == 0 || k == n
#		return 1
#	else
#		return (binomial(k - 1, n - 1, m) % m + binomial(k, n - 1, m) % m) % m
#	end
#end

#println("Naive approch :")
#for n = 1:25
#	for k = 1:n
#		if binomial(k, n, 2) == 1
#			print("#")
#		else
#			print(" ")
#		end
#	end
#	println("")
#end
#println("\n\n\n")

print(true)
print(42)
print(false)
println()
println("Hello, world!")
println("Hello", ",", " ", "world", "!")
println(42, "newline\nafter newline\t", nothing)
println("\\\\")
println("\\\"")
println("Il doit y avoir afficher true: ", !false)
println("Il doit y avoir afficher false: ", !true)
println("Tada!")
