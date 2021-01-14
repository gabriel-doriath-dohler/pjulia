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
println("---------------------------")
println("Test of binop.")
println("3 + 2 = ", 3 + 2)
println("3 * 2 = ", 3 * 2)
println("2 * 2 = ", 2 * 2)
println("3 + 4 = ", 3 + 4)
println("2 * 2 + 3 = ", 2 * 2 + 3)
println("3 + 2 * 2 = ", 3 + 2 * 2)
println("3 * 2 + 2 = ", 3 * 2 + 2)
println("3 * 2 + 2 + 1 = ", 3 * 2 + 2 + 1)
println("1 + 2 + 3 + 4 + 5 = ", 1 + 2 + 3 + 4 + 5)
println("1 + 2 + 3 + 4 + 5 + 0 = ", 1 + 2 + 3 + 4 + 5 + 0)
println("1 + 2 + 3 + 4 + 5 + 0 - 1 - 2 - 3 - 4 = ", 1 + 2 + 3 + 4 + 5 + 0 - 1 - 2 - 3 - 4)
println("2 - 1 - 1 = ", 2 - 1 - 1)
println("- 2 * 3 = ", - 2 * 3)
println("-2 * 3 = ", -2 * 3)
println("- 2 * - 3 = ", - 2 * - 3)
println("true || true = ", true || true)
println("true || false = ", true || false)
println("false || true = ", false || true)
println("false || false = ", false || false)
println("true && true = ", true && true)
println("true && false = ", true && false)
println("false && true = ", false && true)
println("false && false = ", false && false)
println("false || true && false = ", false || true && false)
println("false && true || false = ", false && true || false)
println("false && true || true = ", false && true || true)
println((println(1); true) && (println(2); true) && (println(3); false))
println((println(1); true) && (println(2); true) && (println(3); true))


println("125111114 % 6 = ", 125111114 % 6)
println("125111115 % 6 = ", 125111115 % 6)
println("125111116 % 6 = ", 125111116 % 6)
println("125111117 % 6 = ", 125111117 % 6)
println("125111118 % 6 = ", 125111118 % 6)
println("125111119 % 6 = ", 125111119 % 6)
println("125111120 % 6 = ", 125111120 % 6)
println("-125111114 % 6 = ", -125111114 % 6)
println("(-125111114) % 6 = ", (-125111114) % 6)
#3 % 0
println("(-3)^0 = ", (-3)^0)
println("-3^0 = ", -3^0)
println("-3^1 = ", -3^1)
println("-3^2 = ", -3^2)
println("-7268361881828128663 = 45 ^ 34 = ", 45 ^ 34)
println("4965108644506139253 = 45 ^ 35 = ", 45 ^ 35)
