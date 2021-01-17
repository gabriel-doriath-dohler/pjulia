#function f(x, y) println(x, " ", y) end
#f((println(1); 1), (println(2); 2))
#println(typeof(42))
#println(typeof(nothing))
#println(typeof(true))
#println(typeof(false))
#println(typeof(print()))
x = true
if x
	for i=3:5
		for j=(-9):3
			println(i, " ", j)
		end
	end
else
	for j=(-9):3
		println(j)
	end
end
