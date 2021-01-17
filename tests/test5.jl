function g(x) x = 42 end
function f(x) x = 4; g(x); println(x) end
f(3)
