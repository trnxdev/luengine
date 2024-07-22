println("Hello,".." World!")

function fib(n)
    if n <= 1 then
        return n
    end

    return fib(n - 1) + fib(n - 2)
end

println("Fibonacci of 20 is:")
println(fib(20))
println([1, 2, 3, nil, true, false, fib, println])
