// Recursive Fibonacci sequence
fib := method(n,
  if(n < 3,
    1,
    fib(n - 2) + fib(n - 1)
  )
)

// Iterative fibonacci sequence
fib2 := method(n,
  curr := 1
  prev := 1
  for(_, 3, n,
    last_curr := curr
    curr = curr + prev
    prev = last_curr
  )
  curr
)

// String formatting helper
cell := method(value, value asString alignRight(2))

// Test
for(i, -1, 10,
  writeln(cell(i), ": ", cell(fib(i)), ", ", cell(fib2(i)))
)
