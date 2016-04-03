// Define a boolean xor operator

// There is no Boolean type, so define methods for the `true` and `false` singletons
OperatorTable addOperator("xor", 11)
true xor := method(x, if(x, false, true))
false xor := method(x, if(x, true, false))

// Override `/` to return 0 instead of `inf` when the denominator is 0
div := Number getSlot("/")

Number / = method(denominator,
  if (denominator == 0, 0, self div(denominator))
)

(10 / 1) println
(10 / 2) println
(10 / 0) println
