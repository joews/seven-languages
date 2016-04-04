// Re-implementing Io's conditionals

true _ifTrue := method(/* msg, */
  call evalArgAt(0)
  true
)
true _ifFalse := true

false _ifTrue := false
false _ifFalse := method(/* msg, */
  call evalArgAt(0)
  false
)

// TODO this returns true/false; if() returns the result of evaluating
//  the consequent branch
_if := method(/* condition, truePath, falsePath, */
  condition := call evalArgAt(0)
  conditionIsTruthy := (condition isNil or condition == false) not
  conditionIsTruthy ifTrue(call evalArgAt(1)) ifFalse(call evalArgAt(2))
)

// TODO _then, _else, _elseif
