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

_if := method(/* condition, truePath, falsePath, */
  condition := call evalArgAt(0)
  conditionIsTruthy := (condition isNil or condition == false) not
  result := nil

  conditionIsTruthy ifTrue(
    result = (call evalArgAt(1))) ifFalse(
    result = (call evalArgAt(2)))

  result
)

// TODO _then, _else, _elseif
