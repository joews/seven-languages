// Re-implementing Io's conditionals

// `ifTrue` and `ifFalse` return `self` so they can be chained
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
  hasActions := (call argCount >= 2)

  hasActions _ifTrue(
    // consequent message(s) are set: evaluate and return one
    //  of the branches
    conditionIsTruthy ifTrue(
      result = (call evalArgAt(1))
    ) ifFalse (
      result = (call evalArgAt(2))
    )
  ) _ifFalse (
    // consequent messages are not set: return the "truthiness"
    //  of the condition for then/else chaining
    result = conditionIsTruthy
  )

  result
)

// elseif is identical to `if`
_elseif := getSlot("if")

// `then` and `else` are like `ifFalse` and `ifTrue`,
//  but they need to return `nil` instead of `self` so
//  that illogical chains like `a then(w) then(x) else(y) else(z)`
//  cannot be made

// Evaluate `then` when the receiver is true
true _then := method(/* msg, */
  call evalArgAt(0)
  nil
)

// Chain past `else` for true so we can hit a later `then`!
true _else := true

// Chain pase `then` for false so we can hit a later `else`
false _then := false

// Evaluate `else` when the receiver is false
false _else := method(/* msg, */
  call evalArgAt(0)
  nil
)

// Don't evaluate or chain any message where the receiver is `nil`
nil _then := nil
nil _else := nil
nil _elseif := nil

//
// Test
//

// TODO more tests; test "expression" form where `if`/`else` take branch messages
if(1) then("then" println) elseif(false) then ("elseif/then" println) else ("else" println)
_if(1) _then("then" println) _elseif(false) _then ("elseif/then" println) _else ("else" println)

if(nil) then("then" println) elseif(1) then ("elseif/then" println) else ("else" println)
_if(nil) _then("then" println) _elseif(1) _then ("elseif/then" println) _else ("else" println)

if(nil) then("then" println) elseif(false) then ("elseif/then" println) else ("else" println)
_if(nil) _then("then" println) _elseif(false) _then ("elseif/then" println) _else ("else" println)
