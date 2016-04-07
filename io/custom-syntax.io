# curlyBrackets is a special slot that helps with custom syntax:
#
# Map { "literal": "syntax"  }
#

# { x, y, z } is the same as curlyBrackets(x, y, z)
# here we use it to define a literal syntax for Maps
# { x, y, z } means "create a new map. Pass messages x, y and z to it."
curlyBrackets := method(
  map := Map clone
  call message arguments foreach(arg, map doMessage(arg))
  map
)

# with this we can:
# { atPut("name", "joe"), atPut("age", 29)  }

# but we can go further by creating a special assignment operator,
#  where `key: value` is defined to mean `atPut(key, value)`:

# the key must be escaped, else it is stored with leading and trailing quotes
# I don't yet understand why that is, since at the REPL map atPut("x", "y")
#  uses unquoted x as the key.
# If that didn't happed we could just assign the operator to "atPut".
OperatorTable addAssignOperator(":", "atEscapedPut")

atEscapedPut := method(key, value, 
  escapedKey := key asMutable removePrefix("\"") removeSuffix("\"")
  atPut(escapedKey, value)
)

# TODO is subscript["key"] syntax possible?

# TODO ["list", "literal"]
