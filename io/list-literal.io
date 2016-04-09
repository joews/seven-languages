# list literal syntax
# [1, 2, 3] => list(1, 2, 3)
squareBrackets := method(
  call message arguments
)

# list[index] => value
List squareBrackets := method(index, at(index))

List
