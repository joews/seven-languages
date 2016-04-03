// Activate the "Range" addon
Range

// A prototype for a two-dimensional list
Matrix := Object clone

// Initial state: a (0, 0) matrix
Matrix _list := list()

// Create a 2D Matrix of dimensions x, y
// The optional value `default` will be used to fill the array
// If it is not set, `nil` will be used
Matrix dim := method(x, y, default,
  _list = 1 to(x) map(
    1 to(y) map(_, default)
  )
)

Matrix test := method(_list)

// Set the element at (`x`, `y`) to `value`
Matrix set := method(x, y, value,
  _list at(x) atPut(y, value)
)

// Get the element at (`x`, `y`)
Matrix get := method(x, y,
  _list at(x) at(y)
)

Matrix asString := method(
  _list map(row,
    row map(cell,
      cell asString alignLeft(4)
    ) join
  ) join("\n")
)

// Compute the sum of all entries
Matrix sum := method(
  _list map(sum) sum
)

// Write the matrix to the JSON file specificed by `path`
Matrix write := method(path,
  file := File with(path) remove openForUpdating
  file write(_list asJson)
  file close
)

// Read the matrix from the JSON file specificed by `path`
Matrix read := method(path,
  file := File with(path) openForReading
  json := file contents
  file close

  Yajl parseJson(json)
)

// Testing
m := Matrix clone
m dim(2, 2, 0)
m set(0, 0, 1)
m set(1, 1, 2)
m get(0, 0) println
m get(0, 1) println
m println
m sum println

m write("/tmp/io-matrix.json")
m read("/tmp/io-matrix.json") println
