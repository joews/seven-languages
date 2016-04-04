// Activate the "Range" addon
Range

// Prototype for a mutable two-dimensional list
Matrix := Object clone

// Initial state: a (0, 0) matrix
Matrix _list := list()

// Create a 2D Matrix with the given number of rows and cols
// The optional value `default` will be used to fill the matrix
// If it is not set, `nil` will be used
Matrix dim := method(numCols, numRows, default,
  _list = 1 to(numRows) map(
    1 to(numCols) map(_, default)
  )
)

// Set the element at (col, row) to `value`
Matrix set := method(col, row, value,
  _list at(row) atPut(col, value)
)

// Get the element at (col, row)
Matrix get := method(col, row,
  _list at(row) at(col)
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

  self
)

// Read the matrix from the JSON file specificed by `path`
Matrix read := method(path,
  file := File with(path) openForReading
  json := file contents
  file close

  Yajl parseJson(json)
  self
)

Matrix transpose := method(
  maxRow := _list size - 1
  maxCol := _list at(0) size - 1

  _list = 0 to(maxCol) map (colNum,
    0 to(maxRow) map (rowNum,
      get(colNum, rowNum)
    )
  )

  self
)

// Testing
m := Matrix clone
m dim(2, 3, 0)

m set(0, 0, 1)
m set(0, 1, 2)
m set(0, 2, 3)
m set(1, 0, 4)
m set(1, 1, 5)
m set(1, 2, 6)

m get(0, 0) println
m get(0, 1) println
m println
m sum println

m write("/tmp/io-matrix.json")
m read("/tmp/io-matrix.json") println

m transpose println
