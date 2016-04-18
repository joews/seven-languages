# Messing around with coroutines
# - does giving the root coroutine allow scheduling of code
#   after everything else has completed?
Defer := Object clone
Defer defer := method(
  yield
  Scheduler waitForCorosToComplete
  "Run at the end" println
)

Defer @defer
currentCoro setParentCoroutine(Scheduler yieldingCoros pop)

# Some other async code, just for testing
O := Object clone
O somethingAsync := method(wait(1); "done!" println)

"some program code here" println
A @later
