# Schedule some code to run after everything else
# When coroutines finish, they return control to their parent.
# The top-level coroutine doesn't have a parent, so the process
#  ends when it returns. By explicitly setting a parent we can 
#  schedule code to run after the top-level scope completes.
Defer := Object clone
Defer defer := method(
  yield
  Scheduler waitForCorosToComplete
  "Run at the end" println
)

Defer @defer
# TODO can we guarantee that the last yielding coro is the one
#  we just created?
currentCoro setParentCoroutine(Scheduler yieldingCoros last)

# Some other async code, just for testing
O := Object clone
O somethingAsync := method(wait(1); "somethingAsync is done!" println)

"some program code here" println
O @somethingAsync
