# An actor is an Object that responds to async messages
# Sending an async message to any Object makes it an actor
# An actor is a concurrency primitive. Each actor can process
#  one async message at a time.

# Methods called by async messages are coroutines. They can
#  voluntarily pass control to other coroutines with `yield`
A := Object clone
A a := method(
  "a" println
  yield
  "b" println
)

B := Object clone
B b := method(
  yield
  "c" println
  yield
  "d" println
  # When coroutines complete they return control to their parent coro 
  #  - the coroutine that started it.
)

"start" println
# execute @a and @b with co-operative concurrency; they will pass control
#  to each other with yield.
A @a
B @b

# If we executed
# A @a
# B b
# instead.
# b would be executed _before_ a because coroutines are always executed asyncronously.
# I learned in IRC this order isn't guaranteed by spec, but the current impl does guarantee it.

# Tell the root coroutine to wait for other coroutines to complete
# Otherwise the program will exit here!
Scheduler waitForCorosToComplete
"done" println

# The root coroutine doesn't have a root coro (unless you add one explicitly!),
#  so the program ends when it completes.
