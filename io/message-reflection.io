#
# message-reflection.io
# experiments to understand how message (especially argument)
#  reflection works in Io
#

# When a method declares its arguments, they are evaluated
#  in the calling scope and passed by value:
f := method(msg, "Object#f " .. msg)

test := method(arg, "test(#{arg})" interpolate println)
test(f("hi"))

# When a method does not declare its arguments, any passed
#  arguments are not evaluated. The method is free to evaluate
#  them (or not) in any context. The `call` slot exposes data
#  about the current method call
testReflect := method(
  # the called method
  ("Called " .. call message name) println

  # override `f` in the local scope to show how we can dynamically invoke
  #  argument messages in different contexts
  f := method(arg, "inner f: " .. arg)

  # the passed arguments, which are of type Message
  call message arguments foreach(i, arg,
    "  arg #{i} is #{arg}, which is type #{arg type}" interpolate println
    "    - which we could evaluate in the original context: #{call evalArgAt(i)}" interpolate println
    "    - or a brand new context: #{doMessage(call message argAt(i))}" interpolate println
    # call message argAt(i) is an alias for call message arguments at(i)
  )
)

testReflect("str", f("x"))

# We can still reflect over arguments in methods that declare arguments, 
#  but the messages for declared arguments have already been evaluated in the 
#  calling context. Here we can see that g("x") is logged before "calling testDeclareAndReflect".
g := method(arg, 
  "calling g(#{arg})" interpolate println
  "g(#{arg})" interpolate
)

testDeclareAndReflect := method (arg, 
  "calling testDeclareAndReflect" println

  # override g
  g := method(arg, "inner g(#{arg})" interpolate)

  # a method that doesn't exist in outer scope
  h := method(arg, "g: " .. arg)
  
  ("Declared arg: " .. arg) println
  ("evalArgAt(0): " .. call evalArgAt(0)) println
  ("doMessage:    " .. doMessage(call message argAt(0))) println
) 

testDeclareAndReflect(g("x"))

# With lazy evaluation, we can even pass messages that the calling scope does
#  not respond to. 
testReflectPrivate := method(/* arg, */
  private := method(arg, "private(#{arg})" interpolate)
  "calling testReflectPrivate" println
  doMessage(call message argAt(0)) println
)

# Because testReflectPrivate does not declare the first argument, 
#  mesages will always be evaluated lazily. That means we can pass in 
#  a message the the calling scope (Object) doesn't know how to respond to.
# If testReflectPrivate took an argument, we would get an error.
testReflectPrivate(private("hi"))


# Lazily constructed lists can themselves be evaluated lazily
# This doesn't apply to lists build with 'list(a, b, c)`, which
#  evaluates eagerly. The list literal syntax example is a lazily
#  constructed list.
doFile("./list-literal.io")

reflectList := method(/* list, */
  "calling reflectList" println

  private := method(arg, "private(#{arg})" interpolate)
  listArg := call evalArgAt(0)
  listArg foreach(message, 
    message println
    doMessage(message)
  )
)

# must use the lazily evaluated squareBrackets from list-literal.io,
#  rather than eager evaluated list().
reflectList([private("b")])

# uncommenting the next line will cause an error because list() eagerly
#  evaluates its arguments, and `private` doesn't exit in the outer scope
