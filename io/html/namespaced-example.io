# `doFile` can be sent to any receiver
# That means we can use ordinary objects as namespaces,
#  so doFile imports don't have to pollute the global namespace.

# load the html library onto the object `ns`
ns := Object clone
ns doFile("html.io")

# check that `html` doesn't exist
"Does the slot `ns` exist? #{hasSlot(\"ns\")}" interpolate println
"Does the slot `html` exist? #{hasSlot(\"html\")}" interpolate println

# test that the library works
ns html p("hi", p("inner")) println

