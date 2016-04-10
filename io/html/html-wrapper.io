# Wrapper  to execute html-example.io with map literal syntax

# A wrapper is necessary because Custom operators are only available to files
#  parsed after they are defined.
# - https://github.com/stevedekorte/io/issues/259
doFile("html.io")
doFile("../map-literal.io")

# Now the Map literal syntax is available, run the example
doFile("html-example.io")

