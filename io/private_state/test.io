# An approach to encapsulation with doFile()
# For another approach that doesn't depend on any specific module behaviours, see
# import.io: https://github.com/jwhitfieldseed/import

# module.io creates a private slot called "private" and a public slot
#  called public.

# Namespaced import
"# Namespaced import" println
Test := Object clone
Test doFile("./module.io")

Test hasSlot("private") println # false
Test hasSlot("public") println  # true

# Check we didn't leak anything onto object
"# Checking for leaked slots"
hasSlot("private") println  # false
hasSlot("public") println   # false

# Object import
"# Object import" println 
doFile("./module.io")
hasSlot("private") println  # false
hasSlot("public") println   # true
