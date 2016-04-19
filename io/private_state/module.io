# A module that maintains its own scope for data hiding
# Public slots are explicitly set in the calling, or "target", scope.
# Another approach to the same problem is import.io: https://github.com/jwhitfieldseed/import 
block(target,
  "inside the module's private scope" println
  private := "private"

  target public := "public"
) call(thisContext)
