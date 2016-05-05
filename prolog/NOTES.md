# Prolog notes

Use `trace.` in the REPL to debug. `notrace.` turns it off.

Test integer precision with `current_prolog_flag(min_integer, Min)` and `current_prolog_flag(max_integer, Max)`. My gprolog install has `min_integer` = -1152921504606846976 = `-(2^60)`, i.e. it uses 60-bit signed integers. SWI prolog has unbounded integers - `current_prolog_flag(bounded, B).` has `B = false`.

The Gnu prolog interpreter doesn't support proper tail calls. SWI prolog does. H/T comment on [this Stack Overflow answer](http://stackoverflow.com/a/7863217/2806996).

# Links
* [Factorial tutorial](https://www.cpp.edu/~jrfisher/www/prolog_tutorial/2_2.html)
