# Erlang notes

* [Erlang](http://erlang.org/doc/getting_started/users_guide.html) is the language. It comes with a [standard library](http://erlang.org/doc/apps/stdlib/).
* [OTP](http://erlang.org/doc/design_principles/users_guide.html) is a set of libraries and design patterns for building distributed Erlang apps.

* Docs: [stdlib docs](http://erlang.org/doc/apps/stdlib/) or `❯ erl -man <module name>`

## Running code

Compile and execute the REPL:

```erlang
c(module_name).
module_name:some_function()
```

Compile and execute scripts with [escript](http://erlang.org/doc/man/escript.html). Scripts must export `main/1`.

```bash
# Calls `main/1` in `script.erl` passing arguments as an array of Strings
#  i.e. main(["alpha", "beta"])
❯ escript script.erl alpha beta
```
