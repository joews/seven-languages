# Io notes

* Activate add-ons by calling their slot, e.g. `Range`.
* Activate the `Range` add-on to create ranges from numbers, e.g. `1 to(10)`.
* Import file in a REPL session: `doFile("/path/to/file")`
* `OperatorTable` changes are only available in files parsed _after_ the one in which they are defined. Operators need to be defined in a file up-front, which then includes files that use it (see the `html` example).
* `doFile` can be passed to any receiver. This is useful for namespacing imports (see `html`).
