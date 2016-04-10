# Mini template engine with dynamic method calls

doFile("./map-literal.io")

# TODO I need to run map-literal.io for { "key": "value" } syntax, but 
#  doFile("map-literal.io") doesn't make the ":" operator available

# I can execute both files with:
# $ io map-literal.io html.io

# However, that doesn't write anything to stdout! Opening the REPL immediately
#  after eval does get output:
# $ io -i map-literal.io html.io

html := Object clone

html openTag := method(name, attrMap,
  attrList := attrMap map(key, value, 
    "#{key}=\"#{value}\"" interpolate
  )

  attrStr := if(attrList size > 0,
    " " .. (attrList join(" ")),
    ""
  )

  "<#{name}#{attrStr}>" interpolate
)

html closeTag := method(name,
  "</#{name}>" interpolate
)

# TODO indent HTML  
# TODO shorthand class attributes , e.g. p.intro.hidden("the content")
#  (Io doesn't allow `#` in slot names, so an id shorthand won't work)
html forward := method(
  tagName := call message name

  argValues := call message arguments map(e, 
    html doMessage(e))

  if (argValues first proto == Map,
    attrs := argValues removeFirst,
    attrs := Map
  )

  list(openTag(tagName, attrs)) appendSeq(argValues) push(closeTag(tagName)) join("")
)

html div({ "class": "about-io" },
  p({ "class": "intro" },
    "Click ", 
    a({ "href": "http://iolanguage.org" }, "here"), 
    " pls"
  ),
  p("Bye!")
  "bye"
) println
