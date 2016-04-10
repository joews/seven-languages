# Mini template engine with dynamic method calls
# * Uses forward to change prototype delegation
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

html
