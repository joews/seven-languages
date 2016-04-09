# dynamic HTML generation
html := Object clone

html forward := method(
  tagName := call message name
  openTag := "<#{tagName}>" interpolate 
  closeTag := "</#{tagName}>" interpolate 

  buffer := list(openTag)

  # TODO attributes arg 
  # TODO indent HTML  
  # TODO shorthand class calls, e.g. p.intro.hidden("the content")
  #  (Io doesn't allow `#` in slot names, so an id shorthand won't work)
 
  call message arguments foreach(arg,
    child := if(arg proto == String,
      arg,
      html doMessage(arg)
    )

    buffer push(child)
  )


  buffer push(closeTag)
  buffer join("")
)

html p(
  a.intro("link"), 
  "bob"
) println
  
html
