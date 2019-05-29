var String dict: readFile("test.dict")
var List dict: parseDict(dict)

print(conc("Clicked x: ", makestring(dict[2])))
print(conc("Clickec y: ", makestring(dict[3])))
print(conc("With: ", dict[1]))

wait(3)
