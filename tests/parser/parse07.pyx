# function call commands

# basic func call
fun()

# function call on a var access ending with an array index
objs[0][1].randomProperty1.randomProperty2[0]()

# function call on a var access ending with a dict index
objs[0][1].randomProperty1.randomProperty2["sdfadf"]()

# function call on a var access ending with a dot
objs[0][1].randomProperty1.randomProperty2[0].method()

# function call on a var access ending with an array index, with args
objs[0][1].randomProperty1.randomProperty2[0](1, "2", three())

# function call on a var access ending with a dict index
objs[0][1].randomProperty1.randomProperty2["sdfadf"](1, "2", three())

# function call on a var access ending with a dot
objs[0][1].randomProperty1.randomProperty2[0].method(1, "2", three())
