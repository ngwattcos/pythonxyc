

# The first line is intentionally a newline 

# Variable declaration requires @let or @const
@let t = 1
@let y = "Hello world"
@const x = 7.6

# General expressions
t += 1
t = 5 % t + 1

# t = "!" + y is currently not supported
t = "!" + "Hello world"
