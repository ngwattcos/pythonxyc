# Variable ccess test

# Var access used in an assignment command
obj.items["abc"][0][t].f = 1

# Var access as an expression
@const t = a.b.c.d[a][b][c][d].a

# Var access as a slice
@let t = arr[0:1]
@let t = arr[a:b + 1]

@let d = [1, 2, 3, 4][0:3]

# This works recursively
@let e = [1, 2, 3, 4][0:3][0:3]