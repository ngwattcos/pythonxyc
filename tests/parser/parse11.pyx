# var access test

# var access used in an assignment command
obj.items["abc"][0][t].f = 1

# var access as an expression
@const t = a.b.c.d[a][b][c][d].a

# var access as a slice
@let t = arr[0:1]
@let t = arr[a:b + 1]