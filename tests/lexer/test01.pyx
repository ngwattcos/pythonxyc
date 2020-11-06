# this is a comment

'''this is a
multline comment'''

# variables initialization
t = 0
@const x = "a"
@let y = 2

# variable update
y = y + 1
y += 2

# dicts and lists
@let dict = {
    "a": 1
    2: "b"
}

@let dataList = ["Alice", "Bob", 2, 3]

# function declaration
def func(a, b):
    if (a == b):
        return a
    return b
