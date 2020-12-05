# tests string values
@let str1 = "abc"

@let str2 = 0

# alphanumeric characters, all symbole achievable from holding SHIFT + another key
# on a standard keyboard, and whitespace are allowed
# double quotes are not a valid string character
@let a = "the quick brown fox jumps over the lazy dog"
@let b = "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG"
@let c = "1234567890qwertyuiop"

@let concat1 = "" + ""

@let concat2 = "a" + "b"

@let a = str(var)
@let a = str(1 + 2)

