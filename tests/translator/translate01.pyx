# Lists
@let arr = []
@let arr2 = [1, 2, 3, 4]
@let r = len(arr)
@let c = len([0, 1])

# Dictionaries
@let d = {"a":1, "b":2, "c":4}


@let dict = {

    1: 1,
    # extra newlines
    "2": "2",
    named_var: (another_var + 2)

}


@let dict_base = {}


@let dict_simple = {"a":2}


# Printing function, most types, multiple args
print(len([0, 1]))
print(1)
print("hello")
print([1, 2])
print({})
print(1 + 1)
print(1, 2, 3, "hello", 6)