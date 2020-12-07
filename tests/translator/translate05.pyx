# function definitions and lambdas

# first function: no params
def fun1():
    # some commands
    print("first command")
    print("second command")
    return
@end

# second function: some params
def fun2(arg1, arg2):
    # some commands
    print(arg1)
    print(arg2)
    return 0
@end

# testing indentation
def fun3():
    return "adfsdf"
@end

def fun4():
    return lambda x: x + 1
@end

def fun5():
    return
@end

@let add = lambda a, b: a + c