# multiple newlines should compress to a single one



# a program should be able to have an arbitrary number of newlines between commands
# more than 0
# But how should sequences of back-to-back newlines be interpreted?
# (newlines following many comments like here)

def func(a, b):


    # extend operator should consume an arbitrary amount of whitespace
    # (tabs, spaces, newlines, etc.)
    if a and b or a == False:
        return a ...


        + ...

        
        b
    @end
    return b - 2
@end