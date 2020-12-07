# react components


# empty/base components
def Base(props):
    return <p></p>
@end

# empty/base components with attributes
def BaesAttrib(props):
    @let t2 = <cust a="1" b="2"></cust>
    return t2
@end

# component with exp value
def Exp(props):
    return <p>{abcd}</p>
@end

# component with exp value and attribs
def ExpAttrib(props):
    return <a href="http://www.google.com" className={"class-" + str(2)}>{abcd}</a>
@end

# component with children
def Nested(props):
    return <Cust1>
            <Cust2>
                <Cust3>
                </Cust3>
            </Cust2>
            <Cust4>
            </Cust4>
        </Cust1>
@end

# component with attributes with children with attributes
def Nested(props):
    return <Cust1 className={"class-" + variant}>
            <Cust2 a="a" b="b">
                <Cust3  a={variable} onCancel={callback()}>
                </Cust3>
            </Cust2>
            <Cust4  a={"a"} b={"b"}>
            </Cust4>
        </Cust1>
@end