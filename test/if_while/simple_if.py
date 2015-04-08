'''
program = [Assignment (Name "a") (Number $ Integ 5),
           Assignment (Name "b") (Number $ Flt 2.0),
           If (Binop Eq (Name "a") (Name "b"))
              [[Print $ StrLiteral "equal"],
               [If (Binop Ge (Name "a") (Number $ Integ 2))
                   [[Assignment (Name "c") (Binop Add (Name "a") (Name "b")),
                     Print $ Name "c"],
                    [If (Binop Eq (Name "b") (Number $ Flt 2.0))
                       [[Assignment (Name "d") (Binop Div (Name "a") (Name "b")),
                         Print $ Binop Sub (Name "d") (Name "a")],

                        [Print $ StrLiteral "blah"]]]]]]]
'''

a = 5
b = 2.0

if a == b:
    print "equal"
elif a >= 2:
    c = a + b
    print c
elif b == 2.0:
    d = a / b
    print d - a
else:
    print "blah"
    
