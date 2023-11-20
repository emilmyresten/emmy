type expression = PlusExpr of expression * expression 
                | MinusExpr of expression * expression
                | Integer of int