package jackcompiler

import jackcompiler.ast.*

class AstPrinter extends Visitor {
    def visitLetStatement (v: LetStatement) = {
        print ("(def ")
        v.id.accept(this)
        print (" ")
        v.exp.accept(this)
        print (") ")
    }
    def visitExpression (v: Expression) = {

    }

    def visitVariable (v: Variable) = {
        print(v.varName)
    }

    def visitIntegerLiteral (v: IntegerLiteral) = {
        print(v.value)
    }

    def visitStatements (v: Statements) = {
        print ("( ")

        for (st <- v.sts) st.accept(this) 

        print (")")
    }

    def visitIfStatement (v: IfStatement)  = {
        print ("(if ")

       
        v.condition.accept(this)
       
        v.thenBranch.accept(this)

        for (st <- v.elseBranch) st.accept(this) // else is a option

        print (" ) ")


    }


    def visitWhileStatement (v: WhileStatement)  = {
        
        print ("(while ")

       
        v.condition.accept(this)
       
        v.body.accept(this)

        print (" ) ")
    }



    def visitBinaryExpression (v: BinaryExpression) = {
     
                print("(" + v.operator)
                v.left.accept(this)
                print (" ")
                v.right.accept(this)
                print (")")
         
        
    }

    

}
