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
        print (") ")

        for (st <- v.sts) st.accept(this) 
        
        print ("(")
    }

    def visitIfStatement (v: IfStatement)  = {
        print ("(if ")

        print ("(")
        v.condition.accept(this)
        print (") ")

        v.thenBranch.accept(this)


        print (") ")
        
    }


    def visitBinaryExpression (v: BinaryExpression) = {
     
                print("(" + v.operator)
                v.left.accept(this)
                print (" ")
                v.right.accept(this)
                print (")")
         
        
    }

    

}
