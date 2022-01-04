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

        def visitDoStatement (v: DoStatement) = {
        
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

    def visitReturnStatement (v: ReturnStatement) = {


        print ("(return ")
        
        for (exp <- v.value) exp.accept(this) 

        print (" ) ")

    }


    def visitBinaryExpression (v: BinaryExpression) = {
     
                print("(" + v.operator)
                v.left.accept(this)
                print (" ")
                v.right.accept(this)
                print (")")
         
        
    }

     def visitUnaryExpression (v: UnaryExpression) = {
     
                print("(" + v.operator)
                print (" ")
                v.right.accept(this)
                print (")")
     }

     def visitCall (v: Call) = {
        
    }


     def visitKeywordLiteral (v: KeywordLiteral) = {
         print ("Keyword(")
         print(v.value)   
         print (")")
     }


    

}
