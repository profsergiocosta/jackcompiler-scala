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

}
