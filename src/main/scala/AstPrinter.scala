package jackcompiler

import jackcompiler.ast.*

class AstPrinter extends Visitor {
    def visitLetStatement (v: LetStatement) = {
        print ("(def ")
        visitIdentifier (v.id)
        visitExpression(v.exp)
        print (") ")
    }
    def visitExpression (v: Expression) = {
        v match {
            case IntegerLiteral (v) => print (v)
        }
    }

    def visitIdentifier (v: Identifier) = {
        v match {
            case Variable (varname) => {
                print (" ")
                print(varname)
                 print (" ")
            }
        }
    }
    def visitVariable (v: Variable) = {
        println(v.varName)
    }
    def visitLiteraInteger (v: IntegerLiteral) = {
        println(v.value)
    }
}
