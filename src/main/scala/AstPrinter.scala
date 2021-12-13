package jackcompiler

import jackcompiler.ast.*

class AstPrinter extends Visitor {
    def visitLetStatement (v: LetStatement) = {
        print ("(def ")
        visitExpression (v.id)
        visitExpression(v.exp)
        print (") ")
    }
    def visitExpression (v: Expression) = {
        v match {
            case IntegerLiteral (v) => print (v)
            case Variable (varname) => {
                print (" ")
                print(varname)
                print (" ")
            }

            case _ => {}
        }
    }

}
