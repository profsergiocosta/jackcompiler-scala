
package JackCompiler

import java.io._

class VMWriter (val fn:String) {


    val file = new File(fn)
    val bw = new BufferedWriter(new FileWriter(file))

    def write (s:String) {
        bw.write (s)
        bw.newLine()
    }


    def writeLabel(label:String) { 
        write ( "label " + label )
    }

    

    def close () = bw.close


}