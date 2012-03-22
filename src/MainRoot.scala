import com.hagl.Scanner
import com.hagl.SynaxParser
import com.hagl.SymbolBase
import com.hagl.ProgramGenerator
import com.hagl.ExpressionList

object MainRoot {
	def main(args : Array[String]){
	  val scanner = new Scanner
	  scanner parse args(0)
	  val symbolList = scanner.symbolList
	  
	  val exp = SynaxParser.parse(symbolList.toArray[SymbolBase])
	  println(exp.get)
	  
	  ProgramGenerator.generate(exp.get.asInstanceOf[ExpressionList])
	  ProgramGenerator.print
	}
}