package com.hagl
import scala.collection.mutable.HashMap

class SynaxError(currentExp : Expression, lastMeetedExp : Expression) extends Exception(currentExp + " can not handle " + lastMeetedExp)
class SymbolParseError(symbol : SymbolBase) extends Exception(symbol + " can not be parsed")

abstract class SynaxParser {
  def parse(sym : SymbolBase) : Option[Expression]
}

object SynaxParser{
	val symbolClassParserMap = HashMap[Class[_], SynaxParser](
			(classOf[DashSymbol], DashSymbolParser),
			(classOf[BraceSymbol], BraceSymbolParser),
			(classOf[OperatorSymbol], OperatorSymbolParser),
			(classOf[DeclareSymbol], DeclareSymbolParser),
			(classOf[DigitSymbol], DigitSymbolParser),
			(classOf[TypeSymbol], TypeSymbolParser),
			(classOf[SenamicsSymbol], SenamicsParser)
	)
	
	def parseOneSymbol(sym : SymbolBase) : Option[Expression] = {
		symbolClassParserMap.get(sym.getClass()) match{
			case None => None
			case e : Some[SynaxParser] => {
				e.get.parse(sym)
			}
		}
	}
	
	def parse(arr : Array[SymbolBase]) : Option[Expression] = {
		var firstExp = parseOneSymbol(arr(0))
		if(firstExp == None){
			throw new SymbolParseError(arr(0))
		}
		var i = 1
		var lastExp : Option[Expression] = None
		var last : Option[Expression] = None
		
		while(i < arr.size){
			firstExp match{
				case None => {
					throw new SynaxError(lastExp.get,last.get)
					}
				case e : Some[Expression] => {
					if(arr(i) == LineSpitSymbol){
						if(!firstExp.get.isInstanceOf[ExpressionList]){
							lastExp = firstExp
							firstExp = Some[Expression](new ExpressionList(firstExp.get))
						}
					}
					else{
						parseOneSymbol(arr(i)) match{
							case None => throw new SymbolParseError(arr(i))
							case g : Some[Expression] => {
								last = g
								lastExp = firstExp
								firstExp = firstExp.get.accept(g.get)
							} 
						}
					}
				}
			}
			i+=1
		}			
		firstExp
	}
}

object DeclareSymbolParser extends SynaxParser{
	def parse(sym : SymbolBase) : Option[Expression] = {
		val declare = sym.asInstanceOf[DeclareSymbol]
		Some[Expression](new IdentifierExpression(declare.str))
	}
}

object DigitSymbolParser extends SynaxParser{
	def parse(sym : SymbolBase) : Option[Expression] = {
		val digit = sym.asInstanceOf[DigitSymbol]
		Some[Expression](new DigitExpression(digit.value))
	}
}

object TypeSymbolParser extends SynaxParser{
	val demension = HashMap[Int, Type](
			(16, Float44),
			(4, Float4),
			(3, Float3),
			(2, Float2),
			(1, Float1),
			(0, VoidType)
	)
			
	def parse(sym : SymbolBase) : Option[Expression] = {
		val tybe = sym.asInstanceOf[TypeSymbol]
		demension.get(tybe.dimension)
	}
}

object SenamicsParser extends SynaxParser{
	def parse(sym : SymbolBase) : Option[Expression] = {
		val senamics = sym.asInstanceOf[SenamicsSymbol]
		if(senamics.fs){
			Some[Expression](new FragmentSampler(senamics.index))
		}
		else if(senamics.isConst){
			if(senamics.isOnVertex){
				Some[Expression](new VertexConst(senamics.index))
			}else{
				Some[Expression](new FragmentConst(senamics.index))
			}
		}else if(senamics.isVarying){
			Some[Expression](new Varying(senamics.index))
		}else if(senamics.isOnVertex){
			Some[Expression](new VertexVariable(senamics.index))
		}else{
			None
		}
	}
}

object OperatorSymbolParser extends SynaxParser{
	def parse(sym : SymbolBase) : Option[Expression] = {
		val operSym = sym.asInstanceOf[OperatorSymbol]
		operSym.str match{
			case "=" => Some[Expression](EqualMark) 
			case "," => Some[Expression](Comma)
			case "+" => Some[Expression](AddOp)
			case "-" => Some[Expression](SubOp)
			case "*" => Some[Expression](MulOp)
			case "/" => Some[Expression](DivideOp)
			case ":" => Some[Expression](Colon)
			case _ => None
		}
	}
}

abstract class BlockSymbolParser[T <: SymbolContainer] extends SynaxParser{
	protected def emptyBlock : ExpressionContainer
	
	def parse(sym : SymbolBase) : Option[Expression] = {
		if(!sym.isInstanceOf[T]){
			None
		}else{
			val dashsym = sym.asInstanceOf[T]
			if(dashsym.symbolList.size == 0){
				Some[Expression](emptyBlock)
			}else{
				val arr = new Array[SymbolBase](dashsym.symbolList.size)
				dashsym.symbolList.copyToArray(arr)
				SynaxParser.parse(arr) match{
					case None => None
					case e : Some[Expression] => {
						if(e.get.isInstanceOf[ExpressionList]){
							e
						}
						else if(e.get.isInstanceOf[ParameterList]){
							e
						}
						else{
							val newList = emptyBlock
							newList add e.get
							Some[Expression](newList)
						}
					}
				}
			}
		}
	}
}

object DashSymbolParser extends BlockSymbolParser[DashSymbol]{
	protected def emptyBlock = new ParameterList
}
object BraceSymbolParser extends BlockSymbolParser[BraceSymbol]{
	protected def emptyBlock = new ExpressionList
}
