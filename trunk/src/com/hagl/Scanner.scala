package com.hagl

import scala.io.Source
import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

trait Initialzable{ 
  def init(str : String)
}

abstract class SymbolBase{
}

object LineSpitSymbol extends SymbolBase{
  override def toString() =  {"endLine"}
}

abstract class SingleSymbol extends SymbolBase with Initialzable

abstract class StringSymbol extends SingleSymbol{
  private var _value : String = null
  
  override def init(str : String){
    _value = str
  }
  
  def str = {
    _value
  }
  
  override def toString() = {
    getClass().getName + "(" + _value + ")"
  }
} 

class DigitSymbol extends SingleSymbol{
  private var _value : Float = Float.NaN
  
  override def init(str : String)  {
    _value = str.toInt
  }
  
  def value = {
    _value
  }
  
  override def toString() = {
    getClass().getName + "(" + _value + ")"
  }
}

class SenamicsSymbol extends SingleSymbol{
	private var _isOnVertex = false
	private var _isVarying = false
	private var _isConst = false
	private var _index = -1
	private var _fs = false
	
	def isOnVertex = _isOnVertex
	def isVarying = _isVarying
	def isConst = _isConst
	def index = _index 
	def fs = _fs
	
	override def init(str : String)  {
		if(str.length == 2 && str.charAt(0) == 'v'){
			_isVarying = true
			_index = str.charAt(1) - '0';
		}
		else{
			str.charAt(0) match{
				case 'v' => _isOnVertex = true
				case 'f' => 
				case _ => throw new UnrecognizedSymbol
		  }
			str.charAt(1) match{
				case 'c' => _isConst = true
				case 'a' => if(str.charAt(0) == 'f') throw new UnrecognizedSymbol
				case 's' => if(str.charAt(0) == 'v') throw new UnrecognizedSymbol else _fs = true
				case _ => throw new UnrecognizedSymbol
			}
			_index = str.charAt(2) - '0'
		}
	}	
}

class TypeSymbol extends SingleSymbol{
  private var _dimension : Int = -1
  
  override def init(str : String)  {
  	if(str.equals("void")){
  		_dimension = 0
  	}else{
  		_dimension = str.last.toInt - '0'.toInt
  	}
		if(_dimension == 44) 16 else _dimension
	}
  
  def dimension = {
    _dimension
  }
  
  override def toString() = {
    getClass().getName + (if(_dimension > 0) 
    	"(float" + _dimension + ")" 
    	else
    		"void")
  }  
}

class OperatorSymbol extends StringSymbol
class DeclareSymbol extends StringSymbol

abstract class SymbolContainer extends SymbolBase{
  private val _symbolList = ListBuffer[SymbolBase]()
  
  def append(symbol : SymbolBase){
    _symbolList += symbol
  }
  
  def symbolList = {
    _symbolList
  } 
  
  override def toString() = {
    val strBuffer = new StringBuffer
    strBuffer.append(getClass().getName)
    strBuffer.append("\r\n{\r\n")
    
    _symbolList foreach{itr => {
      strBuffer.append(itr.toString)
      strBuffer.append("\r\n")
    }}
    
    strBuffer.append("}\r\n")
    strBuffer.toString
  } 
}

class BraceSymbol extends SymbolContainer
class DashSymbol extends SymbolContainer

abstract class BlockEnderSymbol extends SymbolBase

object BraceEndSymbol extends BlockEnderSymbol
object DashEndSymbol extends BlockEnderSymbol

abstract class SymbolParser{
  def parse(str : String) : (Int, Option[SymbolBase])
}

abstract class RegexSymbolParser[T <: SingleSymbol](val regex : Regex, val clazz : Class[T]) extends SymbolParser{
  override def parse(str : String) : (Int, Option[SymbolBase]) = {
    regex findPrefixMatchOf str match{
      case None => (0, None)
      case e : Some[Match] => (e.get.matched.size, Some[SymbolBase](createSymbol(e.get.matched)))
    }
  }
  
  protected def createSymbol(str : String) = {
    val ret = clazz.newInstance().asInstanceOf[T]
    ret.init(str)
    ret
  }
}

object DigitParser extends RegexSymbolParser[DigitSymbol]("([0-9]\\.[0-9]*)|(\\.[0-9]*)|([0-9]+)".r, classOf[DigitSymbol])

object SenamicsSymbolParser extends RegexSymbolParser[SenamicsSymbol]("((v[ac])|(f[cs])|v)\\d".r, classOf[SenamicsSymbol])

object TypeParser extends RegexSymbolParser[TypeSymbol]("(float(44|[1-4]))|(void)".r, classOf[TypeSymbol])
object DeclareParser extends RegexSymbolParser[DeclareSymbol]("\\w+\\d*".r, classOf[DeclareSymbol])

object OperatorParser extends RegexSymbolParser[OperatorSymbol]("[(==)|([\\+\\-\\*\\/,=:])]".r, classOf[OperatorSymbol])

object DashParser extends SymbolParser{
  override def parse(str : String) : (Int, Option[SymbolBase]) = {
    str.charAt(0) match{
      case '(' =>  (1, Some[SymbolBase](new DashSymbol()))
      case ')' =>  (1, Some[SymbolBase](DashEndSymbol))
      case _ => (0, None)
    }
  }
} 

object BraceParser extends SymbolParser{
  override def parse(str : String) : (Int, Option[SymbolBase]) = {
    str.charAt(0) match{
      case '{' =>  (1, Some[SymbolBase](new BraceSymbol()))
      case '}' =>  (1, Some[SymbolBase](BraceEndSymbol))
      case _ => (0, None)
    }
  }
}

class UnrecognizedSymbol extends Error

class Scanner {
  val containerStack = new Stack[SymbolContainer]
  val symbolList = new ListBuffer[SymbolBase] 
  val symbolParser = ListBuffer[SymbolParser](DashParser, BraceParser, OperatorParser, SenamicsSymbolParser, TypeParser, DigitParser, DeclareParser)
  
  private def onSymbol(symbol : SymbolBase){
    if(symbol.isInstanceOf[SymbolContainer]){
      val block = symbol.asInstanceOf[SymbolContainer]
      containerStack push block
    }else if(symbol.isInstanceOf[BlockEnderSymbol]){
      val block = containerStack.pop
      if(containerStack.size > 0){
        containerStack.top.append(block)
      }else{
    	symbolList += block
      }
    }else if(containerStack.size > 0){
      containerStack.top.append(symbol)
    }else{
      symbolList += symbol
    }
  }
  
  private def parseItem(item : String) = {
    var ret : Int = 0 
    symbolParser.find{(parser) => {
      val result = parser.parse(item)
      result._2 match{
        case None => false
        case e : Some[SymbolBase] => {
          ret = result._1
          onSymbol(e.get)
          true
        }
      }
    }}
    if(ret == 0){
      throw new UnrecognizedSymbol
    }
    ret
  }
  
  private def parseLine(line : String) = {
    var current = line.trim
    while(current.length > 0){
      val pos = parseItem(current)
      current = current.substring(pos).trim
    }
    if(containerStack.size > 0){
      if(containerStack.top.symbolList.size > 0){
        containerStack.top.append(LineSpitSymbol)
      }
    }else if(symbolList.size > 0 && (symbolList.last ne LineSpitSymbol)){
      symbolList += LineSpitSymbol
    }
  }
  
  def parse(filePath : String){
  	Source.fromFile(filePath).getLines().foreach {parseLine(_)}
  }
  
  def print {
  	assert(containerStack.size == 0)
    symbolList foreach{itr => {
	  println(itr.toString)
    }}
  }
}