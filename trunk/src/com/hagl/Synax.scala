package com.hagl
import scala.collection.mutable.HashMap
import java.util.LinkedList
import java.util.ArrayList

abstract class Expression{
  def accept(nextExpression : Expression) : Option[Expression]
  def isNested = false
}

abstract class ExpressionContainer(first : Expression*) extends Expression{
	private val _list = new LinkedList[Expression]
	first foreach(_list add _)
	
  def list = _list
  
  def add(e : Expression){
    _list add e
  }
}

class ExpressionList(first : Expression*) extends ExpressionContainer{
	first foreach(list add _)
	
  override def toString() = {
  	val strBuilder = new StringBuilder
  	strBuilder.append("expList{\r\n")
  	val itr = list.iterator
  	while(itr.hasNext){
  		strBuilder.append(itr.next.toString)
  	}
  	strBuilder.append("}\r\n")
  	strBuilder.toString
  }
  
  def accept(nextExpression : Expression) : Option[Expression] = {
    if(list.size == 0){
      list add nextExpression
    }
    val last = list.removeLast
		last accept nextExpression match{
		  case None => {
		  	list add last
		  	list add nextExpression
		  }
		  case e : Some[Expression] => {
		  	val ee = e.get
		    if(e.get.isInstanceOf[ExpressionList]){
		      val itr = e.get.asInstanceOf[ExpressionList].list.iterator
		      while(itr.hasNext) {list add itr.next}
		    }else{
		      list add e.get
		    }
		  }
    }
    Some[Expression](this)
  }
}

abstract class Type extends Expression{
	override def accept(nextExpression : Expression) : Option[Expression] = {
		if(nextExpression.isInstanceOf[IdentifierExpression]){
			Some[Expression](new Declaration(nextExpression.asInstanceOf[IdentifierExpression].name, this))
		}
		else if(nextExpression.isInstanceOf[ParameterList]){
			Some[Expression](new VectorExpression(this, nextExpression.asInstanceOf[ParameterList]))
		}
		else{
			None
		}
	}
}

object VoidType extends Type{
	override def toString() = {
		"void" + "\r\n"
	}
}

abstract class VectorType(val tybe : Class[_ <: AnyVal], val vectorCount : Int) extends Type{
	override def toString() = {
		"float " + vectorCount + "\r\n"
	}
}

class Declaration(val name : String, val tybe : Type, var senamics : Senamics = null) extends Expression{
	private var _acceptingSenamics = false
	
  def accept(nextExpression : Expression) : Option[Expression] = {
    if(nextExpression == EqualMark){
      val ret = new ExpressionList
      ret add this
      ret add new Assignment(name, null)
      Some[Expression](ret)
    }
    else if((senamics eq null) && !_acceptingSenamics){
    	if(nextExpression.isInstanceOf[ParameterList]){
	    	Some[Expression](new FunctionDefine(name, tybe, nextExpression.asInstanceOf[ParameterList]))
	    }
	    else if(nextExpression == Comma){
	    	Some[Expression](new ExpressionList(this))
	    }
	    else if(nextExpression == Colon){
	    	_acceptingSenamics = true
	    	Some[Expression](this)
	    }else{
	    	None
	    }
    }
    else if(nextExpression.isInstanceOf[Senamics] && (senamics eq null)){
    	_acceptingSenamics = false
    	senamics = nextExpression.asInstanceOf[Senamics]
    	Some[Expression](this)
    }
    else if(senamics ne null){
    	senamics.accept(nextExpression) match{
    		case None => None
    		case _ => Some[Expression](this)
    	}
    }
    else{
      None
    }
  }
  
  override def toString() = {
  	"declare " + name + "\r\n" 
  }
}

abstract class Senamics(val index : Int, val label : String) extends Expression{
	def accept(nextExpression : Expression) : Option[Expression] = None
	def appendedMessage = ""
}

class VertexConst(index:Int) extends Senamics(index, "vc")
class Varying(index:Int) extends Senamics(index, "v")
class VertexVariable(index:Int) extends Senamics(index, "va")
class FragmentConst(index:Int) extends Senamics(index, "fc")
class VertexTmp(index:Int) extends Senamics(index, "vt")
class FragmentTmp(index:Int) extends Senamics(index, "ft")

class FragmentSampler(index : Int) extends Senamics(index, "fs"){
	var _para : ParameterList = null
	
	def para = _para
	
	override def accept(nextExpression : Expression) : Option[Expression] = {
		if(nextExpression.isInstanceOf[ParameterList]){
			_para = nextExpression.asInstanceOf[ParameterList]
			Some[Expression](this)
		}else{
			None
		}
	}
	
	override def toString() = {
		"fs" + index + appendedMessage + "\r\n"
	}
	
	override def appendedMessage = {
		if(_para.list.size == 0){
			""
		}else{
			val strBuf = new StringBuffer("<")
			val itr = _para.list.iterator
			if(itr.hasNext){
				strBuf.append(itr.next.asInstanceOf[IdentifierExpression].name)
			}
			while(itr.hasNext){
				strBuf.append("," + itr.next.asInstanceOf[IdentifierExpression].name)
			}
			strBuf.append(">")
			
			strBuf.toString
		}
		
	}
}

class IdentifierExpression(val name : String) extends Expression{
  def accept(nextExpression : Expression) : Option[Expression] = {
  	if(nextExpression == Comma){
      Some[Expression](new ParameterList(this))
    }
  	else if(nextExpression.isInstanceOf[Operator]){
      val op = nextExpression.asInstanceOf[Operator]
      if(op == EqualMark){
      	Some[Expression](new Assignment(name, null))
      }
      else if(op.allowedBinary){
        Some[Expression](new BinerayOperatorExpression(this, null, nextExpression.asInstanceOf[Operator]))
      }
      else{
        None
      }
    }
    else if(nextExpression.isInstanceOf[ParameterList]){
    	val parameterList = nextExpression.asInstanceOf[ParameterList]
      Some[Expression](new CallFunc(this.name, parameterList))
    }else{
      None
    }
  }
  
  override def toString() = {
  	"identifier " + name + "\r\n" 
  }
}

abstract class ConstExpression extends Expression{
	def accept(nextExpression : Expression) : Option[Expression] = {
    if(nextExpression == Comma){
      Some[Expression](new ParameterList(this))
    }
    else if(nextExpression.isInstanceOf[Operator]){
      val op = nextExpression.asInstanceOf[Operator]
      if(op.allowedBinary){
    	Some[Expression](new BinerayOperatorExpression(this, null, nextExpression.asInstanceOf[Operator]))
      }else{
        None
      }
    }
    else{
      None
    }
  }
}

class DigitExpression(val value : Float) extends ConstExpression{
  override def toString() = {
  	"digit " + value + "\r\n" 
  }
}

class VectorExpression(val tybe : Type, val paramList : ParameterList) extends ConstExpression{
	override def toString() = {
		"vector expression{\r\n" + paramList.toString + "}\r\n"
	}
}

abstract class Operator(val allowedUnivary : Boolean, val allowedBinary : Boolean, val instrument : String = null) extends Expression{
  def accept(nextExpression : Expression) : Option[Expression] = None
}

object AddOp extends Operator(true, true, "add")
object SubOp extends Operator(true, true, "sub")
object MulOp extends Operator(false, true, "mul")
object DivideOp extends Operator(false, true, "div")

object Float44 extends VectorType(classOf[Float], 16)
object Float4 extends VectorType(classOf[Float], 4)
object Float3 extends VectorType(classOf[Float], 3)
object Float2 extends VectorType(classOf[Float], 2)
object Float1 extends VectorType(classOf[Float], 1)

object Comma extends Operator(false, true)
object Colon extends Operator(false, false)
object EqualMark extends Operator(false, false)

class ParameterList(val first : Expression*) extends ExpressionContainer{
  first.foreach(list.add(_))
  
  private var currentFinished = true
  
  override def toString() = {
  	 val strBuilder = new StringBuilder
  	strBuilder.append("paraList{\r\n")
  	val itr = list.iterator
  	while(itr.hasNext){
  		strBuilder.append(itr.next.toString)
  	}
  	strBuilder.append("}\r\n")
  	strBuilder.toString
  }
  
  def accept(nextExpression : Expression) : Option[Expression] = {
    if(nextExpression == Comma){
      currentFinished = true
    }else if(currentFinished){
      list add nextExpression
    }else{
      list.peek.accept(nextExpression) match{
        case None => return None
        case e : Some[Expression] => {
          list.removeLast
          list push e.get
        }
      }
    }
    Some[Expression](this)
  }
}

class Assignment(val name : String, var expression : Expression) extends Expression{
  def accept(nextExpression : Expression) : Option[Expression] = {
    if(expression eq null){
      expression = nextExpression
      Some[Expression](this)
    }else{
      expression.accept(nextExpression) match{
        case None => None
        case e : Some[Expression] => {
          expression = e.get
          Some[Expression](this)
        }
      }
    }
  }
  
  override def isNested = {
  	expression.isNested
  }
  
  override def toString() = {
  	"assign " + name + " to " + expression.toString
  }
}

class BinerayOperatorExpression(val left : Expression, var right : Expression, val operator : Operator) extends Expression{
  def accept(nextExpression : Expression) : Option[Expression] = {
    if(right eq null){
      right = nextExpression
      Some[Expression](this)
    }else if(nextExpression == Comma){
      val list = new ParameterList(this)
      Some[Expression](list)
    }else{
      (right accept nextExpression) match{
        case None => None
        case e : Some[Expression] => {
          right = e.get
          Some[Expression](this)
        }
      }
    }
  }
  
  override def isNested = {
  	if(left.isInstanceOf[BinerayOperatorExpression] || right.isInstanceOf[BinerayOperatorExpression]){
  		true
  	}
  	else if(left.isInstanceOf[CallFunc] || right.isInstanceOf[CallFunc]){
  		true
  	}
  	false
  }
  
  override def toString() = {
  	"binerayOp{ \r\n " + left + ", " + right + "}\r\n"
  }
}

class FunctionDefine(val name : String, val retType : Type, var args : ParameterList, var body : ExpressionList = null) extends Expression{
	def accept(nextExpression : Expression) : Option[Expression] = {
		if(body == null && nextExpression.isInstanceOf[ExpressionList]){
			body = nextExpression.asInstanceOf[ExpressionList]
			Some[Expression](this)
		}
		else{
			Some[Expression](new ExpressionList(this, nextExpression))
		}
	}
	
	override def toString() = {
		"function " + name + " type " + retType + " args: " + args.toString + " body: " + body.toString()
	}
}

class CallFunc(val name : String, var args : ParameterList) extends Expression{
  def accept(nextExpression : Expression) : Option[Expression] = {
    if((args eq null) && nextExpression.isInstanceOf[ParameterList]){
      args = nextExpression.asInstanceOf[ParameterList]
      Some[Expression](this)
    }
    else if((args ne null) && nextExpression == Comma){
    	Some[Expression](new ParameterList(this))
    }
    else{
      None
    }
  }
  
  override def toString() = {
  	"callFunc " + name + "{ \r\n " + args.toString + "}\r\n"
  }
  
  override def isNested = {
  	val itr = args.list.iterator
  	var nested = false 
  	while(itr.hasNext && !nested){
  		val par = itr.next
  		if(par.isInstanceOf[BinerayOperatorExpression] || par.isInstanceOf[CallFunc]){
  			nested = true
  		}
  	}
  	nested
  }
}