package com.hagl
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import java.util.LinkedList
import scala.collection.mutable.HashSet

class SenamicsError(symbolName : String) extends Exception(symbolName + " has invalid senamics define")
class RedefinedSymbol(symbolName : String) extends Exception(symbolName + "redefined")
class IndexOccupy(symbolName : String, index : Int) extends Exception(symbolName + " senamics " + index + "has been occupied")
class NoSuchSymbol(symbolName : String) extends Exception(symbolName + " not found")
class IllegalGenerationState(expression : Expression) extends Exception(expression.toString + " illegal for generate")

trait InstrumentGenerator{
	def generate(exp : Expression, dest : String) : Unit
	
	def generateFor(exp : Expression) = {
		val destIndex = ProgramGenerator.currentFunc.currentTmpIndexUsed
		ProgramGenerator.currentFunc.currentTmpIndexUsed += 1
		var output : String = if(ProgramGenerator.currentFunc.isVertex){
			"vt" + destIndex
		}else{
			"ft" + destIndex
		}
		
		generate(exp, output)
		
		output
	}
}

object InstrumentGenerator{
	def getVariableName(name : String) = {
		name match{
			case "op" => "op"
			case "oc" => "oc"
			case _ =>{ 
				ProgramGenerator.variables.get(name) match{
				case None => {
					ProgramGenerator.currentFunc.tmpValues.get(name) match{
						case None => throw new NoSuchSymbol(name)
						case e : Some[Declaration] => e.get.senamics.label + e.get.senamics.index
					}
				}
				case e : Some[Declaration] => {
						e.get.senamics.label + e.get.senamics.index + e.get.senamics.appendedMessage
				  }
				} 
			}
		}
	}
	
	def generate(exp : Expression, dest : String){
		if(exp.isInstanceOf[CallFunc]){
			CallFuncGenerator.generate(exp, dest)
		}else if(exp.isInstanceOf[BinerayOperatorExpression]){
			OperatorGenerator.generate(exp, dest)
		}
	}
	
	def generateFor(exp : Expression) : String = {
		if(exp.isInstanceOf[Assignment]){
			val assigntment = exp.asInstanceOf[Assignment]
			val dest = getVariableName(assigntment.name)
			if(assigntment.expression.isInstanceOf[IdentifierExpression]){
				val src = getVariableName(assigntment.expression.asInstanceOf[IdentifierExpression].name)
				ProgramGenerator.currentFunc.instrumentList.add("mov " + dest + ", " +  src)
			}else{
				generate(assigntment.expression, dest)
			}
			dest
		}else if(exp.isInstanceOf[IdentifierExpression]){
			val identifiier = exp.asInstanceOf[IdentifierExpression]
			getVariableName(identifiier.name)
		}else if(exp.isInstanceOf[CallFunc]){
			CallFuncGenerator.generateFor(exp)
		}else if(exp.isInstanceOf[BinerayOperatorExpression]){
			OperatorGenerator.generateFor(exp)
		}else{
			throw new IllegalGenerationState(exp)
		}
	}
}

object CallFuncGenerator extends InstrumentGenerator{
	val unaryFuncList = HashSet[String]("kil")
	def generate(exp : Expression, dest : String){
		val orgIndexUsed = ProgramGenerator.currentFunc.currentTmpIndexUsed
		val callFunc = exp.asInstanceOf[CallFunc]
		val itr = callFunc.args.list.iterator
		if(unaryFuncList.contains(callFunc.name)){
			ProgramGenerator.currentFunc.instrumentList.add(callFunc.name + " " + InstrumentGenerator.generateFor(callFunc.args.list.iterator.next))
		}else{
			val strBuff = new StringBuffer(callFunc.name + " " + dest)
			var i = 0
			while(itr.hasNext){
				val param = itr.next
				strBuff.append(",")
				strBuff append InstrumentGenerator.generateFor(param)
				i += 1
			}
			ProgramGenerator.currentFunc.instrumentList.add(strBuff.toString)
		}
		ProgramGenerator.currentFunc.currentTmpIndexUsed = orgIndexUsed
	}
}

object OperatorGenerator extends InstrumentGenerator{
	def generate(exp : Expression, dest : String) {
		val orgIndexUsed = ProgramGenerator.currentFunc.currentTmpIndexUsed
		val oper = exp.asInstanceOf[BinerayOperatorExpression]
		val strBuff = new StringBuffer(oper.operator.instrument + " " + dest +  ",")
		strBuff append InstrumentGenerator.generateFor(oper.left)
		strBuff append ("," + InstrumentGenerator.generateFor(oper.right))
		ProgramGenerator.currentFunc.currentTmpIndexUsed = orgIndexUsed
		ProgramGenerator.currentFunc.instrumentList.add(strBuff.toString)
	}
}


class Func(val isVertex : Boolean){
	val tmpValues = HashMap[String, Declaration]()
	var currentTmpIndexUsed : Int = 0
	val instrumentList = new LinkedList[String]
	
	def generate(expressionList : ExpressionList) = {
		var itr = expressionList.list.iterator
		while(itr.hasNext){
			val cur = itr.next
			if(cur.isInstanceOf[Declaration]){
				val declare = cur.asInstanceOf[Declaration]
				if(isVertex){
					declare.senamics = new VertexTmp(currentTmpIndexUsed)	
				}else{
					declare.senamics = new FragmentTmp(currentTmpIndexUsed)
				}
				currentTmpIndexUsed += 1
				tmpValues.put(declare.name, declare)
			}
		}
		itr = expressionList.list.iterator
		while(itr.hasNext){
			val cur = itr.next
			if(!cur.isInstanceOf[Declaration]){
				InstrumentGenerator.generateFor(cur)
			}
		}
	}
}

object ProgramGenerator {
	val declaredVertexConst = HashMap[String, Declaration]()
	val vcIndexToDeclaration = HashMap[Int, Declaration]()
	
	val declaredVertexVariable = HashMap[String, Declaration]()
	val vaIndexToDeclaration = HashMap[Int, Declaration]()
	
	val declaredFragmentConst = HashMap[String, Declaration]()
	val fcIndexToDeclaration = HashMap[Int, Declaration]()
	
	val varying = HashMap[String, Declaration]()
	val vIndexToDeclaration = HashMap[Int, Declaration]()
	
	val fs = HashMap[String, Declaration]()
	val fsIndexToDeclareation = HashMap[Int, Declaration]()
	
	val variables = HashMap[String, Declaration]()
	
	var vertexFunc : Func = null
	var fragmentFunc : Func = null
	
	val instrumentList = ListBuffer[String]() 
	
	var currentFunc : Func = null
	
	val senamicsTypeToDictMap = HashMap[Class[_], HashMap[String, Declaration]](
			(classOf[Varying], varying),
			(classOf[VertexConst], declaredVertexConst),
			(classOf[FragmentConst], declaredFragmentConst),
			(classOf[VertexVariable], declaredVertexVariable),
			(classOf[FragmentSampler], fs)
	)
	
	val senamicsTypeToIndexMap = HashMap[Class[_], HashMap[Int, Declaration]](
			(classOf[Varying], vIndexToDeclaration),
			(classOf[VertexConst], vcIndexToDeclaration),
			(classOf[FragmentConst], fcIndexToDeclaration),
			(classOf[VertexVariable], vaIndexToDeclaration),
			(classOf[FragmentSampler], fsIndexToDeclareation)
	)
	
	private def _processDeclaration(exp : Declaration) = {
		if(exp.senamics eq null){
			throw new SenamicsError(exp.name)
		}
		if(variables.contains(exp.name)){
			throw new RedefinedSymbol(exp.name)
		}
		variables.put(exp.name, exp)
		
		senamicsTypeToIndexMap.get(exp.senamics.getClass) match{
			case None => throw new Error("unknow senamics arround " + exp.name)
			case indexMap : Some[HashMap[Int, Declaration]] => {
				if(indexMap.get.contains(exp.senamics.index)){
					throw new IndexOccupy(exp.name, exp.senamics.index)
				}
				indexMap.get += ((exp.senamics.index, exp))
				val dictMap = senamicsTypeToDictMap.get(exp.senamics.getClass)
				assert(dictMap != None)
				dictMap.get += ((exp.name, exp))
			}
		}
	}
	
	private def _processVertexFunc(funcDefine : ExpressionList) = {
		vertexFunc = new Func(true)
		currentFunc = vertexFunc
		vertexFunc.generate(funcDefine)
	}
	
	private def _processFragmentFunc(funcDefine : ExpressionList) = {
		fragmentFunc = new Func(false)
		currentFunc = fragmentFunc
		fragmentFunc.generate(funcDefine)
	}
	
	def print = {
		println("vertex shader:") 
		var itr = vertexFunc.instrumentList.iterator
		while(itr.hasNext){
			println(itr.next)
		}
		
		println("fragment shader:")
		itr = fragmentFunc.instrumentList.iterator
		while(itr.hasNext){
			println(itr.next)
		}
	}
	
	def generate(expressionList : ExpressionList) = {
		val itr = expressionList.list.iterator
		while(itr.hasNext){
			val expression = itr.next
			if(expression.isInstanceOf[Declaration]){
				_processDeclaration(expression.asInstanceOf[Declaration])
			}
			else if(expression.isInstanceOf[FunctionDefine]){
				val funcDefine = expression.asInstanceOf[FunctionDefine]
				funcDefine.name match{
					case "vertexMain" => _processVertexFunc(funcDefine.body)
					case "fragmentMain" => _processFragmentFunc(funcDefine.body)
				}
			}
		}
	}
}