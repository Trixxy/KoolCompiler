package koolc
package ast

import Trees._

object Printer {
  def apply(t: Tree, depth: Int = 0): String = {
    var sb = new StringBuilder
    t match {
      case Program(main: MainObject, classes: List[ClassDecl]) => {
        sb.append(Printer(main))
        sb.append(expandList(classes, depth))
      }
      case MainObject(id: Identifier, stats: List[StatTree]) => {
        sb.append("object ")
        sb.append(Printer(id))
        sb.append(" {\n")
        sb.append(indent(depth))
        sb.append("def main() : Unit = {\n")
        sb.append(expandList(stats, depth+1))
        sb.append(indent(depth))
        sb.append("}\n")
        sb.append("}\n")
      }
      case ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl]) => {
        sb.append("class ")
        sb.append(Printer(id))
        
        //OPTIONAL PARENT
        parent match {
          case None => ""
          case Some(value) => {
            sb.append(" extends ")
            sb.append(Printer(value))
          }
        }
        sb.append(" {\n");
        sb.append(expandList(vars, depth))
        sb.append(expandList(methods, depth))
        sb.append("}\n")
      }
      case VarDecl(tpe: TypeTree, id: Identifier) => {
        sb.append(indent(depth))
        sb.append("var ")
        sb.append(Printer(id))
        sb.append(" : ")
        sb.append(Printer(tpe))
        sb.append(";\n")
      }
      case MethodDecl(retType: TypeTree, id: Identifier, args: List[Formal], vars: List[VarDecl], stats: List[StatTree], retExpr: ExprTree) => {
        sb.append(indent(depth))
        sb.append("def ")
        sb.append(Printer(id))
        sb.append("(")
        //ARGUMENTS WITH ', '
        var first = true
        for(l <- args) {
          if(!first) sb.append(", ")
          sb.append(Printer(l))
          first = false
        }
        sb.append(") : ")
        sb.append(Printer(retType))
        sb.append(" = {\n")
        sb.append(expandList(vars, depth+1))
        sb.append(expandList(stats, depth+1))
        sb.append(indent(depth+1))
        sb.append("return ")
        sb.append(Printer(retExpr))
        sb.append(";\n")
        sb.append(indent(depth))
        sb.append("}\n")
      }
      case Formal(tpe: TypeTree, id: Identifier) => {
        sb.append(Printer(id))
        sb.append(": ")
        sb.append(Printer(tpe))
      }
      case IntArrayType() => {
        sb.append("int[]")
      }
      case IntType() => {
        sb.append("int")
      }
      case BooleanType() => {
        sb.append("boolean")
      }
      case StringType() => {
        sb.append("string")
      }
      case Block(stats: List[StatTree]) => {
        sb.append("{\n")
        sb.append(expandList(stats, depth))
        sb.append(indent(depth-1))
        sb.append("}\n")
      }
      case If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) => {
        sb.append(indent(depth))
        sb.append("if(")
        sb.append(Printer(expr))
        sb.append(")")
        sb.append(Printer(thn, depth+1))
        els match {
          case None => ""
          case Some(value) => {
            sb.append(indent(depth))
            sb.append("else")
            sb.append(Printer(value, depth+1))
          }
        }
      }
      case While(expr: ExprTree, stat: StatTree) => {
        sb.append(indent(depth))
        sb.append("while(")
        sb.append(Printer(expr))
        sb.append(")")
        sb.append(Printer(stat, depth+1))
      }
      case Println(expr: ExprTree) => {
        sb.append(indent(depth))
        sb.append("println(")
        sb.append(Printer(expr))
        sb.append(");\n")
      }
      case Assign(id: Identifier, expr: ExprTree) => {
        sb.append(indent(depth))
        sb.append(Printer(id))
        sb.append(" = ")
        sb.append(Printer(expr))
        sb.append(";\n")
      }
      case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) => {
        sb.append(indent(depth))
        sb.append(Printer(id))
        sb.append("[")
        sb.append(Printer(index))
        sb.append("] = ")
        sb.append(Printer(expr))
        sb.append(";\n")
      }
      case And(lhs: ExprTree, rhs: ExprTree) => {
        sb.append(Printer(lhs))
        sb.append(" && ")
        sb.append(Printer(rhs))
      }
      case Or(lhs: ExprTree, rhs: ExprTree) => {
        sb.append(Printer(lhs))
        sb.append(" || ")
        sb.append(Printer(rhs))
      }
      case Plus(lhs: ExprTree, rhs: ExprTree) => {
        sb.append(Printer(lhs))
        sb.append(" + ")
        sb.append(Printer(rhs))
      }
      case Minus(lhs: ExprTree, rhs: ExprTree) => {
        sb.append(Printer(lhs))
        sb.append(" - ")
        sb.append(Printer(rhs))
      }
      case Times(lhs: ExprTree, rhs: ExprTree) => {
        sb.append(Printer(lhs))
        sb.append(" * ")
        sb.append(Printer(rhs))
      }
      case Div(lhs: ExprTree, rhs: ExprTree) => {
        sb.append(Printer(lhs))
        sb.append(" / ")
        sb.append(Printer(rhs))
      }
      case LessThan(lhs: ExprTree, rhs: ExprTree) => {
        sb.append(Printer(lhs))
        sb.append(" < ")
        sb.append(Printer(rhs))
      }
      case Equals(lhs: ExprTree, rhs: ExprTree) => {
        sb.append(Printer(lhs))
        sb.append(" == ")
        sb.append(Printer(rhs))
      }
      case ArrayRead(arr: ExprTree, index: ExprTree) => {
        sb.append(Printer(arr))
        sb.append("[")
        sb.append(Printer(index))
        sb.append("]")
      }
      case ArrayLength(arr: ExprTree) => {
        sb.append(Printer(arr))
        sb.append(".length")
      }
      case MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) => {
        sb.append(Printer(obj))
        sb.append(".")
        sb.append(Printer(meth))
        sb.append("(")
        //ARGUMENTS WITH ', '
        var first = true
        for(l <- args) {
          if(!first) sb.append(", ")
          sb.append(Printer(l))
          first = false
        }
        sb.append(")")
      }
      case IntLit(value: Int) => {
        sb.append(value)
      }
      case StringLit(value: String) => {
        sb.append('"');
        sb.append(value)
        sb.append('"');
      }
      case True() => {
        sb.append("true")
      }
      case False() => {
        sb.append("false")
      }
      case Identifier(value: String) => {
        sb.append(value)
      }
      case This() => {
        sb.append("this")
      }
      case NewIntArray(size: ExprTree) => {
        sb.append(" new int[")
        sb.append(Printer(size))
        sb.append("]")
      }
      case New(tpe: Identifier) => {
        sb.append(" new ")
        sb.append(Printer(tpe))
      }
      case Not(expr: ExprTree) => {
        sb.append("!")
        sb.append(Printer(expr))
      }
    }

    def expandList(list: List[Tree], depth: Int): String = {
      var sb = new StringBuilder
      
      list.foreach { l => sb.append(Printer(l, depth)) }
      
      sb.toString()
    }
    
    def indent(depth: Int): String = {
      var sb = new StringBuilder
      
      for(i <- 0 to depth) {
        sb.append("\t")
      }
      
      sb.toString
    }

    sb.toString
  }
}


/*
  case class Program(main: MainObject, classes: List[ClassDecl]) extends Tree
  case class MainObject(id: Identifier, stats: List[StatTree]) extends Tree
  case class ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl]) extends Tree
  case class VarDecl(tpe: TypeTree, id: Identifier) extends Tree
  case class MethodDecl(retType: TypeTree, id: Identifier, args: List[Formal], vars: List[VarDecl], stats: List[StatTree], retExpr: ExprTree) extends Tree
  sealed case class Formal(tpe: TypeTree, id: Identifier) extends Tree

  sealed trait TypeTree extends Tree
  case class IntArrayType() extends TypeTree
  case class IntType() extends TypeTree
  case class BooleanType() extends TypeTree
  case class StringType() extends TypeTree

  sealed trait StatTree extends Tree
  case class Block(stats: List[StatTree]) extends StatTree
  case class If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) extends StatTree
  case class While(expr: ExprTree, stat: StatTree) extends StatTree
  case class Println(expr: ExprTree) extends StatTree
  case class Assign(id: Identifier, expr: ExprTree) extends StatTree
  case class ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) extends StatTree

  sealed trait ExprTree extends Tree
  case class And(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Or(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Plus(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Minus(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Times(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Div(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class LessThan(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Equals(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class ArrayRead(arr: ExprTree, index: ExprTree) extends ExprTree
  case class ArrayLength(arr: ExprTree) extends ExprTree
  case class MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) extends ExprTree
  case class IntLit(value: Int) extends ExprTree
  case class StringLit(value: String) extends ExprTree

  case class True() extends ExprTree
  case class False() extends ExprTree
  case class Identifier(value: String) extends TypeTree with ExprTree

  case class This() extends ExprTree
  case class NewIntArray(size: ExprTree) extends ExprTree
  case class New(tpe: Identifier) extends ExprTree
  case class Not(expr: ExprTree) extends ExprTree
*/