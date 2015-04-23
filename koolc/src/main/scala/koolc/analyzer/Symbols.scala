package koolc
package analyzer

import utils._

object Symbols {
  trait Symbolic[S <: Symbol] {
    private var _sym: Option[S] = None

    def setSymbol(sym: S): this.type = {
      _sym = Some(sym)
      this
    }

    def getSymbol: S = _sym match {
      case Some(s) => s
      case None => sys.error("Accessing undefined symbol.")
    }
  }

  sealed abstract class Symbol extends Positioned {
    val id: Int = ID.next
    val name: String
  }

  private object ID {
    private var c: Int = 0

    def next: Int = {
      val ret = c
      c = c + 1
      ret
    }
  }

  class GlobalScope {
    var mainClass: ClassSymbol = _
    var classes = Map[String, ClassSymbol]()

    def lookupClass(n: String): Option[ClassSymbol] = classes.get(n)
  }

  class ClassSymbol(val name: String) extends Symbol {
    var parent: Option[ClassSymbol] = None
    var methods = Map[String, MethodSymbol]()
    var members = Map[String, VariableSymbol]()

    def lookupMethod(n: String, recursive: Boolean = true): Option[MethodSymbol] = {
      def parentChildCheck(currentClass: Option[ClassSymbol]): Option[MethodSymbol] = {
        currentClass match {
          case None => None
          case Some(res) => res.lookupMethod(n)
        }
      }

      methods.get(n) match {
        case None => {
          if (recursive) parentChildCheck(parent) else None
        }
        case Some(res) => methods.get(n)
      }
    }

    def lookupVar(n: String): Option[VariableSymbol] = {
      members.get(n) match {
        case None => {
          parent match {
            case None => None
            case Some(parent_ref) => parent_ref.lookupVar(n)
          }
        }
        case _ => members.get(n)
      }
    }

  }

  class MethodSymbol(val name: String, val classSymbol: ClassSymbol) extends Symbol {
    var params = Map[String, VariableSymbol]()
    var members = Map[String, VariableSymbol]()
    var argList: List[VariableSymbol] = Nil
    var overridden: Option[MethodSymbol] = None

    //value, may_shadow
    // 0 - Is ClassMembers
    // 1 - Is MethodMembers
    // 2 - Is MethodParam
    def lookupVar(n: String): (Option[VariableSymbol], Int) = {
      params.get(n) match {
        case None => {
          members.get(n) match {
            case None => (classSymbol.lookupVar(n), 0)
            case Some(res) => (members.get(n), 1) //if a member - may shadow params
          }
        }
        case Some(res) => (params.get(n), 2) // if param, may shadow other
      }
    }
  }

  class VariableSymbol(val name: String) extends Symbol
}
