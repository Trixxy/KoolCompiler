package koolc
package analyzer

import utils._
import ast.Trees._
import Symbols._

object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    var gs = new GlobalScope
    var variable_syms = Map[Int, VariableSymbol]()

    def firstRound() {
      _MainObject(prog.main)
      prog.classes.foreach(c => gs.classes += c.id.value -> _ClassDeclaration(c))

      //MainObject(id: Identifier, stats: List[StatTree]) extends Tree with Symbolic[ClassSymbol]
      def _MainObject(main: MainObject) = {
        gs.mainClass = new ClassSymbol(main.id.value).setPos(main.id)
      }

      //ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl])
      def _ClassDeclaration(c: ClassDecl): ClassSymbol = {
        if (c.id.value == gs.mainClass.name) {
          error("Class " + c.id.value + " has the same name as the main class.", c.id)
        }

        gs.lookupClass(c.id.value) match {
          case None      => {}
          case Some(res) => error("Class " + c.id.value + " is defined more than once. First definition here: " + res.position, c.id)
        }

        var cls = new ClassSymbol(c.id.value).setPos(c)
        cls.setType(new Types.TObject(cls))

        c.id.setSymbol(cls).setType(cls.getType)

        //        var cls: ClassSymbol = null
        //        gs.lookupClass(c.id.value) match {
        //          case None => sys.error("Internal error, please report with error code 1.")
        //          case Some(res) => {
        //            cls = res
        //          }
        //        }

        for (v <- c.vars) {
          val tmp = _VarDeclaration(v)
            /*new VariableSymbol(v.id.value).setPos(v.id)

          v.tpe match {
            case BooleanType()             => tmp.setType(Types.TBoolean)
            case IntType()                 => tmp.setType(Types.TInt)
            case IntArrayType()            => tmp.setType(Types.TIntArray)
            case StringType()              => tmp.setType(Types.TString)
            case Identifier(value: String) => tmp.setType(Types.TObject(gs.lookupClass(value).getOrElse(null)))
            //TODO if null pointer exception - this is a potential
          }*/

          cls.lookupVar(v.id.value) match {
            case None          => {}
            case Some(var_ref) => error(var_ref.name + " is declared more than once. First definition here: " + var_ref.position, v.id)
          }
          cls.members += v.id.value -> tmp
          variable_syms += tmp.id -> tmp
        } //List[VarDecl]
        for (m <- c.methods) {
          cls.lookupMethod(m.id.value, false) match {
            case None             => cls.methods += m.id.value -> _MethodDeclaration(m, cls)
            case Some(method_ref) => error("method '" + m.id.value + "' is defined twice. First definition here: " + method_ref.position, m)
          }
        } //List[MethodDecl])

        cls
      }

      //MethodDecl(retType: TypeTree, id: Identifier, args: List[Formal], vars: List[VarDecl], stats: List[StatTree], retExpr: ExprTree)
      def _MethodDeclaration(m: MethodDecl, cls: ClassSymbol): MethodSymbol = {
        var ms = new MethodSymbol(m.id.value, cls).setPos(m)

        matchingType(m.retType, ms)
        /*m.retType match {
          case BooleanType()             => ms.setType(Types.TBoolean)
          case IntType()                 => ms.setType(Types.TInt)
          case IntArrayType()            => ms.setType(Types.TIntArray)
          case StringType()              => ms.setType(Types.TString)
          case Identifier(value: String) => ms.setType(Types.TObject(gs.lookupClass(value).getOrElse(null)))
          //TODO if null pointer exception - this is a potential
        }*/

        for (a <- m.args) {
          ms.lookupVar(a.id.value) match {
            case (_, 2) => error("Parameter name " + a.id.value + " is used twice in " + m.id.value + ".", a)
            case (_, 0) => {
              val tmp = _FormalDeclaration(a)
              ms.params += a.id.value -> tmp //new VariableSymbol(a.id.value).setPos(a.id)
              ms.argList = ms.argList :+ tmp//new VariableSymbol(a.id.value).setPos(a.id)
            }
            case (_, _) => {}
          }
        } // Map[String,VariableSymbol]()

        for (v <- m.vars) {
          ms.lookupVar(v.id.value) match {
            case (_, 2)               => error("Declaration of " + v.id.value + " as local shadows method parameter of the same name.", v.id)
            case (Some(firstDecl), 1) => error(v.id.value + " is declared more than once. First declaration here: " + firstDecl.position, v.id)
            case (_, 0) => {
              val tmp = _VarDeclaration(v)
              /*new VariableSymbol(v.id.value).setPos(v.id)
              v.tpe match {
                case BooleanType()             => tmp.setType(Types.TBoolean)
                case IntType()                 => tmp.setType(Types.TInt)
                case IntArrayType()            => tmp.setType(Types.TIntArray)
                case StringType()              => tmp.setType(Types.TString)
                case Identifier(value: String) => tmp.setType(Types.TObject(gs.lookupClass(value).getOrElse(null)))
                //TODO if null pointer exception - this is a potential
              }*/
              ms.members += v.id.value -> tmp
              variable_syms += tmp.id -> tmp
            }
            case (_, _) => ???
          }
        }

        ms
      }

      def _VarDeclaration(v: VarDecl): VariableSymbol = {
        val tmp = new VariableSymbol(v.id.value).setPos(v.id)
        matchingType(v.tpe, tmp)
        /*v.tpe match {
          case BooleanType()             => tmp.setType(Types.TBoolean)
          case IntType()                 => tmp.setType(Types.TInt)
          case IntArrayType()            => tmp.setType(Types.TIntArray)
          case StringType()              => tmp.setType(Types.TString)
          case Identifier(value: String) => tmp.setType(Types.TObject(gs.lookupClass(value).getOrElse(null)))
          //TODO if null pointer exception - this is a potential
        }*/
        tmp
      }
      def _FormalDeclaration(v: Formal): VariableSymbol = {
        val tmp = new VariableSymbol(v.id.value).setPos(v.id)
        matchingType(v.tpe, tmp)
        /*v.tpe match {
          case BooleanType()             => tmp.setType(Types.TBoolean)
          case IntType()                 => tmp.setType(Types.TInt)
          case IntArrayType()            => tmp.setType(Types.TIntArray)
          case StringType()              => tmp.setType(Types.TString)
          case Identifier(value: String) => tmp.setType(Types.TObject(gs.lookupClass(value).getOrElse(null)))
          //TODO if null pointer exception - this is a potential
        }*/
        tmp
      }
      
      def matchingType(tpe: TypeTree, sym: Symbol): Unit = {
        tpe match {
          case BooleanType()             => sym.setType(Types.TBoolean)
          case IntType()                 => sym.setType(Types.TInt)
          case IntArrayType()            => sym.setType(Types.TIntArray)
          case StringType()              => sym.setType(Types.TString)
          case Identifier(value: String) => sym.setType(Types.TObject(gs.lookupClass(value).getOrElse(null)))
          //TODO if null pointer exception - this is a potential
        }
      }
    }

    //Process
    def process_hierarchy() = {
      for (c <- prog.classes) {
        var currentCls: ClassSymbol = null
        gs.lookupClass(c.id.value) match {
          case None => sys.error("Internal error, please report with error code 1.")
          case Some(res) => {
            currentCls = res
          }
        }
        c.parent match {
          case None => {}
          case Some(parent_ref) => {
            if (parent_ref.value == currentCls.name) {
              error("Class " + c.id.value + " cannot extend itself.", c)
            } else {
              gs.lookupClass(parent_ref.value) match {
                case None => error("Class " + c.id.value + " extends class " + parent_ref.value + " which is not defined.", parent_ref)
                case Some(class_ref) => {
                  parent_ref.setSymbol(class_ref).setType(class_ref.getType)
                  currentCls.parent = gs.lookupClass(parent_ref.value)
                }
              }
            }
          }
        }

        var cyclic, cyc_check_done = false
        var current_parent = currentCls.parent.getOrElse(null)
        var sb = new StringBuilder
        //        sb.append(current_parent.name)
        //        sb.append(" <: ")
        sb.append(currentCls.name)
        while (!cyc_check_done) {
          if (current_parent != null && current_parent == currentCls) {
            cyclic = true
            cyc_check_done = true
          } else if (current_parent != null) {
            sb.append(" <: ")
            sb.append(current_parent.name)
            current_parent = current_parent.parent.getOrElse(null)
          } else {
            cyc_check_done = true
          }
        }

        if (cyclic) {
          sb.append(" <: ")
          sb.append(currentCls.name)
          fatal("Cyclic inheritance graph: " + sb)
        }
      }
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////////////////////////////////////
    def secondRound() {

      _MainObject(prog.main)
      prog.classes.foreach(c => _ClassDeclaration(c))

      //MainObject(id: Identifier, stats: List[StatTree]) extends Tree with Symbolic[ClassSymbol]
      def _MainObject(main: MainObject) = {
        main.id.setSymbol(gs.mainClass).setType(gs.mainClass.getType)
        main.stats.foreach(s => _Statement(s, new MethodSymbol(null, null)))
      }

      //ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl])
      def _ClassDeclaration(c: ClassDecl) = {
        //    	gs.classes = new Map[String,ClassSymbol]

        var currentCls: ClassSymbol = null
        gs.lookupClass(c.id.value) match {
          case None => sys.error("Internal error, please report with error code 1.")
          case Some(res) => {
            currentCls = res
          }
        }

        for (v <- c.vars) {
          currentCls.lookupVar(v.id.value) match {
            case None => sys.error("Internal error, please report with error code 2.")
            case Some(var_ref) => {
              v.id.setSymbol(var_ref).setType(var_ref.getType)
            }
          }
        }

        for (v <- c.vars) {
          if (v.tpe.isInstanceOf[Identifier]) {
            gs.lookupClass(v.tpe.asInstanceOf[Identifier].value) match {
              case None            => fatal("Undeclared type: " + v.tpe.asInstanceOf[Identifier].value + ".", v.tpe)
              case Some(class_ref) => v.tpe.asInstanceOf[Identifier].setSymbol(class_ref).setType(class_ref.getType)
            }
          }
        }

        for ((_, v) <- currentCls.members) {
          currentCls.parent match {
            case None => {}
            case Some(parent_ref) => {
              parent_ref.lookupVar(v.name) match {
                case None          => {}
                case Some(var_ref) => { error(v.name + " member declaration overrides previous declaration at " + var_ref.position, v) }
              }
            }
          }
        }

        //TODO just double check overridden in type checking
        currentCls.parent match {
          case None => {}
          case Some(parent_ref) => {
            for ((_, m) <- currentCls.methods)
              parent_ref.lookupMethod(m.name) match {
                case None => {}
                case Some(method_ref) => {
                  m.overridden = parent_ref.lookupMethod(m.name)
                  if (method_ref.argList.size != m.argList.size)
                    error(m.name + " overrides previous definition from " + method_ref.position + " with a different number of parameters.", m)
                  else {
                    sys.error("Need to compare the types of both lists!")
                    method_ref.argList.diff(m.argList); //Diffar men kollar ej typen, måste göras på annat sätt
                  }
                }
              }
          }
        }

        c.methods.foreach(m => _MethodDeclaration(m, currentCls)) //List[MethodDecl])
      }

      //MethodDecl(retType: TypeTree, id: Identifier, args: List[Formal], vars: List[VarDecl], stats: List[StatTree], retExpr: ExprTree)
      def _MethodDeclaration(m: MethodDecl, cls: ClassSymbol) = {
        var currentMs: MethodSymbol = null

        cls.lookupMethod(m.id.value, false) match {
          case None => sys.error("Internal error, please report with error code 3.")
          case Some(method_ref) => {
            m.id.setSymbol(method_ref).setType(method_ref.getType)
            currentMs = method_ref
          }
        }

        for (a <- m.args) {
          currentMs.lookupVar(a.id.value) match {
            case (None, _)          => sys.error("Internal error, please report with error code 4.")
            case (Some(var_ref), _) => a.id.setSymbol(var_ref).setType(var_ref.getType)
          }
        }
        for (v <- m.vars) {
          currentMs.lookupVar(v.id.value) match {
            case (None, _)          => sys.error("Internal error, please report with error code 5.")
            case (Some(var_ref), _) => v.id.setSymbol(var_ref).setType(var_ref.getType)
          }
        }

        for (v <- m.vars) {
          if (v.tpe.isInstanceOf[Identifier]) gs.lookupClass(v.tpe.asInstanceOf[Identifier].value) match {
            case None            => fatal("Undeclared type: " + v.tpe.asInstanceOf[Identifier].value + ".", v.tpe)
            case Some(class_ref) => v.tpe.asInstanceOf[Identifier].setSymbol(class_ref).setType(class_ref.getType)
          }
        }

        m.stats.foreach(s => _Statement(s, currentMs))

        _Expression(m.retExpr, currentMs)
      }

      def _Statement(s: StatTree, ms: MethodSymbol): Unit = {
        s match {
          case Block(stats: List[StatTree]) => {
            stats.foreach(stat => _Statement(stat, ms))
          }
          case If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) => {
            _Expression(expr, ms)
            _Statement(thn, ms)
            els match {
              case None      => {}
              case Some(res) => _Statement(res, ms)
            }
          }
          case While(expr: ExprTree, stat: StatTree) => {
            _Expression(expr, ms)
            _Statement(stat, ms)
          }
          case Println(expr: ExprTree) => {
            _Expression(expr, ms)
          }
          case Assign(id: Identifier, expr: ExprTree) => {
            if (ms.name == null) {
              error("Undeclared identifier: " + id.value)
            }
            ms.lookupVar(id.value) match {
              case (None, _) => error("Undeclared identifier: " + id.value + ".", id)
              case (Some(var_ref), _) => {
                _Expression(id, ms)
                _Expression(expr, ms)
              }
            }
          }
          case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) => {
            if (ms.name == null) {
              error("Undeclared identifier: " + id.value)
            }
            ms.lookupVar(id.value) match {
              case (None, _) => error("'" + id.value + "' was not declared in this scope at " + id.position)
              case (Some(var_ref), _) => {
                _Expression(id, ms)
                _Expression(index, ms)
                _Expression(expr, ms)
              }
            }
          }
        }
      }

      def _Expression(expr: ExprTree, ms: MethodSymbol): ClassSymbol = {
        var newClass: ClassSymbol = null
        expr match {
          case And(lhs: ExprTree, rhs: ExprTree) => {
            _Expression(lhs, ms)
            _Expression(rhs, ms)
          }
          case Or(lhs: ExprTree, rhs: ExprTree) => {
            _Expression(lhs, ms)
            _Expression(rhs, ms)
          }
          case Plus(lhs: ExprTree, rhs: ExprTree) => {
            _Expression(lhs, ms)
            _Expression(rhs, ms)
          }
          case Minus(lhs: ExprTree, rhs: ExprTree) => {
            _Expression(lhs, ms)
            _Expression(rhs, ms)
          }
          case Times(lhs: ExprTree, rhs: ExprTree) => {
            _Expression(lhs, ms)
            _Expression(rhs, ms)
          }
          case Div(lhs: ExprTree, rhs: ExprTree) => {
            _Expression(lhs, ms)
            _Expression(rhs, ms)
          }
          case LessThan(lhs: ExprTree, rhs: ExprTree) => {
            _Expression(lhs, ms)
            _Expression(rhs, ms)
          }
          case Equals(lhs: ExprTree, rhs: ExprTree) => {
            _Expression(lhs, ms)
            _Expression(rhs, ms)
          }
          case ArrayRead(arr: ExprTree, index: ExprTree) => {
            _Expression(arr, ms)
            _Expression(index, ms)
          }
          case ArrayLength(arr: ExprTree) => {
            _Expression(arr, ms)
          }
          case MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) => {
            val tmp = _Expression(obj, ms)
            if (tmp != null)
              tmp.lookupMethod(meth.value) match {
                case None => error("'" + meth.value + "' was not declared in this scope at " + meth.position)
                case Some(method_ref) => {
                  meth.setSymbol(method_ref).setType(method_ref.getType)
                  args.foreach(a => _Expression(a, method_ref))
                }
              }

          }
          case IntLit(value: Int) => {

          }
          case StringLit(value: String) => {

          }

          case True() => {

          }
          case False() => {

          }
          case Identifier(value: String) => {
            ms.lookupVar(value) match {
              case (None, _) => error("Undeclared identifier: " + value, expr)
              case (Some(class_ref), _) => {
                expr.asInstanceOf[Identifier].setSymbol(class_ref).setType(class_ref.getType)

                variable_syms -= expr.asInstanceOf[Identifier].getSymbol.id

                newClass = ms.classSymbol
              }
            }
          }

          case This() => {

          }
          case NewIntArray(size: ExprTree) => {
            _Expression(size, ms)
          }
          case New(tpe: Identifier) => {
            gs.lookupClass(tpe.value) match {
              case None => error("Undeclared type: " + tpe.value + ".", tpe)
              case Some(class_ref) => {
                tpe.setSymbol(class_ref).setType(class_ref.getType);
                //		        _Expression(tpe, ms)
                newClass = class_ref
              }
            }
          }
          case Not(expr: ExprTree) => {
            _Expression(expr, ms)
          }
        }

        if (newClass != null) newClass else ms.classSymbol
      }
    }

    def warningRound() = {
      variable_syms.foreach(f => warning("Variable " + f._2.name + " is declared but never used.", f._2))
    }

    firstRound()
    terminateIfErrors
    process_hierarchy()
    terminateIfErrors
    secondRound()
    terminateIfErrors
    warningRound()

    terminateIfErrors
    //    println(prog)

    // Step 1: Collect symbols in declarations
    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check for all constraints
    prog
  }
}
