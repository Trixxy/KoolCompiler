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

    //Explores every class and its class variables (one pass each)
    def explore_classes() {
      //MainObject(id: Identifier, stats: List[StatTree]) extends Tree with Symbolic[ClassSymbol]
      gs.mainClass = new ClassSymbol(prog.main.id.value).setPos(prog.main.id)
      prog.main.setSymbol(gs.mainClass)
      prog.main.id.setSymbol(gs.mainClass)

      for (c <- prog.classes) {
        val cls = _ClassDeclaration(c);

        gs.classes += c.id.value -> cls;

        c.setSymbol(cls)
        c.id.setSymbol(cls)
      }

      prog.classes.foreach(c => _ClassVariables(c))

      //ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl])
      def _ClassDeclaration(c: ClassDecl): ClassSymbol = {
        if (c.id.value == gs.mainClass.name) { //If class already exist (same name as main object)
          error("Class " + c.id.value + " has the same name as the main class.", c.id)
        }

        if (!gs.lookupClass(c.id.value).isEmpty) { //If class already exist (same name as another class)
          error("Class " + c.id.value + " is defined more than once. First definition here: " + gs.lookupClass(c.id.value).get.position, c.id)
        }

        val cls = new ClassSymbol(c.id.value).setPos(c)
        cls.setType(Types.TObject(cls))

        cls
      }

      def _ClassVariables(c: ClassDecl) = {
        val cls = c.getSymbol
        for (v <- c.vars) { //Go through all the class variables and...
          val vs = new VariableSymbol(v.id.value).setPos(v.id)

          if (!cls.lookupVar(v.id.value).isEmpty) { //Check whether the variable is declared twice within the current class
            error(cls.lookupVar(v.id.value).get.name + " is declared more than once. First definition here: " + cls.lookupVar(v.id.value).get.position, v.id)
          }
          //TODO: Should we add those despite the error?
          cls.members += v.id.value -> vs
          variable_syms += vs.id -> vs

          setSymbolType(v.tpe, vs)

          v.setSymbol(vs)
          v.id.setSymbol(vs)

        } //List[VarDecl]
      }
    }

    //Process
    def process_hierarchy() = {
      for (c <- prog.classes) {
        var currentCls: ClassSymbol = c.getSymbol

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
            sb.append(" <: ").append(current_parent.name)
            current_parent = current_parent.parent.getOrElse(null)
          } else {
            cyc_check_done = true
          }
        }

        if (cyclic) {
          sb.append(" <: ").append(currentCls.name)
          fatal("Cyclic inheritance graph: " + sb)
        }
      }
    }

    //Explores all methods in all classes, sets symbols and types for variables and arguments
    def explore_methods() = {
      for (c <- prog.classes) for (m <- c.methods) {
        c.getSymbol.lookupMethod(m.id.value, false) match {
          case None => {
            val ms = _MethodDeclaration(m, c.getSymbol)
            c.getSymbol.methods += m.id.value -> ms
            m.setSymbol(c.getSymbol.methods.get(m.id.value).get)
            m.id.setSymbol(c.getSymbol.methods.get(m.id.value).get)
          }
          case Some(method_ref) => error(m.id.value + " is defined twice. First definition here: " + method_ref.position, m)
        }
      } //List[MethodDecl])    	

      //MethodDecl(retType: TypeTree, id: Identifier, args: List[Formal], vars: List[VarDecl], stats: List[StatTree], retExpr: ExprTree)
      def _MethodDeclaration(m: MethodDecl, cls: ClassSymbol): MethodSymbol = {
        var ms = new MethodSymbol(m.id.value, cls).setPos(m)
        
        setSymbolType(m.retType, ms)

        for (a <- m.args) { //For each parameter, check the name
          ms.lookupVar(a.id.value) match {
            case (_, 2) => error("Parameter name " + a.id.value + " is used twice in " + m.id.value + ".", a)
            case (_, 0) => {
              val vs = new VariableSymbol(a.id.value).setPos(a.id)
              ms.params += a.id.value -> vs
              ms.argList = ms.argList :+ vs

              setSymbolType(a.tpe, vs)

              a.setSymbol(vs)
              a.id.setSymbol(vs)
            }
            case (_, _) => {}
          }
        } // Map[String,VariableSymbol]()

        for (v <- m.vars) {
          ms.lookupVar(v.id.value) match {
            case (_, 2) => error("Declaration of " + v.id.value + " as local shadows method parameter of the same name.", v.id)
            case (Some(firstDecl), 1) => error(v.id.value + " is declared more than once. First declaration here: " + firstDecl.position, v.id)
            case (_, 0) => {
              val vs = new VariableSymbol(v.id.value).setPos(v.id)
              ms.members += v.id.value -> vs
              variable_syms += vs.id -> vs

              setSymbolType(v.tpe, vs)

              v.setSymbol(vs)
              v.id.setSymbol(vs)
            }
            case (_, _) => {}
          }
        }

        ms
      }
    }

    def explore_mainObject() = {
      prog.main.stats.foreach(s => _Statement(s, new MethodSymbol(null, null)))
    }

    def explore_classHierarchy() = {
      prog.classes.foreach(c => _ClassDeclaration(c))

      //ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl])
      def _ClassDeclaration(c: ClassDecl) = {
        var currentCls: ClassSymbol = c.getSymbol

        //Check whether class variable overrides a previous one
        for ((_, v) <- currentCls.members) {
          if (!currentCls.parent.isEmpty) {
            val parent_ref = currentCls.parent.get
            if (!parent_ref.lookupVar(v.name).isEmpty) {
              val var_ref = parent_ref.lookupVar(v.name).get
              error(v.name + " member declaration overrides previous declaration at " + var_ref.position, v)
            }
          }
        }

        if (!currentCls.parent.isEmpty) {
          val parent_ref = currentCls.parent.get

          for ((_, m) <- currentCls.methods) {
            parent_ref.lookupMethod(m.name) match {
              case None => {}
              case Some(method_ref) => {
                m.overridden = parent_ref.lookupMethod(m.name)
                if (method_ref.argList.size != m.argList.size)
                  error(m.name + " overrides previous definition from " + method_ref.position + " with a different number of parameters.", m)
                else argTypesEqual(method_ref.argList, m.argList)
              }
            }
          }
          def argTypesEqual(l1: List[VariableSymbol], l2: List[VariableSymbol]): Unit = {
            if (l1.isEmpty) return
            else if (l1.head.getType == l2.head.getType) argTypesEqual(l1.tail, l2.tail)
            else error("Formal type in overriding method " + l2.head.name + " does not match type in overridden method.", l2.head)
          }
        }
      }
    }

    def explore_methodStatements() {
      prog.classes.foreach(c => c.methods.foreach(m => _MethodDeclaration(m))) //List[MethodDecl])
      prog.classes.foreach(c => c.methods.foreach(m => _MethodOverridden(m))) //List[MethodDecl])
      prog.classes.foreach(c => c.methods.foreach(m => m.stats.foreach(s => _Statement(s, m.getSymbol))))
      
      //MethodDecl(retType: TypeTree, id: Identifier, args: List[Formal], vars: List[VarDecl], stats: List[StatTree], retExpr: ExprTree)
      def _MethodDeclaration(m: MethodDecl) = {
        _Expression(m.retExpr, m.getSymbol)
        m.retType.setType(m.getSymbol.getType)
      }

      def _MethodOverridden(m: MethodDecl) = {
        val methOverridden = m.getSymbol.overridden.getOrElse(null)
        if (methOverridden != null && m.getSymbol.getType != methOverridden.getType) {
          error("Method " + methOverridden.name + " overrides parent method with a different return type (" +
            m.getSymbol.getType + " and " + methOverridden.getType + ")", m.retExpr)
        }
      }
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
            case None => {}
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

    def _Expression(expr: ExprTree, ms: MethodSymbol): Unit = {
      expr match {
        case And(lhs: ExprTree, rhs: ExprTree) => {
          _Expression(lhs, ms)
          _Expression(rhs, ms)

          if (lhs.getType == Types.TBoolean && rhs.getType == Types.TBoolean) expr.setType(Types.TBoolean)
          else expr.setType(Types.TError)
        }
        case Or(lhs: ExprTree, rhs: ExprTree) => {
          _Expression(lhs, ms)
          _Expression(rhs, ms)

          if (lhs.getType == Types.TBoolean && rhs.getType == Types.TBoolean) expr.setType(Types.TBoolean)
          else expr.setType(Types.TError)
        }
        case Plus(lhs: ExprTree, rhs: ExprTree) => {
          _Expression(lhs, ms)
          _Expression(rhs, ms)

          //    T1 = TInt and T2 = TInt implies Ts = TInt
          //    T1 = TString and T2 = TInt implies Ts = TString
          //    T1 = TInt and T2 = TString implies Ts = TString
          //    T1 = TString and T2 = TString implies Ts = TString
          
          if (lhs.getType == Types.TInt && rhs.getType == Types.TInt) expr.setType(Types.TInt)
          else if ((lhs.getType == Types.TInt && rhs.getType == Types.TString)
            || (lhs.getType == Types.TString && rhs.getType == Types.TInt)
            || (lhs.getType == Types.TString && rhs.getType == Types.TString)) expr.setType(Types.TString)
          else expr.setType(Types.TError)
        }
        case Minus(lhs: ExprTree, rhs: ExprTree) => {
          _Expression(lhs, ms)
          _Expression(rhs, ms)

          if (lhs.getType == Types.TInt && rhs.getType == Types.TInt) expr.setType(Types.TInt)
          else expr.setType(Types.TError)
        }
        case Times(lhs: ExprTree, rhs: ExprTree) => {
          _Expression(lhs, ms)
          _Expression(rhs, ms)

          if (lhs.getType == Types.TInt && rhs.getType == Types.TInt) expr.setType(Types.TInt)
          else expr.setType(Types.TError)
        }
        case Div(lhs: ExprTree, rhs: ExprTree) => {
          _Expression(lhs, ms)
          _Expression(rhs, ms)

          if (lhs.getType == Types.TInt && rhs.getType == Types.TInt) expr.setType(Types.TInt)
          else expr.setType(Types.TError)
        }
        case LessThan(lhs: ExprTree, rhs: ExprTree) => {
          _Expression(lhs, ms)
          _Expression(rhs, ms)

          if (lhs.getType == Types.TInt && rhs.getType == Types.TInt) expr.setType(Types.TBoolean)
          else expr.setType(Types.TError)
        }
        case Equals(lhs: ExprTree, rhs: ExprTree) => {
          _Expression(lhs, ms)
          _Expression(rhs, ms)

          //          	T1 		T2 		type checks?
          //			int 	int 	yes
          //			string 	string 	yes
          //			string 	A 		no
          //			A 		int 	no
          //			A 		B 		yes
          //			A 		int[] 	no
          //			int 	int[] 	no

          if (lhs.getType == rhs.getType && (lhs.getType == Types.TInt || lhs.getType == Types.TString || lhs.getType == Types.TObject)) expr.setType(Types.TBoolean)
          else expr.setType(Types.TError)
        }
        case ArrayRead(arr: ExprTree, index: ExprTree) => {
          _Expression(arr, ms)
          _Expression(index, ms)

          if(arr.getType == Types.TIntArray && index.getType == Types.TInt) expr.setType(Types.TInt)
          else expr.setType(Types.TError)
        }
        case ArrayLength(arr: ExprTree) => {
          _Expression(arr, ms)

          if(arr.getType == Types.TIntArray) expr.setType(Types.TInt)
          else expr.setType(Types.TError)
        }
        case MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) => {
          _Expression(obj, ms)
          val tmp = gs.lookupClass(obj.getType.toString()).getOrElse(null)
          if (tmp != null) {
            tmp.lookupMethod(meth.value) match {
              case None => error("'" + meth.value + "' was not declared in this scope at " + meth.position)
              case Some(method_ref) => {
                args.foreach(a => _Expression(a, ms)) //TODO

                meth.setSymbol(method_ref).setType(method_ref.getType)
                expr.setType(method_ref.getType)
                if(args.size == method_ref.argList.size) argTypesEqual(args, method_ref.argList)
                else error("Call to method " + method_ref.name + " with a different number of parameters.", ms)
                def argTypesEqual(l1: List[ExprTree], l2: List[VariableSymbol]): Unit = {
                  if (l1.isEmpty) return                  
                  else if (l1.head.getType.isSubTypeOf(l2.head.getType)) argTypesEqual(l1.tail, l2.tail)
                  else error("Formal type in method call " + l2.head.name + " does not match type in refered method.", l1.head)
                }
              }
            }
          }
        }
        case IntLit(value: Int) => {
          expr.setType(Types.TInt)
        }
        case StringLit(value: String) => {
          expr.setType(Types.TString)
        }

        case True() => {
          expr.setType(Types.TBoolean)
        }
        case False() => {
          expr.setType(Types.TBoolean)
        }
        case Identifier(value: String) => {
          ms.lookupVar(value) match {
            case (None, _) => error("Undeclared identifier: " + value, expr)
            case (Some(var_ref), _) => {
              expr.asInstanceOf[Identifier].setSymbol(var_ref).setType(var_ref.getType)
              expr.setType(var_ref.getType)
              variable_syms -= expr.asInstanceOf[Identifier].getSymbol.id
            }
          }
        }

        case This() => {
          expr.setType(ms.classSymbol.getType)
        }
        case NewIntArray(size: ExprTree) => {
          _Expression(size, ms)
          
          if(size.getType == Types.TInt) expr.setType(Types.TIntArray)
          else expr.setType(Types.TError)
        }
        case New(tpe: Identifier) => {
          gs.lookupClass(tpe.value) match {
            case None => error("Undeclared type: " + tpe.value + ".", tpe)
            case Some(class_ref) => {
              tpe.setSymbol(class_ref).setType(class_ref.getType);
              expr.setType(class_ref.getType)
            }
          }
        }
        case Not(nexpr: ExprTree) => {
          _Expression(nexpr, ms)
          
          if(nexpr.getType == Types.TBoolean) expr.setType(Types.TBoolean)
          else expr.setType(Types.TError)
        }
      }
    }

    //I have verified this, and integrated the setSymbol for this case; hence it's bullet proof
    def setSymbolType(tpe: TypeTree, sym: Symbol): Unit = {
      tpe match {
        case BooleanType() => sym.setType(Types.TBoolean)
        case IntType() => sym.setType(Types.TInt)
        case IntArrayType() => sym.setType(Types.TIntArray)
        case StringType() => sym.setType(Types.TString)
        case Identifier(value: String) => {
          setObjectTypeSymbol(tpe)
          sym.setType(Types.TObject(gs.lookupClass(value).getOrElse(null)))
        }
      }
    }

    def setObjectTypeSymbol(tpe: TypeTree) = {
      if (tpe.isInstanceOf[Identifier]) gs.lookupClass(tpe.asInstanceOf[Identifier].value) match {
        case None => fatal("Undeclared type: " + tpe.asInstanceOf[Identifier].value + ".", tpe)
        case Some(class_ref) => {
          tpe.asInstanceOf[Identifier].setSymbol(class_ref).setType(class_ref.getType) //.setType(class_ref.getType)
        }
      }
    }

    //check if variables is unused and warn about it here
    def warningRound() = variable_syms.foreach(f => warning("Variable " + f._2.name + " is declared but never used.", f._2))

    //The rounds we run:
    explore_classes() //Explores every class, its class variables, methods, methods parameters and variables
    explore_methods()
    terminateIfErrors
    process_hierarchy() //Explore hierarchy settings parents and detect cycles
    terminateIfErrors
    explore_mainObject()
    explore_classHierarchy()
    explore_methodStatements()
    terminateIfErrors
    warningRound() //Done, check if variables is unused and warn about it here
    terminateIfErrors
    //    println(prog)

    // Step 1: Collect symbols in declarations
    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check for all constraints
    prog
  }
}
