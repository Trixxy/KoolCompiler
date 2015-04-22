package koolc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._

/* The Parser is a code representation of the language KOOLs grammar.
 * If a program is written according to the grammar, it will return
 * a Tree of the program
 * If a program is NOT written according to the grammar, it will output
 * an error message of what the problem is.
 */

object Parser extends Pipeline[Iterator[Token], Program] {
  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._

    /** Store the current token, as read from the lexer. */
    var currentToken: Token = new Token(BAD)
    var nextToken: Token = new Token(BAD)
    var peekaboo = false

    //Initialize the nextToken
    if (tokens.hasNext) {
      nextToken = tokens.next
      // skips bad tokens
      while (nextToken.kind == BAD && tokens.hasNext) {
        nextToken = tokens.next
      }
    }

    //Reads next Token
    def readToken: Unit = {
      if (tokens.hasNext) {
        // uses nextToken from the Lexer trait

        currentToken = nextToken
        nextToken = tokens.next

        // skips bad tokens
        while (nextToken.kind == BAD && tokens.hasNext) {
          nextToken = tokens.next
        }
        //If no more tokens can be read to nextToken
        //This occurs only once
      } else if (!peekaboo) {
        currentToken = nextToken
        nextToken = new Token(BAD)
        peekaboo = true
      }
    }

    /** ''Eats'' the expected token, or terminates with an error. */
    def eat(kind: TokenKind): Unit = {
      if (currentToken.kind == kind) {
        readToken
      } else {
        expected(kind)
      }
    }

    /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenKind */
    def expected(kind: TokenKind, more: TokenKind*): Nothing = {
      fatal("expected: " + (kind :: more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
    }

    /**
     * Goal ::= MainObject ( ClassDeclaration )* <EOF>
     * }
     */
    def parseGoal: Program = {
      val pos = currentToken
      val main = _MainObject()
      val classes = _ClassDeclaration()
      eat(EOF)
      new Program(main, classes).setPos(pos)
    }

    /**
     * MainObject ::= object Identifier { def main ( ) : Unit = { ( Statement )* } }
     */
    def _MainObject(): MainObject = {
      val pos = currentToken
      eat(OBJECT)
      val id = _Identifier()
      eat(LBRACE)
      eat(DEF)
      eat(MAIN)
      eat(LPAREN)
      eat(RPAREN)
      eat(COLON)
      eat(UNIT)
      eat(EQSIGN)
      eat(LBRACE)
      val stats = _Statement()
      eat(RBRACE)
      eat(RBRACE)
      new MainObject(id, stats).setPos(pos)
    }

    /**
     * ClassDeclaration ::= class Identifier ( extends Identifier )? { ( VarDeclaration )* ( MethodDeclaration )* }
     */
    def _ClassDeclaration(): List[ClassDecl] = {
      //OPTIONAL

      var classes: List[ClassDecl] = Nil

      //Do for every CLASS declaration
      while (currentToken.kind == CLASS) {
        val pos = currentToken
        eat(CLASS)
        val id = _Identifier()

        var parent: Option[Identifier] = None
        if (currentToken.kind == EXTENDS) {
          eat(EXTENDS)
          parent = Option(_Identifier())
        }

        eat(LBRACE)
        val vars = _VarDeclaration()
        val methods = _MethodDeclaration()
        eat(RBRACE)

        classes = classes :+ new ClassDecl(id, parent, vars, methods).setPos(pos)
      }

      classes
    }

    /**
     * VarDeclaration ::= var Identifier : Type ;
     */
    def _VarDeclaration(): List[VarDecl] = {
      //OPTIONAL
      var vars: List[VarDecl] = Nil

      //Do for every VAR declaration 
      while (currentToken.kind == VAR) {
        val pos = currentToken
        eat(VAR)
        val id = _Identifier()
        eat(COLON)
        val tpe = _Type()
        eat(SEMICOLON)

        vars = vars :+ new VarDecl(tpe, id).setPos(pos)
      }

      vars
    }

    /**
     * MethodDeclaration ::=
     * def Identifier ( ( Identifier : Type ( , Identifier : Type )* )? ) : Type = {
     * ( VarDeclaration )*
     * ( Statement )*
     * return Expression ;
     * }
     */
    def _MethodDeclaration(): List[MethodDecl] = {
      //OPTIONAL
      var methods: List[MethodDecl] = Nil

      //Do for every METHOD declaration
      while (currentToken.kind == DEF) {
        val pos = currentToken
        eat(DEF)
        val id = _Identifier()
        eat(LPAREN)

        var args: List[Formal] = Nil

        if (currentToken.kind != RPAREN) {
          val pos = currentToken
          val arg_id = _Identifier()
          eat(COLON)
          val arg_type = _Type()

          args = args :+ new Formal(arg_type, arg_id).setPos(pos)

          while (currentToken.kind == COMMA) {
            eat(COMMA)
            val pos = currentToken
            val arg_id = _Identifier()
            eat(COLON)
            val arg_type = _Type()
            args = args :+ new Formal(arg_type, arg_id).setPos(pos)
          }
        }

        eat(RPAREN)
        eat(COLON)
        val retType = _Type()
        eat(EQSIGN)
        eat(LBRACE)
        val vars = _VarDeclaration()
        val stats = _Statement()
        eat(RETURN)
        val retExpr = _Expression()
        eat(SEMICOLON)
        eat(RBRACE)

        methods = methods :+ new MethodDecl(retType, id, args, vars, stats, retExpr).setPos(pos)
      }

      methods
    }

    /**
     * Type  ::= Int [ ]
     * | Bool
     * | Int
     * | String
     * | Identifier
     */
    def _Type() = {
      val pos = currentToken
      currentToken.kind match {
        case INT => {
          eat(INT)
          if (currentToken.kind == LBRACKET) {
            eat(LBRACKET)
            eat(RBRACKET)
            new IntArrayType().setPos(pos)
          } else {
            new IntType().setPos(pos)
          }
        }
        case BOOLEAN => {
          eat(BOOLEAN)
          new BooleanType().setPos(pos)
        }
        case STRING => {
          eat(STRING)
          new StringType().setPos(pos)
        }
        case IDKIND => {
          _Identifier()
        }
        case _ => expected(INT, BOOLEAN, STRING, IDKIND)
      }
    }

    /*
    Statement ::= { ( Statement )* }
                | if ( Expression ) Statement ( else Statement )?
                | while ( Expression ) Statement
                | println ( Expression ) ;
                | Identifier = Expression ;
                | Identifier [ Expression ] = Expression ;
    */
    def _Statement(): List[StatTree] = {
      //OPTIONAL

      var stats: List[StatTree] = Nil

      //Do for every STATEMENT
      while (currentToken.kind == LBRACE || currentToken.kind == IF || currentToken.kind == WHILE || currentToken.kind == PRINTLN ||
        (currentToken.kind == IDKIND && (nextToken.kind == EQSIGN || nextToken.kind == LBRACKET))) {
        stats = stats :+ one_Statement()
      } //While

      //Representation of ONE STATEMENT
      def one_Statement(): StatTree = {
        val pos = currentToken
        currentToken.kind match {
          case LBRACE => {
            eat(LBRACE)
            val stats = _Statement()
            eat(RBRACE)

            new Block(stats).setPos(pos)
          }
          case IF => {
            eat(IF)
            eat(LPAREN)
            val expr = _Expression()
            eat(RPAREN)
            val thn = one_Statement()

            var els: Option[StatTree] = None
            if (currentToken.kind == ELSE) {
              eat(ELSE)
              els = Option(one_Statement())
            }

            new If(expr, thn, els).setPos(pos)
          }
          case WHILE => {
            eat(WHILE)
            eat(LPAREN)
            val expr = _Expression()
            eat(RPAREN)
            val stat = one_Statement()

            new While(expr, stat).setPos(pos)
          }
          case PRINTLN => {
            eat(PRINTLN)
            eat(LPAREN)
            val expr = _Expression()
            eat(RPAREN)
            eat(SEMICOLON)

            new Println(expr).setPos(pos)
          }
          case IDKIND => {
            val id = _Identifier()
            val pos = currentToken
            currentToken.kind match {
              case EQSIGN => {
                eat(EQSIGN)
                val expr = _Expression()
                eat(SEMICOLON)

                new Assign(id, expr).setPos(pos)
              }
              case LBRACKET => {
                eat(LBRACKET)
                val index = _Expression()
                eat(RBRACKET)
                eat(EQSIGN)
                val expr = _Expression()
                eat(SEMICOLON)

                new ArrayAssign(id, index, expr).setPos(pos)
              }
              case _ => expected(EQSIGN, LBRACKET)

            } //IDKIND Match

          } //IDKIND case
          case _ => expected(LBRACE, IF, WHILE, PRINTLN, IDKIND)

        } //Match

      } //innerFunc

      stats

    } //Func

    /**
     * Expression  ::= Expression ( && | || | == | < | + | - | * | / ) Expression
     * | Expression [ Expression ]
     * | Expression . length
     * | Expression . Identifier ( ( Expression ( , Expression )* )? )
     * | <INTEGER_LITERAL>
     * | " <STRING_LITERAL> "
     * | true
     * | false
     * | Identifier
     * | this
     * | new Int [ Expression ]
     * | new Identifier ( )
     * | ! Expression
     * | ( Expression )
     */
    def _Expression(): ExprTree = {

      //    --> Expr || AndTerm
      //      | AndTerm 
      def _Expr(): ExprTree = {
        var expr: ExprTree = _AndTerm()

        //      --> || AndTerm Expr_prime
        //        | ɛ
        while (currentToken.kind == OR) {
          val pos = currentToken
          eat(OR)
          expr = new Or(expr, _AndTerm()).setPos(pos)
        }

        expr
      }

      //    --> AndTerm && RelExpr
      //      | RelExpr
      def _AndTerm(): ExprTree = {
        var expr: ExprTree = _RelExpr()

        //      --> && RelTerm AndTerm_prime
        //        | ɛ
        while (currentToken.kind == AND) {
          val pos = currentToken
          eat(AND)
          expr = new And(expr, _RelExpr()).setPos(pos)
        }

        expr
      }

      //    --> RelExpr < NumExpr
      //      | RelExpr = NumExpr
      //      | NumExpr
      def _RelExpr(): ExprTree = {
        var expr: ExprTree = _NumExpr()

        //      --> = NumExpr RelExpr_prime
        //        | < NumExpr RelExpr_prime
        //        | ɛ
        while (currentToken.kind == EQUALS || currentToken.kind == LESSTHAN) {
          val pos = currentToken
          currentToken.kind match {
            case EQUALS => {
              eat(EQUALS)
              expr = new Equals(expr, _NumExpr()).setPos(pos)
            }
            case LESSTHAN => {
              eat(LESSTHAN)
              expr = new LessThan(expr, _NumExpr()).setPos(pos)
            }
            case _ => {}
          }
        }

        expr
      }

      //    --> NumExpr + Term 
      //      | NumExpr - Term
      //      | Term
      def _NumExpr(): ExprTree = {
        var expr: ExprTree = _Term()

        //      --> + Term NumExpr_prime
        //        | - Term NumExpr_prime
        //        | ɛ
        while (currentToken.kind == PLUS || currentToken.kind == MINUS) {
          val pos = currentToken
          currentToken.kind match {
            case PLUS => {
              eat(PLUS)
              expr = new Plus(expr, _Term()).setPos(pos)
            }
            case MINUS => {
              eat(MINUS)
              expr = new Minus(expr, _Term()).setPos(pos)
            }
            case _ => {}
          }
        }

        expr
      }

      //    --> Term * Value 
      //      | Term / Value
      //      | Value
      def _Term(): ExprTree = {
        var expr: ExprTree = _Value()

        //      --> * Term NumExpr_prime
        //        | / Term NumExpr_prime
        //        | ɛ
        while (currentToken.kind == TIMES || currentToken.kind == DIV) {
          val pos = currentToken
          currentToken.kind match {
            case TIMES => {
              eat(TIMES)
              expr = new Times(expr, _Value()).setPos(pos)
            }
            case DIV => {
              eat(DIV)
              expr = new Div(expr, _Value()).setPos(pos)
            }
            case _ => {}
          }
        }

        expr
      }

      //    --> ! Factor
      //      | Factor      
      def _Value(): ExprTree = {
        var expr: ExprTree = null
        //      --> ! Value_prime
        //        | ɛ
        if (currentToken.kind == BANG) {
          val pos = currentToken
          eat(BANG)
          expr = new Not(_Value()).setPos(pos)
        } else {
          expr = _Factor()
        }

        expr
      }

      //    --> 
      def _Factor(): ExprTree = {
        var expr: ExprTree = null //Place holder
        val pos = currentToken
        currentToken.kind match {
          case INTLITKIND => {
            val num = currentToken.toString
            val Pattern = """INT\((.+)\)""".r
            val Pattern(value) = num
            eat(INTLITKIND)

            expr = new IntLit(value.toInt).setPos(pos)
          }
          case STRLITKIND => {
            val str = currentToken.toString
            val Pattern = """STR\((.*)\)""".r
            val Pattern(value) = str
            eat(STRLITKIND)

            expr = new StringLit(value).setPos(pos)
          }
          case TRUE => {
            eat(TRUE)

            expr = new True().setPos(pos)
          }
          case FALSE => {
            eat(FALSE)

            expr = new False().setPos(pos)
          }
          case IDKIND => {
            expr = _Identifier()
          }
          case THIS => {
            eat(THIS)

            expr = new This().setPos(pos)
          }
          case NEW => {
            eat(NEW)

            currentToken.kind match {
              case INT => {
                eat(INT)
                eat(LBRACKET)
                val size = _Expr()
                eat(RBRACKET)

                expr = new NewIntArray(size).setPos(pos)
              }
              case IDKIND => {
                val tpe = _Identifier()
                eat(LPAREN)
                eat(RPAREN)

                expr = new New(tpe).setPos(pos)
              }
              case _ => expected(INT, IDKIND)
            } //match whether int or id
          } //case: NEW

          case LPAREN => {
            eat(LPAREN)
            expr = _Expr()
            eat(RPAREN)
          }

          case _ => expected(INTLITKIND, STRLITKIND, TRUE, FALSE, IDKIND, THIS, NEW, LPAREN)
        } //MegaFactor Match

        //Match whether expandable
        var expand = true
        while (expand) {
          val pos = currentToken
          currentToken.kind match {
            case LBRACKET => {
              eat(LBRACKET)
              val index = _Expr()
              eat(RBRACKET)

              expr = new ArrayRead(expr, index).setPos(pos)
            }
            case DOT => {
              eat(DOT)
              currentToken.kind match {
                case LENGTH => {
                  eat(LENGTH)

                  expr = new ArrayLength(expr).setPos(pos)
                }
                case IDKIND => {
                  val meth = _Identifier()
                  eat(LPAREN)

                  //GET OUT THE ARGUMENTS IF ANY
                  var args: List[ExprTree] = Nil
                  if (currentToken.kind == INTLITKIND || currentToken.kind == STRLITKIND || currentToken.kind == TRUE || currentToken.kind == FALSE || currentToken.kind == IDKIND || currentToken.kind == THIS || currentToken.kind == NEW || currentToken.kind == LPAREN || currentToken.kind == BANG) {
                    args = args :+ _Expr()
                    while (currentToken.kind == COMMA) {
                      eat(COMMA)
                      args = args :+ _Expr()
                    }
                  }

                  eat(RPAREN)

                  expr = new MethodCall(expr, meth, args).setPos(pos)
                }
                case _ => expected(DOT, IDKIND)
              } //match after dot     
            } //case dot
            case _ => expand = false
          } //optional match for expandables
        }

        expr

      } //Factor

      _Expr()

    }

    /**
     * Identifier  ::= <IDENTIFIER>
     */
    def _Identifier(): Identifier = {
      val id: String = currentToken.toString
      var pos = currentToken
      eat(IDKIND)
      val Pattern = """ID\((.+)\)""".r
      val Pattern(value) = id
      new Identifier(value).setPos(pos)
    }

    readToken
    val tree = parseGoal
    terminateIfErrors
    tree
  }

}
