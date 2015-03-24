package koolc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._

object Parser extends Pipeline[Iterator[Token], Program] {
  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._

    /** Store the current token, as read from the lexer. */
    var currentToken: Token = new Token(BAD)

    def readToken: Unit = {
      if (tokens.hasNext) {
        // uses nextToken from the Lexer trait
        currentToken = tokens.next

        // skips bad tokens
        while (currentToken.kind == BAD) {
          currentToken = tokens.next
        }
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
      fatal("expected: " + (kind::more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
    }

    def parseGoal: Program = {
      _MainObject()
      _ClassDeclaration()
      eat(EOF)
    }

    def _MainObject() = {
      eat(OBJECT) 
      _Identifier()
      eat(LBRACE)
      eat(DEF)
      eat(MAIN) 
      eat(LPAREN) 
      eat(RPAREN) 
      eat(COLON) 
      eat(UNIT) 
      eat(EQSIGN) 
      eat(LBRACE) 
      _Statement() 
      eat(RBRACE) 
      eat(RBRACE)
    }
    def _ClassDeclaration() = {
      eat(CLASS) 
      _Identifier()
      
      if(currentToken.kind == EXTENDS){
        eat(EXTENDS) 
        _Identifier() 
      }

      eat(LBRACE) 
      _VarDeclaration() 
      _MethodDeclaration() 
      eat(RBRACE)  
    }
    def _VarDeclaration() = {
      //OPTIONAL
      if(currentToken.kind == VAR){
        eat(VAR) 
        _Identifier() 
        eat(COLON)
        _Type() 
        eat(SEMICOLON)
      }
    }
    def _MethodDeclaration() = {
      //OPTIONAL
      if(currentToken.kind == DEF){
        eat(DEF) 
        _Identifier() 
        eat(LPAREN)

        if(currentToken.kind != RPAREN){
          _Identifier() 
          eat(COLON)
          _Type() 
          
          while(currentToken.kind == COMMA){
            eat(COMMA) 
            _Identifier() 
            eat(COLON) 
            _Type()
          }
        }

        eat(RPAREN) 
        eat(COLON) 
        _Type()
        eat(EQSIGN) 
        eat(LBRACE)
        _VarDeclaration() 
        _Statement()
        eat(RETURN) 
        _Expression()
        eat(SEMICOLON) 
        eat(RBRACE)
      }
    }
    def _Type() = {
      currentToken.kind match {
        case INT => {
          eat(INT)
          if(currentToken.kind == LBRACKET){
            eat(LBRACKET)
            eat(RBRACKET)
          }
        }  
        case BOOLEAN => eat(BOOLEAN)
        case STRING => eat(STRING)
        case IDKIND => _Identifier()
        case _ => expected(INT, BOOLEAN, STRING, IDKIND)
      }
    }
    def _Statement () : Unit = {
      //OPTIONAL
      currentToken.kind match {
        case LBRACE => {
          eat(LBRACE)
          _Statement() 
          eat(RBRACE)
        }
        case IF => {
          eat(IF) 
          eat(LPAREN) 
          _Expression() 
          eat(RPAREN) 
          _Statement() 
          if(currentToken.kind == ELSE){
            eat(ELSE) 
            _Statement()
          }
        }
        case WHILE => {
          eat(WHILE)
          eat(LPAREN) 
          _Expression() 
          eat(RPAREN) 
          _Statement()
        }
        case PRINTLN => {
          PRINTLN 
          eat(LPAREN) 
          _Expression() 
          eat(RPAREN) 
          eat(SEMICOLON)
        }
        case IDKIND => {
          _Identifier()
          
          currentToken.kind match {
            case EQSIGN => {
              eat(EQSIGN) 
              _Expression() 
              eat(SEMICOLON)
            }
            case LBRACKET => {
              eat(LBRACKET) 
              _Expression() 
              eat(RBRACKET) 
              eat(EQSIGN) 
              _Expression() 
              eat(SEMICOLON)
            }
            case _ => expected(EQSIGN, LBRACKET)
          }

        }

      }

    }
    def _Expression  () : Unit = {

      //FACTOR
      currentToken.kind match{
        case INTLITKIND => {
          eat(INTLITKIND)
        }
        case STRLITKIND => {
          eat(STRLITKIND)
        }
        case TRUE => {
          eat(TRUE)
        }
        case FALSE => {
          eat(FALSE)
        } 
        case IDKIND => {
          _Identifier()
        }
        case THIS => {
          eat(THIS)
        }
        case NEW => {
          eat(NEW)
          currentToken.kind match{
            case INT => {
              eat(INT) 
              eat(LBRACKET) 
              _Expression() 
              eat(RBRACKET)
            }
            case IDKIND => {
              _Identifier() 
              eat(LPAREN) 
              eat(RPAREN)
            }
            case _ => expected(INT, IDKIND)
          }
          
        }
        case BANG => {
          eat(BANG)
          _Expression()
        }
        case LPAREN => {
          eat(LPAREN)
          _Expression()
          eat(RPAREN)
        }
        case _ => expected(INTLITKIND, STRLITKIND, TRUE, FALSE, IDKIND, THIS, NEW, BANG, LPAREN)      

      }



      //EXTENDABLES


      currentToken.kind match{
        case AND => {
          eat(AND)
          _Expression()
        }  
        case OR => {
          eat(OR)
          _Expression()
        }  
        case EQUALS => {
          eat(EQUALS)
          _Expression()
        }  
        case LESSTHAN => {
          eat(LESSTHAN)
          _Expression()
        }  
        case PLUS => {
          eat(PLUS)
          _Expression()
        }  
        case MINUS => {
          eat(MINUS)
          _Expression()
        }  
        case TIMES => {
          eat(TIMES)
          _Expression()
        }  
        case DIV => {
          eat(DIV)
          _Expression()
        }

        case LBRACKET => {
          eat(LBRACKET) 
          _Expression() 
          eat(RBRACKET)
        }

        case DOT => {
          eat(DOT)
          currentToken.kind match{
            case LENGTH => {
              eat(LENGTH)
            }
            
            case IDKIND => {
              _Identifier()
              eat(LPAREN) 

              if(currentToken.kind == INTLITKIND ||  currentToken.kind == STRLITKIND ||  currentToken.kind == TRUE ||  currentToken.kind == FALSE ||  currentToken.kind == IDKIND ||  currentToken.kind == THIS ||  currentToken.kind == NEW ||  currentToken.kind == BANG ||  currentToken.kind == LPAREN){
                _Expression()
                while(currentToken.kind == COMMA){
                  eat(COMMA)
                  _Expression()
                }
              }
              eat(RPAREN)
            }

            case _ => expected(LENGTH, IDKIND)

          } 
                             
        }

      }



    }
    def _Identifier  () = {
      eat(IDKIND)
    }

    readToken
    val tree = parseGoal
    terminateIfErrors
    tree
  }

}


/*

      currentToken match {
      case A => // compare x to A, because of the uppercase
      case b => // assign x to b
      case `b` => // compare x to b, because of the backtick
      case _ => error()
    }

*/

/****************************************************************/

