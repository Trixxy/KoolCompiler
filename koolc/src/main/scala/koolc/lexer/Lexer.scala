package koolc
package lexer

import utils._
import scala.io.Source
import java.io.File

object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._

  def run(ctx: Context)(f: File): Iterator[Token] = {
    val source = Source.fromFile(f)
    import ctx.reporter._

    // Complete this file
  
    var isEOF = false
    var pos = 0
    
    /** Store the current token, as read from the lexer. */
    var currentChar: Char = ' '
    var nextChar: Char = ' '
    var peekaboo = false

    if(!source.isEmpty){
      nextChar = source.next
      pos = source.pos
    } else {
      isEOF = true
    }
    
    val keywords = Map(
      "object" -> new Token(OBJECT),
      "class" -> new Token(CLASS),
      "def" -> new Token(DEF),
      "var" -> new Token(VAR),
      "Unit" -> new Token(UNIT),
      "main" -> new Token(MAIN),
      "string" -> new Token(STRING),
      "extends" -> new Token(EXTENDS),
      "int" -> new Token(INT),
      "boolean" -> new Token(BOOLEAN),
      "while" -> new Token(WHILE),
      "if" -> new Token(IF),
      "else" -> new Token(ELSE),
      "return" -> new Token(RETURN),
      "length" -> new Token(LENGTH),
      "true" -> new Token(TRUE),
      "false" -> new Token(FALSE),
      "this" -> new Token(THIS),
      "new" -> new Token(NEW),
      "println" -> new Token(PRINTLN))
    val symbols = Map(
      ":" -> new Token(COLON),
      ";" -> new Token(SEMICOLON),
      "." -> new Token(DOT),
      "," -> new Token(COMMA),
      "=" -> new Token(EQSIGN),
      "==" -> new Token(EQUALS),
      "!" -> new Token(BANG),
      "(" -> new Token(LPAREN),
      ")" -> new Token(RPAREN),
      "[" -> new Token(LBRACKET),
      "]" -> new Token(RBRACKET),
      "{" -> new Token(LBRACE),
      "}" -> new Token(RBRACE),
      "&&" -> new Token(AND),
      "||" -> new Token(OR),
      "<" -> new Token(LESSTHAN),
      "+" -> new Token(PLUS),
      "-" -> new Token(MINUS),
      "*" -> new Token(TIMES),
      "/" -> new Token(DIV))

    new Iterator[Token] {
      def hasNext = {
        !isEOF
      }
      
      def next = {
        def readChar: Unit = {
          if (!source.isEmpty) {
            currentChar = nextChar
            nextChar = source.next
          } else if(!peekaboo){
            currentChar = nextChar
            nextChar = ' '
            peekaboo = true
            isEOF = true
          }
        }
        def isIgnorable(c: Char) = {
          c match{
            case ' ' => true
            case '\t' => true
            case '\n' => true
            case _ => false
          }
        }

        /* Definition:
         * State      Descr.
         * 0          DEFAULT
         * 1          ID
         * 2          INT
         * 3          STRING
         * 4          SYMBOL
         * 5          COMMENT
         */
        
        var state = 0
        var sb = new StringBuilder
        
        //DETECT STATE:
        pos = source.pos
        
        if(hasNext) {
          readChar

          if(currentChar.isLetter) {
            state = 1
          } else if(currentChar.isDigit) {
            state = 2
          } else if(currentChar == '"') {
            state = 3
          } else if (currentChar == '/' && (nextChar == '/' || nextChar == '*')) {
            if(nextChar == '/') {
              state = 4
            } else if(nextChar == '*') {
              state = 5
            }
          } else if (isIgnorable(currentChar)) {
            state = 6
          } else {
            state = 7
          }

          if(state == 1 || state == 2 || state == 7) {
            sb.append(currentChar)
          }
          
          state match{
            case 1 => {
              while(nextChar.isLetterOrDigit || nextChar == '_') {
                readChar
                sb.append(currentChar)
              }

              keywords.get(sb.toString) match {
                case None => new ID(sb.toString).setPos(f, pos)
                case Some(res) => res.setPos(f, pos)
              }
            }
            case 2 => {
              if(currentChar != '0') {
                while(nextChar.isDigit) {
                  readChar
                  sb.append(currentChar)
                }
              } 

              new INTLIT(sb.toInt).setPos(f, pos)
            }
            case 3 => {
              while(nextChar != '"') {
                readChar
                sb.append(currentChar)
              }
              readChar

              new STRLIT(sb.toString).setPos(f, pos)
            }
            case 4 => {
              var stop = false
              while(nextChar != '\n' && hasNext) {
                readChar
              }
              readChar

              next
            }
            case 5 => {
              var stop = false
              while(!stop && hasNext) {
                readChar
                if(nextChar == '*') {
                  readChar
                  if(nextChar == '/') {
                    stop = true
                  }
                }
              }
              readChar

              next
            }
            case 6 => {
              while(isIgnorable(nextChar) && hasNext) {
                readChar
              }

              next
            }
            case _ => {
              if(nextChar == '=') {
                readChar
                sb.append(currentChar)
              } else if(nextChar == '&') {
                readChar
                sb.append(currentChar)
              } else if(nextChar == '|') {
                readChar
                sb.append(currentChar)
              }

              symbols.get(sb.toString) match {
                case None => new Token(BAD).setPos(f, pos)
                case Some(res) => res.setPos(f, pos)
              }
            }
          }
        } else {
          new Token(EOF).setPos(f, pos)
        }
      }
    }
  }
}
