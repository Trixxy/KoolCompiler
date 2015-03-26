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
    /*var ch: Char = source.next
    var previousChar: Char = ch
    var nextChar: Char = ch
    */
    
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
      
      /** Store the current token, as read from the lexer. */
      var currentChar: Char = ' '
      var nextChar: Char = ' '
      var peekaboo = false
  
      def isIgnorable(c: Char) = {
        c match{
          case ' ' => true
          case '\t' => true
          case '\n' => true
          case _ => false
        }
      }
      
      if(!source.isEmpty){
        nextChar = source.next
        // skips bad tokens
        while (isIgnorable(nextChar) && !source.isEmpty) {
          nextChar = source.next
        }
      } else {
        isEOF = true
      }
  
      def readChar: Unit = {
        if (!source.isEmpty) {
          currentChar = nextChar
          nextChar = source.next
          
          while (isIgnorable(nextChar) && !source.isEmpty) {
            nextChar = source.next
          }
        }else if(!peekaboo){
          currentChar = nextChar
          nextChar = ' '
          peekaboo = true
        } else {
          isEOF = true
        }
      }
      
      def next = {
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
        readChar
        var pos = source.pos
        
        if(hasNext) {
          if(currentChar.isLetter) {
            state = 1
          } else if(currentChar.isDigit) {
            state = 2
          } else if(currentChar == '"') {
            state = 3
          } else if (currentChar == '/' && (nextChar == '/' || nextChar == '*')) {
            state = 4
          } else {
            state = 5
          }
          
          state match{
            case 1 => {
              while(currentChar.isLetterOrDigit || currentChar == '_') {
                sb.append(currentChar)
                readChar
              }
              keywords.get(sb.toString) match {
                case None => new ID(sb.toString).setPos(f, pos)
                case Some(res) => res.setPos(f, pos)
              }
            }
            case 2 => {
              
            }
            case 3 => {
              
            }
            case 4 => {
              
            }
            case 5 => {
              
            }
            case _ => {
              new Token(BAD)
            }
          }
        } else {
          new Token(EOF)
        }
        
        /*
        var pos = source.pos
        if(!source.isEmpty) {
          var b = new StringBuilder
          
          //Check if it's white space, tab or newline, then ignore the coming chars
          //AND check if it's a comment, then ignore the coming chars
          var doneSkipping = false
          while(!doneSkipping) {
            doneSkipping = true
            while((ch == ' ' || ch == '\t' || ch == '\n') && !source.isEmpty) {
              ch = source.next
              doneSkipping = false
            }
            
            if(ch == '/') {
              pos = source.pos
              if(!source.isEmpty) {
                previousChar = ch
                ch = source.next
              }
              if(ch == '/') {
                doneSkipping = false
                var stop = false
                while(ch != '\n' && !stop) {
                  if(!source.isEmpty) {
                    ch = source.next
                  } else {
                    stop = true
                  }
                }
              } else if(ch == '*') {
                doneSkipping = false
                ch = source.next
                var stop = false
                while(!stop) {
                  if(ch == '*') {
                    ch = source.next
                    if(ch == '/') {
                      stop = true
                    }
                  }
                  if(!source.isEmpty) {
                    ch = source.next
                  }
                }
              } else {
                nextChar = ch
                ch = previousChar
                doneSkipping = true
              }
            }
          }
          
          if(ch != '/') {
            pos = source.pos
          }
          
          if(source.isEmpty) {
            isEOF = true
            new Token(EOF).setPos(f, pos)
          } else if(ch.isLetter) {
            while(ch.isLetterOrDigit || ch == '_') {
              b.append(ch)
              ch = source.next
            }
            keywords.get(b.toString) match {
              case None => new ID(b.toString).setPos(f, pos)
              case Some(res) => res.setPos(f, pos)
            }
          } else if(ch.isDigit) {
            if(ch != '0') {
              while(ch.isDigit) {
                b.append(ch)
                ch = source.next
              }
            } else {
              b.append(ch)
              ch = source.next
            }
            new INTLIT(b.toInt).setPos(f, pos)
          } else if(ch == '"') {
            ch = source.next
            while(ch != '"') {
              b.append(ch)
              ch = source.next
            }
            ch = source.next
            new STRLIT(b.toString).setPos(f, pos)
          } else {
            b.append(ch)
            if(ch == '/') {
              ch = nextChar
            } else { 
              ch = source.next
            }
            
            if(ch == '=') {
              b.append(ch)
              ch = source.next
            } else if(ch == '&') {
              b.append(ch)
              ch = source.next
            } else if(ch == '|') {
              b.append(ch)
              ch = source.next
            }

            symbols.get(b.toString) match {
              case None => new Token(BAD).setPos(f, pos)
              case Some(res) => res.setPos(f, pos)
            }
          }
        } else {
          isEOF = true
          new Token(EOF).setPos(f, pos)
        }
        
        
        */
      }

    }
  }
}
