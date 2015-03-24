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
    var ch: Char = source.next
    var previousChar: Char = ch
    var nextChar: Char = ch

    val keywords = Map(
      "object" -> new Token(OBJECT),
      "class" -> new Token(CLASS),
      "def" -> new Token(DEF),
      "var" -> new Token(VAR),
      "unit" -> new Token(UNIT),
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
      "println" -> new Token(PRINTLN)     
    )
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
      "/" -> new Token(DIV)
    ) 

    new Iterator[Token] {
      def hasNext = {
        !isEOF
      }

      def next = {
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
                      ch = source.next
                      stop = true
                    }
                  }
                  if(!stop) {
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
            while(ch.isDigit) {
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
      }
    }
  }
}
