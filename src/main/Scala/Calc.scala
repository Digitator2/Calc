import scala.annotation.tailrec

object Calc extends App {

  val setNumbers = Set('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')

  type TSTR = Seq[Char]

  def parseNumber(str:TSTR) : (Double,  TSTR) = {

    @tailrec def recMant(value: String, str: TSTR): (String, TSTR) = str match {
      case head +: tail if setNumbers.contains(head) && head != '.' => recMant( value :+ head, tail )
      case head +: tail if head == '+' || head == '-' =>
        if(!value.isEmpty) throw new Exception("Parse number mantissa error '" + head + "', '" + value + "'")
        recMant( value :+ head, tail )
      case _ => (value, str)
    }

    @tailrec def rec(value: String, str: TSTR): (String, TSTR) = str match {
      case head +: tail if value.isEmpty && (head == '-' || head == '+') => rec( value :+ head, tail )
      case head +: tail if setNumbers.contains(head) || head == '.'  => rec( value :+ head, tail )
      case head +: tail if head == 'e' || head == 'E' =>
        if(value.isEmpty) throw new Exception("Parse number mantissa error '" + value + "'")
        val (resMant, outStr) = recMant("", tail)
        (value + "E" + resMant, outStr)
      case _ => (value, str)
    }

    val (valueStr, outStr) = rec("", str)
    val value = try { valueStr.mkString.toDouble } catch { case _ => throw new Exception("Error parse string '" + valueStr +"'" ) }
    (value, outStr)
  }

  def calc(str:String):Double = {   // вычисляет со скобками, без стека, унарные и бинарные операторы

    abstract class Elem()
    case class Number(n:Double) extends Elem

    abstract class Operand(val symbol:Char, val priority:Int) extends Elem
    case class Op1(override val symbol:Char,override val priority:Int, f : Double => Double) extends Operand(symbol, priority)
    case class Op2(override val symbol:Char,override val priority:Int, f : (Double,Double) => Double) extends Operand(symbol, priority)

    val operands = Seq(
      Op2('*', 900, (a,b)=>a*b ),
      Op2('/', 900, (a,b)=>a/b ),
      Op2('+', 500, (a,b)=>a+b ),
      Op2('-', 500, (a,b)=>a-b ),
      Op1('~', 990, a => (~a.toInt) )
    )

    def runBraces(str : TSTR): (Number,  TSTR)=  {
      val (outVals, outStr) = run( Seq(), str)
      val res:Number = outVals match {
        case (num @ Number(head)) +: tail => num
        case head +: tail => throw new Exception("Not Number in head " + head)
        case Seq() => throw new Exception("Empty seq")
      }
      (res, outStr)
    }

    def calcBraces( tokens: Seq[Elem]):Double = { // вычисляет через Seq

      @tailrec def rec( tokensFirst: Seq[Elem], tokens : Seq[Elem]):Seq[Elem] = tokens match {

        case Seq(n1 @ Number(x)) =>
          if (tokensFirst.isEmpty)  tokens  else rec(Seq(), tokensFirst ++ tokens)

        case (n1 @ Number(x1)) +: (o1 @ Op2(_,_,_)) +: (n2 @ Number(x2)) +: Seq() =>
          val expr = Seq(Number(o1.f(x1, x2)))
          rec( Seq(), tokensFirst ++ expr )

        case ( n1 @ Number(x1)) +: (o1 @ Op2(_,p1,_)) +: (n2 @ Number(x2)) +: (o2 @ Op2(_,p2,_)) +: tail  =>
          if (p1 >= p2) {
            val expr = Seq(Number(o1.f(x1, x2)))
            rec( Seq(), tokensFirst ++ expr ++ Seq(o2) ++ tail )
          }
          else rec( tokensFirst ++ Seq(n1) ++ Seq(o1) ,  Seq(n2) ++ Seq(o2) ++ tail )

        case Seq( o1 @ Op1(_,_,_), n1 @ Number(x1), tail @_*) =>
          val expr = Seq(Number(o1.f(x1)))
          rec( Seq(), tokensFirst ++ expr ++ tail )

        case Seq( n1 @ Number(_), o1 @ Op2(_,p1,_), o2 @ Op1(_,p2,_), tail @_* ) =>
          if (p2 >= p1) rec( tokensFirst ++ Seq(n1) ++ Seq(o1), Seq(o2) ++ tail )
          else throw new Exception("op2 operands has more priority than op1 operand: " + tokens)

        case _ => throw new Exception("Error in expression: " + tokens)
      }

      rec(Seq(), tokens) match {
        case Seq(Number(x)) => x
        case r => throw new Exception("Unknown result in expression: " + r )
      }
    }

    @tailrec def run( tokens: Seq[Elem], str: TSTR): ( Seq[Elem], TSTR) = (tokens, str) match {
      case (tokens, str) if str.isEmpty =>
        (tokens, str)

      case ( tokens, str @ head +: tail) =>
        head match {
          case head if head == ' ' || head == '\t' || head == '\n' => run(tokens, tail)

          case head if setNumbers.contains(head) || head == '.' || (tokens.isEmpty && (head == '-' || head == '+')) =>
            val (n, outStr) = parseNumber(str)
            run(  tokens :+ Number(n), outStr )

          case head if operands.exists( _.symbol == head ) =>
            val newOp = operands.find(_.symbol == head).getOrElse(throw new Exception("in operands not found " + head ))
            if(newOp.isInstanceOf[Op2] && tokens.isEmpty) throw new Exception("op2 without left parameter " + head )
            run( tokens :+ newOp, tail)

          case head if head == '(' =>
            val (bv, outStr) = runBraces(tail)
            run( tokens :+ bv, outStr )

          case head if head == ')' =>
            val outAcc = calcBraces(tokens)
            ( Seq(Number(outAcc)), tail )
        }
    }

    val r = run(Seq(), "("+str+")")
    r match {
      case (Seq(Number(x)), _ ) => x
      case (Seq(x), _ ) => throw new Exception("first element not number " + x)
      case _ => throw new Exception("Unknown result")
    }
  }

  val r = calc("3.93748521E-2 + 0.683629E1 / (0.4 - 8 * 3.74) * 0.022E2 ") // -0.4701047549460704
  //val r = calc("9.32342342 + 0.23422E1")  // 11,66562342
  //val r = calc("9 * (10+8+(6-2 * ( 4 - 6 + ((20 - 8)/2 - 2 - 1 + (34 - 30 ) * 3 ) + 6 / 3 + 7*2 ))+18)")
  //val r = calc("9 * ( 18+ ( 6 - 2 * 29 +18))  ")  // -144
  //val r = calc( "(-3)+(-4)")
  //val r = calc( "2*~1")
  println(r)

}