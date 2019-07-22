import scala.util.parsing.combinator._

/**
  * Made by Peyman on 1/26/2018.
  */

trait Line

sealed abstract class Element

case class Name(name: String) extends Element

case class Number(number: String) extends Element

case class TypeP(typeP: String)

case class Def(name: Name, allDef: AllDef) extends Line

sealed abstract class AllDef

case class DefFunc(inp: List[TypeP], out: TypeP) extends AllDef

case class DefVar(typeP: TypeP) extends AllDef

case class DefTimeFunc() extends AllDef

case class Expr(exprInp: ExprInp)

sealed abstract class ExprInp

case class MathP(tempExpr: TempExpr, sig: String, expr: Expr) extends ExprInp

case class TempExpr(tempExprImp: TempExprInp) extends ExprInp

sealed abstract class TempExprInp

case class IntP(element: Element) extends TempExprInp

case class DoubleP(element: Element) extends TempExprInp

case class DateP(name: Name) extends TempExprInp

case class Parenthesis(expr: Expr) extends TempExprInp

case class Assign(name: Name, expr: Expr) extends Line

case class ArgP(expr: Expr)

case class ArgsP(inp: List[ArgP])

case class FuncCall(funcCallImp: FuncCallInp) extends TempExprInp with Line

sealed abstract class FuncCallInp

case class Func3(name: Name, argsP: ArgsP) extends FuncCallInp

case class Func1() extends FuncCallInp

case class Func2(argP: ArgP) extends FuncCallInp

case class Func4(argP1: ArgP, argP2: ArgP) extends FuncCallInp

case class Func5(st: String, argP1: ArgP, argP2: ArgP) extends FuncCallInp

case class Program(line: Line)

class ProjectParser extends RegexParsers {
  def spliter(types: List[Any], st: String): List[Any] = types match {
    case ((st: String) ~ typeP) :: rest => typeP :: spliter(rest, st)
    case any => any
  }

  def helpType(types: List[Any]): List[TypeP] = spliter(types, ",").map { case d: TypeP => d }

  def helpArg(types: List[Any]): List[ArgP] = spliter(types, ",").map { case d: ArgP => d }

  def name: Parser[Name] = """[a-z]+[a-z|0-9]*""".r ^^ Name

  def number: Parser[Number] = """[0-9]+|[0-9]+\.[0-9]*""".r ^^ Number

  def typeP: Parser[TypeP] = """Int|Date|Double|Contract""".r ^^ TypeP

  def defFunc: Parser[DefFunc] = "(" ~ typeP ~ rep("," ~ typeP) ~ ")" ~ "->" ~ typeP ^^ {
    case "(" ~ first ~ after ~ ")" ~ "->" ~ ret => DefFunc(first :: helpType(after), ret)
  }

  def defVar: Parser[DefVar] = typeP ^^ DefVar

  def defTimeFunc: Parser[DefTimeFunc] = "TimeFunc" ~ "(" ~ "Date" ~ ")" ~ "−|-".r ~ ">" ~ "Double" ^^ (_ => DefTimeFunc())

  def defP: Parser[Def] = name ~ "::" ~ (defVar ||| defFunc ||| defTimeFunc) ^^ {
    case start ~ "::" ~ end => Def(start, end)
  }

  def intP: Parser[IntP] = (name | number) ^^ IntP

  def doubleP: Parser[DoubleP] = (name | number) ^^ DoubleP

  def dateP: Parser[DateP] = name ^^ DateP

  def parenthesis: Parser[Parenthesis] = "(" ~ expr ~ ")" ^^ { case "(" ~ ex ~ ")" => Parenthesis(ex) }

  def expr: Parser[Expr] = (tempExpr ||| mathP) ^^ Expr

  def mathP: Parser[MathP] = tempExpr ~ ("*" | "/" | "+" | "-") ~ expr ^^ {
    case ex1 ~ "*" ~ ex2 => MathP(ex1, "*", ex2)
    case ex1 ~ "/" ~ ex2 => MathP(ex1, "/", ex2)
    case ex1 ~ "+" ~ ex2 => MathP(ex1, "+", ex2)
    case ex1 ~ "-" ~ ex2 => MathP(ex1, "-", ex2)
  }

  def tempExpr: Parser[TempExpr] = (funcCall | intP | doubleP | parenthesis | dateP) ^^ TempExpr

  def assign: Parser[Assign] = name ~ "=" ~ expr ^^ { case name ~ "=" ~ ex => Assign(name, ex) }

  def argP: Parser[ArgP] = expr ^^ ArgP

  def argsP: Parser[ArgsP] = argP ~ rep("," ~ argP) ^^ {
    case first ~ reap => ArgsP(first :: helpArg(reap))
  }

  def funcCall: Parser[FuncCall] = (func1 | func2 | func4 | func5 | func3) ^^ FuncCall

  def func3: Parser[Func3] = name ~ "(" ~ argsP ~ ")" ^^ { case name ~ "(" ~ args ~ ")" => Func3(name, args) }

  def func1: Parser[Func1] = "one" ~ "(" ~ ")" ^^ (_ => Func1())

  def func2: Parser[Func2] = "give" ~ "(" ~ argP ~ ")" ^^ { case "give" ~ "(" ~ arg ~ ")" => Func2(arg) }

  def func4: Parser[Func4] = "mkdate" ~ "(" ~ argP ~ "," ~ argP ~ ")" ^^ { case "mkdate" ~ "(" ~ arg1 ~ "," ~ arg2 ~ ")" => Func4(arg1, arg2) }

  def func5: Parser[Func5] =
    """scaleX|and|then|scale|truncate""".r ~ "(" ~ argP ~ "," ~ argP ~ ")" ^^ {
      case name ~ "(" ~ arg1 ~ "," ~ arg2 ~ ")" => Func5(name, arg1, arg2)
    }

  def program: Parser[Program] = (assign ||| funcCall ||| defP) ^^ Program

  def applyP(input: String): Any = parse(program, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }
}

object ProjectGenerator {
  def generate(inp: Any): String = inp match {
    case inp: String => inp
    case Name(o) => o match {
      case "give" => "defGive"
      case "and" => "defAnd"
      case "then" => "defThen"
      case "scaleX" => "defScaleX"
      case "truncate" => "defTruncate"
      case _ => s"${generate(o)}_name"
    }
    case TypeP(o) => o match {
      case "Int" => "int"
      case "Date" => "int"
      case "Double" => "double"
      case "Contract" => "Contract"
    }
    case Number(o) => s"${generate(o)}"
    case Def(o, DefTimeFunc()) => s"double ${generate(o)} (int){"
    case Def(o, DefVar(v)) => s"${generate(v)} ${generate(o)}"
    case Def(o, DefFunc(f1, f2)) => s"${generate(f2)} ${generate(o)} (${generate(f1)}){"
    case Expr(o) => s"${generate(o)}"
    case MathP(o1, o2, o3) => s"${generate(o1)} ${generate(o2)} ${generate(o3)}"
    case TempExpr(t1) => s"${generate(t1)}"
    case IntP(o) => s"${generate(o)}"
    case DoubleP(o) => s"${generate(o)}"
    case DateP(o) => s"${generate(o)}"
    case Parenthesis(o) => s"(${generate(o)})"
    case Assign(o1, o2) => s"${generate(o1)} = ${generate(o2)}"
    case ArgP(o) => s"${generate(o)}"
    case ArgsP(o) => s"${generate(o)}"
    case FuncCall(o) => s"${generate(o)}"
    case Func1() => s"Contract(InfDate, 1)"
    case Func2(o) => s"defGive(${generate(o)})"
    case Func3(o1, o2) => s"${generate(o1)}(${generate(o2)})"
    case Func4(o1, o2) => s"defMakeDate(${generate(o1)}, ${generate(o2)})"
    case Func5(o1, o2, o3) => s"def${generate(o1)}(${generate(o2)}, ${generate(o3)})"
    case Program(o) => s"${generate(o)}"

    case l: List[Any] => l match {
      case first :: rest => rest match {
        case Nil => s"${generate(first)}"
        case any => s"${generate(first)}, ${generate(any)}"
      }
      case Nil => ""
    }
    case s => s.toString
  }
}


object main extends ProjectParser {
  def main2(args: Array[String]): Unit = {
    val expr = applyP("f = arg1 - 363 * 24")
    println(expr)
  }

  def main(args: Array[String]): Unit = {
    var lst: Array[String] = Array()
    var temp = ""
    do {
      lst = lst :+ temp
      temp = scala.io.StdIn.readLine()
    } while (!"END".equals(temp))


    temp = ""
    for (temp2 <- lst) {
      if (temp2 != "") {
        val for_generate = applyP(temp2.trim)
        temp = temp + ProjectGenerator.generate(for_generate) + "\n"
      }
    }

    val lines: Array[String] = temp.split("\n")


    println(
      """

#include <iostream>
using namespace std;
#define InfDate 365*24
int today;
int start;
class Contract{
public:
    int exDate;
    double value;
    Contract(int exDate, double value){
        this->exDate = exDate;
        this->value = value;
    }
    Contract(){
    }
    double getVal(){
        if (today > exDate) return 0;
        else return value;
    }
};

int defMakeDate(int day, int hour){
    return (day -1) * 24 + hour;
}

Contract defOne(){
    return Contract(InfDate, 1);
}

Contract defGive(Contract con){
    return Contract(con.exDate, -1 * con.value);
}

Contract defscale(double scale, Contract con){
    return Contract(con.exDate, scale * con.value);
}

Contract deftruncate(int date, Contract con){
    if (date < con.exDate) return Contract(date, con.value);
    else return Contract(con.exDate, con.value);
}

Contract defand(Contract con1, Contract con2){
	if(con1.exDate >= con2.exDate) return Contract(con1.exDate, con1.getVal() + con2.getVal());
    else return Contract(con2.exDate, con1.getVal() + con2.getVal());
}

Contract defthen(Contract con1, Contract con2){
	if(con1.exDate >= con2.exDate) return Contract(con1.exDate, con1.getVal());
	else if(start > con1.exDate) return Contract(con2.exDate, con2.getVal());
	else if(con1.getVal() > con2.getVal()) return Contract(con2.exDate, con1.getVal());
	else Contract(con2.exDate, con2.getVal());
}

Contract defscaleX(double d, Contract con){
     return Contract(con.exDate, con.value * d);
}

"""
    )

    var i = 0
    while (i < lines.length) {
      lines(i) match {
        case st0 if st0.contains("defscaleX") =>
          var tempS = ""
          tempS += lines(i).substring(0, lines(i).indexOf(","))
          tempS += "(today)"
          tempS += lines(i).substring(lines(i).indexOf(","), lines(i).length)
          lines(i) = tempS
          i += 1
        case st if st.endsWith("{") =>
          var count = 1
          printf(lines(i).substring(0, lines(i).indexOf("(")))
          val str = lines(i).substring(lines(i).indexOf("("), lines(i).length)
          var j = 0
          while (j < str.length) {
            str.substring(j) match {
              case a if a.startsWith("int") =>
                j += 3
                printf("int arg" + count + "_name")
                count += 1
              case a if a.startsWith("double") =>
                j += 6
                printf("double arg" + count + "_name")
                count += 1
              case any =>
                printf("" + any.charAt(0))
                j += 1
            }
          }
          println
          println("\treturn " + lines(i + 1).substring(lines(i + 1).indexOf('=') + 1) + "; \n}")
          lines(i) = ""
          lines(i + 1) = ""
          i += 2
        case _ => i += 1
      }
    }

    val temp3 = scala.io.StdIn.readLine().split(" ")
    val n = temp3(0).toInt
    val t = temp3(1).toInt
    println(
      s"""
int main(){
  start = $t;
  double cons[$n] = {0};
  for(int i=$t; i<=365*24-1; i++){
  today = i;
			""")

    for (temp2 <- lines) {
      if (temp2 != "") {
        println(temp2 + ";")
      }
    }


    for (i <- 1 until n + 1) {
      temp = scala.io.StdIn.readLine()
      println(s"if(cons[${i - 1}]<${temp}_name.getVal()) cons[${i - 1}]=${temp}_name.getVal();")
    }


    println("}\nlong double ret = 0;")
    println(
      s"""for (int i = 0; i< $n; i++)
         |ret += cons[i];
         |""".stripMargin)


    println("cout << fixed << ret;\nreturn 0;\n}")
  }
}


