package net.ivoah.slippy

import scala.collection.mutable.{Stack, ListBuffer}

case class VarArgs(fn: Seq[Value] => Value)
case class Function(types: Seq[Type[?]], fn: Seq[Value] => Value) {
  def apply(args: Seq[Value]): Value = {
    if (args.length != types.length) throw Exception(s"Wrong number of arguments passed: expected ${types.length} got ${args.length}")
    fn(args.zip(types).map{case (arg, t) => arg.cast(t)})
  }
}

enum Type[+T] {
  case Nil extends Type[Null]
  case Integer extends Type[Int]
  case Double extends Type[Double]
  case String extends Type[String]
  case Vector extends Type[Seq[?]]
  case Dictionary extends Type[Map[?, ?]]
  case Keyword extends Type[Keyword]
  case Function extends Type[Function]

  def cast(v: Value): Value = Value(this, v.v.asInstanceOf[T])
}

val implicits = Map[(Type[?], Type[?]), Value => Value](
  (Type.Integer, Type.Double) -> ((i: Value) => Value(Type.Double, i.v.asInstanceOf[Int].toDouble))
)

// type Value = Null | Boolean | Double | String | Function
// type Value = Any

// extension(v: Value) {
//   def cast[T]: T = v match {
//     case i: Int => i.toDouble.asInstanceOf[T]
//     case _ => v.asInstanceOf[T]
//   }
// }

case class Value(t: Type[?], v: Any) {
  def cast(to: Type[?]): Value = implicits.getOrElse((t, to), to.cast)(this)
  def toScala[T]: T = v.asInstanceOf[T]
}

object Value {
  def apply(v: Any): Value = v match {
    case null         => Value(Type.Nil, null)
    case i: Int       => Value(Type.Integer, i)
    case d: Double    => Value(Type.Double, d)
    case s: String    => Value(Type.String, s)
    case v: Seq[?]    => Value(Type.Vector, v)
    case d: Map[?, ?] => Value(Type.Dictionary, d)
    case k: Keyword   => Value(Type.Keyword, k)
    case f: Function  => Value(Type.Function, f)
  }
}

type LispObj = Map[String, Value]

type Environment = Map[String, Value]

sealed trait Expr {
  def eval(implicit env: Environment): Value
}

// case class FnDef(parameters: VectorExpr, body: Expr) extends Expr {
//   override def toString(): String = s"(fn $parameters $body)"

//   override def eval(implicit env: Environment): Value = {
//     parameters.elements.map(_.asInstanceOf[Identifier]) match {
//       case _ => Value(null)
//       // case Seq() => () => body.eval(env ++ Map())
//       // case Seq(p1) => (pp1: Value) => body.eval(env ++ Map(p1.name -> pp1))
//       // case Seq(p1, p2) => (pp1: Value, pp2: Value) => body.eval(env ++ Map(p1.name -> pp1, p2.name -> pp2))
//       // case Seq(p1, p2, p3) => (pp1: Value, pp2: Value, pp3: Value) => body.eval(env ++ Map(p1.name -> pp1, p2.name -> pp2, p3.name -> pp3))
//       // case Seq(p1, p2, p3, p4) => (pp1: Value, pp2: Value, pp3: Value, pp4: Value) => body.eval(env ++ Map(p1.name -> pp1, p2.name -> pp2, p3.name -> pp3, p4.name -> pp4))
//       // case Seq(p1, p2, p3, p4, p5) => (pp1: Value, pp2: Value, pp3: Value, pp4: Value, pp5: Value) => body.eval(env ++ Map(p1.name -> pp1, p2.name -> pp2, p3.name -> pp3, p4.name -> pp4, p5.name -> pp5))
//     }
//   }
// }

case class ObjAccess(obj: Identifier, field: Identifier) extends Expr {
  override def toString: String = s"(. $obj $field)"

  override def eval(implicit env: Environment): Value = {
    obj.eval().asInstanceOf[LispObj](field.name)
  }
}

case class LetBlock(bindings: VectorExpr, body: Expr) extends Expr {
  override def toString(): String = s"(let $bindings $body)"

  override def eval(implicit env: Environment): Value = {
    body.eval(env ++ bindings.elements.grouped(2).map {
      case Seq(identifier, value) => identifier.asInstanceOf[Identifier].name -> value.eval
    }.toMap)
  }
}

case class ListExpr(elements: Expr*) extends Expr {
  override def toString(): String = elements.mkString("(", " ", ")")

  override def eval(implicit env: Environment): Value = {
    val args = elements.tail.map(_.eval)
    elements.head.eval() match {
      case Value(Type.Function, fn: Function) => fn(args)
      // case VarArgs(fn) => fn(args)
      // case fn: Function0[_] => Value(fn())
      // case fn: Function1[t0, _] => Value(fn(args(0).v.asInstanceOf[t0]))
      // case fn: Function2[t0, t1, _] => Value(fn(args(0).v.asInstanceOf[t0], args(1).v.asInstanceOf[t1]))
      // case fn: Function3[t0, t1, t2, _] => fn(args(0).cast[t0], args(1).cast[t1], args(2).cast[t2])
      // case fn: Function4[t0, t1, t2, t3, _] => fn(args(0).cast[t0], args(1).cast[t1], args(2).cast[t2], args(3).cast[t3])
      // case fn: Function5[t0, t1, t2, t3, t4, _] => fn(args(0).cast[t0], args(1).cast[t1], args(2).cast[t2], args(3).cast[t3], args(4).cast[t4])
      // case fn: Function6[t0, t1, t2, t3, t4, t5, _] => fn(args(0).cast[t0], args(1).cast[t1], args(2).cast[t2], args(3).cast[t3], args(4).cast[t4], args(5).cast[t5])
      // case fn: Function7[t0, t1, t2, t3, t4, t5, t6, _] => fn(args(0).cast[t0], args(1).cast[t1], args(2).cast[t2], args(3).cast[t3], args(4).cast[t4], args(5).cast[t5], args(6).cast[t6])
      // case fn: Function8[t0, t1, t2, t3, t4, t5, t6, t7, _] => fn(args(0).cast[t0], args(1).cast[t1], args(2).cast[t2], args(3).cast[t3], args(4).cast[t4], args(5).cast[t5], args(6).cast[t6], args(7).cast[t7])
    }
  }
}

case class VectorExpr(elements: Expr*) extends Expr {
  override def toString(): String = elements.mkString("[", " ", "]")
  override def eval(implicit env: Environment): Value = Value(Type.Vector, elements.map(_.eval))
}

case class DictExpr(elements: Expr*) extends Expr {
  override def toString(): String = elements.mkString("{", " ", "}")
  override def eval(implicit env: Environment): Value = Value(Type.Dictionary, elements.map(_.eval).grouped(2).map { case Seq(key, value) => key -> value }.toMap)
}

case class If(condition: Expr, trueCase: Expr, falseCase: Expr) extends Expr {
  override def toString(): String = s"(if $condition $trueCase $falseCase)"
  override def eval(implicit env: Environment): Value = if (condition.eval.asInstanceOf[Boolean]) trueCase.eval else falseCase.eval
}

case class Constant[T](value: T) extends Expr {
  override def toString(): String = value match {
    case v if v == null => "nil"
    case string: String => s"\"$string\""
    case other => other.toString
  }
  override def eval(implicit env: Environment): Value = Value(value)
}

case class Keyword(val name: String) extends Expr {
  override def toString(): String = s":$name"
  override def eval(implicit env: Environment): Value = Value(this)
}

case class Identifier(val name: String) extends Expr {
  override def toString(): String = name
  override def eval(implicit env: Environment): Value = env(name)
}

def atom(value: String): Expr = {
  value match {
    case "nil"                                          => Constant(null)
    case boolean    if boolean.toBooleanOption.nonEmpty => Constant(boolean.toBoolean)
    case int        if int.toIntOption.nonEmpty         => Constant(int.toInt)
    case double     if double.toDoubleOption.nonEmpty   => Constant(double.toDouble)
    case s"\"$str\""                                    => Constant(str)
    case s":$keyword"                                   => Keyword(keyword)
    case identifier                                     => Identifier(identifier)
  }
}

type Token = String

def tokenize(chars: String): Seq[Token] = {
  val parens = "()[]{}".toSet
  Iterator.unfold(("", 0)) { case (curTok, i) =>
    chars.lift(i) match {
      case Some(paren) if parens.contains(paren) => Some(Seq(curTok, paren.toString), ("", i + 1))
      case Some(whitespace) if whitespace.isWhitespace && curTok.nonEmpty => Some(Seq(curTok), ("", i + 1))
      case Some(whitespace) if whitespace.isWhitespace => Some(Seq(), ("", i + 1))
      case Some('"') =>
        val endQuote = chars.indexOf("\"", i + 1)
        Some(Seq(chars.substring(i, endQuote + 1)), ("", endQuote + 1))
      case Some(';') =>
        val endOfLine = chars.indexOf("\n", i + 1)
        Some(Seq(curTok), ("", if (endOfLine == -1) chars.length else endOfLine + 1))
      case Some(c) => Some(Seq(), ((curTok + c), i + 1))
      case None if curTok.nonEmpty => Some(Seq(curTok), ("", i))
      case None => None
    }
  }.toSeq.flatten.filter(_.nonEmpty)
}

def parse(tokens: Stack[Token]): Expr = {
  if (tokens.isEmpty) throw Exception("Unexpected EOF")
  tokens.pop() match {
    case "(" =>
      val L = ListBuffer[Expr]()
      while (tokens.top != ")") {
        L.append(parse(tokens))
      }
      tokens.pop()
      L.toSeq match {
        // case Seq(Identifier("fn"), parameters: VectorExpr, body: Expr) => FnDef(parameters, body)
        case Seq(Identifier("."), obj: Identifier, field: Identifier) => ObjAccess(obj, field)
        case Seq(Identifier("let"), bindings: VectorExpr, body: Expr) => LetBlock(bindings, body)
        case Seq(Identifier("if"), condition: Expr, trueCase: Expr, falseCase: Expr) => If(condition, trueCase, falseCase)
        case l => ListExpr(l*)
      }
    case "[" =>
      val V = ListBuffer[Expr]()
      while (tokens.top != "]") {
        V.append(parse(tokens))
      }
      tokens.pop()
      VectorExpr(V.toSeq*)
    case "{" =>
      val D = ListBuffer[Expr]()
      while (tokens.top != "}") {
        D.append(parse(tokens))
      }
      tokens.pop()
      DictExpr(D.toSeq*)
    case ")" | "]" | "}" => throw Exception("Unexpected )")
    case token => atom(token)
  }
}

def parse(code: String): Expr = {
  val tokens = Stack.from(tokenize(code))
  val expr = parse(tokens)
  if (tokens.nonEmpty) throw Exception(s"Expected end of input")
  expr
}

def eval(code: String)(implicit env: Environment) = parse(code).eval()

import scala.util.Random

// def doubleType(v: Value): Double = {
//   v match {
//     case i: Int => summon[Int => Double](i)
//     case d: Double => d
//     case _ => throw Exception(s"Could not convert $v to Double")
//   }
// }

implicit val stdlib: Environment = Map(
  "+" -> Value(Function(
    Seq(Type.Integer, Type.Integer),
    {
      case Seq(a: Value, b: Value) =>
        Value(a.toScala[Int] + b.toScala[Int])
    }
  ))
  // "+" -> VarArgs(_.map(_.cast[Double]).sum),
  // "-" -> VarArgs {
  //   case Seq(a: Double) => -a
  //   case Seq(a: Double, b: Double) => a - b
  // },
  // // "-" -> Fn(Seq(doubleType, doubleType), {
  // //   case Seq(a: Double) => -a
  // //   case Seq(a: Double, b: Double) => a - b
  // // }),
  // "*" -> VarArgs(_.map(_.cast[Double]).product),
  // "/" -> ((a: Double, b: Double) => a/b),
  // "%" -> ((a: Double, b: Double) => a%b),
  // "=" -> VarArgs(args => args.forall(_ == args.head)),
  // "int" -> ((n: Value) => n match {
  //   case int:    Int    => int.toInt
  //   case double: Double => double.toInt
  //   case string: String => string.toInt
  // }),
  // "float" -> ((n: Value) => n match {
  //   case int:    Int    => int.toDouble
  //   case double: Double => double.toDouble
  //   case string: String => string.toDouble
  // }),
  // "Pi" -> math.Pi,
  // "cos" -> math.cos,
  // "sin" -> math.sin,
  // "select" -> VarArgs {
  //   case Seq(target, cases*) => cases.grouped(2).find(c => c.head == target).map(_.last).getOrElse(null)
  // },
  // "range" -> VarArgs {
  //   case Seq(to: Double) => 0 until to.toInt
  //   case Seq(from: Double, to: Double) => from.toInt until to.toInt
  // },
  // "map" -> ((seq: Seq[Value], fn: (Value => Value)) => seq.map(fn)),
  // "concat" -> VarArgs(_.map(_.cast[Seq[Value]]).flatten),
  // "rand" -> VarArgs {
  //   case Seq() => Random.nextDouble()
  //   case Seq(a: Double, b: Double) => Random.between(a, b)
  // },
  // "flatten" -> ((seq: Seq[Seq[Value]]) => seq.flatten),
  // "print" -> ((x: Any) => print(x)),
  // "println" -> ((x: Any) => println(x)),
)
