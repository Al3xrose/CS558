// Alex Rose
//-------------------------------------------------------------------------

// EL1 Compiler
//
// Usage: linux> scala Comp1 <source file>
//
import EL1._
import Machine1._

object Comp1 {
  var nextLabel: Int = 0

  val plus = Plus
  val times = Times
  val divrem = Divrem
  val lessequ = Lessequ
  val pop = Pop
  val dup = Dup
  val swap = Swap
  val print = Print

  def comp(e:Expr): Program = e match {
    case Var(x) => Load(x)::Nil
    case Num(i) => Const(i)::Nil
    case Add(e1,e2) => comp(e1):::comp(e2):::plus::Nil
    case Sub(e1,e2) => comp(e1):::comp(e2):::Const(-1)::times::plus::Nil
    case Mul(e1,e2) => comp(e1):::comp(e2):::times::Nil
    case Div(e1,e2) => comp(e1):::comp(e2):::divrem::pop::Nil
    case Rem(e1,e2) => comp(e1):::comp(e2):::divrem::swap::pop::Nil
    case Le(e1,e2)  => comp(e1):::comp(e2):::lessequ::Nil
    case Assgn(x,e) => comp(e):::comp(e):::Store(x)::Nil
    case While(c,b) => {
      val startLabel = newLabel()
      val endLabel = newLabel()
      Label(startLabel)::comp(c):::Branchz(endLabel)::comp(b):::pop::Branch(startLabel)::Label(endLabel)::Const(0)::Nil
    }
    case If(c,t,f)  =>{
      val falseLabel = newLabel()
      val endLabel = newLabel()
      comp(c):::Branchz(falseLabel)::comp(t):::Branch(endLabel)::Label(falseLabel)::comp(f):::Label(endLabel)::Nil
    }
    case Write(e)   => {
      comp(e):::comp(e):::print::Nil
    }
    case Seq(e1,e2) => comp(e1):::pop::comp(e2):::Nil
    case Skip()     => Const(0)::Nil
    //case For(x,e1,e2,e3) => 
  }

  def newLabel() = {
    val next = nextLabel
    nextLabel = nextLabel + 1
    next
  }

  def compile(e:Expr) = {
    nextLabel = 0
    comp(e)
  }

  def process(s:String, debug:Int = 0): Int = {
    try {
      val e: Expr = parse(s,debug)
      val p: Program = compile(e)
      exec(p,debug)
    } catch {
      case ex: ExecException =>
        { println("Exec Error:" + ex.string) ; throw ex }
      case ex: ParseException =>
        { println("Parser Error:" + ex.string) ; throw ex }
    }
  }

  // Test driver
  import scala.io.Source
  def main(argv: Array[String]) = {
    val s = Source.fromFile(argv(0)).getLines.mkString("\n")
    val d = if (argv.length > 1) argv(1).toInt else 0
    process(s,d)
    ()
  }
}
//
