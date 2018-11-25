// Alex Rose
//-------------------------------------------------------------------------

// EL4 Typechecker
//
// Usage: linux> scala Check <source file>
//
import collection.immutable.Map._
import EL4._

object Check {
  case class TypingException(string: String) extends RuntimeException

  type Env = Map[String,Type]

  val emptyEnv:Env = Map[String,Type]()

  def check(e:Expr,debug:Int = 0) = {

    def checkVar(env:Env,x:String): Type =
      env.getOrElse(x, throw TypingException("undefined variable:" + x))
  
    def checkE(env:Env,e:Expr): Type = e match {
      case Num(_)   => IntTy
      case Bool(_)  => BoolTy
      case Var(x)   => checkVar(env,x)
      case Add(l,r) => arithBinOp(l,r,env)
      case Sub(l,r) => arithBinOp(l,r,env)
      case Mul(l,r) => arithBinOp(l,r,env)
      case Div(l,r) => arithBinOp(l,r,env)
      case And(l,r) => boolBinOp(l,r,env)
      case Or(l,r)  => boolBinOp(l,r,env)
      case Le(l,r)  => (checkE(env,l),checkE(env,r)) match {
        case (IntTy,IntTy) => BoolTy
        case _ => throw TypingException("Non integer expression passed to Le")
      }
      case If(c,t,e) => checkE(env, c) match {
        case BoolTy => {
          val t1 : Type = checkE(env,t)
          val t2 : Type = checkE(env,e)
          if(t1 == t2)
            t1
          else
            throw TypingException("If true and false statements must evaluate to same Type")
        }
        case _ => throw TypingException("Non boolean If condition")
      }
      case Seq(e1,e2) => { checkE(env,e1); checkE(env,e2) }
      case Fun(p,t,b) => {
        val t1 : Type = checkE(env + (p -> t), b) 
        FunTy(t, t1)
      }
      case Apply(f,e) => (checkE(env, f)) match {
        case FunTy(tx, ty) => {
          val t1 : Type = checkE(env, e)
          if(t1 == tx)
            ty
          else
            throw TypingException("Second argument to Apply is of incorrect type")
        }
        case _ => throw TypingException("First argument of Apply needs to be of type FunTy")
      }

      case Let(x,t,d,b) => {
        val t1 : Type = checkE(env, d)
        if(t1 == t)
          checkE(env + (x -> t), b)
        else
          throw TypingException("In Let(x,t,d,b) d is not of Type t")
      }
      
      case LetRec(x,t,d,b) => t match {
        case FunTy(tx, ty) => {
          val t1 : Type = checkE(env + (x -> t), d)
          if(t1 == t)
            checkE(env + (x -> t), b)
          else
            throw TypingException("Problem with d in LetRec(x,t,d,b)")
        }
        case _ => throw TypingException("In LetRec(x,t,d,b) t must be of type FunTy(tx,ty)")
      }
    }

    def arithBinOp(l:Expr, r:Expr, env:Env) : Type = (checkE(env,l),checkE(env,r)) match {
      case (IntTy,IntTy) => IntTy
      case _ => throw TypingException("Non integer expression used in binary arithmetic operation")
    }

    def boolBinOp(l:Expr, r:Expr, env:Env) : Type = (checkE(env,l),checkE(env,r)) match {
      case (BoolTy,BoolTy) => BoolTy
      case _ => throw TypingException("Non boolean expression used in boolean operation")
    }
    
    
    checkE(emptyEnv,e)
    if (debug > 0) println("Checked")
  }

  def process(s:String, debug:Int = 0) = {
    try {
      val e: Expr = parse(s,debug)
      check(e,debug)
    } catch {
      case ex: TypingException => 
        { println("Typing error: " + ex.string); throw ex }
      case ex: ParseException => 
        { println("Parser Error:" + ex.string); throw ex }
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
