// Alex Rose
//-------------------------------------------------------------------------

// EL1 Interpreter
//
// Usage: linux> scala Interp1 <source file>
//
import EL1._

object Interp1 {
  case class InterpException(string: String) extends RuntimeException

  type Store = collection.mutable.Map[String,Int]
  
  def interp(e:Expr, debug:Int = 0): Int = {
    
    val st: Store = collection.mutable.Map[String,Int]()

    def interpE(e:Expr): Int = { 
      if (debug > 1) { println("  expr = " + e); println("  store = " + st) } 
      
      e match { 
        case Num(n) => n 
        case Var(x) => st get x match {
            case Some(v) => v
            case None    => 0
          }
        case Add(l,r) => interpE(l) + interpE(r)
        case Sub(l,r) => interpE(l) - interpE(r)
        case Mul(l,r) => interpE(l) * interpE(r)
        case Div(l,r) => {
          if(interpE(r) == 0) throw InterpException("Cannot divide by 0!")
          interpE(l) / interpE(r)
        }
        case Rem(l,r) => {
          if(interpE(r) == 0) throw InterpException("Cannot divide by 0!")
          interpE(l) % interpE(r)
        }
        case Le(l,r) => if(interpE(l) <= interpE(r))1 else 0
        case Assgn(id,e) => {
          st(id) = interpE(e)
          interpE(e)
        }


        case While(c,b) => {
          while(interpE(c) != 0){
            interpE(b)
          }
          0
           
        }


        // ... add code ...

        case If(c,t,f) =>{ 
          if(interpE(c) != 0)
            interpE(t)
          else
            interpE(f)
        }

        // ... add code ...

        case Write(e) => {
          val v = interpE(e)
      	  println(v);
          v
        }
        case Seq(e1,e2) => {
          val v1 = interpE(e1)
          val v2 = interpE(e2) 
          v2
        }
        case Skip() => 0

        case For(x,e1,e2,e3) => {

          st(x) = interpE(e1) 
          var loop=1 

          //while(st(x) <= interpE(e2)){
          while(loop == 1){

            val v2 = interpE(e2)
            val vx = st(x)
            
            if(vx > v2)
              loop = 0
            else{
              interpE(e3)
            
              st(x) = st(x) + 1
            }
          }
          0
        }


      }
    }

    val v = interpE(e)
    if (debug > 0) println("Evaluates to: " + v)
    v
  } 
  
  def process(s:String, debug:Int = 0): Int = {
    try {
      val e: Expr = parse(s,debug)
      interp(e,debug)
    } catch {
      case ex: InterpException => 
        { println("Interp Error:" + ex.string) ; throw ex }
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
