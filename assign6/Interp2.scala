// Alex Rose
//-------------------------------------------------------------------------

// EL2 Interpreter
//
// Usage: linux> scala Interp2 <source file>
//
import EL2._

object Interp2 {
  case class InterpException(string: String) extends RuntimeException

  sealed abstract class Addr() {
    def +(offset:Int): Addr
  }
  case class GlobalAddr(index:Int) extends Addr {
    def +(offset:Int) = GlobalAddr(index+offset)
  }
  case class HeapAddr(index:Int) extends Addr {
    def +(offset:Int) = HeapAddr(index+offset)
  }
  case class StackAddr(index:Int) extends Addr {
    def +(offset:Int) = StackAddr(index+offset)
  }

  sealed abstract class Value
  case class NumV(num:Int) extends Value
  case class PairV(a:Addr) extends Value

  type Index = Int

  class Store {
    case class UndefinedContents(string: String) extends RuntimeException
    private val contents = collection.mutable.Map[Index,Value]()
    def get(i:Index) = contents.getOrElse(i, throw UndefinedContents("" + i))
    def set(i:Index,v:Value) = contents += (i->v)
    override def toString: String = contents.toString
  }

  class HeapStore extends Store {
    private var nextFreeIndex:Index = 0
    def allocate(n:Int): Addr = {
      val i = nextFreeIndex
      nextFreeIndex += n
      HeapAddr(i)
    }
    // there is no mechanism for deallocation
    override def toString: String = "[next=" + nextFreeIndex + "] " + super.toString
  }

  class StackStore extends Store {
    private var stackPointer:Index = 0;
    def push(): Addr = {
      val i = stackPointer
      stackPointer += 1
      StackAddr(i)
    }
    def pop() = stackPointer -= 1
    override def toString: String = "[sp=" + stackPointer + "] " + super.toString
  }

  type Env = Map[String,Addr]

  def interp(p:Program, debug:Int = 0): Int = {
    
    val global = new Store()
    val heap = new HeapStore()
    val stack = new StackStore()

    def get(a:Addr) = a match {
      case GlobalAddr(i) => global.get(i)
      case HeapAddr(i)   => heap.get(i)
      case StackAddr(i)  => stack.get(i)
    }

    def set(a:Addr,v:Value) = a match {
      case GlobalAddr(i) => global.set(i,v)
      case HeapAddr(i)   => heap.set(i,v)
      case StackAddr(i)  => stack.set(i,v)
    }

    var genv : Env = Map[String,Addr]() // initial env, containing global vars

    def interpE(env:Env, e:Expr): Value = {
      if (debug > 1) {
        println("expr = "+ e)
        println("env = " + env)
        println("stack = " + stack)
        println("heap = " + heap)
      } 
      e match {
        case Num(n) => NumV(n)
        case Var(x) => {
          val a = env.getOrElse(x, throw InterpException("undefined variable:" + x))
          get(a)
        }
        case Add(l,r) => (interpE(env,l),interpE(env,r)) match {
          case (NumV(lv),NumV(rv)) => NumV(lv+rv)
          case _ => throw InterpException("non-numeric argument to Add")
        }
        case Sub(l,r) => (interpE(env,l),interpE(env,r)) match {
          case (NumV(lv),NumV(rv)) => NumV(lv-rv)
          case _ => throw InterpException("non-numeric argument to Sub")
        }

        case Mul(l,r) => (interpE(env,l),interpE(env,r)) match {
          case (NumV(lv),NumV(rv)) => NumV(lv*rv)
          case _ => throw InterpException("non-numeric argument to Mul")
        }

        case Le(l,r) => (interpE(env,l),interpE(env,r)) match {
          case (NumV(lv),NumV(rv)) => if(lv <= rv)NumV(1) else NumV(0)
          case _ => throw InterpException("non-numeric argument to Le")
        }

        case Eq(l,r) => (interpE(env,l),interpE(env,r)) match {
          case (NumV(lv),NumV(rv)) => if(lv == rv)NumV(1) else NumV(0)
          case (PairV(a1),PairV(a2)) => if(a1 == a2)NumV(1) else NumV(0)
          case _ => throw InterpException("equality comparison of different types")
        }

    //    case Deq(l,r) =>
          // if you choose to implement this expr, replace the following line
          // with actual code
     //     throw InterpException("Deq implementation is optional")

        case Assgn(x,e) => {
          val a = env.getOrElse(x, throw InterpException("undefined variable:" + x))
          val v = interpE(env, e) 
          set(a, v)
          v
        }
          // lookup x's address from env (error if not found, cf. Var(x)'s 
          // code); evaluate e, and set e's value to x; yield e's value as 
          // Assgn's value

          // ... add code ...

        case If(c,t,e) => (interpE(env,c)) match {
          case (NumV(v)) => { 
            if(v != 0) interpE(env, t)
            else interpE(env, e) 
          }
          case _ => throw InterpException("If condition must evaluate to integer value")
        }


        case Write(e) => {
          val v = interpE(env,e)
          def show(v:Value): String = v match {
            case NumV(i) => "" + i
            case PairV(a) => "(" + show(get(a)) + "." + show(get(a+1)) + ")"
          }
      	  println(show(v)); 
          v
        }
        case Seq(e1,e2) => {
          val v1 = interpE(env,e1)
          val v2 = interpE(env,e2)
          v2
        }
        case Skip() => NumV(0)
        
        case Let(x,e,b) => {
          val v = interpE(env, e)
          val a = stack.push()
          set(a, v)
          val v2 = interpE(env + (x -> a), b)
          stack.pop()
          v2
        }
          // evaluate e to get a value, and bind it to x; x's value needs 
          // to be stored on the stack, use push() to get a stack address; 
          // x's binding needs to be added to env for evaluating b (only);
          // x's value needs to be removed before returning - use pop()

          // ... add code ...

        case Pair(l,r) => {
          val lv = interpE(env,l)
          val rv = interpE(env,r)
          val heapAddr : Addr = heap.allocate(2)
          set(heapAddr, lv)
          set(heapAddr+1, rv)
          PairV(heapAddr)
        }
        
        case IsPair(e) => interpE(env, e) match {
          case PairV(a) => NumV(1) 
          case _ => NumV(0)
        }

        case Fst(e) => interpE(env,e) match {
          case PairV(a) => get(a)
          case _ => throw InterpException("non-pair argument to fst")
        }

        case Snd(e) => interpE(env,e) match {
          case PairV(a) => get(a+1)
          case _ => throw InterpException("non-pair argument to snd")
        }

        case SetFst(p,e) => interpE(env,p) match {
          case PairV(a) => {
            val v = interpE(env, e)
            set(a, v)
            PairV(a)
          }
          case _ => throw InterpException("non-pair argument to SetFst")
        }

        case SetSnd(p,e) => interpE(env,p) match {
          case PairV(a) => {
            val v = interpE(env, e)
            set(a+1, v)
            PairV(a)
          }
          case _ => throw InterpException("non-pair argument to SetSnd")
        }
      }
    }

    // process the global definitions
    var a = GlobalAddr(0)
    for (gdef <- p.gdefs) {
      val v = interpE(genv,gdef.d)
      genv = genv + (gdef.id -> a)
      set(a,v)
      if (debug > 0) println("Global definition:" + gdef.id + " evaluates to: " + v)
      a += 1
    }

    // process the main body expression
    val v = interpE(genv,p.body)
    if (debug > 0) println("Body evaluates to: " + v)
    v match {
      case NumV(n) => n
      case _ => throw InterpException("main body returns non-integer")
    }
  } 
  
  def process(s:String, debug:Int = 0): Int = {
    try {
      val p: Program = parse(s,debug)
      interp(p,debug)
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
