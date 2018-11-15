// Alex Rose
//-------------------------------------------------------------------------

// EL3 Interpreter
//
// Usage: linux> scala Interp3 <source file>
//
import EL3._

object Interp3 {
  case class InterpException(string: String) extends RuntimeException

  var useHeap = false;
  var callByName = false;

  sealed abstract class Addr() {
    def +(offset:Int): Addr
  }
  case class HeapAddr(index:Int) extends Addr {
    def +(offset:Int) = HeapAddr(index+offset)
  }
  case class StackAddr(index:Int) extends Addr {
    def +(offset:Int) = StackAddr(index+offset)
  }

  sealed abstract class Value
  case class NumV(num:Int) extends Value
  case class ClosureV(x:String,b:Expr,env:Env) extends Value

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

  val emptyEnv: Env =  Map[String,Addr]() 

  def interp(p:Expr,debug:Int = 0): Int = {
    if (debug > 0) println("expr: " + p)

    val heap = new HeapStore()
    val stack = new StackStore()

    def get(a:Addr) = a match {
      case HeapAddr(i)  => heap.get(i)
      case StackAddr(i) => stack.get(i)
    }

    def set(a:Addr,v:Value) = a match {
      case HeapAddr(i)  => heap.set(i,v)
      case StackAddr(i) => stack.set(i,v)
    }

    def interpVar(env:Env,x:String): Addr =
      env.getOrElse(x, throw InterpException("undefined variable:" + x))

    def interpArithBinOp(env:Env,l: Expr, r:Expr) (op: (Int,Int) => Int) = {
      val lv = interpE(env,l)
      val rv = interpE(env,r)
      (lv,rv) match {
      case (NumV(ln),NumV(rn)) => NumV(op(ln, rn))
      case _ => throw InterpException("non-numeric argument to arithmetic operator")
      }   
    }

    def interpE(env:Env,e:Expr): Value = {
      if (debug > 1) {
        println("expr = "+ e)
        println("env = " + env)
        println("stack = " + stack)
        println("heap = " + heap)
      } 
      e match {
        case Num(n) => NumV(n)
        case Var(x) => get(interpVar(env,x))
        case Add(l,r) => interpArithBinOp(env,l,r) ((lv,rv) => lv + rv)
        case Sub(l,r) => interpArithBinOp(env,l,r) ((lv,rv) => lv - rv)  
        case Mul(l,r) => interpArithBinOp(env,l,r) ((lv,rv) => lv * rv)  
        case Div(l,r) => interpArithBinOp(env,l,r) ((lv,rv) => if (rv!=0) (lv/rv) else throw InterpException("divide by zero"))
        case Le(l,r)  => interpArithBinOp(env,l,r) ((lv,rv) => if (lv <= rv) 1 else 0)
        case If(c,t,e) => interpE(env,c) match {

          case NumV(0) => interpE(env, e)
          case _ => interpE(env, t)

        }

        case Seq(e1,e2) => {
          val v1 = interpE(env,e1)
          val v2 = interpE(env,e2)
          v2
        }
        case Skip() => NumV(0)

        case Let(x,b,e) => {
          val bVal = interpE(env, b)
          var a : Addr = null
          if(useHeap)
            a = heap.allocate(1)
          else
            a = stack.push()
          set(a, bVal)
          val ret = interpE(env + (x -> a), e)
          if(!useHeap)
            stack.pop()
          ret
        }
     
        case LetRec(f,b,e) =>  interpE(env, b) match {
          case ClosureV(x, b2, env) => {
            var a : Addr = null
            if(useHeap)
              a = heap.allocate(1)
            else
              a = stack.push()
            val bVal = interpE(env + (f -> a), b)
            set(a, bVal)
            val ret = interpE(env + (f -> a), e)
            if(!useHeap)
              stack.pop()
            ret
          }
          case _ => throw InterpException("Must pass fun to LetRec")
        } 

        case Fun(x,b) => {
          ClosureV(x, b, env)
        }

        case Apply(f,e) => interpE(env, f) match {
          case ClosureV(x, b, env2) => {
            val func = interpE(env, e)
            var a : Addr = null
            if(useHeap)
              a = heap.allocate(1)
            else
              a = stack.push()
            set(a, func)
            val ret = interpE(env2 + (x -> a), b)
            if(!useHeap)
              stack.pop()
            ret
          }
          case _ => throw InterpException("Must supply valid function to Apply")
        }
      }
    }

    // process the top-level expression
    val v = interpE(emptyEnv,p)
    if (debug > 0) println("Expression evaluates to: " + v)
    v match {
      case NumV(n) => n
      case _ => throw InterpException("main body returns non-integer")
    }
  }

  def process(s:String,heap:Boolean=false,cbn:Boolean=false,debug:Int=0): Int = {
    try {
      val p: Expr = parse(s,debug)
      useHeap = heap;
      callByName = cbn;
      interp(p,debug)
    } catch {
      case ex: InterpException => { println("Interp Error:" + ex.string) ; throw ex }
      case ex: ParseException => { println("Parser Error:" + ex.string) ; throw ex }
    }
  }

  // Test driver
  import scala.io.Source
  def main(argv: Array[String]) = {
    val s = Source.fromFile(argv(0)).getLines.mkString("\n")
    var heap = false
    var cbn = false
    var debug = 0
    for (arg <- argv) {
      if (arg == "heap") heap = true
      if (arg == "cbn") cbn = true
      if (arg == "1") debug = 1
      if (arg == "2") debug = 2
    }
    val v = process(s,heap,cbn,debug)
    println(v)
  }
}

//
