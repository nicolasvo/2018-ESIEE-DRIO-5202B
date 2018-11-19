val test = List(1, 2, 4, 5, 3, 8)
test(1)

def lesser(xs: List[Int], p: Int): List[Int] =
  for(x<-xs; if x<=p) yield x

def greater(xs: List[Int], p: Int): List[Int] =
  for(x<-xs; if x>=p) yield x

lesser(test, 5)

greater(test, 4)

def quicksort(xs: List[Int]): List[Int] =
  xs match {
    case List() => List()
    case y::ys => quicksort(lesser(ys, y)) ++ List(y) ++ quicksort(greater(ys, y))
  }

quicksort(test)

def sum(xs: List[Int]): Int =
  xs match {
    case List() => 0
    case x::tail => x + sum(tail)
  }


sum(test)

def product(xs: List[Int]): Int =
  xs match {
    case List() => 1
    case x::tail => x * product(tail)
  }

product(test)

product(List())

def reverse(xs: List[Int]): List[Int] =
  xs match {
    case List() => xs
    case x::tail => reverse(tail) ++ List(x)
  }

reverse(test)

def frsum(xs: List[Int]): Int = xs.foldRight(0)(_ + _)
def flsum(xs: List[Int]): Int = xs.foldLeft(0)(_ + _)

def flreverse(xs: List[Int]): List[Int] = xs.foldLeft(List[Int]())((x, tail)=>tail::x)
def frreverse(xs: List[Int]): List[Int] = xs.foldRight(List[Int]())((x, tail)=>tail:::List(x))

flreverse(test)
frreverse(test)