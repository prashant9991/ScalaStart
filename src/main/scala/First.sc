//1. last element of a list
def last(ls:List[Any]) = {
  ls.last
}
last(List(1, 2, 3, 4, 5, 6, 7))

//2.second last element

def secondLast(ls:List[Any]) = {
  val len = ls.length
  ls.apply(len-2)
}
secondLast(List(1,2,4))


//3.kth element
def kth(ls:List[Any],k:Int):Any = {
  ls.apply(k)
}
kth(List(1,2,4),2)

//4.number of elements
def length(ls:List[Any]) = {
  ls.length
}

length(List(1,2,4))

//5.reverse the list
def reverseList[A](ls: List[A]): List[A] = {
  ls.reverse
}
reverseList(List(1,2,3))

//6. Palindrom or not
def Palindrome[A](ls: List[A]) = {
  if (ls == ls.reverse) true else false
}
Palindrome(List(1,2,4))

//7. Flatten a nested list structure

def flatten(ls: List[Any]): List[Any] =
  {
    ls flatMap {
      case list: List[Any] => flatten(list)
      case element => List(element)
    }
  }
flatten(List(1,List(1,2,3)))

//8.Eliminate consecutive duplicates elements

def compress[A](ls: List[Any]): List[Any] =
  {
    ls match {
      case Nil => Nil
      case head :: tail => head :: compress(tail.dropWhile(_==head))
    }
  }
compress(List(1,2,2,2,3))

//9. Packing

def packing[A](ls: List[A]): List[List[A]] = {
  if (ls.isEmpty) List(List())
  else {
    val (packed, next) = ls span { _ == ls.head }
    if (next == Nil) List(packed)
    else packed :: packing(next)
  }
}

//10. encoding

def encode[A](ls: List[A]): List[(Int, A)] ={
  packing(ls) map { e => (e.length, e.head) }
}

encode(List(1,1,1,2,2))

//11. modified encoding
def encodeModified[A](ls: List[A]): List[Any] = {
  encode(ls) map { t => if (t._1 == 1) t._2 else t }
}

//12. decode
def decode(list:List[(Int, Any)]) = {
  list.flatMap {
    e => Range(0, e._1).map(iter => e._2)

  }
}

//decode(List((3,1), (2,2)))


//13. duplicate
def duplicate[Any](ls: List[Any]): List[Any] = {
  ls flatMap { e => List(e, e) }
}

//14 .split
def split(count:Int, list:List[Any]) = {
  list.splitAt(count)
}
//15. slice
def slice(head:Int, tail:Int, list:List[Any]) = {
  list.drop(head).take(tail-head)
}

//16. duplicateN
def duplicateN(n: Int, ls: List[Any]): List[Any] = {
  ls flatMap{ e => List.fill(n)(e) }
}
//duplicateN(2,List(1,2))

//17.rotate

def rotate(ncount:Int, ls:List[Any]) = {
  val rotated = if(ncount > 0)ls.splitAt(ncount) else ls.splitAt(ls.length+ncount)
  rotated._2 ++ rotated._1
}

//18. remove at
def removeAt(pos:Int, list:List[Any]) = {
  if(list.length == 0 || pos > list.length) (Nil, Nil)
  else{
    val element = list(pos)
    val splitList = list.splitAt(pos);
    val newList = splitList._1 ++ splitList._2.drop(1)
    (newList, element)
  }
}

//19 drop the element
def drop[Any](pos:Int, list:List[Any]) = {
  list.zipWithIndex.filterNot(entry => (entry._2+1)%pos == 0).map(entry => entry._1)
}

//20.encode direct

def encodeDirect[A](ls: List[Any]): List[(Int, Any)] = {
  if (ls.isEmpty) Nil
  else {
    val (packed, next) = ls span {_ == ls.head}
    (packed.length, packed.head) :: encodeDirect(next)
  }
}