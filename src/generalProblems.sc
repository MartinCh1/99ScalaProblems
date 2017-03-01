//http://aperiodic.net/phil/scala/s-99/

object generalProblems {


//------------------------WORKING WITH LIST-------------------------------


var lost = List(4,8,15,16,23,42)                  //> lost  : List[Int] = List(4, 8, 15, 16, 23, 42)
//P01: Find the last element of a list
// last(List(1,1,2,3,5,8))
// res0: Int = 8
  def last(l: List[Int]): Int = {
  	def lastIter(l: List[Int], acc: Int): Int = {
	  	if (l isEmpty) acc
	  	else lastIter (l.tail, l.head)
	  }
	  lastIter(l.tail, l.head)
  }                                               //> last: (l: List[Int])Int
	last(lost)                                //> res0: Int = 42
 
 
//P02: Find the lust but one element of a list
// penultimate(List(1,1,2,3,5,8))
// res0: Int = 5
	def penultimate(l: List[Int]): Int ={
		def penultimateIter(l: List[Int], acc:Int): Int ={
			if (l.length == 1) acc
		  	else penultimateIter (l.tail, l.head)
		}
		penultimateIter(l.tail, l.head)
	}                                         //> penultimate: (l: List[Int])Int
	penultimate (List(4,8,15,16,23,42))       //> res1: Int = 23

//P03: Find the Kth element of a list
// nth(2, List(1,1,2,3,5,8))
// res0: Int = 2
	def nth(n: Int, l: List[Int]): Int = {
		def nthIter(n: Int, l: List[Int], acc: Int): Int ={
			if (l.length == n) acc
				else nthIter(n, l.tail, l.head)
		}
		nthIter(n,l.reverse,l.head)
	}                                         //> nth: (n: Int, l: List[Int])Int
	nth(3,List(4,8,15,16,23,42))              //> res2: Int = 16
 
//P04: Find the number of elements of a list
// length(List(1,1,2,3,5,8))
// res0: Int = 6
	def lengthR(l: List[Int]): Int = l match{
		case Nil => 0
		case (_::tail) => 1 + lengthR(tail)
	}                                         //> lengthR: (l: List[Int])Int
	lengthR(lost)                             //> res3: Int = 6

//P05: reverse a list
	def reverseR(l: List[Int]): List[Int] = l match{
		case Nil => Nil
		case h :: t => reverseR(t) ::: List(h)
	}                                         //> reverseR: (l: List[Int])List[Int]
 
 reverseR(lost)                                   //> res4: List[Int] = List(42, 23, 16, 15, 8, 4)
                                                
//P06: Find out whether a list is a palindrome
	def isPalindrome(l: List[Int]): Boolean = l == reverseR(l)
                                                  //> isPalindrome: (l: List[Int])Boolean
	isPalindrome(List(1,2,3,2,1))             //> res5: Boolean = true
                                                
//P07: Flatten a nested list resource
	def flatten(l: List[Any]): List[Any] = l flatMap{
		case ll: List[_] => flatten(ll)
		case e => List(e)
	}                                         //> flatten: (l: List[Any])List[Any]
	flatten(List(List(1, 1), 2, List(3, List(5, 8))))
                                                  //> res6: List[Any] = List(1, 1, 2, 3, 5, 8)
//P08: Eliminate consecutive duplicates of list elements
	def compress(l: List[Any]): List[Any] = l match{
		case Nil => Nil
		case h :: tail => h :: compress(tail.dropWhile(_==h))
	}                                         //> compress: (l: List[Any])List[Any]
	compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
                                                  //> res7: List[Any] = List('a, 'b, 'c, 'a, 'd, 'e)
//P09: Pack consecutive duplicates of list elements into sublist
	def pack(l: List[Any]): List[Any] = {
		if (l.isEmpty) List(List())
		else{
			val (packed, tail) = l span {_ == l.head}
			if (tail == Nil) List(packed)
			else packed :: pack(tail)
		}
	}                                         //> pack: (l: List[Any])List[Any]
	pack(List('a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e))
                                                  //> res8: List[Any] = List(List('a, 'a), List('b), List('c, 'c), List('a, 'a), 
                                                  //| List('d), List('e, 'e))
//P10: Run-length encoding of a list
	def encode(l: List[Any]): List[(Int,Any)] = l map {
		case ll: List[_] => (ll.length, ll.head)
	}                                         //> encode: (l: List[Any])List[(Int, Any)]
	
	encode(pack(List('a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e)))
                                                  //> res9: List[(Int, Any)] = List((2,'a), (1,'b), (2,'c), (2,'a), (1,'d), (2,'e
                                                  //| ))
//P11: Modified run-length encoding
	def encodeMod(l: List[Any]): Any = l map{
		case ll: List[_] => if (ll.length==1)
												ll.head
												else
												(ll.length, ll.head)
	}                                         //> encodeMod: (l: List[Any])Any
	encodeMod(pack(List('a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e)))
                                                  //> res10: Any = List((2,'a), 'b, (2,'c), (2,'a), 'd, (2,'e))

//P12: Decode a run-length encoded list
	def decode[A](ls: List[(Int, A)]): List[A] = ls flatMap { e => List.fill(e._1)(e._2) }
                                                  //> decode: [A](ls: List[(Int, A)])List[A]
	
	decode(encode(pack(List('a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e))))
                                                  //> res11: List[Any] = List('a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e)

//P13: Run-length encoding of a list (direct solution)
	def encodeDirect(l: List[Any]): List[(Int,Any)] = {
		if (l.isEmpty) Nil
		else {
		val (packed, tail) = l span {_ == l.head}
		(packed.length, packed.head) :: encodeDirect(tail)
		}
	}                                         //> encodeDirect: (l: List[Any])List[(Int, Any)]
	encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
                                                  //> res12: List[(Int, Any)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'
                                                  //| e))
//P14: Duplicate the elements of a list
	def duplicate(l: List[Any]): List[Any] = l flatMap { e => List(e ,e) }
                                                  //> duplicate: (l: List[Any])List[Any]

	duplicate(lost)                           //> res13: List[Any] = List(4, 4, 8, 8, 15, 15, 16, 16, 23, 23, 42, 42)
                                                  
//P15: Duplicate the elements of a list a given number of times
	def duplicateN(n: Int, l: List[Any]): List[Any] = l flatMap { List.fill(n)(_)	}
                                                  //> duplicateN: (n: Int, l: List[Any])List[Any]
	
	duplicateN(3, List('a, 'b, 'c))           //> res14: List[Any] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c)

//P16: Drop every Nth element from a list
	def drop(n: Int, ls: List[Any]): List[Any] = {
	  def dropR(c: Int, curList: List[Any], result: List[Any]): List[Any] = (c, curList) match {
	    case (_, Nil)       => result.reverse
	    case (1, _ :: tail) => dropR(n, tail, result)
	    case (_, h :: tail) => dropR(c - 1, tail, h :: result)
	  }
	  dropR(n, ls, Nil)
	}                                         //> drop: (n: Int, ls: List[Any])List[Any]
	
	drop(2,lost)                              //> res15: List[Any] = List(4, 15, 23)
                                               
//P17: Split a list into two parts
	def split(n: Int, ls: List[Any]): (List[Any], List[Any]) = (n,ls)  match{
	  case (_, Nil)	=> (Nil, Nil)
	  case (0, ls) => (Nil, ls)
	  case (n, h::t) => {
	  	val (pre, post) = split(n-1, t)
	  	(h :: pre, post)
		}
	}                                         //> split: (n: Int, ls: List[Any])(List[Any], List[Any])
	
	split(2,lost)                             //> res16: (List[Any], List[Any]) = (List(4, 8),List(15, 16, 23, 42))

//P18: Extract a slice from a list
	def sliceR(n: Int, m: Int, ls: List[Any]): (List[Any]) = (n,m,ls) match{
		case (_,_,Nil) => Nil
		case (_,e,_) if (e<=0) => Nil
		case (s,e,h::t) if (s<=0) => h::sliceR(0,e-1,t)
		case (s,e,h::t) => sliceR(s-1,e-1,t)
	}                                         //> sliceR: (n: Int, m: Int, ls: List[Any])List[Any]
	sliceR(3,5,lost)                          //> res17: List[Any] = List(16, 23)

//P19: Rotate a list N places to the left
	def rotate(n: Int, ls: List[Any]): List[Any] = (n,ls) match{
		case (_, Nil)	=> Nil:::Nil
	  case (0, ls) => Nil:::ls
	  case (n, h::t) => {
	  	val (pre, post) = split(n-1, t) //la funcion split no empieza contando en 0, es decir split(2,(1,2,3,4))=(1,2),(3,4)
	  	post::: h::pre
		}
	}                                         //> rotate: (n: Int, ls: List[Any])List[Any]
	
	rotate (2,lost)                           //> res18: List[Any] = List(15, 16, 23, 42, 4, 8)
	
//P20: Remove the Kth element from a list
	def	removeAt(n: Int, ls: List[Any]): (List[Any],Any) = (n,ls) match{
		case (_, Nil) => (Nil, Nil)
		case (0, h::t) => (t,h)
		case (n, h::t) => {
			val (tail,e) = removeAt(n-1,ls.tail)
			(ls.head::tail,e)
			}
	}                                         //> removeAt: (n: Int, ls: List[Any])(List[Any], Any)
	
	removeAt(2,lost)                          //> res19: (List[Any], Any) = (List(4, 8, 16, 23, 42),15)
	
//P21: Insert an element at a given position into a list
	def insertAt(e: Any, n: Int, ls: List[Any]):List[Any] = (e,n,ls) match{
		case (e, _,Nil) => List(e)
		case (e, 0, h::t) => e::h::t
		case (e, n, h::t) => {
			val (tail) = insertAt(e,n-1,ls.tail)
			(ls.head::tail)
		}
	}                                         //> insertAt: (e: Any, n: Int, ls: List[Any])List[Any]
	
	insertAt(2,3,lost)                        //> res20: List[Any] = List(4, 8, 15, 2, 16, 23, 42)
	
//P22: Create a list containing all integers within a given range
	def range(x: Int, y: Int): List[Any] = {
		if (x>y) Nil
		else x::range(x+1,y)
	}                                         //> range: (x: Int, y: Int)List[Any]
	
	range(4,9)                                //> res21: List[Any] = List(4, 5, 6, 7, 8, 9)
	
//P23: Extract a given number of randomly selected elements from a list
	def randomSelect(n: Int, ls: List[Any]): List[Any] = {
		if (n <= 0) Nil
		else {
			val (rest,e)= removeAt((new util.Random).nextInt(ls.length),ls)
			e :: randomSelect(n-1,rest)
			}
	}                                         //> randomSelect: (n: Int, ls: List[Any])List[Any]
	
	randomSelect(2,lost)                      //> res22: List[Any] = List(42, 16)
	
//P24: Lotto: Draw N different random numbers from the set 1..M
	def lotto(n: Int, m: Int): List[Any] = {
		randomSelect(n, List.range(1, m+1))
	}                                         //> lotto: (n: Int, m: Int)List[Any]
	
	lotto(6,6)                                //> res23: List[Any] = List(2, 4, 6, 5, 1, 3)
	
//P25: Generate a random permutation of the elements of a list
	def randomPermute(ls: List[Any]):List[Any] ={
		randomSelect (ls.length,ls)
	}                                         //> randomPermute: (ls: List[Any])List[Any]
	  
	randomPermute(lost)                       //> res24: List[Any] = List(8, 4, 16, 23, 42, 15)
	
//P26: Generate the combinatios of K distinct objects chosen from the elements of a list
	def flatMapSublists[A,B](ls: List[A])(f: (List[A])=>List[B]): List[B] = ls match {
		case Nil => Nil
		case sublist@(_::tail) => f(sublist) ::: flatMapSublists(tail)(f)
	}                                         //> flatMapSublists: [A, B](ls: List[A])(f: List[A] => List[B])List[B]
	
	def combinations[A](n: Int, ls: List[A]): List[List[A]] =
		if (n ==0) List(Nil)
		else flatMapSublists(ls) { sl => combinations(n-1, sl.tail) map {sl.head :: _}}
                                                  //> combinations: [A](n: Int, ls: List[A])List[List[A]]
	combinations(2, List('a, 'b, 'c))         //> res25: List[List[Symbol]] = List(List('a, 'b), List('a, 'c), List('b, 'c))
	
//P27: Group the elements of a set into a disjoint subsets

//P28: Sorting a list of list according to length of sublist


//------------------------ARITHMETIC-------------------------------


//P31: Determine whether a given integer number is prime
	def isPrime(n: Int): Boolean = {
		(for(k <- 2 until n-1) yield n % k) forall (x => x != 0)
	}                                         //> isPrime: (n: Int)Boolean
	 isPrime(4)                               //> res26: Boolean = false

//P32: Determine the greatest common divisor of two positive integer numbers
	def gcd(x: Int, y: Int): Int = (x,y) match{
		case (0,y) => y
		case (x,0) => x
		case (x,y) => gcd(y,x%y)
	}                                         //> gcd: (x: Int, y: Int)Int

gcd(270,192)                                      //> res27: Int = 6


	
	}
	
	