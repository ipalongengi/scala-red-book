package redbook

import scala.annotation.tailrec

object Chapter3 {
  // LIST
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    // Exercise 3.2
    // Removes the first element of a list
    def tail[A](ls: List[A]): List[A] = ls match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }

    // Exercise 3.3
    // Replaces the first element of a List with a different value
    def setHead[A](ls: List[A], newHead: A): List[A] = ls match {
      case Nil => Cons(newHead, Nil)
      case Cons(_, xs) => Cons(newHead, xs)
    }

    // Exercise 3.4
    // Removes the first n elements from a list
    @tailrec
    def drop[A](ls: List[A], n: Int): List[A] = ls match {
      case Nil => Nil
      case Cons(_, xs) if n > 0 => drop(xs, n - 1)
    }

    // Exercise 3.5
    // Removes elements from the List prefix as long as they match a predicate
    @tailrec
    def dropWhile[A](ls: List[A], f: A => Boolean): List[A] = ls match {
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case _ => ls
    }

    def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

    // Exercise 3.6
    // Returns a List consisting of all but the last element of a List
    def init[A](ls: List[A]): List[A] = ls match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, Cons(_, Nil)) => Cons(x, Nil)
      case Cons(x, xs) => Cons(x, init(xs))
    }

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    // Exercise 3.7
    // There is no way to short-circuit the execution of recursion when the 0.0 value is encountered in the list.
    // No matter what happens, fold will always traverse the whole List

    // Exercise 3.8
    // Applying foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) will return
    // the same original input list. In other word, the operation is idempotent
    // In other words, the relationship between foldRight and data constructors of List
    // is they produce the same result.

    // Exercise 3.9
    // Computes the length of the a list using foldRight
    def length[A](as: List[A]): Int = foldRight(as, 0)((_, length) => 1 + length)

    // Exercise 3.10
    @tailrec
    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    // Exercise 3.11
    def foldSum(as: List[Int]): Int = foldLeft(as, 0)(_ + _)
    def foldProduct(as: List[Double]): Double = foldLeft(as, 1.0)(_ * _)
    def foldLength[A](as: List[A]): Int = foldLeft(as, 0)((length, _) => length + 1)

    // Exercise 3.12
    def reverseList[A](as: List[A]): List[A] = foldLeft(as, Nil:List[A])((accList, cur) => Cons(cur, accList))

    // Exercise 3.13 (HARD)
    // Implements foldLeft in terms of foldRight, and vice-versa
//    def foldLeftFromFoldRight[A](as: List[A]): List[A] = ???
//    def foldRightFromFoldLeft[A](as: List[A]): List[A] = ???

    // Exercise 3.14
    // Implements append in terms of foldLeft or foldRight
    def appendWithFoldLeft[A](a1: List[A], a2: List[A]): List[A] = foldLeft(reverseList(a1), a2)((accList, cur) => Cons(cur, accList))
    def appendWithFoldRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _ ))

    // Exercise 3.15 (HARD)
    // Concatenates a list of lists into a single list
    def concatListOfLists[A](as: List[List[A]]): List[A] = foldRight(as, Nil:List[A])(append)

    // Exercise 3.16
    def addOne(as: List[Int]): List[Int] = as match {
      case Nil => Nil
      case Cons(x, xs) => Cons(x + 1, addOne(xs))
    }

    // Exercise 3.17
    def convertToString(as: List[Double]): List[String] = as match {
      case Nil => Nil
      case Cons(x, xs) => Cons(x.toString, convertToString(xs))
    }

    // Exercise 3.18
    def map[A,B](as: List[A])(f: A => B): List[B] = as match {
      case Nil => Nil
      case Cons(x, xs) => Cons(f(x), map(xs)(f))
    }

    // Exercise 3.19
    def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) Cons(x, filter(xs)(f)) else filter(xs)(f)
    }

    // Exercise 3.20
    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
      case Nil => Nil
      case Cons(x, xs) => append(f(x), flatMap(xs)(f))
    }

    // Exercise 3.21
    def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(x => if (f(x)) List(x) else Nil)

    // Exercise 3.22
    def addTwoLists(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
      case (Nil, Nil) => Nil
      case (Cons(_, _), Nil) => Nil
      case (Nil, Cons(_, _)) => Nil
      case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(x1 + x2, addTwoLists(xs1, xs2))
    }

    // Exercise 3.23
    def zip[A,B,C](l1: List[A], l2:List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
      case (Nil, Nil) => Nil
      case (Cons(_, _), Nil) => Nil
      case (Nil, Cons(_, _)) => Nil
      case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(f(x1, x2), zip(xs1, xs2)(f))
    }

    // Exercise 3.24 (HARD)
    // Checks whether a List contains another list as a subsequence
//    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???
  }

  // TREE
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    // Exercise 3.25
    // Counts the number of nodes (leaves and branches)
    def size[A](t: Tree[A]): Int = t match {
      case Branch(left, right) =>  1 + size(left) + size(right)
      case Leaf(_) => 1
    }

    // Exercise 3.26
    // Returns the maximum element in a Tree[Int]
    def maximum(t: Tree[Int]): Int = t match {
      case Leaf(x) => x
      case Branch(left, right) => Math.max(maximum(left), maximum(right))
    }

    // Exercise 3.27
    // Returns the *maximum* path length from the root of a tree
    def depth[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(left, right) => Math.max(1 + depth(left), 1 + depth(right))
    }

    // Exercise 3.28
    // Applies a function f to transform the Tree
    def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(x) => Leaf(f(x))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }
}