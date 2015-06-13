\mainmatter

#Introduction

> *Programming languages are not just technology, but what programmers think in. They're half technology and half religion.*

> Paul Graham - "Beating the Averages"

## Context

### Functional programming
It is well known that the modern computers are not becoming "faster" like they used to be, because frequency scaling, for now, hit the limit. They increase their potential productivity by adding cores.

This implies that to benefit most from this architecture, the programs have to be parallellized. But parallel programming is way harder than sequential programming, due to a lot of new challenges it brings.

Functional programming helps to get rid of some of these challenges, e.g. race conditions don't apply if you only use immutable variables and methods without side effects. The learning curve for functional programming is often steep, but the learning curve for parallel programming might be even steeper, and not at all intuitive.

The world is full of stateful objects. The machine you program itself is a stateful machine. Sure, functional programming has ways to model state, but there's a deep impedance mismatch between a stateless model being implemented on a stateful machine trying to emulate a stateful world.

However, there are several programming problems in the world that are easy to map to the FP model. Problems involving large data sets, multi-processing are easy to map to the FP model. In practice, however, problems involving large data sets and multi-processing also require maximum efficiency, and the FP languages in use today cannot match frameworks that take some of the best practices in FP programming and use them in imperative languages. For instance, you can see the MapReduce/Hadoop frameworks as implementing FP in C++ and Java with the aim of providing maximum performance in a distributed environment without paying the cognitive or interpretive overhead of a typical FP language.

### Java
Java is a general-purpose programming language that is concurrent, class-based, object-oriented, and specifically designed to have as few implementation dependencies as possible. Java code can run on all platforms that support Java without the need for recompilation.Java applications are typically compiled to bytecode that can run on any Java virtual machine (JVM) regardless of computer architecture. As of 2015, Java is one of the most popular programming languages in use [@TIOBEIndex2015] (see Figures \ref{lang-rank} and \ref{history-rank}). Java was originally developed by James Gosling at Sun Microsystems and released in 1995. The language derives much of its syntax from C and C++, but it has fewer low-level facilities than either of them [@JavaWiki2015].

The reference implementation Java compilers, virtual machines, and class libraries were open-sourced in May 2007 under the GNU General Public License.

![TIOBE Index for June 2015 [@TIOBEIndex2015] \label{lang-rank} ](figures/ranking.png)

![Positions of the top 10 programming languages of many years back. [@TIOBEIndex2015] \label{history-rank} ](figures/history_rank.pdf)

#### Java 8
Starting from Java 8, java supports aspects of functional programming. Two core concepts introduced in Java 8 are Lambda expressions and functional interfaces.

A Lambda expression is an anonymous function that can be declared with a comma separated list of the formal parameters enclosed in parentheses, an arrow token (->), and a body. Data types of the parameters can always be omitted, as can the parentheses if there is only one parameter. The body can consist of a single statement or a statement block.

Syntax:

```
	(arg1, arg2...) -> { body }

	(type1 arg1, type2 arg2...) -> { body }
```

Examples:

```
	(int x, int y) -> x + y

	() -> 42

	(String s) -> { System.out.println(s); }

	() -> { return 3.1415 };
```

In Java, the lambda expressions are represented as objects, and so they must be bound to a particular object type known as a functional interface. A functional interface is an interface that defines exactly one abstract method. An extremely valuable property of functional interfaces is that they can be instantiated using lambdas.

#### The Java Virtual Machine
A Java virtual machine (JVM) is an abstract computing machine defined by a specification. The specification formally describes what is required of a JVM implementation. Having a single specification ensures all implementations are interoperable. A JVM implementation is a software platform that meets the requirements of the JVM specification in a compliant and preferably performant manner [@JVMWiki2015].

One of the main goals of Java design is portability, and Java is indeed platform independent. That is achieved by compiling the Java language code to an intermediate representation called Java bytecode, instead of directly to architecture-specific machine code. Java bytecode instructions are analogous to machine code, but they are intended to be executed by a virtual machine written specifically for the host hardware. Moreover, Just-in-Time (JIT) compilers were introduced from an early stage that compile bytecodes to machine code during runtime. Thus a JVM is platform dependent, because it must convert Java bytecode into machine language which depends on the architecture and operating system being used. End users commonly use a Java Runtime Environment (JRE) installed on their own machine for standalone Java applications, or in a web browser for Java applets [@JavaWiki2015].

The Oracle Corporation, which owns the Java trademark, distributes the Java Virtual Machine implementation HotSpot together with an implementation of the Java Class Library under the name Java Runtime Environment (JRE).

#### JVM based Languages

### Scheme
Scheme is a dialect of the computer programming language Lisp. It follows a minimalist design philosophy that specifies a small standard core accompanied by powerful tools for meta-programming.

Scheme was created during the 1970s at the MIT AI Lab by Guy L. Steele and Gerald Jay Sussman. It was the first dialect of Lisp to choose lexical scope and the first to require implementations to perform tail-call optimisation. It was also one of the first programming languages to support first-class continuations.

Scheme is a general-purpose computer programming language. It is a high-level language, supporting operations on structured data such as strings, lists, and vectors, as well as operations on more traditional data such as numbers and characters. Scheme is a fairly simple language to learn, since it is based on a handful of syntactic forms and semantic concepts and since the interactive nature of most implementations encourages experimentation.

The storage required to hold the contents of an object is dynamically allocated as necessary and retained until no longer needed, then automatically deallocated, typically by a garbage collector that periodically recovers the storage used by inaccessible objects. Simple atomic values, such as small integers, characters, booleans, and the empty list, are typically represented as immediate values and thus incur no allocation or deallocation overhead.

Scheme programs share a common printed representation with Scheme data structures. As a result, any Scheme program has a natural and obvious internal representation as a Scheme object. For example, variables and syntactic keywords correspond to symbols, while structured syntactic forms correspond to lists. This representation is the basis for the syntactic extension facilities provided by Scheme for the definition of new syntactic forms in terms of existing syntactic forms and procedures. It also facilitates the implementation of interpreters, compilers, and other program transformation tools for Scheme directly in Scheme, as well as program transformation tools for other languages in Scheme.

In Scheme, a procedure definition may appear within another block or procedure, and the procedure may be invoked at any time thereafter, even if the enclosing block has completed its execution. To support lexical scoping, a procedure carries the lexical context (environment) along with its code.

Furthermore, Scheme procedures are not always named. Instead, procedures are first-class data objects like strings or numbers, and variables are bound to procedures in the same way they are bound to other objects.

The Scheme language is standardized in the Revised\textsuperscript{n} Report on the Algorithmic Language Scheme (RnRS).

#### Scheme basics

Scheme syntax is essential, it provides a minimal set of special forms: define, quote, lambda, cond, let/let*

`define` is used to define new names.

```
	(define x 10)
	(define square (lambda (x) (* x x)))
```

`quote` prevents the argument to be evaluated as an expression, returning it as literal data (symbols or lists).

```
	(quote hello)           => hello
	(quote (1 2 3))         => (1 2 3)

	; the tick-mark ' is syntactic sugar
	'(1 2 foo bar)          => (1 2 foo bar)
```

`lambda` is used to create anonymous functions.

```
	(lambda (x) (+ x 10)                    ; anonymous function
	(define plus10 (lambda (x) (+ x 10)))   ; named the function now
```

`cond` is a general conditional.

```
	(cond
	  ((eq? 'foo 'bar) 'hello)
	  ((= 10 20) 'goodbye)
	  (#t 'sorry))                  => sorry
```

`let` is used to declare/use temporary variables.

```
	(let ((x 10)
		  (y 20))
	  (+ x y))
```

Built-in types are integers, rationals, floats, characters, strings, booleans, symbols, lists, and vectors.
A set of built-in functions we can use on these types:

```
	;; arithmetic:  +, -, *, /
	;; relational: <, <=, >, >=, =
	(+ 1 2)                    => 3
	(= 1 2)                    => #f   ; use = for numbers
```

Equality and identity tests:

```
	(eq? 'hello 'goodbye)      => #f   ; eq? is an identity test
	(eq? 'hello 'hello)        => #t
	(eq? '(1 2) '(1 2))        => #f
	(define foo '(1 2))
	(define foo bar)
	(eq? foo bar)              => #t
	(equal? foo bar)           => #t   ; equality: they look the same
	(equal? foo '(1 2))        => #t
```

Being a dialect of Lisp, Scheme provides a set of built-in functions for List manipulation:  cons, car, and cdr.

```
	;; Three equivalent ways to create the list (1 2 3),
	;; calling it foo
	(define foo '(1 2 3))
	(define foo (cons 1 (cons 2 (cons 3 ()))))
	(define foo (list 1 2 3))

	;; list precessing
	(null? '(1 2))             => #f
	(null? ())                 => #t
	(car '(1 2))               => 1
	(cdr '(1 2))               => (2)
```

Iteration via recursion:

```
	;; Exponentiation function x^n
	(define expt
	  (lambda (x n)
        (if (= n 0)
		  1
	      (* x (expt x (- n 1))))))

	;; List length
	(define length
	  (lambda (lst)
        (if (null? lst)
          0
	      (+ 1 (length (cdr lst))))))
```

It is straightforward to create and use higher order functions. Indeed functions are first-class in Scheme, they can be passed as arguments to other functions:

```
    (define compose
      (lambda (f g x)
        (f (g x))))

    (compose even? (lambda (x) (- x 1)) 10)   => #f

    ;; takes a function and applies it to every element of a list
    (define (map f lst)
      (let loop ((newlst lst))
        (cond ((pair? newlst)
 	      (cons (f (car newlst)) (loop (cdr newlst))))
 	     ((null? newlst)
 	      '())
 	     (else
 	      (error "second argument is not a list:"  lst)))))

	(map even? '(1 2 3 4))        => (#f #t #f #t)
```

### Kawa
Kawa is a language framework written in Java that implements the programming language Scheme. It provides a set of Java classes useful for implementing dynamic languages, such as those in the Lisp family. Kawa is also an implementation of almost all of R7RS Scheme (First-class continuations being the major missing feature), and which compiles Scheme to the bytecode instructions of the Java Virtual Machine.

Kawa gives run-time performance a high priority. The language facilitates compiler analysis and optimisation,and most of the time the compiler knows which function is being called, so it can generate code to directly invoke a method. Kawa also tries to catch errors at compile time.

To aid with type inference and type checking, Kawa supports optional type specifiers, which are specified using two colons. For example:

```
	(define (add-int x::int y::int) :: String
		(String (+ x y)))
```

This defines a procedure add-int with two parameters: x and y are of type Java `int`; the return type is a `java.lang.String`.

The Kawa runtime start-up is much faster than other scripting languages based on the Java virtual machine (JVM). This allows Kawa to avoid using an interpreter. Each expression typed into the REPL is compiled on-the-fly to JVM bytecodes, which (if executed frequently) may be compiled to native code by the just-in-time (JIT) compiler.

\iffalse TODO add something \fi

### Continuations
The usual way to control the flow of execution of a computer program is via procedure calls and returns; a stack data structure is how high-level programming languages keep track of the point to which each active subroutine should return control when it finishes executing. However, to solve real-world problems, procedure call and primitive expressions are not enough. Thus most high-level programming languages also provide other control-flow primitives, like conditionals, loops, and exception handling.

Scheme also supports first-class continuations. A continuation is a Scheme function that embodies “the rest of the computation.” The continuation of any Scheme expression (one exists for each, waiting for its value) determines what is to be done with its value. This continuation is always present, in any language implementation, since the system is able to continue from each point of the computation. Scheme simply provides a mechanism for obtaining this continuation as a closure. The continuation, once obtained, can be used to continue, or restart, the computation from the point it was obtained, whether or not the computation has previously completed, i.e., whether or not the continuation has been used, explicitly or implicitly. This is useful for nonlocal exits in handling exceptions, or in the implementation of complex control structures such as coroutines or tasks.

Considering a computation such as `(* (+ 2 4) (+ 1 6))`, there are several continuations involved. The continuation for `(+ 2 4)` can be expressed in this way: take this value (6), keep it aside; now add one and six, take the result and multiply it with the value we had kept aside; then finish. The continuation for `(+ 1 6)` means: take this value, multiply it with the value (6) that was previously kept aside; then finish. Notice in particular how the result of `(+ 2 4)` is part of the continuation of `(+ 1 6)`, because it has been calculated and kept aside. Continuations are not static entities that can be determined at compile time: they are dynamic objects that are created and invoked during program execution.

Using the syntactic form call-with-current-continuation (usually abbreviated call/cc), a program can obtain its own continuation. This continuation is a Scheme closure that may be invoked at any time to continue the computation from the point of the call/cc . It may be invoked before or after the computation returns; it may be invoked more than one time.

The standard idiom for call/cc has an explicit lambda term as its argument:

```
	(call/cc (lambda (current-continuation)
	  body))
```

During the execution of the expression body, the variable current-continuation is bound to the current continuation. If invoked, current-continuation immediately returns from the call to call/cc, and call/cc returns whatever value was passed to current-continuation.

When applied to a function f, call/cc captures and aborts the entire continuation k, reinstate a copy of k, and applies f to k.

Consider a first example:

```
	(call/cc
	  (lambda (k)
		(k 42)))
```

This applies call/cc to the function `(lambda (k) (k 42))`; which is called with argument `k`, the current continuation. Being the body of the function `(k 42)`, the continuation is thrown the value 42. This makes the `call/cc` return the value 42. Hence, the entire expression evaluates to 42.

Now consider

```
	(call/cc
	  (lambda (k)
	    (+ (k 42) 100)))
```

In this case, the function throws the value 42 to the continuation, but there is another computation afterwards. that computation has no effect, because when a continuation is invoked with a value, the program reinstates the invoked continuation, and the continuation which was going to take a value `x` and perform `(+ x 100)` has been aborted. The result is still 42.

On the other hand, consider

```
	(call/cc
	  (lambda (k) 42))
```

Here, the function applied by `call/cc` does not make use of the current continuation. It performs a real return, with the value 42.

Actually, although a continuation can be called as a procedure, it is not a real function, which takes a value and returns another. An invoked continuation takes a value and does everything that follows to it, never returning a value to the caller.

As an other example, consider the following code:

```
	(display
		(call/cc (lambda (k)
              (display "This is executed.\n")
              (k "Value passed to the continuation.\n")
              (display "But not this.\n"))))
```

it will display:

```
	This is executed.
	Value passed to the continuation.
```

An interesting feature of first-class continuations is that the continuation may still be called even after the call to call/cc is finished. When applied to a value `v`, a continuation `k` aborts its entire execution context, reinstates `k` as the current entire continuation, and returns the value `v` to the continuations `k`, which is "waiting for a value” in order to perform some computation with it.

For example, the following causes an infinite loop that prints `goto start` forever:

```
	(let ((start #f))
      (if (not start)
        (call/cc (lambda (cc)
                   (set! start cc))))

      (display "goto start\n")
      (start))
```

#### Delimited Continuations

## This work

## Outline
outline
