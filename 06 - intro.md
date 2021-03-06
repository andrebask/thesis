\mainmatter

#Introduction

> *"Programming languages are not just technology, but what programmers think in. They're half technology and half religion."*
\begin{flushright}
Paul Graham, Beating the Averages
\end{flushright}

## Context

### Functional programming
It is well known that the modern computers are not improving their performance like in the past decades, because frequency scaling, for silicon, has reached a limit. For this reason, processors manufacturers increase the potential productivity of their products by adding cores [@TurnConcurrency2015].

![Intel CPU Trends [@TurnConcurrency2015] \label{cpu-freq} ](figures/cpu.png)

This implies that to benefit most from this architecture, programs have to be parallellized. But parallel programming is quite harder than sequential programming, due to several new challenges it brings. *Functional programming* (FP) helps to get rid of some of these challenges, and it has recently risen in importance because it is well suited for parallel, concurrent and event-driven (or "reactive") programming, thanks to the use of immutable variables and functions without side effects. The learning curve for functional programming is often steep, but parallel and concurrent programming with imperative languages is not intuitive and its learning curve might be even steeper.

Functional programming is often used in synergy with other programming paradigms, since the world is made of stateful objects, while FP uses a mainly stateless computation model. FP has ways to model state, but there is an essential mismatch in a stateless model trying to represent a stateful world.

However, there are several programming problems in the world that are easy to map to the FP model. Problems involving concurrency, parallelism, large data sets and multi-processing.

### Java
*Java* is a general-purpose programming language that is concurrent, class-based, object-oriented, and specifically designed to have as few implementation dependencies as possible. Java code can run on all platforms that support Java without the need for recompilation. Java applications are typically compiled to bytecode that can run on any *Java Virtual Machine* (JVM) regardless of computer architecture. As of 2015, Java is one of the most popular programming languages in use [@TIOBEIndex2015] (see Figures \ref{lang-rank} and \ref{history-rank}). Java was originally developed by James Gosling at Sun Microsystems and released in 1995. The language derives much of its syntax from C and C++, but it has fewer low-level facilities than either of them [@JavaWiki2015].

The reference implementation Java compilers, virtual machines, and class libraries were open-sourced in May 2007 under the GNU General Public License.

![TIOBE Index for June 2015 [@TIOBEIndex2015] \label{lang-rank} ](figures/ranking.png)

![Positions of the top 10 programming languages of many years back. [@TIOBEIndex2015] \label{history-rank} ](figures/history_rank.pdf)

#### Java 8
Starting from release 8, Java supports aspects of functional programming. Two core concepts introduced in Java 8 are *lambda expressions* and *functional interfaces* [@OracleLambda2015].

A lambda expression is an anonymous function that can be declared with a comma separated list of the formal parameters enclosed in parentheses, an arrow token (->), and a body. Data types of the parameters can always be omitted, as can the parentheses if there is only one parameter. The body can consist of a single statement or a statement block.

Syntax:

```java
	(arg1, arg2...) -> { body }

	(type1 arg1, type2 arg2...) -> { body }
```

Examples:

```java
	(int x, int y) -> x + y

	() -> 42

	(String s) -> { System.out.println(s); }

	() -> { return 2.7182 };
```

In Java, lambda expressions are represented as objects, and so they must be bound to a particular object type known as a functional interface. A functional interface is an interface that defines exactly one abstract method. An extremely valuable property of functional interfaces is that they can be instantiated using lambdas.

An example of a functional interface is `java.lang.Runnable`. It has only one method void `run()` declared. Before Java 8, anonymous inner classes were used to instantiate objects of functional interface. With Lambda expressions, this can be simplified.

Each lambda expression can be implicitly assigned to one functional interface. For example we can create `Runnable` interface’s reference from lambda expression like below:

```java
	Runnable r = () -> System.out.println("running");
```

This type of conversion is automatically handled by the compiler when we dont specify the functional interface. For example:

```java
	new Thread(
		() -> System.out.println("running")
	).start();
```

In above code, compiler automatically deduced that lambda expression can be casted to Runnable interface from `Thread` class’s constructor signature `public Thread(Runnable r) { }`.

Few examples of lambda expressions and their functional interface:

```java
	Consumer<Integer>  c = (int x) -> { System.out.println(x) };

	BiConsumer<Integer, String> b = (Integer x, String y)
		                              -> System.out.println(x + y);

	Predicate<String> p = (String s) -> { s == null };
```

With the addition of Lambda expressions to arrays operations, Java introduced a key concept into the language of *internal iteration*. Using that paradigm, the actual iteration over a collection on which a Lambda function is applied is now carried out by the core library itself [@WhyLambda2013]. An relevant possibility opened by this design pattern is to enable operations carried out on long arrays (such as sorting, filtering and mapping) to be carried out in parallel by the framework. For example:

```java
	List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5, 6);

	// old way
	for (int number : numbers) {
		System.out.println(number);
	}

	// new way
	numbers.forEach(value -> System.out.println(value));
```

In Java 8 it is also possible to reference both a static and an instance method using the new `::` operator:

```java
	numbers.forEach(System.out::println);
```

Passing a lambda expression to another function allows to pass not only values but also behaviours and this enables to project more generic, flexible and reusable API. For instance declaring the following method:

```java
	public void evaluate(List<integer> list,
		                 Predicate<integer> predicate) {
        for(Integer n: list)  {
            if(predicate.test(n)) {
                System.out.println(n + " ");
            }
        }
    }
```

we can use the `Predicate` functional interface to create a test and print the elements that pass the test:

```java
	System.out.println("Print all numbers:");
	evaluate(numbers, (n)->true);

	System.out.println("Print even numbers:");
	evaluate(numbers, (n)-> n%2 == 0 );

	System.out.println("Print odd numbers:");
	evaluate(numbers, (n)-> n%2 == 1 );

```

Java 8 brings to developers another interesting feature from functional programming: *Streams*, that is, lazy *evaluation*. Streams are a new abstraction that allows to process data in a declarative way:

```java
	System.out.println(
	    numbers.stream()
		    .filter(Lazy::isEven)
		    .map(Lazy::doubleIt)
		    .filter(Lazy::isGreaterThan5)
		    .findFirst()
	);
```

You can create a Stream from any Collection by invoking the `stream()` method on it. A Stream provides an interface to a sequenced set of values of a specific element type. However, streams don’t actually store elements; they are computed on demand. They consume from a data-providing source such as collections, arrays, or I/O resources and support common operations, such as filter, map, reduce, find, match, sorted. Furthermore, many stream operations return a stream themselves. This allows operations to be chained to form a larger pipeline, enabling also certain optimisations.

#### The Java Virtual Machine
A Java Virtual Machine (JVM) is an abstract computing machine defined by a specification. The specification formally describes what is required of a JVM implementation. Having a single specification ensures all implementations are interoperable. A JVM implementation is a software platform that meets the requirements of the JVM specification in a compliant and preferably performant manner [@JVMWiki2015].

One of the main goals of Java design is portability, and Java is indeed platform independent. That is achieved by compiling the Java language code to an intermediate representation called Java bytecode, instead of directly to architecture-specific machine code. Java bytecode instructions are analogous to machine code, but they are intended to be executed by a virtual machine written specifically for the host hardware. Moreover, Just-in-Time (JIT) compilers were introduced from an early stage that compile bytecodes to machine code during runtime. Thus a JVM is platform dependent, because it must convert Java bytecode into machine language which depends on the architecture and operating system being used. End users commonly use a Java Runtime Environment (JRE) installed on their own machine for standalone Java applications, or in a web browser for Java applets [@JavaWiki2015].

The Oracle Corporation, which owns the Java trademark, distributes the Java Virtual Machine implementation HotSpot together with an implementation of the Java Class Library under the name Java Runtime Environment (JRE).

#### JVM based Languages
The JVM is not only for Java. Several hundred JVM programming languages are available to be run on it. These languages ultimately compile to bytecode in class files, which the JVM can then execute.

Some JVM languages include more features than Java and aim to let developers write code in a more concise way. Features like collection literals, pattern matching, and a more sophisticated type inference were the motivation for languages such as Scala, Groovy, Xtend, Ceylon, Kotlin, and Fantom [@JVMLang2015].

Then there are existing languages that were ported to the JVM. Python, Erlang, Ruby, Scheme and Javascript, for instance, all have an implementation targeting the JVM (respectively Jython, Erjang, JRuby, Kawa and Rhino). Another popular language ported to the JVM is Clojure, a dialect of Lisp  with an emphasis on functional and concurrent programming [@JVMWiki2015].

Many less-known JVM languages implement new research ideas, are suited only for a specific domain, or are just experimental.

### Scheme
*Scheme* is a dialect of the computer programming language Lisp. It follows a minimalist design philosophy that specifies a small standard core accompanied by powerful tools for meta-programming.

Scheme was created during the 1970s at the MIT AI Lab by Guy L. Steele and Gerald Jay Sussman. It was the first dialect of Lisp to choose lexical scope and the first to require implementations to perform tail-call optimisation. It was also one of the first programming languages to support first-class continuations [@SchemeWiki2015].

Scheme is a general-purpose computer programming language. It is a high-level language, supporting operations on structured data such as strings, lists, and vectors, as well as operations on more traditional data such as numbers and characters. Scheme is a fairly simple language to learn, since it is based on a handful of syntactic forms and semantic concepts and since the interactive nature of most implementations encourages experimentation [@dybvig2009scheme].

The storage required to hold the contents of an object is dynamically allocated as necessary and retained until no longer needed, then automatically deallocated, typically by a garbage collector. Simple atomic values, such as small integers, characters, booleans, and the empty list, are represented as primitive types and thus incur no allocation or deallocation overhead [@dybvig2009scheme].

Scheme is *homoiconic*, i.e,  programs share a common representation with Scheme data structures. As a result, any Scheme program has an internal representation as a Scheme object. For example, variables and syntactic keywords correspond to symbols, while structured syntactic forms correspond to lists. This representation is the basis for the syntactic extension facilities provided by Scheme for the definition of new syntactic forms. It also facilitates the implementation of interpreters, compilers, and other program transformation tools [@dybvig2009scheme].

In Scheme, a procedure definition may appear within another block or procedure, and the procedure may be invoked at any time thereafter, even if the enclosing block has completed its execution. To support lexical scoping, a procedure carries the lexical context (environment) along with its code.

Furthermore
orover, Scheme provides anonymous procedures. Indeed procedures are first-class data objects like strings or numbers, and variables are bound to procedures in the same way they are bound to other objects.

The Scheme language is standardized in the Revised\textsuperscript{n} Report on the Algorithmic Language Scheme (RnRS), where the \textsuperscript{n} indicates the revision number. The last report is R7RS, released in 2013.

#### Scheme basics

Scheme syntax is essential, it provides a minimal set of special forms: define, quote, lambda, cond, let/let*

`define` is used to define new names.

```scheme
	(define x 10)
	(define square (lambda (x) (* x x)))
```

`quote` prevents the argument to be evaluated as an expression, returning it as literal data (symbols or lists).

```scheme
	(quote hi!)           => hi!
	(quote (1 2 3))         => (1 2 3)

	; the tick-mark ' is syntactic sugar
	'(1 2 foo bar)          => (1 2 foo bar)
```

`lambda` is used to create anonymous functions.

```scheme
	(lambda (x) (* x 10)                   ; anonymous function
	(define times10 (lambda (x) (* x 10))) ; named the function now
```

`cond` is a general conditional.

```scheme
	(cond
	  ((eq? 'foo 'bar) 'hello)
	  ((= 10 20) 'goodbye)
	  (else 'sorry))                  => sorry
```

`let` is used to declare/use temporary variables.

```scheme
	(let ((x 10)
		  (y 20))
	  (+ x y))
```

Built-in types are integers, rationals, floats, characters, strings, booleans, symbols, lists, and vectors.
A set of built-in functions we can use on these types:

```scheme
	;; arithmetic:  +, -, *, /
	;; relational: <, <=, >, >=, =
	(+ 1 2)                    => 3
	(= 1 2)                    => #f ; '=' is for numbers
```

Equality and identity tests:

```scheme
	(eq? 'hello 'goodbye)      => #f ; eq? is an identity test
	(eq? 'hello 'hello)        => #t
	(eq? '(1 2) '(1 2))        => #f
	(define foo '(1 2))
	(define bar foo)
	(eq? foo bar)              => #t
	(equal? foo bar)           => #t ; equality: they look the same
	(equal? foo '(1 2))        => #t
```

Being a dialect of Lisp, Scheme provides a set of built-in functions for List manipulation:  cons, car, and cdr.

```scheme
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

```scheme
	;; Exponentiation function x^n
	(define (expt x n
      (if (= n 0)
		  1
	      (* x (expt x (- n 1))))))

	;; List length
	(define (length lst
	  (if (null? lst)
          0
	      (+ 1 (length (cdr lst))))))
```

It is straightforward to create and use higher order functions. Indeed functions are first-class in Scheme, they can be passed as arguments to other functions:

```scheme
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

### Continuations
Computer programs usually control the flow of execution via procedure calls and returns; a stack of frames is how high-level programming languages keep track of the point to which each active subroutine should return control when it finishes executing. However, to solve real-world problems, procedure call and primitive expressions are not enough. Thus most high-level programming languages also provide other control-flow primitives, like conditionals, loops, and exception handling.

Scheme also supports *first-class continuations*. A continuation is a Scheme function that embodies “the rest of the computation”. The continuation of any Scheme expression determines what is to be done with its value. This continuation is always present, in any language implementation, since the system is able to continue from each point of the computation. Scheme provides a mechanism for capturing this continuation as a closure. The obtained continuation can be used to continue, or resume, the computation from the point it was captured, whether or not the computation has previously completed. This is useful for nonlocal exits in handling exceptions, or in the implementation of complex control structures such as coroutines or generators [@dybvig1987three].

Considering a computation such as `(* (+ 2 4) (+ 1 6))`, there are several continuations involved. The continuation for `(+ 2 4)` can be expressed in this way: take this value (6), keep it aside; now add one and six, take the result and multiply it with the value we had kept aside; then finish. The continuation for `(+ 1 6)` means: take this value, multiply it with the value (6) that was previously kept aside; then finish. Notice in particular how the result of `(+ 2 4)` is part of the continuation of `(+ 1 6)`, because it has been calculated and kept aside. Continuations are not static entities that can be determined at compile time: they are dynamic objects that are created and invoked during program execution.

Using the syntactic form `call-with-current-continuation` (usually abbreviated `call/cc`), a program can obtain its own continuation. This continuation is a Scheme closure that may be invoked at any time to continue the computation from the point of the `call/cc`. It may be invoked before or after the computation returns; it may be invoked more than one time [^1].

[^1]: For more explanation and examples on continuations see [@ContByExample2015; @WhyContCool2015; @PageCallcc2015].

The standard idiom for `call/cc` has an explicit lambda term as its argument:

```scheme
	(call/cc (lambda (current-continuation)
	  body))
```

During the execution of the expression body, the variable current-continuation is bound to the current continuation. If invoked, current-continuation immediately returns from the call to `call/cc`, and `call/cc` returns whatever value was passed to current-continuation.

When applied to a function `f`, `call/cc` captures and aborts the entire continuation `k`, reinstate a copy of `k`, and applies `f` to `k`.

Consider a first example:

```scheme
	(call/cc
	  (lambda (k)
		(k 42)))
```

This applies `call/cc` to the function `(lambda (k) (k 42))`, which is called with argument `k`, the current continuation. Being the body of the function `(k 42)`, the continuation is thrown the value 42. This makes the `call/cc` return the value 42. Hence, the entire expression evaluates to 42.

Now consider

```scheme
	(call/cc
	  (lambda (k)
	    (+ (k 42) 100)))
```

In this case, the function throws the value 42 to the continuation, but there is another computation afterwards. That computation has no effect, because when a continuation is invoked with a value, the program reinstates the invoked continuation, and the continuation which was going to take a value `x` and perform `(+ x 100)` has been aborted. The result is still 42.

On the other hand, consider

```scheme
	(call/cc
	  (lambda (k) 42))
```

Here, the function applied by `call/cc` does not make use of the current continuation. It performs a real return, with the value 42.

Actually, although a continuation can be called as a procedure, it is not a real function, which takes a value and returns another. An invoked continuation takes a value and does everything that follows to it, never returning a value to the caller.

As an other example, consider the following code:

```scheme
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

An interesting feature of first-class continuations is that the continuation may still be called even after the call to call/cc is finished. When applied to a value `v`, a continuation `k` aborts its entire execution context, reinstates `k` as the current entire continuation, and returns the value `v` to the continuations `k`, which is "waiting for a value” in order to perform some computation with it. In some Scheme implementations, the value passed to a continuation can be a void one.

For example, the following causes an infinite loop that prints `goto start` forever:

```scheme
	(let ((start #f))
      (if (not start)
        (call/cc (lambda (cc)
                   (set! start cc))))

      (display "goto start\n")
      (start))
```

#### Delimited Continuations
Continuations captured by `call/cc` is the whole continuation that includes all the future computation. In some cases, we want to manipulate only a part of computation. This is possible with a kind of continuations called *delimited* or *composable* continuations [@Asai2011].

A continuation is delimited when it produces an intermediate answer rather than the final outcome of the entire computation. In other words, a delimited continuation is a representation of the "rest of the computation" from the current computation up to a designated boundary. Unlike regular continuations, delimited continuations return a value, and thus may be reused and composed [@kiselyov2007delimited].

Various operators for delimited continuations have been proposed in the research literature, such as `prompt` and `control`, `shift` and `reset`, `cupto`, `fcontrol`, and others [@RacketContinuations2015]. In this introduction we will consider only the `shift` and `reset` operators.

The `reset` operator sets the limit for the continuation while the `shift` operator captures or reifies the current continuation up to the innermost enclosing `reset`. The `shift` operator passes the captured continuation to its body, which can invoke, return or ignore it. Whatever result that `shift` produces is provided to the innermost `reset`, discarding the continuation in between the `reset` and `shift`. The continuation, if invoked, effectively reinstates the entire computation up to the `reset`. When the computation is completed, the result is returned by the delimited continuation [@DelimitedWiki2015]. For example, consider the following snippet in Scheme:

```scheme
	(* 2 (reset (+ 1 (shift k (k 5)))))
```

The `reset` delimits the continuation that `shift` captures. When this code is executed, the use of `shift` will bind `k` to the continuation `(+ 1 [])` where `[]` represents the part of the computation that is to be filled with a value. This is exactly the code that surrounds the `shift` up to the `reset`. Since the body of `shift` immediately invokes the continuation, the previous expression is equivalent to the following:

```scheme
	(* 2 (+ 1 5))
```

Once the execution of the `shift`'s body is completed, the continuation is discarded, and execution restarts outside `reset`. For instance:

```scheme
	(reset (* 2 (shift k (k (k 4)))))
```

invokes `(k 4)` first, which produces 8 as result, and then `(k 8)`, which returns 16. At this point, the `shift` expression has terminated, and the rest of the `reset` expression is discarded. Therefore, the final result is 16.

### Kawa
*Kawa* is a language framework written in Java that implements an extended version of the programming language Scheme. It provides a set of Java classes useful for implementing dynamic languages, such as those in the Lisp family. Kawa is also an implementation of almost all of R7RS Scheme (First-class continuations being the major missing feature), and which compiles Scheme to the bytecode instructions of the JVM [@Kawa2015]. The author and project leader of Kawa is Per Bothner, who started its development in 1996.

Kawa gives run-time performance a high priority. The language facilitates compiler analysis and optimisation, and most of the time the compiler knows which function is being called, so it can generate code to directly invoke a method. Kawa also tries to catch errors at compile time.

To aid with type inference and type checking, Kawa supports optional type specifiers, which are specified using two colons. For example:

```scheme
	(define (add-int x::int y::int) :: String
		(String (+ x y)))
```

This defines a procedure add-int with two parameters: x and y are of type Java `int`; the return type is a `java.lang.String`.

The Kawa runtime start-up is quite fast for a language based on the JVM. This allows Kawa to avoid using an interpreter. Each expression typed into the REPL is compiled on-the-fly to JVM bytecodes, which may be compiled to native code by the just-in-time (JIT) compiler.

Kawa Scheme has several extensions for dealing with Java objects. It allows to call methods of Java objects/classes, create objects and implement classes and interfaces.

For example, the following is Kawa code for an instance of a anonymous class:
```scheme
	(object (<java.lang.Runnable>)
      ((run) <void>
	   (display "running!\n")))
```

Here a simple class definition:

```scheme
	(define-simple-class Person ()
	  (last ::String)
	  (first ::String)
	  ((*init* f l)
	   (set! first f)
	   (set! last l))
	  ((sayHello)
	   (display "Hello ")
	   (display (string-append first
                               " "
                               last
                               "!\n"))))

	(let ((p (Person "Alyssa" "P. Hacker")))
	  (p:sayHello)) ; => Hello Alyssa P. Hacker!
```

## Thesis Contributions
My main contribution is an implementation of `call/cc` in a Scheme compiler targeting the JVM. The only other Scheme implementations targeting the JVM are SISC, which is an heap based interpreter, and Bigloo, which is a compiler but does not support continuations in the JVM back-end. Scala implements a different type of control operator, `shift` and `reset`. Although Ruby has `callcc`, JRuby does not support it.

I address the problem of providing a control operator that copies the stack in an environment that prevents direct stack manipulation. Unlike other solutions proposed to implement continuations on the JVM, we perform a transformation on the syntax tree produced by Kawa, instead of a transformation at the bytecode level. This make our transformation independent of the JVM version.

I present a variant of generalised stack inspection, described by Pettyjohn et al., as an extension of the Kawa compiler. The transformation is global, thus has been developed as an optional compiler pass, to avoid adding overhead to programs that do not use continuations.

## Outline
The following chapters are organized as follows. Chapter 2 provides a survey of related work. It discusses common techniques for implementing `call/cc` present in literature. Then it also compares different approaches to implement first-class continuations on the JVM.

Chapter 3 presents the issues in delivering first-class continuations on the JVM. It describes the details of the code transformation technique employed to enable the capture and resume of first-class continuations.

Chapter 4 demonstrates the viability of the design by providing an implementation of the entire transformation.

Chapter 5 shows how the proposed implementation can be used to add debugging facilities to Kawa, and to implement new control flow constructs.

Chapter 6 provides a performance evaluation and discusses some issues related this approach. The advantages and limitations of this approach are also discussed in detail.

Finally, Chapter 7 summarizes the contributions of this thesis and discusses possible future work.
