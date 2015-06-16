# Implementing First-Class Continuations on the JVM

## The stack manipulation dilemma
The use of virtual machines for the implementation of programming languages has become the norm in recent compiler developments. Unlike low-level languages, such as C, that permit access to the stack through use of pointer arithmetic, higher level languages, such as Java or C# do not provide instructions for installing and saving the run-time stack. Compiling Scheme, or any other language that uses first-class continuations to the JVM thus poses a challenging problem. At at first glance, the implementors must either give up implementing continuations or manage a heap-stored stack. The first choice limits the programmers of these languages, besides automatically making the Scheme implementation non standard-compliant. The second choice precludes many of the advantages that these machines supposedly offer. Indeed, the major problem with heap allocation of call frames and environments is the overhead associated with the use of a heap. This overhead includes the direct cost of allocating objects in the heap when building the call frames and environments, and of following references instead of increasing and decreasing a stack or frame pointer when accessing pieces of the frame or environment. The overhead also includes the indirect cost of garbage collection to manage stack frames and environments and the indirect cost of using significant amounts of memory. Furthermore, the use of the heap rather than a stack prevents the exploitation of commonly available hardware or microcode-supported stack push, pop and index instructions and the use of function call and return instructions.

## Generalized stack inspection for a JVM-based Scheme
This section shows how the generalized stack inspection technique described by Pettyjohn et al. can be adapted to be used in the JVM, and how it can be included in a Scheme compiler. We will see also how some issues leaved open by the original paper have been tackled.

### Trasformed fibonacci in java

#### Benchmarks

### A-Normalization
This is an adaptation and extension of the A-normalization algoritm described in the paper "The Essence of Compiling with Continuations"
by Flanagan et al. and in "A-Normalization: Why and How" by Matt Might
(http://matt.might.net/articles/a-normalization/).

The algoritm performs a monadic transformation combining three steps:

1. Monadic conversion:

```
   (+ 1                             (bind (if (>= x 0)
      (if (>= x 0)                            (f x)
          (f x)             -->               (return 0))
          0))                             (lambda (t) (+ 1 t)))
```

2. The result is interpreted in the Identity monad:

```
                (return a)  =>  a

   (bind a (lambda (x) b))  =>  (let ((x a)) b)


   (bind (if (>= x 0)               (let ((t (if (>= x 0)
             (f x)          -->                  (f x)
             (return 0))                         (return 0))))
          (lambda (t) (+ 1 t)))        (+ 1 t))
```

3. Nested let are flattened:

```
   (let ((x (let ((y a))               (let ((y a))
              b)))          -->          (let ((x b))
     c)                                    c))
```

In the actual Java code "return" operation is called "identity", while the "bind" operation is called "normalizeName" as in the Flanagan et al. paper. The ExpVisitor type matching mechanism replaces the role of the "match" in the paper, while the Context class replaces the "k" parameter. Lambdas are simulated with classes for backward compatibility with Java version 7 and lower.

Each visit[...]Exp method is called with two parameters, an expression and a context (the very first context is the identity function that returns its argument). If the visit method is called with a non-atomic expression a new context is created and the passed context is called only inside the new one. The new-context is then passed to "normalizeName"  that has two purposes:

1. to create a context, that generates a "let" expression to let-bind the expression next to come in the "visiting" process;
2. to call visit() on the passed expression, to continue the syntax tree traversing.

This chain will finish when a leaf (an atomic expression) is encountered in the tree, in this case the passed context is invoked (which in turn will invoke the previous context and so on). At this point the chain of context invocations starts to wrap each expression in a "let" binding, processing the expressions backward, and enclosing them step by step in nested "let" expressions. This backward traversing stops when the context called is the identity function.

When the expression to normalize is a conditional, "normalizeTerm" is used on each branch expression. Instead of creating a let binding for each branch, as they cannot be evaluated before the test outcome, "normalizeTerm" calls the visit method with the identity context, restarting the normalization in each branch.

### Code fragmentation
A visitor that fragments the code in a sequence of function calls and performs instrumentation of computation steps to capture and resume continuations.

This transformation works on code previously a-normalized (see {@link gnu.expr.ANormalize}). Each let-bind expression is enclosed in a lambda closure that accepts one argument. The argument is an other lambda closure that has in the body the call to the next code fragment. In this way the original source is rewrited as a sequence of function calls, each call representing a computation step.

Beside fragmentation, instrumentation is performed using exception handlers. A try-catch expression is created around each computation to capture a possible ContinuationException. The installed exception handler add a new Frame (An invokable object representing the next computation step) to the list of Frames included inside the Exception object, than rethrows the exception.

This implementation is inspired to the technique described by Pettyjohn, et al. in "Continuations from Generalized Stack Inspection" and in http://www.ccs.neu.edu/racket/pubs/stackhack4.html

An example of the entire transformation is showed below:

1. original source

```
    (define incr #f)

    (+ (call/cc
          (lambda (k)
              (set! incr k)
              0))
      1) ; => 1
```

2. after a-normalization

```
    (let ((v1 (lambda (k)
	        (let ((v0 (set! incr k)))
	          0))))
     (let ((v2 (call/cc v1)))
       (+ v2 1))))
```

3. after fragmentation

```
    ((lambda (incr_an1)
      (let ((v1 (lambda (k)
	           (let ((v0 (set! incr k)))
		     0))))
         (incr_an1 v1)))
     (lambda (v1)
       ((lambda (incr_an2)
          (let ((v2 (call/cc v1)))
	    (incr_an2 v2)))
        (lambda (v2)
          (+ v2 1)))))
```

4. after instrumentation
```
    ((lambda (incr_an1)
      (let ((v1 (lambda (k)
	           (let ((v0 (set! incr k)))
		     0))))
         (incr_an1 v1)))
     (lambda (v1)
       ((lambda (incr_an2)
          (let ((v2 (try-catch (call/cc v1)
		     (cex <ContinuationException>
		          (let ((f (lambda (continue-value)
					    (incr_an2 continue-value))))
			    (cex:extend (<ContinuationFrame> f))
			    (throw cex))))))
	    (incr_an2 v2)))
        (lambda (v2)
          (+ v2 1)))))
```

### Code Instrumentation

## Open problems to solve
