# Implementing First-Class Continuations on the JVM

> *"I don't care what anything was designed to do. I care about what it can do."*
\begin{flushright}
Gene Kranz, Apollo 13
\end{flushright}

## The stack manipulation dilemma
The use of virtual machines for the implementation of programming languages has become the norm in recent compiler developments. Unlike low-level languages, such as C, that permit access to the stack through use of pointer arithmetic, higher level languages, such as Java or C# do not provide instructions for installing and saving the run-time stack. Compiling Scheme, or any other language that uses first-class continuations to the JVM thus poses a challenging problem. At at first glance, the implementors must either give up implementing continuations or manage a heap-stored stack. The first choice limits the programmers of these languages, besides automatically making the Scheme implementation non standard-compliant. The second choice precludes many of the advantages that these machines supposedly offer. Indeed, the major problem with heap allocation of call frames and environments is the overhead associated with the use of a heap. This overhead includes the direct cost of allocating objects in the heap when building the call frames and environments, and of following references instead of increasing and decreasing a stack or frame pointer when accessing pieces of the frame or environment. The overhead also includes the indirect cost of garbage collection to manage stack frames and environments and the indirect cost of using significant amounts of memory. Furthermore, the use of the heap rather than a stack prevents the exploitation of commonly available hardware or microcode-supported stack push, pop and index instructions and the use of function call and return instructions.

## Generalised stack inspection for a JVM-based Scheme
This section shows how the generalised stack inspection technique described by Pettyjohn et al. can be adapted to be used in the JVM, and how it can be included in a Scheme compiler. We will see also how some issues leaved open by the original paper have been tackled.

### Transformed fibonacci in java
I translated the C# code in "stackhack" to Java. I wrote five versions of the code, the first using nested static classes to implement continuation frames of the function to be run (a simple recursive fibonacci function), the second using MethodHandles, the third using dynamicInvoker of CallSite to generate an invokedynamic instruction (this is available in java 7), the fourth using Java-8 lambdas, specified with the new Java syntax, and the last version that generates lambdas explicitly using LambdaMetafactory. The lambda case is very fast, if compared with MethodHandles, but also the explicit use of LambdaMetafactory gives good results, provided that the call to LambdaMetafactory.metafactory is cached in a static field.. In all the three versions I suppressed fillInStackTrace. I executed some benchmarks to understand which technique works better. I found out that using classes is much faster than using MethodHandles, while use invokedynamic doesn't give an advantage.

#### Benchmarks

![Performance comparison of different types of call in Java ](figures/calls-table.pdf)

![Performance comparison of different types of call in Java \label{calls} ](figures/calls.png)

### A-Normalization
This is an adaptation and extension of the A-normalization algoritm described in the paper "The Essence of Compiling with Continuations" by Flanagan et al. and in "A-Normalization: Why and How" by Matt Might
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


### Code fragmentation
A visitor that fragments the code in a sequence of function calls and performs instrumentation of computation steps to capture and resume continuations.

This transformation works on code previously a-normalized (see {@link gnu.expr.ANormalize}). Each let-bind expression is enclosed in a lambda closure that accepts one argument. The argument is an other lambda closure that has in the body the call to the next code fragment. In this way the original source is rewrited as a sequence of function calls, each call representing a computation step.

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

### Code Instrumentation
Beside fragmentation, instrumentation is performed using exception handlers. A try-catch expression is created around each computation to capture a possible ContinuationException. The installed exception handler add a new Frame (An invokable object representing the next computation step) to the list of Frames included inside the Exception object, than rethrows the exception.

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

## Open problems to solve
