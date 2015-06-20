# A call/cc implementation for Kawa

> *"Do... or do not. There is no try."*
\begin{flushright}
The Empire Strikes Back (film, 1980)
\end{flushright}

## A demonstrative instance of the transformation in Java
As a first preliminary step, I ported the C# code in [@StackHack2005] to Java, to study the feasibility of the technique on the JVM. The code represents a single instance of the transformation for a simple fibonacci function, and implements some support functions and data structures. Given that the global transformation fragments the original source in many function calls, I produced four versions of the transformed code, to compare the performance of different type of calls:

1. The first one uses nested static classes to implement the continuation frames of the function to be run:

```java
    class fib_frame0 extends Frame {

        int x;

        public fib_frame0(int x) {
            this.x = x;
        }

        @Override
        public Object invoke(Object return_value)
                throws ContinuationException, Throwable {
            // call to the next fragment
            return fib_an0(x);
        }

    }

	public int fib_an(int x)
		  throws ContinuationException, Throwable {
        try {
            pause();
        } catch (ContinuationException sce) {
            sce.extend(new ContinuationFrame(new fib_frame0(x)));
            throw sce;
        }

        return fib_an0(x);
    }
```

2. the second version uses `MethodHandle`s, that were introduced in Java 7. A `MethodHandle` is a typed, directly executable reference to an underlying method, constructor or field:

```java
    static Object fib_frame0_invoke(Object x, Object continue_value)
            throws SaveContinuationException, Exception {
         return fib_an0 ((int) x);
    }

    static MethodHandle fib_frame0(int x)
            throws Exception {
		MethodType mt = MethodType.methodType(Object.class,
		                                      Object.class,
		                                      Object.class);
	    MethodHandle handle = lookup.findStatic(fib_mh.class,
	                                            "fib_frame0_invoke",
		                                        mt);

        return handle.bindTo(x);
    }

	public static int fib_an(int x)
		   throws SaveContinuationException, Exception {
        try {
            pause();
        } catch (SaveContinuationException sce) {
            sce.Extend(new ContinuationFrame(fib_frame0(x)));
            throw sce;
        }

        return fib_an0(x);
    }
```

3. the third using Java 8 lambdas, specified with the new Java syntax:

```java
    static Object fib_frame0_invoke(Object x, Object continue_value)
            throws SaveContinuationException, Exception {
         return fib_an0 ((int) x);
    }

    static Frame fib_frame0(int x)
            throws Exception {
        Frame f = (Object continue_value)
                    -> { return fib_frame0_invoke(x, continue_value);};

        return f;
	}

    public static int fib_an(int x)
            throws SaveContinuationException, Exception {
        try {
            pause();
        } catch (SaveContinuationException sce) {
            sce.Extend(new ContinuationFrame(fib_frame0(x)));
            throw sce;
        }

        return fib_an0(x);
    }

```

4. last version that generates lambdas explicitly using LambdaMetafactory, an API introduced in Java 8 to facilitate the creation of simple "function objects".

```java
    fib_frame0_factory = LambdaMetafactory
                    .metafactory(lookup,
                                 "invoke",
                                 invokedType,
                                 methodType,
                                 lookup.findStatic(fib_meta.class,
							                       "fib_frame0_invoke",
								                   implType),
                                 methodType)
                    .dynamicInvoker();

    static Object fib_frame0_invoke(Object x, Object continue_value)
            throws SaveContinuationException, Throwable {
         return fib_an0 ((int) x);
    }

    static Frame fib_frame0(int x)
            throws Throwable {
        return (Frame) fib_frame0_factory.invoke(x);
    }
```

I tested each type of method call with JMH, a benchmarking framework for the JVM. Figures \ref{calls-table, calls} show the results. The lambda case is quite fast, if compared with MethodHandles, but also the explicit use of LambdaMetafactory gives good results, provided that the call to LambdaMetafactory.metafactory is cached in a static field. However, the difference in performance between lambda calls and regular method calls is negligible. Thus is not worth to loose the compatibility with previous version of the JVM for such a small improvement.

![Performance comparison of different types of call in Java \label{calls-table}](figures/calls-table.pdf)

![Performance comparison of different types of call in Java \label{calls}](figures/calls.png)

### Exceptions performance in Java
The capture of a continuation, and in particular the stack coping mechanism, is driven by exception throwing and exception handling. Therefore, is crucial to understand how the installation of exception handlers and the construction of an Exception object impact the performance.

In Java, when throwing an exception, the most expensive operation is the construction of the stack trace, that is useful for debugging reasons. As well as we are not using exceptions with they original purpose, we can have rid of the stack trace construction and optimise the `Exception` object. It is sufficient to override the `fillInStackTrace` method of `Throwable`:

```java
    public static class FastException extends Exception {

        @Override
        public Throwable fillInStackTrace() {
            return this;
        }
    }
```

I performed a straightforward benchmark, comparing the time spent by a regular method call, a method call surrounded by an exception handler, a method call throwing a cached exception and a method call throwing a `FastException`.

```java
        // case 1
		t.method1(i);

        // case 2
		try {
			t.method2(i);
		} catch (Exception e) {
			// We will *never* get here
		}

        // case 3
		try {
			t.method3(i);
		} catch (Exception e) {
			// We will get here
		}

        // case 4
		try {
			t.method4(i);
		} catch (FastException e) {
			// We will get here
		}
```

The results from 10 million iterations are shown in the following table.

|                    | time (ms) |
|--------------------|-----------|
| regular            | 1225      |
| non cached         | 1240      |
| catched            | 35482     |
| catched, optimised | 1330      |

## Support code
The Java port of the support code was also optimised by using arrays instead of

## A brief overview of Kawa's compilation process

## A-Normalization in Kawa
In the actual Java code "return" operation is called "identity", while the "bind" operation is called "normalizeName" as in the Flanagan et al. paper. The ExpVisitor type matching mechanism replaces the role of the "match" in the paper, while the Context class replaces the "k" parameter. Lambdas are simulated with classes for backward compatibility with Java version 7 and lower.

Each visit[...]Exp method is called with two parameters, an expression and a context (the very first context is the identity function that returns its argument). If the visit method is called with a non-atomic expression a new context is created and the passed context is called only inside the new one. The new-context is then passed to "normalizeName"  that has two purposes:

1. to create a context, that generates a "let" expression to let-bind the expression next to come in the "visiting" process;
2. to call visit() on the passed expression, to continue the syntax tree traversing.

This chain will finish when a leaf (an atomic expression) is encountered in the tree, in this case the passed context is invoked (which in turn will invoke the previous context and so on). At this point the chain of context invocations starts to wrap each expression in a "let" binding, processing the expressions backward, and enclosing them step by step in nested "let" expressions. This backward traversing stops when the context called is the identity function.

When the expression to normalize is a conditional, "normalizeTerm" is used on each branch expression. Instead of creating a let binding for each branch, as they cannot be evaluated before the test outcome, "normalizeTerm" calls the visit method with the identity context, restarting the normalization in each branch.

## Code fragmentation in Kawa


### Creating lambda closures

## Code Instrumentation in Kawa

### Creating try-catch expressions

### Install top level handlers
