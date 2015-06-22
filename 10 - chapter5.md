# A call/cc implementation for Kawa

> *"Do... or do not. There is no try."*
\begin{flushright}
The Empire Strikes Back (film, 1980)
\end{flushright}

## An instance of the transformation in Java
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
For capturing and resuming continuations we need a framework to support all the required operations, such as construct an object that models the continuation, and turn a continuation object back into an actual continuation.

```java
    public static class ContinuationFrame {

        Procedure computation;
        ArrayList<ContinuationFrame> continuation;

        public ContinuationFrame(Procedure frame) {
            computation = frame;
        }
    }
```

The basic blocks of a continuation are its `ContinuationFrame`s. A `ContinuationFrame` (for brevity, a frame) is a simple data structure which contains a single computation (a `Procedure` that takes one argument), and a list of `ContinuationFrame`s. The list is used by the next capture of a continuation. All the frames needed to assemble a continuation are collected using a `ContinuationException`. This class extends `FastException` and stores the list of frames which is extended step by step by the chain of throws. It contains also a list of frames that have been already reloaded by a previously call to `call/cc`. When the exception reaches the top level exception handler, this calls the method `toContinuation` that builds a new `Continuation` object using the two lists.

```java
    public static class ContinuationException extends FastException {

        ArrayList<ContinuationFrame>
        newCapturedFrames = new ArrayList<ContinuationFrame>();

        ArrayList<ContinuationFrame> reloadedFrames;

        public void extend(ContinuationFrame extension) {
            newCapturedFrames.add(extension);
        }

        public void append(ArrayList<ContinuationFrame> oldFrames) {
            reloadedFrames = oldFrames;
        }

        public Continuation toContinuation() throws Exception {
            return new Continuation(newCapturedFrames,
                                    reloadedFrames);
        }
    }
```

The `Continuation` constructor takes the two lists and assembles the continuation.

```java
public class Continuation extends Procedure0or1 {

    ArrayList<ContinuationFrame> frames;

    public Continuation(ArrayList<ContinuationFrame> newFrames,
                        ArrayList<ContinuationFrame> oldFrames) {

        frames = (oldFrames != null)
                 ? new ArrayList<ContinuationFrame>(oldFrames)
		         : new ArrayList<ContinuationFrame>();

        for(int i = newFrames.size()-1; i >= 0; i--) {
            ContinuationFrame newFrame = newFrames.get(i);
            if (newFrame.continuation != null) {
                throw new Error("Continuation should be empty here");
            }
            newFrame.continuation
			    = new ArrayList<ContinuationFrame>(frames);
            frames.add(newFrame);
        }
    }
```

When a continuation is invoked, we actually call the `apply` method of `Continuation`. Here we create a new procedure which, when called, resumes the continuation. We wrap the procedure in an exception so that, throwing it, we unload the current continuation. The top level handler will receive this exception and will use it to resume the invoked continuation.

```java
    public Object apply0() throws Throwable {
        return apply1(Values.empty);
    }

    public Object apply1(final Object val) throws Throwable {

        Procedure t = new Procedure1() {

            public Object apply1(Object ignored) throws Throwable {
                return reloadFrames(frames.size()-2, val);
            }
        };

        throw new ExitException(t);
    }
```

The `Continuation` object also contains the method to resume the continuation. `reloadFrames` iterates over the list of frames in reverse order to re-establish the saved continuation reconstructing the stack. The topmost frame gets the restart value passed into it.

```java
    Object resume(final Object restartValue) throws Throwable {
        return reloadFrames(frames.size()-1, restartValue);
    }

    Object reloadFrames(int endIndex, Object restartValue)
    throws Throwable {
        Object continueValue = restartValue;
        for (int i = endIndex; i >= 0; i -= 1) {
            ContinuationFrame frame = frames.get(i);
            try {
                continueValue = frame.computation.apply1(continueValue);
            } catch (ContinuationException sce) {
                sce.append(frame.continuation);
                throw sce;
            }
        }
        return continueValue;
    }

}
```

`TopLevelHandler` deals with running top level calls in an exception handler that catches instances of `ContinuationException`, thrown by `call/cc`, and `ExitException`, thrown by a continuation invocation. In the first case it creates a continuation object and resumes the execution of the function passed to `call/cc`. In the second case it calls the function enclosed in the `ExitException`, which reinstates the continuation.

```java
public class TopLevelHandler extends Procedure1 {

    public Object apply1(Object arg1) throws Throwable {
        return runInTopLevelHandler((Procedure) arg1);
    }

    public void compile(...) { ... }

    public static Object runInTopLevelHandler(Procedure initialFrame)
    throws Throwable {
        while (true) {
            try {
                return invokeFrame(initialFrame);
            } catch (ExitException rce) {
                initialFrame = rce.thunk;
            }
        }
    }

    private static Object invokeFrame(final Procedure initialFrame)
    throws Throwable {
        try {
            return initialFrame.apply1(null);
        } catch (ContinuationException sce) {
            final Continuation k = sce.toContinuation();

            Procedure f = new Procedure1() {

                public Object apply1(Object arg) throws Throwable {
                    return k.resume(k);
                }
            };

            throw new ExitException(f);
        }
    }
}
```

The `CallCC` procedure implements `call/cc`. It throws a new `ContinuationException`, saving in it the `call/cc` argument (a Procedure).

```java
public class CallCC extends Procedure1 {

    public Object apply1(Object arg1) throws Throwable {
        return call_cc((Procedure) arg1);
    }

    public void compile(...) { ... }

    public static Object call_cc(final Procedure receiver)
    throws ContinuationException {
        try {
            throw new ContinuationException();
        } catch (ContinuationException sce) {
            sce.extend(new ContinuationFrame(receiver));
            throw sce;
        }
    }
}
```

A significant variation with respect to the implementation proposed by Pettyjohn et al. is that the function that resumes the stack frames is implemented using iteration instead of recursion. This avoids using to much stack, as the JVM, differently from the C# MSIL, does not support tail call optimisation. Another difference is in the representation of the list of frames. Instead of using a linked list adding elements at the beginning, I used a Java ArrayList, adding elements at the end of the list. This allows to avoid reversing a list at every capture, and saves an object allocation at each list extension.

## A brief overview of Kawa's compilation process
In Kawa there are mainly four compilation stages:

1. Syntactic analysis - the first compilation stage reads the source input. The result is one or more Scheme forms (S-expressions), represented as lists.

2. Semantic analysis - the main source form is rewritten into a set of nested `Expression` objects, which represents Kawa's abstract syntax tree. For instance, a `QuoteExp` represents a literal, or a quoted form, a `ReferenceExp` is a reference to a named variable, an `ApplyExp` is an application of a procedure func to an argument list args and a `LetExp` is used for let binding forms. The Scheme primitive syntax lambda is translated into a `LambdaExp`. Other sub-classes of `Expression` are `IfExp`, used for conditional expressions, `BeginExp`, used for compound expressions and `SetExp`, used for assignments. The top-level `Expression` object is a `ModuleExp` and can be considered the root of the AST. This stage also handles macro expansion and lexical name binding.

3. Optimisation - an intermediate pass performs type-inference and various optimisation, such as constant folding, dead code elimination, function inlining.

4. Code generation - the `ModuleExp` object is translated into one or more byte-coded classes. This is done by invoking the compile method recursively on the Expressions, which generates JVM instructions using the bytecode package, writing out the resulting class files.

5. Loading - if the code is compiled and then immediately executed, the compiled code can be immediately turned into Java classes using the Java `ClassLoader` feature. Then the bytecode can be loaded into the Kawa run-time.


## A-Normalization in Kawa

```java
	[...]
    ANormalize.aNormalize(mexp, this); // <-- A-normalization
    FragmentAndInstrument.fragmentCode(mexp, this);
    InlineCalls.inlineCalls(mexp, this);
    ChainLambdas.chainLambdas(mexp, this);
    FindTailCalls.findTailCalls(mexp, this);
	[...]
```

```java
    public static void aNormalize(Expression exp, Compilation comp) {
        ANormalize visitor = new ANormalize();
        visitor.setContext(comp);

        // Starts the normalization of expression exp. It has the effect of
        // monadic conversion plus interpreting it in the Identity monad.
        visitor.visit(exp, identity);
    }
```

```java
    protected Expression visitModuleExp(ModuleExp exp, Context context) {
        if (exp.body instanceof ApplyExp
            && ((ApplyExp)exp.body).isAppendValues()) {
            ApplyExp body = ((ApplyExp)exp.body);
            for (int i = 0; i < body.args.length; i++) {
              body.args[i] = visit(body.args[i], context);
            }
            return exp;
        }

        return visitExpression(exp, context);
    }

```

```java

```

```java

```
## Code fragmentation in Kawa

```java
	[...]
    ANormalize.aNormalize(mexp, this);
    FragmentAndInstrument.fragmentCode(mexp, this); // <-- fragmentation
	                                                //    and instrumentation
    InlineCalls.inlineCalls(mexp, this);
    ChainLambdas.chainLambdas(mexp, this);
    FindTailCalls.findTailCalls(mexp, this);
	[...]
```

### Creating lambda closures

## Code Instrumentation in Kawa

### Install top level handlers

### Creating try-catch expressions

## in higher order functions
