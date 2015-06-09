# State of the art

## Stack-based implementation techniques for first-class continuations
The most common approach to implement first-class continuations is to use a stack-based execution architecture and to reify the current continuation by making a copy of the stack, which is reinstated when the continuation is invoked. This is the approach taken by many language implementations that are in direct control of the runtime system. This section describes the most used implementation strategies for first class continuations.

### The garbage-collection strategy
The simplest strategy for ensuring that continuations have unlimited extent is to allocate them in the heap and to rely on garbage collection to recover their storage. This is called the gc strategy. The gc strategy is not a zero-overhead strategy and it is optimised for programs in which every continuation frame is captured. Few real programs capture all continuations, however, so the gc strategy may not perform as well as a zero-overhead strategy. The most important indirect cost of the gc strategy is that the compiler must allocate a separate continuation frame for each non-tail call, unless the compiler can prove that the continuation will not be captured during the non-tail call. The gc strategy also suffers more cache misses than the other strategies described in this section [@Clinger1999].

### The spaghetti strategy
This is a variation of the gc strategy. The spaghetti stack is in effect a separate heap in which storage is reclaimed by reference counting rather than garbage collection. Though complex, the spaghetti stack was at one time more efficient than using a gc strategy with a non-generational garbage collector, because the spaghetti stack’s storage management is optimised to support procedure call, return, and a host of related operations. When all frames have dynamic extent, the spaghetti stack behaves as a conventional stack. When a fast garbage collector is available, the spaghetti strategy is probably slower than the gc strategy. Moreover captures and throws require updating the reference counts, thus appears that the gc strategy should always perform better than the spaghetti strategy [@Clinger1999].

### The heap strategy
In the heap strategy, a one-bit reference count in each frame indicates whether the frame has been captured. Continuation frames are allocated in a garbage-collected heap, as in the gc strategy, but a free list of uncaptured frames is also used. When a frame is needed by a procedure call, it is taken from the free list unless the free list is empty. If the free list is empty, then the frame is allocated from the heap. When a frame is returned through, it is linked onto the free list if its reference count indicates that it has not been captured. Otherwise it is left for the garbage collector to reclaim. The heap strategy is not a zero-overhead strategy and it is most practical if all continuation frames are the same size; otherwise multiple free lists may be required. This is an indirect cost of the heap strategy. Another indirect cost is that, like the gc strategy, the heap strategy makes it difficult to reuse a continuation frame for multiple non-tail calls [@Clinger1999].

### The stack strategy
In the stack strategy, the active continuation is represented as a contiguous stack in an area of storage called the stack cache. Non-tail calls push continuation frames onto this stack cache, and returns pop frames from the stack cache, just as in an ordinary stack-based implementation. When a continuation is captured, however, a copy of the entire stack cache is made and stored in the heap. When a continuation is thrown to, the stack cache is cleared and the continuation is copied back into the stack cache. A first-class continuation thus resides in the heap, but is cached in the stack cache whenever it is the active continuation. The stack strategy is a zero-overhead strategy. Capturing, recapturing, and throwing to a continuation take time proportional to the size of the continuation. An indirect cost of the stack strategy is introduced by the fact that it repeatedly copies the same continuation from the stack cache to the heap. This can increase the asymptotic storage space required. The stack strategy prevents a compiler from allocating storage for mutable variables within a continuation frame, because there are other copies of it. Mutable variables must generally be allocated in registers or in the heap, that is another indirect cost of the stack strategy [@Clinger1999].

### The chunked-stack strategy
By maintaining a small bound on the size of the stack cache, and copying portions of the stack cache into the heap or back again as the stack-cache overflows and underflows, the chunked-stack strategy reduces the worst-case latency of captures and throws. This strategy works well with generational garbage collection because limiting the size of the stack cache limits the size of the root set that the garbage collector must scan on each garbage collection. The portion of the continuation that resides in the heap will be scanned only when its generation is collected. The chunked-stack strategy is a zero-overhead strategy, because the cost of stack-cache overflows and underflows is usually negligible. On the other hand, the chunked-stack strategy requires a stack cache that is large enough to avoid stack-cache overflows and underflows, that degrade performance [@Clinger1999].

### The stack/heap strategy
The stack/heap strategy is similar to the stack strategy. All continuation frames are allocated in the stack cache. When a continuation is captured, however, the contents of the stack cache are moved into the heap and the stack cache is cleared. When a continuation is thrown to, the new active continuation is left in the heap and the stack cache is cleared; this can be done in constant time. Since the current continuation may reside in either the stack cache or in the heap, each procedure return must test to see whether the frame should be popped off the stack cache. The stack/heap strategy makes throwing and recapturing a previously captured continuation very fast. A disadvantage of the stack/heap strategy is that it prevents the compiler from reusing a single continuation frame for multiple non-tail calls [@Clinger1999].

### The incremental stack/heap strategy
The incremental stack/heap strategy is a variation of the stack/heap strategy: When returning through a continuation frame that isn’t in the stack cache, a trap occurs and copies the frame into the stack cache. The trap can be implemented by maintaining a permanent continuation frame at the bottom of the stack cache. This frame’s return address points to system code that copies one or more frames from the heap into the stack cache, and immediately returns through the first of those continuation frames. The incremental stack/heap strategy is a zero-overhead strategy, with the same calling sequence as the stack strategy. Since the incremental stack/heap strategy copies frames from the heap into the stack cache, mutable variables cannot be kept within a continuation frame [@Clinger1999].

### The Hieb-Dybvig-Bruggeman strategy
A variation of the incremental stack/heap strategy that uses multiple stack segments that are allocated in the heap. The stack segment that contains the current continuation serves as the stack cache. When the stack cache overflows, a new stack cache is allocated and linked to the old one. Stack-cache underflow is handled by an underflow frame, as in the incremental stack/heap strategy. When a continuation is captured, the stack cache is split by allocating a small data structure representing the captured continuation. The data structure points to the current continuation frame within the stack cache. The unused portion of the stack cache becomes the new stack cache, and an underflow frame is installed at its base. A throw is handled as in the incremental stack/heap strategy: the current stack cache is cleared, and some number of continuation frames are copied into it. The underflow frame at the base of the stack cache is linked to the portion of the new continuation that was not copied. This is a zero-overhead strategy, in which mutable variables generally cannot be allocated within a continuation frame, but continuation frames may be reused for multiple non-tail calls [@Clinger1999].

## First-class continuations on the Java Virtual Machine
The implementations described in the previous section require to directly manipulate the stack, thus they are not suitable for being used on the Java Virtual Machine, which do not permit direct access or modification of stack contents. This section describes some implementation designed to implement first class continuations on the Java Virtual Machine.

### Heap based model
In a typical implementation of a programming language, a true stack is used to record call frames. Each call frame consists at least of a return address, variable bindings and a link to the previous frame. The variable bindings are the actual parameters and local variables used by the called procedure. A call frame is typically built by the calling procedure (caller). The caller pushes on the stack the actual parameters, a link to its stack frame and the return address, then jumps to the called procedure (callee). The callee augments the frame by pushing values of local variables. If the callee in turn calls another routine, it creates a new stack frame in the same way. When the callee has reached the end of its code, it returns to the caller by resetting the frame link, removing the frame, and jumping to the saved return address. The state of each active call is recorded on the stack, and it is destroyed once the call has been completed [@Dybvig1987].

Because of restricted access of stack content on the JVM, for languages that support first-class continuations this structure is not sufficient. First-class continuations require heap allocation of the call frames as well as the environment. This is because the natural implementation of a continuation is to retain a pointer into the call stack. Because the continuation is a first-class object, there is no restriction on when it may be invoked. In particular, it may be invoked even after control has returned from the point where it was obtained. If so, the stack may have since grown, overwriting some of the stack frames in the continuation. The natural solution, then, is to maintain a linked list of heap-allocated stack frames. As the stack grows, a new frame is allocated in an unused portion of the heap so that the old stack frames remain intact [@Dybvig1987].

The main disadvantage of heap allocation of call frames and environments is the overhead associated with the use of a heap. This overhead includes the additional cost of finding space in the heap when building the call frames and environments, the cost of storage reclamation to deallocate those frames and environments and  the cost of following links instead of indexing a stack or frame pointer. The overhead also includes the indirect cost of using excessive amounts of memory. Furthermore, use of the heap rather than a stack prevents the use of some hardware-optimised or microcode-supported instructions for managing the stack [@Dybvig1987].

The heap-based model has been used by several implementations, including Smalltalk, StacklessPython, Ruby, SML. On the JVM, this technique has been utilised by SISC [@Miller2002], a fully R5RS compliant interpreter of Scheme, with proper tail-recursion and first-class continuations.

### Continuations from continuation passing-style transform
An other approach to implement first-class continuations is to transform programs into continuation passing-style (CPS) [@appel2006compiling; @adams1986orbit]. The standard CPS-transform is a whole-program transformation, in which all explicit or implicit return statements are replaced by function calls and all state is kept in closures. One effect of CPS is that the stack is completely bypassed during execution, and this is not ideal for a stack-based architecture like the JVM.

Considering that manually written CPS code shows that only a small number of functions in a program actually need to pass along continuations, Tiark Rompf et al. developed a selective CPS transform for the Scala programming language [@Rompf2009] that is applied only where it is actually needed, and allows to maintain a stack-based runtime discipline for the majority of code. Thus, they made use of Scala’s pluggable typing facilities and introduce a type annotation, so that the CPS transform could be carried out by the compiler on the basis of expression types (i.e. it is type-directed). An advantage of this technique is that by design it avoids the performance problems associated with implementations of delimited continuations in terms of undelimited ones. However, there are some drawbacks. Because of the global transformation performed by the continuations compiler plugin, there are some control constructs that can not be used when calling a CPS function. For instance, using return statements in a CPS function may cause type mismatch compiler errors, thus is better to avoid using them. The compiler plugin does not handle `try` blocks, so it is not possible to catch exceptions within CPS code [@McBeath2010].

There are also some issues with looping constructs. Capturing delimited continuations inside a while loop turns the loop into a general recursive function. Therefore each invocation of shift within a looping construct allocates another stack frame, so after many iterations it is possible to run into a stack overflow. Moreover, some looping constructs can not be used with a `shift` inside them, because everything on the call path between a `shift` and its enclosing `reset` must be CPS-transformed. That means that a `shift` cannot be used into the regular foreach, map and filter methods, because they are not CPS-transformed [@McBeath2010].

### Continuations from generalized stack inspection
In [@Pettyjohn2005], Pettyjohn et al. show how to translate a program into a form that allows it to capture and restore its own stack without requiring stack manipulation primitives. They demonstrate that the native exception handling mechanism can be used to propagate captured control state down the stack. Their work is an extension of previous work by Sekiguchi et al. [@Sekiguchi2001] and Tao [@tao2001portable]. Variants of this technique has been described in [@Loitsch2007] for JavaScript, in [@Marshall2009] for a Scheme interpreter targeting the .NET CLR and in Kilim [@Srinivasan2006; @Bolton2000], a message-passing framework for Java. The basic idea is to break up the code into fragments (as top level methods) where the last instruction of any fragment is a call to the next fragment in the chain. To achieve this result, they have specialised continuation objects that maintain the state needed for each fragment and an overridden Invoke method to invoke the corresponding fragment. Each fragment knows exactly which fragment to invoke next [@Srinivasan2006]. The transform differs from continuation passing-style in that the call/return stack continues to be the primary mechanism for representing continuations; a heap representation of the continuation is only constructed when necessary. This may result in better performance than CPS-conversion for those programs that make only occasional use of first-class continuations [@StackHack2005].

This transformation preserves the calling signature of a procedure, but it augments the behavior of the procedure when a continuation is to be captured [@StackHack2005]. We therefore introduce into each method an additional control path that extracts the dynamic state of the method and appends it to a data structure. To capture a continuation, we throw a special exception to return control to the method along the alternate control path. After appending the dynamic state, the method re-throws the exception. This causes the entire stack to be emptied and the corresponding chain of reified frames to be built. A handler installed at the base of the stack is the ultimate receiver of the exception and it creates a first-class continuation object in the heap using the chain of reified frames [@Marshall2009].

This implementation technique is substantially equivalent to the stack strategy described in the first section of this chapter [@StackHack2005]. Moreover, it can nearly be a zero-overhead technique, for platforms in which  exception handlers are not expensive, especially when no exception is thrown. This is the case for the Java Virtual Machine [@Longjumps2015].

The process consists of six steps [@StackHack2005; @Marshall2009]:

1. Assignment Conversion - Capturing and re-instating a continuation will cause variables to be unbound and rebound multiple times. Variable bindings that are part of a lexical closure must not be unshared when this occurs. To avoid problems with unsharing that may occur when the stack is reified, assignment conversion introduces cells to hold the values of assigned variables. This conversion is best explained by showing it in Scheme source code:
```
	(lambda (x) ... x ... (set! x value) ...)
		=>
	(lambda (x)
		(let ((y (make-cell x)))
			... (contents y) ... (set-contents! y value) ...))
```
where `(make-cell x)` returns a new `cell` containing the value `x`, `(contents cell)` returns the value in `cell`, and `(set-contents! cell val)` updates `cell` with the new value `val`. After assignment conversion, the values of variables can no longer be altered - all side-effects are to data structures. This greatly simplifies the code transformation, because values may now be freely substituted for variables without having to first check to see whether they are assigned [@adams1986orbit].
2. ANF Conversion - The code is converted to A-normal form.  Converting the code into A-normal form [@Flanagan1993] gives names to the temporary values and linearizes the control flow by replacing compound expressions with an equivalent sequence of primitive expressions and variable bindings. After ANF conversion, all procedure calls will either be the right-hand side of an assignment statement or a return statement.
For instance, the following Scheme code shows the ANF transformation of a very simple expression:
```
	(f (g x) (h y))
		=>
	(let ((v0 (g x)))
		(let ((v1 (h y)))
			(f v0 v1)))
```
The following snippet shows the transformation for a fibonacci function in Java, considering as primitive subexpressions that can be evaluated without a method call:
```
	int fib (int x) {
		if (x < 2)
			return x;
		else
			return fib (x - 2) + fib (x - 1);
    }
		=>
	int fib_an (int x) {
		if (x < 2)
			return x;
		else {
			int temp0 = fib_an (x - 2);
			int temp1 = fib_an (x - 1);
			return temp0 + temp1;
		}
	}
```
3. Live variable analysis - It is necessary to note what variables are live at each continuation. We are only interested in those variables that are live after a procedure or method call returns. Those variables that are last used as arguments to the procedure or method call are no longer live. Unused variables are not copied when the continuation is captured.
4. Procedure Fragmentation - For each actual procedure, we create a number of procedures each of which has the effect of continuing in the middle of the original procedure. This allows to restart execution right after each call site. Each procedure fragment will make a tail-recursive call to the next fragment. Fragmentation also replaces iteration constructs with procedure calls.
```
    int fib_an (int x) {
        if (x < 2)
            return x;
        else {
            int temp0 = fib_an (x - 2);
            return fib_an0 (temp0, x);
        }
    }

    int fib_an0 (int temp0, int x) {
        int temp1 = fib_an (x - 1);
        return fib_an1 (temp1, temp0);
    }

    int fib_an1 (int temp1, int temp0) {
        return temp0 + temp1;
    }
```
5. Closure conversion - A continuation is composed of a series of frames, that are closed over the live variables in the original procedure. Each frame also has a method that accepts a single value (the argument to the continuation) and invokes the appropriate procedure fragment. These closures can be automatically generated if the underlying language were to support anonymous methods.
```
    abstract class Frame {

        abstract Object invoke(Object arg)
			throws Throwable;
    }

	class fib_frame0 extends Frame {

        int x;

        fib_frame0(int x) {
            this.x = x;
        }

        @Override
        Object invoke(Object return_value)
			throws ContinuationException, Throwable {
            return fib_an0(x);
        }

    }
```
6. Code annotation - The fragmented code is annotated so that it can save its state in the appropriate continuation frame. Each procedure call is surrounded by an exception handler. This intercepts the special exception thrown for reifying the stack, constructs the closure object from the live variables, appends it to the list of frames contained by the special exception, and re-throws the exception. The calls in tail position are not annotated.
```
    int fib_an (int x) {
        if (x < 2)
            return x;
        else {
            int temp0;
            try {
                temp0 = fib_an (x - 2);
            } catch (ContinuationException sce) {
                sce.extend (new fib_frame0 (x));
                throw sce;
            }
            return fib_an0 (temp0, x);
        }
    }

    int fib_an0 (int temp0, int x) {
        int temp1;
        try {
            temp1 = fib_an (x - 1);
        } catch (ContinuationException sce) {
            sce.extend (new fib_frame1 (temp0));
            throw sce;
        }
        return fib_an1 (temp1, temp0);
    }

    int fib_an1 (int temp1, int temp0) {
        return temp0 + temp1;
    }
```

### Java frameworks implementing continuations

#### Kilim
The Kilim framework [@Srinivasan2006; @Bolton2000] provides lightweight actors, a type system that guarantees memory isolation between threads and a library with I/O support and synchronisation constructs and schedulers. It uses a restricted form of continuations that always transfers control to its caller but maintain an independent stack. Kilim implements a variant of direct stack inspection is generalized stack inspection [@Pettyjohn2005]. It transforms compiled programs at the bytecode-level, inserting copy and restore instructions to save the stack contents into a separate data structure (called a fiber) when a continuation is to be accessed. Its implementation is based on threee main architectural choices:

##### Suspend-Resume
Kilim preserves the standard call stack, but provides a way to pause (suspend) the current stack and to store it in a continuation object called Fiber. The Fiber is resumed at some future time. Calling Fiber.pause() pops activation frames until it reaches the method that initiated resume(). This pair of calls is akin to shift/reset from the literature on delimited continuations; they delimit the section of the stack to be saved.

##### Schedulable Continuations
Kilim actors are essentially thread-safe wrappers around Fibers. A scheduler chooses which Actor to resume on which kernel thread. Kernel threads are treated as virtual processors while actors are viewed as agents that can migrate between kernel threads.

##### Generators
Generators are intended to be used by a single actor at a time, and run on the thread-stack of that actor. Even if the actor is running, it is prevented from executing any of its code until the generator yields the next element.

#### JavaFlow
The Apache Commons JavaFlow [@Javaflow2015] is a library providing a continuations API for Java, accomplished via bytecode instrumentation which modifies the ordinary control flow of method calls to accomodate the ability to suspend and resume code execution at arbitrary points. JavaFlow transforms a method if it can reach a suspend() invocation. It transforms all non-pausable methods reachable from there as well, that are modified such that they can distinguish between normal execution, continuation capturing, and continuation resuming. This leads to inefficiencies, even when no continuations are used [@Stadler2009]. The instrumentation can be performed in advance or by a special class loader, which adds complexity either to the build process or to the application itself [@Srinivasan2006; @Bolton2000].

#### RIFE
RIFE [@RIFE2015] is Java web application framework which allows web applications to benefit from first-class continuations. RIFE's pure Java continuation engine, which uses Java bytecode manipulation to implement continuations, has been extracted into a standalone Java library. It works similar to the Javaflow library, but it allows continuation capturing only within a specific method (`processElement`), so that there is always only one activation frame per continuation [@Stadler2009].

#### PicoThreads
A PicoThread is a lightweight, user-level Java thread that can be cooperatively-scheduled, dispatched and suspended [@begel2000picothreads]. PicoThreads are implemented in the Java bytecode language via a Java class-to-class translation. The translation produces threaded programs that yield control and a continuation sufficient to restart the thread where it left off. A PicoThread continuation is a Java object which contains a reference to the object and method in which it was created. Since Java’s procedure call stacks do not have dynamic extent, PicoThread continuations also contain extra state to store a method’s local variables. PicoThread continuations extend Java exceptions, so that they can take advantage of Java’s zero-cost exception mechanism to pass continuations from method to method. However the authors PicoThreads were unable to find a Java implementation fast enough to use the library effectively.

#### Matthias Mann’s continuations library
Matthias Mann’s continuations library implements continuations in Java using the ASM bytecode manipulation and analysis framework.  The library provides an API allows to write coroutines and iterators in a sequential way [@ContinuationsLib2015].

### Kawa's continuations
Kawa provides a restricted type of continuations, that are implemented using Java exceptions, and can be used for early exit, but not to implement coroutines or generators [@Bothner1998].
```
    class callcc extends Procedure1 {
	    ...;
	    public Object apply1(Object arg1) {
		    Procedure proc = (Procedure) arg1;
		    Continuation cont
			    = new Continuation ();
		    try { return proc.apply1(cont); }
		    catch (CalledContinuation ex) {
			    if (ex.continuation != cont)
				    throw ex;  // Re-throw.
			    return ex.value;
		    } finally {
			    cont.mark_invalid();
		    }
	    }
    }
```
 The `Procedure` that implements `call-with-current-continuation` creates a continuation object `cont`, that represents the current continuation, and passes it to the incoming Procedure `proc`. If `callcc` catches a `CalledContinuation` exception it means that `proc` invoked some `Continuation`. If it is the continuation of the current `callcc` instance, the code returns the value passed to the continuation; otherwise it re-throws up the stack until a matching handler is reached.

The method `mark_invalid` marks a continuation as invalid, to detect unsupported invocation of cont after `callcc` returns. (A complete implementation of continuations would instead make sure the stacks are moved to the heap, so they can be returned to an an arbitrary future time.)
```
    class Continuation extends Procedure1 {
	    ...;
	    public Object apply1(Object arg1) {
		    throw new CalledContinuation (arg1, this);
	    }
    }
```
A `Continuation` is the actual continuation object that is passed to `callcc`'s argument; when it is invoked, it throws a `CalledContinuation` that contains the continuation and the value returned.
```
    class CalledContinuation
	    extends RuntimeException {
	    ...;
	    Object value;
	    Continuation continuation;
	    public CalledContinuation
		    (Object value, Continuation cont) {
		    this.value = value;
		    this.continuation = cont;
	    }
    }
```
