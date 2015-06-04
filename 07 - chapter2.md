# State of the art

## Classical stack-based implementation techniques for first-class continuations
The most common approach to implement first-class continuations is to stick with a stack-based execution architecture and to reify the current continuation by making a copy of the stack, which is reinstated when the continuation is invoked. This is the approach taken by many language implementations that are in direct control of the runtime system. This section describes the most used implementation strategies for first class continuations.

### The garbage-collection strategy
The simplest strategy for ensuring that continuations have unlimited extent is to allocate them in the heap and to rely on garbage collection or reference counting to recover their storage. We call this the gc strategy. The gc strategy is not a zero-overhead strategy and it is optimised for programs in which every continuation frame is captured. Few real programs capture all continuations, however, so the gc strategy may not perform as well as a zero-overhead strategy. The most important indirect cost of the gc strategy is that the compiler must allocate a separate continuation frame for each non-tail call, unless the compiler can prove that the continuation will not be captured during the non-tail call. The gc strategy also suffers more cache misses than the other strategies described in this paper.

### The spaghetti strategy
The spaghetti stack used in is a variation of the gc strategy. The spaghetti stack is in effect a separate heap in which storage is reclaimed by reference counting rather than garbage collection. Though complex, the spaghetti stack was at one time more efficient than using a gc strategy with a non-generational garbage collector, because the spaghetti stack’s storage management is optimised to support procedure call, return, and a host of related operations. In the normal case, when all frames have dynamic extent, the spaghetti stack behaves as a conventional stack. When a fast garbage collector is available, the spaghetti strategy is probably slower than the gc strategy. Captures and throws require updating the reference counts. It therefore appears that the gc strategy should always perform better than the spaghetti strategy.

### The heap strategy
In the heap strategy, a one-bit reference count in each frame indicates whether the frame has been captured. Continuation frames are allocated in a garbage-collected heap, as in the gc strategy, but a free list of uncaptured frames is also used. When a frame is needed by a procedure call, it is taken from the free list unless the free list is empty. If the free list is empty, then the frame is allocated from the heap. When a frame is returned through, it is linked onto the free list if its reference count indicates that it has not been captured. Otherwise it is left for the garbage collector to reclaim. The heap strategy is not a zero-overhead strategy. The heap strategy is most practical if all continuation frames are the same size; otherwise multiple free lists may be required. This is an indirect cost of the heap strategy. Like the gc strategy, the heap strategy makes it difficult to reuse a continuation frame for multiple non-tail calls. This is another indirect cost.

### The stack strategy
This suggests the stack strategy, in which the active continuation is represented as a contiguous stack in an area of storage we call the stack cache. Non-tail calls push continuation frames onto this stack cache, and returns pop frames from the stack cache, just as in an ordinary stack-based implementation. When a continuation is captured, however, a copy of the entire stack cache is made and stored in the heap. When a continuation is thrown to, the stack cache is cleared and the continuation is copied back into the stack cache. A first-class continuation thus resides in the heap, but is cached in the stack cache whenever it is the active continuation. The stack strategy is a zero-overhead strategy. Capturing, recapturing, and throwing to a continuation take time proportional to the size of the continuation. The stack strategy repeatedly copies the same continuation from the stack cache to the heap. This can increase the asymptotic storage space required by an implementation, which must be counted as an indirect cost of the stack strategy. The stack strategy prevents a compiler from allocating storage for mutable variables within a continuation frame, because there are other copies of it. Mutable variables must generally be allocated in registers or in the heap, that is another indirect cost of the stack strategy.

### The chunked-stack strategy
By maintaining a small bound on the size of the stack cache, and copying portions of the stack cache into the heap or back again as the stack-cache overflows and underflows, The chunked-stack strategy reduces the worst-case latency of captures and throws. The chunked-stack strategy works well with generational garbage collection because limiting the size of the stack cache limits the size of the root set that the garbage collector must scan on each garbage collection. The portion of the continuation that resides in the heap will be scanned only when its generation is collected. We regard the chunked-stack strategy as a zero-overhead strategy, because the cost of stack-cache overflows and underflows is usually negligible. On the other hand, the chunked-stack strategy requires a stack cache that is large enough to avoid stack-cache overflows and underflows, that degrade performance. This indirect cost.

### The stack/heap strategy
The stack/heap strategy is similar to the stack strategy. All continuation frames are allocated in the stack cache. When a continuation is captured, however, the contents of the stack cache are moved into the heap and the stack cache is cleared. Likewise when a continuation is thrown to, the new active continuation is left in the heap and the stack cache is cleared; this can be done in constant time. Since the current continuation may reside in either the stack cache or in the heap, each procedure return must test to see whether the frame should be popped off the stack cache. The stack/heap strategy makes throwing very fast, and recapturing a previously captured continuation is very fast also. A disadvantage of the stack/heap strategy is that it prevents the compiler from reusing a single continuation frame for multiple non-tail calls.

### The incremental stack/heap strategy
The incremental stack/heap strategy is a variation of the stack/heap strategy: When returning through a continuation frame that isn’t in the stack cache, a trap occurs and copies the frame into the stack cache. The trap can be implemented by maintaining a permanent continuation frame at the bottom of the stack cache. This frame’s return address points to system code that copies one or more frames from the heap into the stack cache, and immediately returns through the first of those continuation frames.
The incremental stack/heap strategy is a zero-overhead strategy, with the same calling sequence as the stack strategy. Since the incremental stack/heap strategy copies frames from the heap into the stack
cache, mutable variables cannot be kept within a continuation frame.

### The Hieb-Dybvig-Bruggeman strategy
This strategy is a variation of the incremental stack/heap strategy that uses multiple stack segments that are allocated in the heap. The stack segment that contains the current continuation serves as the stack cache.
When the stack cache overflows, a new stack cache is allocated and linked to the old one. Stack-cache underflow is handled by an underflow frame, as in the incremental stack/heap strategy.
When a continuation is captured, the stack cache is split by allocating a small data structure that points to the current continuation frame within the stack cache. This data structure represents the captured continuation. The unused portion of the stack cache becomes the new stack cache, and an underflow frame is installed at its base.
A throw is handled as in the incremental stack/heap strategy: the current stack cache is cleared, and some number of continuation frames are copied into it. The underflow frame at the base of the stack cache is linked to the portion of the new continuation that was not copied.
The Hieb-Dybvig-Bruggeman strategy is a zero-overhead strategy. As with the stack strategy and the incremental stack/heap strategy, mutable variables generally cannot be allocated within a continuation frame, but continuation frames may be reused for multiple non-tail calls.

### Comparison
figure

## Alternative techniques for first class continuations on the Java Virtual Machine
The implementations described in the previous section require to directly manipulate the stack, thus they are not suitable for being used on the Java Virtual Machine, which do not permit direct access or modification of stack contents.
This section describes some implementation designed to implement first class continuations on the Java Virtual Machine.

### Heap based model
In a typical implementation of a lexically-scoped language, a true stack is used to record call frames. Each call frame contains a return address, variable bindings, a link to the previous frame, and sometimes additional information. The variable bindings are the actual parameters of the called routine and local variables used by the called routine. A call frame is typically built by the calling routine, or caller. The caller pushes the actual parameters on the stack, a link to its stack frame, the return address, and jumps to the called routine, or callee. The callee augments the frame by pushing values of local variables. If the callee in turn calls another routine, it creates a new stack frame by pushing the actual parameters, frame link, and return address, and so on. When the callee has reached the end of its code, it returns to the caller by resetting the frame link, removing the frame, and jumping to the saved return address. In this manner, the state of each active call is recorded on the stack, and this state is destroyed once the call has been completed.

Because of Scheme’s first-class closures and continuations, and because of restricted access of stack content on the JVM, this structure is not sufficient. First-class closures are capable of retaining argument bindings indefinitely. For this reason, it is not possible to store argument bindings in the stack frame. Instead, a heap-allocated environment is created to hold the actual parameters, and a pointer to this environment is placed in the call frame in their place. When a closure is created, a pointer to this environment is placed in the closure object.

Moving the variable bindings into the heap saves the bindings from being overwritten as the stack shrinks and grows. However, first-class continuations require heap allocation of the call frames as well as the environment. This is because the natural implementation of a continuation is to retain a pointer into the call stack. Because the continuation is a first-class object, there is no restriction on when it may be invoked. In particular, it may be invoked even after control has returned from the point where it was obtained. If so, the stack may have since grown, overwriting some of the stack frames in the continuation. The natural solution, then, is to maintain a linked list of heap-allocated stack frames. As the stack grows, a new frame is allocated in an unused portion of the heap so that the old stack frames remain intact.

The heap-based model has been used by several implementations, including Smalltalk, StacklessPython, Ruby, SML. On the JVM, this technique has been utilised by SISC, a fully R5RS compliant interpreter of Scheme, with proper tail-recursion and first-class continuations.

### Scala's Continuations
An other approach to implement first-class continuations is to transform programs into continuation passing-style (CPS). Unfortunately, the standard CPS-transform is a whole-program transformation. All explicit or implicit return statements are replaced by function calls and all state is kept in closures, completely bypassing the stack. For a stack-based architecture like the JVM, of course, this is not a good fit.
Noting that manually written CPS code shows that only a small number of functions in a program actually need to pass along continuations, Tiark Rompf, Ingo Maier and Martin Odersky developed a selective CPS transform for the Scala programming language that is applied only where it is actually needed, and allows us to stick to a regular, stack-based runtime discipline for the majority of code. Thus, they made use of Scala’s pluggable typing facilities and introduce a type annotations, so that the CPS transform could be type-directed, i.e. carried out by the compiler on the basis of expression types. As a side effect, this by design avoids the performance problems associated with implementations of delimited continuations in terms of undelimited ones.

### Pettyjohn et al. technique
blablabla

### Java frameworks implementing continuations

#### Kilim
The Kilim framework provides ultra-lightweight actors, a type system that guarantees memory isolation between threads and, a library with I/O support and customizable synchronization constructs and schedulers. It uses a restricted form of continuations that always transfers control to its caller but maintain an independent stack. Kilim implements a variant of direct stack inspection is generalized stack inspection (Pettyjohn et al. 2005). It transforms compiled programs at the bytecode-level, inserting copy and restore instructions to save the stack contents into a separate data structure (called a fiber) when a continuation is to be accessed. Its implementation is based on threee main architectural choices:

##### Suspend-Resume
Kilim preserves the standard call stack, but provides a way to pause (suspend) the current stack and to store it in a continuation object called Fiber. The Fiber is resumed at some future time. Calling Fiber.pause() pops (and squirrels away) activation frames until it reaches the method that initiated resume(). This pair of calls is akin to shift/reset from the literature on delimited continuations; they delimit the section of the stack to be squirrelled away.

##### Schedulable Continuations
Kilim actors are essentially thread-safe wrappers around Fibers. A scheduler chooses which Actor to resume on which kernel thread, thereby multiplexing hundreds of thousands of actors onto a handful of kernel threads. Kernel threads are treated as virtual processors while actors are viewed as agents that can migrate between kernel threads.

##### Generators
The Kilim framework also provides generators, essentially suspendable iterators. When resumed, they yield the next element and promptly pause again. Generators are intended to be used by a single actor at a time, and run on the thread-stack of that actor, which means that although the actor is technically running, it is prevented from executing any of its code until the generator yields the next element.

#### JavaFlow
The JavaFlow project uses thread-local variables to pass the continuation instead of modifying method signatures, as Kilim does. JavaFlow transforms a method if it can reach a suspend() invocation. It transforms all non-pausable methods reachable from there as well, leading to inefficiencies.

#### RIFE

#### PicoThreads

### Kawa's continuations
Kawa continuations are implemented using Java exceptions, and can be used for early exit, but not to implement coroutines or generators.
```
class callcc extends Procedure1
{ ...;
  public Object apply1(Object arg1)
  {
    Procedure proc = (Procedure) arg1;
    Continuation cont
       = new Continuation ();
    try { return proc.apply1(cont); }
    catch (CalledContinuation ex)
      {
        if (ex.continuation != cont)
             throw ex;  // Re-throw.
        return ex.value;
      }
    finally
      {
        cont.mark_invalid();
      }
  }
}
```
 The `Procedure` that implements `call-with-current-continuation` creates a continuation object `cont`, which is the “current continuation”, and passes it to the incoming Procedure `proc`. If `callcc` catches a `CalledContinuation` exception it means that `proc` invoked some `Continuation`. If it is the continuation of the current `callcc` instance, the code returns the value passed to the continuation; otherwise it re-throws up the stack until a matching handler is reached.

The method `mark_invalid` marks a continuation as invalid, to detect unsupported invocation of cont after `callcc` returns. (A complete implementation of continuations would instead make sure the stacks are moved to the heap, so they can be returned to an an arbitrary future time.)
```
class Continuation extends Procedure1
{ ...;
  public Object apply1(Object arg1)
  {
    throw new CalledContinuation
	      (arg1, this);
  }
}
```
A `Continuation` is the actual continuation object that is passed to `callcc`'s argument; when it is invoked, it throws a `CalledContinuation` that contains the continuation and the value returned.
```
class CalledContinuation
    extends RuntimeException
{ ...;
  Object value;
  Continuation continuation;
  public CalledContinuation
    (Object value, Continuation cont)
  {
    this.value = value;
    this.continuation = cont;
  }
}
```
