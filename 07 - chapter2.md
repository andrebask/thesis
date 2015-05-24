# State of the art

## Classical techniques
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
SISC, a fully R5RS compliant heap-based interpreter of the functional language Scheme, with proper tail-recursion and first-class continuations.

### Scala's Continuations
blablabla

### Kilim, JavaFlow etc.
blablabla

### Pettyjohn et al. technique
blablabla

### Kawa previous limited implementation
blablabla
