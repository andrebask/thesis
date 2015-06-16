# Implementing First-Class Continuations on the JVM

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


### Code fragmentation

### Code Instrumentation

## Open problems to solve
