# First-Class Continuations and the JVM

## Stack manipulation issues
Unlike low-level languages, such as C, that permit access to the stack through use of pointer arithmetic, higher level languages, such as Java or C# do not provide instructions for installing and saving the run-time stack. Compiling Scheme, or any other language that uses first-class continuations to the JVM thus poses a challenging problem. At at first glance, the implementors must either give up implementing continuations or manage a heap-stored stack. The first choice limits the programmers of these languages, besides automatically making the Scheme implementation non standard-compliant. The second choice precludes many of the advantages that these machines supposedly offer.

## Poor performances of Heap based techniques
The major problem with heap allocation of call frames and environments is the overhead associated with the use of a heap. This overhead includes the direct cost of finding space in the heap when building the call frames and environments, and of following links instead of indexing a stack or frame pointer when accessing pieces of the frame or environment. The overhead also includes the indirect cost of storage reclamation to deallocate and reuse stack frames and environments and the indirect cost of using excessive amounts of memory. Furthermore, use of the heap rather than a stack prevents the use of commonly available hardware or microcode-supported stack push, pop and index instructions and the use of function call and return instructions.

## Limits of other implementations
blablabla
