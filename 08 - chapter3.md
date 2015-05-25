# First Class Continuations and the JVM

intro blablabla

## Stack manipulation issues
blablabla

## Poor performances of Heap based techniques
The major problem with heap allocation of call frames and environments is the overhead associated with the use of a heap. This overhead includes the direct cost of finding space in the heap when building the call frames and environments, and of following links instead of indexing a stack or frame pointer when accessing pieces of the frame or environment. The overhead also includes the indirect cost of storage reclamation to deallocate and reuse stack frames and environments and the indirect cost of using excessive amounts of memory 2 . Furthermore, use of the heap rather than a stack prevents the use of commonly available hardware or microcode-supported stack push, pop and index instructions and the use of function call and return instructions.


## Limits of other implementations
blablabla
