# Conclusions and future work

This dissertation has presented an implementation of the `call/cc` control operator in Kawa, a Scheme compiler targeting the JVM. Although the problem was delineated in some works in literature, there was not a compiler providing first-class continuations on the JVM in terms of `call/cc`.

I developed a variant of generalised stack inspection in the Kawa compiler, addressing the problem of defining an A-normalisation algorithm for the Kawa super-set of Scheme, and realising a fragmentation and instrumentation pass using the existing Kawa framework. The whole transformation has been designed to be optional and separated from the existing passes, so that it does not add unnecessary overhead to modules without continuations.

I explored variations of the technique to implement other control operators, such as `shift`/`reset` and prompts, as well as continuation barriers. Moreover, the two passes are flexible enough that could be used on a portion of the syntax tree, instead of the entire program.

I showed the opportunities opened by the availability of `call/cc`, developing a syntax for asynchronous programming, and exploiting the A-normalisation of the syntax tree to create a simple debugger.

The evaluation of performance and memory usage revealed that this technique can be a valid alternative to heap-based implementations of `call/cc`. Benchmarks also showed that the bottleneck of the technique is not exception handling, but closure allocation, leaving room for improvement.

## Future work

This work can be further developed in several interesting directions. I will outline a few possible applications and extensions, which can be based on this contribution to implement new features and obtain improvements in other programming languages or framework.

* Variants of this technique can be employed in other JVM languages to implement first-class delimited and un-delimited continuations and control operators. Languages like Clojure, JRuby, Groovy, Bigloo and others could take advantage of this work.

* The transformation described in the previous pages uses an intermediate A-normalisation pass. ANF has been related in other works to Static Single Assignment (SSA) form [@Chakravarty2004]. It is possible to exploit the formal properties of ANF to implement in the compiler many optimisation present in literature.

* In the past few years several frameworks for concurrency appeared on the Java scene. Many of them use bytecode instrumentation to implement coroutines and provide lightweight threads with low memory and task-switching overhead. Quasar [@QuasarAkka2015], for instance, deliver the actor model on the JVM  using bytecode instrumentation. The transformation presented in this document can give an alternative for those frameworks that aim to provide a concurrency API for Java or JVM languages.

* Research has been done on the use of continuations in the context of web applications [@Matthews2004;@Queinnec2004]. The support for first-class continuations developed in the context of this thesis, can be utilised to implement continuation based web frameworks in Kawa or in Java.

* The research field of Dynamic Software Updating (DSU) pertains to upgrading programs while they are running [@gregersen2014state;@Makris2009]. Different approaches for DSU has been developed, nevertheless it is not currently widely used in industry. Some of the approaches use a stack reconstruction technique similar in many aspects to the `call/cc` implementation described in this dissertation [@Buisson2008]. Future work can start from the achievements of this work to explore an alternative implementation of DSU on the JVM.
