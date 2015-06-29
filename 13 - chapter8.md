# Conclusions and future work

My main contribution is an implementation of call/cc in a Scheme compiler targeting the JVM. The only other Scheme implementations targeting the JVM are SISC, which is an heap based interpreter, and Bigloo, which is a compiler but does not support continuations in the JVM back-end. Scala implements a different type of control operator, shift and reset. Although Ruby has callcc, JRuby does not support it.

I address the problem of providing a control operator that copies the stack in an environment that prevents direct stack manipulation. Unlike other solutions proposed to implement continuations on the JVM, we perform a transformation on the syntax tree produced by Kawa, instead of a transformation at the bytecode level. This make our transformation independent of the JVM version.

I present a variant of generalised stack inspection, described by Pettyjohn et al., as an extension of the Kawa compiler. The transformation is global, thus has been developed as an optional compiler pass, to avoid adding overhead to programs that do not use continuations.

## Future work

This work can be further developed in several interesting directions. I will outline a few possible applications and extensions, which can be based on this contribution to implement new features and obtain improvements in other programming languages or framework.

* Variants of this technique can be employed in other JVM languages to implement first-class delimited and un-delimited continuations and control operators. Languages like Clojure, JRuby, Groovy, Bigloo and others could take advantage of this work.

* The transformation described in the previous pages uses an intermediate A-normalisation pass. ANF has been related in other works to Static Single Assignment (SSA) form [@Chakravarty2004]. It is possible to exploit the formal properties of ANF to implement in the compiler many optimisation present in literature.

* In the past few years several frameworks for concurrency appeared on the Java scene. Many of them use bytecode instrumentation to implement coroutines and provide lightweight threads with low memory and task-switching overhead. Quasar [@QuasarAkka2015], for instance, deliver the actor model on the JVM  using bytecode instrumentation. The transformation presented in this document can give an alternative for those frameworks that aim to provide a concurrency API for Java or JVM languages.

* Research has been done on the use of continuations in the context of web applications [@Matthews2004;@Queinnec2004]. The support for first-class continuations developed in the context of this thesis, can be utilised to implement continuation based web frameworks in Kawa or in Java.

* The research field of Dynamic Software Updating (DSU) pertains to upgrading programs while they are running [@gregersen2014state;@Makris2009]. Different approaches for DSU has been developed, nevertheless it is not currently widely used in industry. Some of the approaches use a stack reconstruction technique similar in many aspects to the `call/cc` implementation described in this dissertation [@Buisson2008]. Future work can start from the achievements of this work to explore an alternative implementation of DSU on the JVM.
