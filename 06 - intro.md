\mainmatter

#Introduction

> *Programming languages are not just technology, but what programmers think in. They're half technology and half religion.*

> Paul Graham - "Beating the Averages"

## Context

### Functional programming
It is well known that the modern computers are not becoming "faster" like they used to be, because frequency scaling, for now, hit the limit. They increase their potential productivity by adding cores.

This implies that to benefit most from this architecture, the programs have to be parallellized. But parallel programming is way harder than sequential programming, due to a lot of new challenges it brings.

Functional programming helps to get rid of some of these challenges, e.g. race conditions don't apply if you only use immutable variables and methods without side effects. The learning curve for functional programming is often steep, but the learning curve for parallel programming might be even steeper, and not at all intuitive.

The world is full of stateful objects. The machine you program itself is a stateful machine. Sure, functional programming has ways to model state, but there's a deep impedance mismatch between a stateless model being implemented on a stateful machine trying to emulate a stateful world.

However, there are several programming problems in the world that are easy to map to the FP model. Problems involving large data sets, multi-processing are easy to map to the FP model. In practice, however, problems involving large data sets and multi-processing also require maximum efficiency, and the FP languages in use today cannot match frameworks that take some of the best practices in FP programming and use them in imperative languages. For instance, you can see the MapReduce/Hadoop frameworks as implementing FP in C++ and Java with the aim of providing maximum performance in a distributed environment without paying the cognitive or interpretive overhead of a typical FP language.

### Java
Why java is important in the market, why it is introducing functional features.

![TIOBE Index for June 2015 [@TIOBEIndex2015] \label{lang-rank} ](figures/ranking.png)

See Figure \ref{lang-rank} for a schematic illustration.

### Scheme
Scheme is a dialect of the computer programming language Lisp. It follows a minimalist design philosophy that specifies a small standard core accompanied by powerful tools for meta-programming.

Scheme was created during the 1970s at the MIT AI Lab by Guy L. Steele and Gerald Jay Sussman. It was the first dialect of Lisp to choose lexical scope and the first to require implementations to perform tail-call optimisation. It was also one of the first programming languages to support first-class continuations.

Scheme is a general-purpose computer programming language. It is a high-level language, supporting operations on structured data such as strings, lists, and vectors, as well as operations on more traditional data such as numbers and characters. Scheme is a fairly simple language to learn, since it is based on a handful of syntactic forms and semantic concepts and since the interactive nature of most implementations encourages experimentation.

The storage required to hold the contents of an object is dynamically allocated as necessary and retained until no longer needed, then automatically deallocated, typically by a garbage collector that periodically recovers the storage used by inaccessible objects. Simple atomic values, such as small integers, characters, booleans, and the empty list, are typically represented as immediate values and thus incur no allocation or deallocation overhead.

Scheme programs share a common printed representation with Scheme data structures. As a result, any Scheme program has a natural and obvious internal representation as a Scheme object. For example, variables and syntactic keywords correspond to symbols, while structured syntactic forms correspond to lists. This representation is the basis for the syntactic extension facilities provided by Scheme for the definition of new syntactic forms in terms of existing syntactic forms and procedures. It also facilitates the implementation of interpreters, compilers, and other program transformation tools for Scheme directly in Scheme, as well as program transformation tools for other languages in Scheme.

In Scheme, a procedure definition may appear within another block or procedure, and the procedure may be invoked at any time thereafter, even if the enclosing block has completed its execution. To support lexical scoping, a procedure carries the lexical context (environment) along with its code.

Furthermore, Scheme procedures are not always named. Instead, procedures are first-class data objects like strings or numbers, and variables are bound to procedures in the same way they are bound to other objects.

The Scheme language is standardized in the Revised\textsuperscript{n} Report on the Algorithmic Language Scheme (RnRS).

### Kawa
Kawa is a language framework written in Java that implements the programming language Scheme. It provides a set of Java classes useful for implementing dynamic languages, such as those in the Lisp family. Kawa is also an implementation of almost all of R7RS Scheme (First-class continuations being the major missing feature), and which compiles Scheme to the bytecode instructions of the Java Virtual Machine.

Kawa gives run-time performance a high priority. The language facilitates compiler analysis and optimisation,and most of the time the compiler knows which function is being called, so it can generate code to directly invoke a method. Kawa also tries to catch errors at compile time.

To aid with type inference and type checking, Kawa supports optional type specifiers, which are specified using two colons. For example:
```
	(define (add-int x::int y::int) :: String
		(String (+ x y)))
```
This defines a procedure add-int with two parameters: x and y are of type Java `int`; the return type is a `java.lang.String`.

The Kawa runtime start-up is much faster than other scripting languages based on the Java virtual machine (JVM). This allows Kawa to avoid using an interpreter. Each expression typed into the REPL is compiled on-the-fly to JVM bytecodes, which (if executed frequently) may be compiled to native code by the just-in-time (JIT) compiler.

### Continuations
Why continuations are usefull, why Kawa doesn't support them, note that they have been introduced in Scala, Haskell and other languages.

## This work

## Outline
outline
