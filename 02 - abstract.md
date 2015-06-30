\frontmatter
\chapter{Abstract}

The spread of multi-core processors, the slow-down of frequency scaling and the need to process large amounts of data pose new challenges for software developers and programming languages implementers. First-class continuations and control operators like `call/cc` are a tool to introduce concurrency and multi-threading features in programming languages.

The widespread diffusion of the Java technology has encouraged the birth of new programming languages on the Java Virtual Machine, languages that brings new features to the Java environment, most of which taken from the functional paradigm. Kawa is an implementation of the programming language Scheme on the Java Virtual Machine. As a Scheme it provides a functional style of programming, dynamic typing, and meta-programming facilities. However, being the Java Virtual Machine devoid of stack manipulation primitives, Kawa lacks of one of the most peculiar Scheme features: First-class continuations.

This dissertation describes an implementation of the `call/cc` control operator in the Kawa compiler. In particular it shows how the exception handling feature, common to many programming languages, can be exploited to implement first-class continuations in an environment without stack manipulation primitives, and how this can be realised in a real compiler.
