# Evaluation

> *“Extraordinary claims require extraordinary evidence.”*
\begin{flushright}
Carl Sagan, Encyclopedia Galactica
\end{flushright}

## Transformation overhead
We saw in the previous chapters how we can implement `call/cc` on a JVM targeting compiler, performing a transformation on the whole source to instrument the original code. We would like to know how this global transformation impacts the overall performances of the program when no continuations are captured. We already observed that exception handlers are not expensive on the JVM, but there are other variables to take in consideration. The code fragmentation implies an increase on the number of function calls, which can reduce performance.

I used a set of benchmark to analyse the behaviour of the running code in the case both transformed code and non-transformed code. All the benchmarks were executed on an Intel i5 dual-core processor with 4GB of RAM. The table in Figure \ref{overhead-table} and the chart in Figure \ref{overhead} show the results.

![Transformed vs non-transformed code, 10 iterations, values in seconds \label{overhead-table}](figures/overhead-table.pdf)

The `fib` benchmark runs a simple Fibonacci function with 30 as input. `tak` implements the Takeuchi function and runs it with 18, 12, 6. `cpstak` is a version of `tak` rewritten in continuation passing style. We can observe that the transformation introduces a considerable overhead, especially in the `fib` benchmark.

![Transformed vs non-transformed code, performance comparison \label{overhead}](figures/overhead.png)

To understand from where this overhead comes from, I profiled the execution of the fib benchmark using HPROF, a profiling tool provided by the Java platform. Considering the cpu usage data (Figure \ref{cpu}), we can observe that approximately a 10% of the cpu time is spent allocating `Proceure` objects (`gnu.mapping.Procedure.<init>` and `gnu.expr.ModuleMethod.<init>`).

![Most called Java methods in the `fib` benchmark \label{cpu}](figures/cpu.pdf)

We can reach the same conclusions analysing the heap usage. Figure \ref{heap} shows which objects are more often allocated during the execution of `fib`. Almost 40% of the heap is used to store object of type `gnu.expr.ModuleMethodWithContext`, that is the runtime object in which closures are allocated.

![Most allocated Java object during the execution of the `fib` benchmark \label{heap}](figures/heap.pdf)

This is not unexpected, as the transformed code is fragmented in a set of closures. However, this suggest that a possible improvement for the technique can be obtained optimising closure allocation.

## Capturing performance
I tested the new `call/cc` implementation on five continuation-intensive benchmarks. `fibc` is a variation of `fib` with continuations. The `loop2` benchmark corresponds to a non-local-exit scenario in which a tight loop repeatedly throws to the same continuation. The `ctak` benchmark is a continuation-intensive variation of the call-intensive `tak` benchmark. The ctak benchmark captures a continuation on every procedure call and throws a continuation on every return. I compared the modified version of Kawa with SISC, the only other JVM Scheme supporting `call/cc`, and other Scheme implementations with a JIT compiler targeting either native machine code or an internal VM.

![Capturing benchmark (interpreted code), 10 iterations, values in secons \label{interp-tab}](figures/interpreted-table.pdf)

Some of the Scheme implementations introduced above can pre-compile code to a bytecode or binary format, which can be later executed without paying the cost for translation. Figures \ref{compiled-tab} and \ref{compiled} compares the execution time of code compiled by five compilers, including the modified version of Kawa.

![Capturing benchmark (interpreted code), 10 iterations \label{interp}](figures/interpreted.png)

![Capturing benchmark (pre-compiled code), 10 iterations, values in secons \label{compiled-tab}](figures/compiled-table.pdf)

![Capturing benchmark (pre-compiled code), 10 iterations \label{compiled}](figures/compiled.png)

Looking at the benchmarks' outcome we can see that Kawa with first-class continuations (Kawa fcc), despite the overhead we measured in the previous section, performs slightly better then SISC. As expected, Kawa fcc performances are far from some JVM compilers, however, when compared with Guile and Racket they are within the same order of magnitude.

## Code size
We saw in Chapter 3 that we expect an increase in code size proportional to the number of code fragments, so we want to measure the actual difference in size between a regular class file and an instrumented one. Figure \ref{codesize-tab} shows a comparison of regular code and transformed code.

![Code size comparison, values in bytes \label{codesize-tab}](figures/codesize-table.pdf)

![Size of compiled classes in bytes \label{codesize}](figures/codesize.png)
