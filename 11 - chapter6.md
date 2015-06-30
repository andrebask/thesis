# Case studies

> *“Who controls the past controls the future. He who controls the present controls the past.”*
\begin{flushright}
George Orwell, 1984
\end{flushright}

## Asynchronous programming: Async and Await

Asynchronous programming is a programming paradigm that facilitates fast and responsive applications. Asynchronous programming is crucial to avoid the inefficiencies caused by blocking activities, such as the accesses to the web. Access to a web resource or to a huge database can be slow or delayed. If such an activity is blocked within a synchronous process, the entire application is stuck. You can avoid performance bottlenecks and enhance the responsiveness of your application by using asynchronous programming. In an asynchronous process, the application can continue with other work that does not depend on the resource to be accessed until the potentially blocking task finishes. However, traditional techniques for writing asynchronous applications can be complicated, making them difficult to write, debug, and maintain. In this section, I propose a syntax similar to the `async`/`await` construct already introduced in C#, that allows to execute asynchronous tasks during the normal execution of the program.

```scheme
	(define (async-call)
	  ... long running operation
		  that returns an int  ...)

	(async async-call
	  ... work independent to the
		  int result here  ...
	  (await x ; <- wait for the result
		... here you can use ...
		... the result ...))
```

We will also see how asynchronous programming features can be added to Scheme using coroutines and delimited continuations.

### Coroutines

Coroutines are functions that can be paused and later resumed. They are necessary to build lightweight threads because they provide the ability to change execution context. Coroutines are considered challenging to implement on the JVM, as they are usually implemented using bytecode instrumentation. However, having first-class continuations, becomes painless to implement coroutines. They can indeed be obtained with few lines of code in Scheme. The following code is a port of safe-for-space cooperative threads presented by Biagioni et al. in [@biagioni1998safe], where the code for managing a queue has been omitted for brevity:

\columnsbegin

```scheme
(define process-queue
           (make-queue))
(define sync-cont #f)

(define (coroutine thunk)
  (enqueue! process-queue thunk))

(define (dispatch)
  (if (null? (car process-queue))
	(when sync-cont
		(sync-cont))
	((dequeue! process-queue))))


(define (exit)
  (dispatch))

(define (sync)
  (call/cc
   (lambda (k)
     (set! sync-cont k)
     (dispatch))))
```

\columnbreak

```scheme
  (define (yield)
    (call/cc
     (lambda (parent)
	   (coroutine (lambda ()
	                (parent #f)))
	   (dispatch))))

  (define (thread-activator)
    (call/cc
     (lambda (parent)
	   (let ((f (call/cc
	             (lambda (fc)
	    	      (parent fc)))))
	     (f)
	     (exit)))))

  (define (fork f)
    (call/cc
     (lambda (parent)
	   (coroutine (lambda ()
	                (parent #f)))
	   ((thread-activator) f))))
```

\columnsend

The function `coroutine` establishes a context for running the passed thunk; the `fork` function starts the execution of a new coroutine. The implementation uses an internal prompt (`thread-activator`) to establish the scope of the coroutine. The state of a running coroutine is saved as a function in the queue when doing a `yield`, than the next coroutine in the queue is started by `dispatch`. To end a process, we can call the `exit` function, which calls `dispatch` without saving the current process in the queue. `sync` allows to wait until all the processes are finished.

Control operators like `call/cc` make the implementation of coroutines simpler because one can separate the management of queues from the processes.

### Async with coroutines
With the availability of coroutines and `reset`/`shift` we can implement an `async`/`await` expression in Scheme with few lines of code:

```scheme
    (define-syntax async
      (syntax-rules (await)
	    ((async call during-exp ...
	       (await var after-exp ...))
         (let ((var #f))
	        (reset
	          (shift (lambda (k)
				      (k)
		              (fork (lambda () ; <- start coroutine
			                  (set! var (call))
			                  (exit)))))
	          (fork (lambda () during-exp ... (exit))))
	        (sync) ; <- wait until all coroutines finish
	        after-exp ...))))
```

Consider the following example. We need to execute a time consuming function call, which can be a loop or a recursive function processing some data, but we would like to do something else in the meantime.

```scheme
    (define (long-call)
      (let loop ((x 1))
        (if (< x 100)
	      (begin (yield)
	             (display x)
	             (newline)
	             (loop (+ x 1)))
	      42)))
```

Calling the long call with the `async` syntax it is possible to execute other code in a concurrent way. We can put `(yield)` call inside the loop to suspend the execution and resume the next coroutine in the queue. We do the same in the code to be executed at the same time. The effect is that of running two tasks at the same time. The `await` keyword allows to wait for the result of the long call, which is bound to the specified variable (`x` in this example).

The logic is implemented using coroutines, the two expressions to be run concurrently are launched using a `fork`, while the result is awaited using `sync`. `reset`/`shift` allows us to delimit the extent of the continuation to be captured, and to change the order of the executed code.

```scheme
	(display "start async call")
    (newline)
    (async long-call
           (display "do other things in the meantime...")
           (newline)
           (let loop ((x 0))
	         (when (< x 100)
	           (begin (yield)
		              (display (- x))
		              (newline)
		              (loop (+ x 1)))))
           (newline)
      (await x
	    (display "result -> ")
	    (display x)
	    (newline)))
```

The above code prints:

```
	start async call
	do other things in the meantime...
	0
	1
	-1
	2
	-2
	[...]
	98
	-98
	99
	-99

	result -> 42

```

### Async with threads
Using threads instead of coroutine we can avoid adding `(yield)` calls in our code, maintaining the same syntax. Kawa provides a simple interface to create parallel threads: `(future expression)` creates a new thread that evaluates `expression`, while `(force thread)` waits for the thread’s expression to finish executing, and returns the result. Kawa threads are implemented using Java threads.

Thus we can remove `(yield)` calls from our code and redefine the `async`/`await` syntax to use Kawa threads:

```scheme
    (define-syntax async
      (syntax-rules (await)
        ((async call during-exp ...
	       (await var after-exp ...))
         (let ((var #f))
	        (reset
	         (shift (lambda (k)
		          (set! var (future (call))) ; <- start thread
		          (k)))
	         during-exp ...)
	        (set! var (force var)) ; <- wait for result
	        after-exp ...))))
```

Now the two tasks are run in parallel, and their printed output is not deterministic:

```
	start async call
	do other things in the meantime...
	0
	-1
	1
	2
    -2
	[...]
	98
	99
	-98
	-99

	result -> 42

```
\newpage

## Kawa debugger
Instrumentation allows to suspend the execution of a program, store its state, and resume it, even multiple times. Thus, we can exploit the instrumentation performed to obtain first-class continuations in Kawa to implement debugging features. I extended the technique described in Chapters 3-4 to implement a simple debugger.

When you enable the debugging mode, the compiler instruments each atomic expression with debugging calls, and generates code to store variable bindings in an internal table. When the resulting code runs, it stops at breakpoints and lets you step through the program and inspect variables.

As an example, suppose we need to debug this snippet of code:

```scheme
	1 (define (get-first pred lst)
	2   (call/cc
	3     (lambda (return)
	4       (for-each (lambda (x)
	5 		            (if (pred x)
	6 		              (return x)))
	7 	                  lst)
	8     #f)))
    9
   10 (get-first negative? '(1 2 3 4 -5 6 7 8 9)) ; => -5
```

We can add a pausing instruction simply calling the `breakpoint` function, as you can see below:

```scheme
	...
		(for-each (lambda (x)
                     (breakpoint) ; <--
			         (if (pred x)
	...
```

Once the program is run the execution stops at the breakpoint line, opening a terminal that accepts some predefined commands. The following commands are supported:

command       | result
--------------|------------------------
s(tep)        | run for one step
c(ontinue)    | run until the next breakpoint
p(rint) [var] | print a variable
q(uit)        | exit the program

The following listing shows a session of the debugger. In this case the user prints some variable values, then steps forward two times, executing one atomic expression at each step, then continues to stop at the breakpoint at each cycle of the `for-each` until the function returns:

```scheme
	### suspended at line 5 ###
	> print x
	| x: 1
	> print return
	| return: #<continuation>
	> step
	### suspended at line 6 ###
	### after expression
	(Apply line:6:8 (Ref/24/Declaration[applyToArgs/2])
	  (Ref/23/Declaration[pred/101])
	  (Ref/25/Declaration[x/135]))
	###
	> step
	### suspended at line 5 ###
	> print x
	| x: 2
	> continue
	### suspended at line 5 ###
	> continue
	### suspended at line 5 ###
	> print x
	| x: 4
	> print #all
	| get-first: #<procedure get-first>
	| pred: #<procedure negative?>
	| x: 4
	| lst: (1 2 3 4 -5 6 7 8 9)
	| return: #<continuation>
	### suspended at line 5 ###
	> continue
	-5
```

### Implementation details
The debugger works adding suspension instruction between each atomic expression. After A-normalisation the code is already transformed in a form suitable for instrumentation. During the fragmentation and instrumentation pass, needed by `call/cc`, the syntax tree visitor adds the debug instructions. When the execution reaches a breakpoint call the program  is suspended and the user can insert his commands.

The breakpoint call also enables the step mode. Suspension instructions between atomic expressions are disabled during the normal execution, but they are activated when the user gives the `step` command. When the step mode is on, the program stops at each atomic instruction running a simple REPL. When the user gives the `continue` command, the step mode is disabled and the programs can run until the next breakpoint call.

```scheme
	(define (breakpoint . args)
	  (dbg:enableStepMode)
	  (suspend (car args) (cadr args)))

	(define (suspend line sourceLine)
	  (when dbg:stepMode
	    (begin
	      (dbg:printInfo line sourceLine (current-output-port))
	      (let loop ()
		    (let* ((in (read-line))
		           (cmds ((in:toString):split " "))
		           (cmd (string->symbol (cmds 0))))
		      (case cmd
		        ((c continue) (dbg:disableStepMode))
		        ((p print)
		         (if (> cmds:length 1)
				   (begin
				     (print (string->symbol (cmds 1)))
				     (loop))))
		        ((s step) (void))
		        ((q quit exit) (exit))
		        (else (display (string-append
				                   "unknown command "
			                       (cmds 0)))
			          (newline)
			          (loop))))))))
```

Regarding the debugging instrumentation, the main part is performed in the `visitLetExp` method. I generate a new suspend expression, besides another instruction to add the value of the bind variable to the debugger table at runtime. For each let-bind expression, when the user calls the `print` command, he gets the last value of that variable from the table.

```java
protected Expression visitLetExp(LetExp exp, Void ignored) {
    Declaration letDecl = exp.firstDecl();
    Expression continueValue = letDecl.getInitValue();
    String symbol = letDecl.getName();

    if (Compilation.enableDebugger) {
        int lnum = continueValue.getLineNumber();
        String codeLine = continueValue.print();;

		// suspension instruction
        ApplyExp suspend = new ApplyExp(applyRef,
                                       suspendProc,
                                       new QuoteExp(lnum),
                                       new QuoteExp(codeLine));

        // add binding to the debugger binding table
        ApplyExp addVar = new ApplyExp(applyRef,
                                       addBindingProc,
                                       QuoteExp.getInstance(symbol),
                                       new ReferenceExp(letDecl));

    	exp.body = new BeginExp(new Expression[]{addVar,
	                                             suspend,
	                                             exp.body});
    }

	...
```
