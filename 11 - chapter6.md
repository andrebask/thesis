# Case studies

## Kawa debugger
Instrumentation allows to suspend the execution of a program, store its state, and resume it, even multiple times. Thus, we can exploit the instrumentation performed to obtain first-class continuations in Kawa to implement debugging features. I extended the technique described in Chapters 3-4 to implement a simple debugger.

When you enable the debugging mode, the compiler instruments each atomic expression with debugging calls, and generates code to store variable declarations in an internal symbol table. When the resulting code runs, it stops at breakpoints and lets you step through the program and inspect variables.

As an example, suppose we need to debug this snippet of code:

```scheme
	1 (define (find-first pred lst)
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


## Asynchronous programming: Async and Await

Asynchronous programming is a programming paradigm that facilitates fast and responsive applications. Asynchronous programming is crucial to avoid the inefficiencies caused by blocking activities, such as the accesses to the web. Access to a web resource or to a huge database can be slow or delayed. If such an activity is blocked within a synchronous process, the entire application is stuck. You can avoid performance bottlenecks and enhance the responsiveness of your application by using asynchronous programming. In an asynchronous process, the application can continue with other work that does not depend on the resource to be accessed until the potentially blocking task finishes. However, traditional techniques for writing asynchronous applications can be complicated, making them difficult to write, debug, and maintain.

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

We will see in this section how asynchronous programming features can be added to Scheme using coroutines and delimited continuations.

### Coroutines

Coroutines are functions that can be paused and later resumed. They are necessary to build lightweight threads because they provide the ability to change execution context. Coroutines are considered challenging to implement on the JVM, as they are usually implemented using bytecode instrumentation. However, having first-class continuations, becomes painless to implement coroutines. They can indeed by obtained with few lines of code in scheme. The following code is a port of safe-for-space cooperative threads presented by Biagioni et al. in [@biagioni1998safe], where the code for managing a queue has been omitted for brevity:

```scheme

    (define process-queue (make-queue))
    (define sync-cont #f)

    (define (coroutine thunk)
      (enqueue! process-queue thunk))

    (define (dispatch)
      (if (null? (car process-queue))
          (when sync-cont (sync-cont))
          ((dequeue! process-queue))))

    (define (exit)
      (dispatch))

    (define (sync)
      (call/cc
       (lambda (k)
         (set! sync-cont k)
         (dispatch))))

    (define (yield)
      (call/cc
       (lambda (k)
         (coroutine (lambda () (k #f)))
         (dispatch))))

    (define (thread-activator)
      (call/cc
       (lambda (k)
         (let ((f (call/cc (lambda (fc) (k fc)))))
         (f)
         (exit)))))

    (define (fork f)
      (call/cc
       (lambda (k)
         (coroutine (lambda () (k #f)))
         ((thread-activator) f))))
```

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
		              (fork (lambda ()   ; <- start call coroutine
			                  (set! var (call))
			                  (exit)))))
	          (fork (lambda () during-exp ... (exit))))
	        (sync)           ; <- wait until all coroutines finish
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

Calling the long call with the `async` syntax it is possible to execute other code in a concurrent way. We can put `(yield)` call inside the loop to suspend the execution and resume the next coroutine in the queue. We do the same in the code to be executed ate the same time. The effect is that of running two tasks at the same time. The `await` keyword allows to wait for the result of the long call, which is bound to the specified variable (`x` in this example).

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
Using threads instead of coroutine we can avoid adding `(yield)` calls in our code, maintaining the same syntax. Kawa provides a simple interface to create parallel threads: `(future expression)` creates a new thread that evaluates expression, while `(force thread)` waits for the thread’s expression to finish executing, and returns the result. Kawa threads are implemented using Java threads.

Thus we can remove `(yield)` calls from our code and redefine the `async`/`await` syntax using Kawa threads

```scheme
    (define-syntax async
      (syntax-rules (await)
        ((async call during-exp ... (await var after-exp ...))
         (let ((var #f))
	        (reset
	         (shift (lambda (k)
		          (set! var (future (call))) ; <- start thread
		          (k)))
	         during-exp ...)
	        (set! var (force var)) ; <- wait for result
	        after-exp ...))))
```

Now the two tasks are run in parallel, and their output is not deterministic:

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
