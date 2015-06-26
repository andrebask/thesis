# Case studies

## Kawa debugger
`call/cc` and first-class continuations allow to suspend the execution of a program, store its state, and resume it, even multiple times. This, in addition to the instrumentation performed to obtain it in Kawa, can be exploited to implement debugging features. I extended the technique described in Chapters 3-4 to implement a simple debugger.

When the modified code runs, it stops at breakpoints and lets you step through the program and inspect variables. The following command are supported:

command       | result
--------------|------------------------
s(tep)        | run for one step
c(ontinue)    | run until the next breakpoint
p(rint) [var] | print a variable
q(uit)        | exit the program

### Example

### Implementation details

## Asynchronous programming: Async and Await

Asynchronous programming is a programming paradigm that facilitates fast and responsive applications. Asynchronous programming is crucial to avoid the inefficiencies caused by blocking activities, such as the accesses to the web. Access to a web resource or to a huge database can be slow or delayed. If such an activity is blocked within a synchronous process, the entire application is stuck. You can avoid performance bottlenecks and enhance the responsiveness of your application by using asynchronous programming. In an asynchronous process, the application can continue with other work that does not depend on the resource to be accessed until the potentially blocking task finishes. However, traditional techniques for writing asynchronous applications can be complicated, making them difficult to write, debug, and maintain.

We will see in this section how asynchronous programming features can be added to Scheme using coroutines and delimited continuations.

### Coroutines

Coroutines are functions that can be paused and later resumed. They are necessary to build lightweight threads because they provide the ability to change execution context. Coroutines are considered challenging to implement on the JVM, as they are usually implemented using bytecode instrumentation. However, having first-class continuations, becomes painless to implement coroutines. They can indeed by obtained with few lines of code in scheme. The following code is a porting of safe-for-space cooperative threads presented by Biagioni et al. in [@biagioni1998safe]:

```scheme


	;;; queue code
    (define (make-queue)
      (cons '() '()))

    (define (enqueue! queue obj)
      (let ((lobj (list obj)))
        (if (null? (car queue))
	    (begin
	      (set-car! queue lobj)
	      (set-cdr! queue lobj))
	    (begin
	      (set-cdr! (cdr queue) lobj)
	      (set-cdr! queue lobj)))
        (car queue)))

    (define (dequeue! queue)
      (let ((obj (car (car queue))))
        (set-car! queue (cdr (car queue)))
        obj))

    ;;; coroutines code
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

### Shift and Reset

```scheme
	(define (escape f)
      (call/cc (lambda (k)
	         (f (lambda x
		      (apply k x))))))

    (define mk #f)

    (define (abort x) (mk x))

    (define (%reset t)
      (escape (lambda (k)
	        (let ((m mk))
	          (set! mk (lambda (r)
			     (set! mk m)
			     (k r)))
	          (abort (t))))))

    (define (shift h)
      (escape (lambda (k)
	        (abort (h (lambda v
			    (%reset (lambda () (apply k v)))))))))

    (define-syntax reset (syntax-rules ()
        ((reset exp ...)
         (%reset (lambda () exp ...)))))

```

### Async with coroutines

```scheme
	(require "control.scm")
    (require "coroutines.scm")

    (define (async-call)
      (let loop ((x 1))
        (if (< x 10)
	    (begin (yield)
	           (display x)
	           (newline)
	           (loop (+ x 1)))
	    42)))

    (define-syntax async
      (syntax-rules (await)
        ((async call during-exp ... (await var after-exp ...))
         (let ((var !undefined))
	        (reset
	         (shift (lambda (k)
		          (k)
		          (fork (lambda ()
			          (set! var (call))
			          (exit)))))
	         (fork (lambda () during-exp ... (exit))))
	        (sync)
	        after-exp ...))))

    (display "start async call")
    (newline)
    (async async-call
           (display "do other things in the meantime...")
           (newline)
           (let loop ((x 0))
	     (when (< x 10)
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

### Async with threads

```scheme
    (require "control.scm")
    (require "coroutines.scm")

    (define (async-call)
      (let loop ((x 1))
        (if (< x 100)
	    (begin
	      (display x)
	      (newline)
	      (loop (+ x 1)))
	    42)))

    (define-syntax async
      (syntax-rules (await)
        ((async call during-exp ... (await var after-exp ...))
         (let ((var !undefined))
	        (reset
	         (shift (lambda (k)
		          (set! var (future (call)))
		          (k)))
	         during-exp ...)
	        (set! var (force var))
	        after-exp ...))))

    (display "start async call")
    (newline)
    (async async-call
           (display "do other things in the meantime...")
           (newline)
           (let loop ((i 0))
	     (when (< i 100)
	       (begin
	         (display (- i))
	         (newline)
	         (loop (+ i 1)))))
      (await x
	     (display "result -> ")
	     (display x)
	     (newline)))
```

## Prompts

### `call-with-continuation-prompt`

### `call-with-continuation-barrier`
