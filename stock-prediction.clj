(ns push307.core
  (:gen-class))

(comment
  "NAME: Lucas Barusek
   Simple stock prediction using GP. Predicts stock prices
   a certain amount of days in the future using various technical
   indicators as inputs.

   Everything is configurable via the global variable below.
   All configuration settings should work with each other.
  " 
)

(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io]
         ;'[oz.core :as oz]
         )

;;;;;;;;;;
;; Constants and Global Variables
(def TIME-FRAME 10) ; number of days in the future to predict for
(def SIMPLIFY true) ; simplify all programs during each generation 
(def max-steps 5000) ;; max steps for interpreting push programs

;; moved these up here just to have consistency with configuration vars
(def max-generations 150)
(def population-size 300)
(def max-initial-plushy-size 100)

; number of iterations to simplify for. Currently very small since 
; simplification is applied every generation
(def simplification-steps 15) 

; Give programs a small penalty for not using all inputs
(def PENALIZE-NOT-USING-INPUTS false)

; calculate novelties for the individuals, and use multi-objective GP selection
; that uses novelty, such as pareto selection and weighted sum
; note: this causes the program to take a long time, so I would suggest decreasing
;       the program and population sizes
(def USE-NOVELTY false) 
(def k 7) ; k-value for k-nearest neighbors in calculating novelties

(def epsilon 30) ;; epsilon value for epsilon lexicase selection
(def tournament-size 7) ;; tournament size for tournament and weighted sum selection

;; plushy sizes of 1 - 2 seem to small for this problem
(def min-initial-plushy-size 15)
;; chance of adding or deleting an instruction during mutation
(def add-del-chance 0.07)

;;;;;;;;;;
;; Examples

; An example Push state
(def example-push-state
  {:exec '(10 ("hello" 12 3 integer_-) integer_+ integer_-)
   :integer '(14 70 3 4 5 6 7)
   :string '("abc" "def")
   :input {:in1 4 :in2 6}})

; An example Plushy genome
(def example-plushy-genome
  '(3 5 integer_* exec_dup "hello" 4 "world" integer_- close))

; An example Push program
; This is the program tha would result from the above Plushy genome
(def example-push-program
  '(3 5 integer_* exec_dup ("hello" 4 "world" integer_-)))

; An example individual in the population
; Made of a map containing, at mimimum, a program, the errors for
; the program, and a total error
(def example-individual
  {:genome '(3 :in1 integer_* exec_dup "hello" 4 "world" integer_- close)
   :program '(3 :in1 integer_* exec_dup ("hello" 4 "world" integer_-))
   :errors [8 7 6 5 4 3 2 1 0 1]
   :total-error 37})

(def correct-individual
  {:genome  '(in1 in1 in1 integer_* integer_* in1 integer_+ 3 integer_+)
   :program  '(in1 in1 in1 integer_* integer_* in1 integer_+ 3 integer_+)})

(def example-stock-inputs 
  (list 
	false 		;; in1: bool for whether there was good or bad news
	419.07		;; in2: current price
	5.02193E7	;; in3: current volume 
	10.538429 	;; in4: MACD value
	14.864922 	;; in5: MACD Signal value
	53.386925))	;; in6: RSI Value

;;;;;;;;;;
; Instructions must all be either functions that take one Push state and return another
; or constant literals.
; The exception is `close`, which is only used in translation from Plushy to Push

; input funcs in seperate list for determining if a program uses all input values
; all except for input 6 are duplicated to encourage them to be in the program
(def input-funcs (list 'in1 'in2 'in3 'in4 'in5 'in6))

(def stock-instructions (list 
  'in1 'in2 'in3 'in4 'in5 'in6 'in1 'in2 'in3 'in4 'in5 'in6 
  'num_+ 'num_- 'num_* 'num_% 'num_+ 'num_- 'num_* 'num_% 'float_sin 'float_cos 'float_tan 
  'close 'close 'close 'exec_dup 'exec-pop 'exec-if 'exec-if 'exec-if 'exec-if
  'num-greater-than 'num-less-than 'num-greater-than-equal 'num-less-than-equal 'num-equal 'num-not-equal
  30.0 70.0 1.0 2.0 0.0 5.0 10.0 20.0 30.0 ; special constants
  (list 'in6 30 'num-less-than) (list 'in6 70 'num-greater-than) ;; rsi
  (list 'in4 'in5 'num-greater-than) (list 'in4 'in5 'num-less-than) ;; macd 
  (list 'in2 'num_+) (list 'in2 'num_-) (list 'in2 'num_%)
  (list 'in6 30 'num-less-than) (list 'in6 70 'num-greater-than) ;; rsi
  (list 'in4 'in5 'num-greater-than) (list 'in4 'in5 'num-less-than) ;; macd 
  (list 'in2 'num_+) (list 'in2 'num_-) (list 'in2 'num_%)))

; number of code blocks opened by instructions (default = 0)
(def opened-blocks
  {'exec_dup 1
   'exec-if 2
  })

;;;;;;;;;
;; Utilities

(defn get-csv-data
  "Reads in CSV data"
  [file-name]
  (with-open [reader (io/reader file-name)]
	(doall
	  (csv/read-csv reader))))

(defn get-stock-data
  "Performs Data Cleaning from csv data
  Mainly casts input strings to their appropriate types"
  ([] (get-stock-data "teslaOneYear.csv")) 
  ([file-name]
    (map 
      (fn [lst] 
          (conj
            ;; cast all of the floats
            (map #(Float/parseFloat %) (take 5 (rest lst))) 
            ;; cast the boolean input
            (boolean (Boolean/valueOf (last lst))))) 
      (rest (get-csv-data file-name)))))

(def empty-state
  {:exec '()
   :integer '()
   :string '()
   :float '()
   :bool '()
   :input {}})

(defn push-to-stack
  "Pushes item onto stack in state, returning the resulting state."
  [state stack item]
  (assoc state stack (conj (stack state) item)))

(defn pop-stack
  "Removes top item of stack, returning the resulting state."
  [state stack]
  (assoc state stack (rest (stack state))))

(defn empty-stack?
  "Returns true if the stack is empty in state."
  [state stack]
  (empty? (stack state)))

(defn peek-stack
  "Returns top item on a stack. If stack is empty, returns :no-stack-item"
  [state stack]
  (if (empty-stack? state stack) 
    :no-stack-item
    (first (stack state))))

(defn length-of-stack
  "Returns the amount of elements on a given stack"
  [state stack]
  (count (stack state)))

(defn assign-in
  "assigns an input to a value on the input map"
  [state in value]
  (assoc state :input (assoc (:input state) in value)))

(defn assign-all-in-values
  "Takes a list of inputs and associates all the values on the input map"
  [state inputs]
  (loop [inputs inputs
         state state
         index 1]
    (if (empty? inputs)
      state
      (recur
        (rest inputs)
        (assign-in state (keyword (str "in" index)) (first inputs))
        (inc index)))))

(defn add-errors
  "Adds :errors and :total-errors keywords to an individual"
  [individual errors]
  (assoc (assoc individual :errors errors) :total-error (apply +' errors)))

(defn get-args-from-stacks
  "Takes a state and a list of stacks to take args from. If there are enough args
  on each of the desired stacks, returns a map with keys {:state :args}, where
  :state is the new state with args popped, and :args is a list of args from
  the stacks. If there aren't enough args on the stacks, returns :not-enough-args."
  [state stacks]
  (loop [state state
         stacks (reverse stacks)
         args '()]
    (if (empty? stacks)
      {:state state :args args}
      (let [stack (first stacks)]
        (if (empty-stack? state stack)
          :not-enough-args
          (recur (pop-stack state stack)
                 (rest stacks)
                 (conj args (peek-stack state stack))))))))

(defn make-push-instruction
  "A utility function for making Push instructions. Takes a state, the function
  to apply to the args, the stacks to take the args from, and the stack to return
  the result to. Applies the function to the args (taken from the stacks) and pushes
  the return value onto return-stack in the resulting state."
  [state function arg-stacks return-stack]
  (let [args-pop-result (get-args-from-stacks state arg-stacks)]
    (if (= args-pop-result :not-enough-args)
      state
      (let [result (apply function (:args args-pop-result))
            new-state (:state args-pop-result)]
        (push-to-stack new-state return-stack result)))))

;; Integer Stuff

(defn integer_+
  "Adds the top two integers and leaves result on the integer stack.
  If integer stack has fewer than two elements, noops."
  [state]
  (if (< (length-of-stack state :integer) 2) 
    state ;; noop
    (make-push-instruction state +' [:integer :integer] :integer)))

(defn integer_-
  "Subtracts the top two integers and leaves result on the integer stack.
  Note: if `first` is the top integer on the stack, and `second` is the 
  second, the result pushed to the stack should be (second - first)."
  [state]
  (if (< (length-of-stack state :integer) 2) 
    state ;; noop
    (make-push-instruction state -' [:integer :integer] :integer)))

(defn integer_*
  "Multiplies the top two integers and leaves result on the integer stack."
  [state]
  (if (< (length-of-stack state :integer) 2) 
    state ;; noop
    (make-push-instruction state *' [:integer :integer] :integer)))

(defn div-zero-help
  "Replaces first item on integer stack with a 1. This is only called when
  the first item is a zero to avoid dividing by 0. Returns the resulting state"
  [state]
  (push-to-stack (pop-stack state :integer) :integer 1))

(defn make-int
  "Converts first element on the integer stack to an integer. Only called 
  when dividing to avoid floats and ratios."
  [state]
  (let [new-int (bigint (peek-stack state :integer))] 
  (push-to-stack (pop-stack state :integer) :integer new-int)))

(defn integer_%
  "This instruction implements 'protected division'.
  In other words, it acts like integer division most of the time, but if the
  denominator is 0, it returns the numerator, to avoid divide-by-zero errors.
  First item in stack is denominator"
  [state]
  (if (< (length-of-stack state :integer) 2) 
    state ;; noop
    (if (zero? (peek-stack state :integer))
      (make-push-instruction (div-zero-help state) / [:integer :integer] :integer)
      (make-int 
        (make-push-instruction state / [:integer :integer] :integer)))))

;; input stuff
;;  -> I decided to hard code a function for each input to avoid any randomness
;;     when selecting what input to use

(defn in-any
  "Move the value for any input onto the exec stack"
  [state in]
  (assoc state :exec (conj (:exec state) (in (:input state)))))

(defn in1
  "Pushes the input labeled :in1 on the inputs map onto the :exec stack.
  Can't use make-push-instruction, since :input isn't a stack, but a map."
  [state]
  (if (contains? (:input state) :in1) 
    (in-any state :in1)
    state))

(defn in2
  "Pushes the input labeled :in2 on the inputs map onto the :exec stack.
  Can't use make-push-instruction, since :input isn't a stack, but a map."
  [state]
  (if (contains? (:input state) :in2) 
    (in-any state :in2)
    state))

(defn in3
  "Pushes the input labeled :in3 on the inputs map onto the :exec stack.
  Can't use make-push-instruction, since :input isn't a stack, but a map."
  [state]
  (if (contains? (:input state) :in3) 
    (in-any state :in3)
    state))

(defn in4
  "Pushes the input labeled :in4 on the inputs map onto the :exec stack.
  Can't use make-push-instruction, since :input isn't a stack, but a map."
  [state]
  (if (contains? (:input state) :in4) 
    (in-any state :in4)
    state))

(defn in5
  "Pushes the input labeled :in5 on the inputs map onto the :exec stack.
  Can't use make-push-instruction, since :input isn't a stack, but a map."
  [state]
  (if (contains? (:input state) :in5) 
    (in-any state :in5)
    state))

(defn in6
  "Pushes the input labeled :in6 on the inputs map onto the :exec stack.
  Can't use make-push-instruction, since :input isn't a stack, but a map."
  [state]
  (if (contains? (:input state) :in6) 
    (in-any state :in6)
    state))

(defn in7
  "Pushes the input labeled :in7 on the inputs map onto the :exec stack.
  Can't use make-push-instruction, since :input isn't a stack, but a map."
  [state]
  (if (contains? (:input state) :in7) 
    (in-any state :in7)
    state))

(defn in8
  "Pushes the input labeled :in8 on the inputs map onto the :exec stack.
  Can't use make-push-instruction, since :input isn't a stack, but a map."
  [state]
  (if (contains? (:input state) :in8) 
    (in-any state :in8)
    state))

;; float stuff / stock stuff

(defn float_+
  "Adds the top two floats and leaves result on the float stack.
  If float stack has fewer than two elements, noops."
  [state]
  (if (< (length-of-stack state :float) 2) 
    state ;; noop
    (make-push-instruction state +' [:float :float] :float)))

(defn float_-
  "Subtracts the top two integers and leaves result on the integer stack.
  Note: if `first` is the top integer on the stack, and `second` is the 
  second, the result pushed to the stack should be (second - first)."
  [state]
  (if (< (length-of-stack state :float) 2) 
    state ;; noop
    (make-push-instruction state -' [:float :float] :float)))

(defn float_*
  "Multiplies the top two integers and leaves result on the integer stack."
  [state]
  (if (< (length-of-stack state :float) 2) 
    state ;; noop
    (make-push-instruction state *' [:float :float] :float)))

(defn div-zero-help-float
  "Replaces first item on integer stack with a 1. This is only called when
  the first item is a zero to avoid dividing by 0. Returns the resulting state"
  [state]
  (push-to-stack (pop-stack state :float) :float 1))

(defn make-float
  "Converts first element on the integer stack to an integer. Only called 
  when dividing to avoid floats and ratios."
  [state]
  (let [new-int (bigint (peek-stack state :float))] 
  (push-to-stack (pop-stack state :float) :float new-int)))

(defn float_%
  "This instruction implements 'protected division'.
  In other words, it acts like integer division most of the time, but if the
  denominator is 0, it returns the numerator, to avoid divide-by-zero errors.
  First item in stack is denominator"
  [state]
  (if (< (length-of-stack state :float) 2) 
    state ;; noop
    (if (zero? (peek-stack state :float))
      (make-push-instruction (div-zero-help-float state) / [:float :float] :float)
      (make-float 
        (make-push-instruction state / [:float :float] :float)))))

(defn float_sin
  "Take the sine of the top float"
  [state]
  (if (empty-stack? state :float)
    state
    (make-push-instruction state #(Math/sin %) [:float] :float)))

(defn float_tan
  "Take the tangent of the top float"
  [state]
  (if (empty-stack? state :float)
    state
    (make-push-instruction state #(Math/tan %) [:float] :float)))

(defn float_cos
  "Take the cosine of the top float"
  [state]
  (if (empty-stack? state :float)
    state
    (make-push-instruction state #(Math/cos %) [:float] :float)))

(defn exec-if
  "Looks at top value on bool stack; if it is true, then it pops the second
  thing on the exec stack. If it is false, then pop the first thing on the
  exec stack"
  [state]
  (if (or (zero? (length-of-stack state :bool)) (< (length-of-stack state :exec) 2))
	state ;; noop if we don't have the required stack items
	(let [bool (peek-stack state :bool)
		  new-state (pop-stack state :bool)]
		(if bool
            ;; if bool is true then pop the second thing on the exec stack
			(assoc new-state :exec (keep-indexed #(if (not= %1 1) %2) (:exec new-state)))
            ;; else pop the first thing
			(pop-stack new-state :exec)))))

(defn inequality
  "Given some comparison function (<, >, = , etc.), compare the top two numbers
  on the number stacks (float or integer depending on what's available). If the
  comparison is true then push true to the bool stack, else false"
  [state comparison]
  (cond 
    ;; noop
    (and (< (length-of-stack state :integer) 2) (< (length-of-stack state :float) 2))
      state
    ;; compare the two floats
    (>= (length-of-stack state :float) 2)
      (let [val1 (peek-stack state :float)
            val2 (peek-stack (pop-stack state :float) :float)
            new-state (pop-stack (pop-stack state :float) :float)]
          (if (comparison val2 val1)
            (push-to-stack new-state :bool true)
            (push-to-stack new-state :bool false)))
    ;; compare the two integers
    (>= (length-of-stack state :integer) 2)
      (let [val1 (peek-stack state :integer)
            val2 (peek-stack (pop-stack state :integer) :integer)
            new-state (pop-stack (pop-stack state :integer) :integer)]
          (if (comparison val2 val1)
            (push-to-stack new-state :bool true)
            (push-to-stack new-state :bool false)))))

(defn num-less-than
  "Less than"
  [state]
  (inequality state <))

(defn num-less-than-equal
  "Less than or equal to"
  [state]
  (inequality state <=))

(defn num-greater-than
  "Greater than"
  [state]
  (inequality state >))

(defn num-greater-than-equal
  "Greater than or equal to"
  [state]
  (inequality state >=))

(defn num-equal
  "Equality"
  [state]
  (inequality state =))

(defn num-not-equal
  "Equality"
  [state]
  (inequality state not=))

(defn num_+
  "Adds the top two floats and leaves result on the float stack.
  If float stack has fewer than two elements, noops."
  [state]
  (cond 
	(>= (length-of-stack state :float) 2)
    	(make-push-instruction state +' [:float :float] :float)
	(and (>= (length-of-stack state :float) 1) (>= (length-of-stack state :integer) 1))
    	(make-push-instruction state +' [:integer :float] :float)
	(>= (length-of-stack state :integer) 2)
    	(make-push-instruction state +' [:integer :integer] :integer)
	:else	
    	state)) ;; noop

(defn num_-
  "subtracts the top two numbers and leaves result on the float stack.
  If float and int stacks don't have enough values, noops"
  [state]
  (cond 
	(>= (length-of-stack state :float) 2)
    	(make-push-instruction state -' [:float :float] :float)
	(and (>= (length-of-stack state :float) 1) (>= (length-of-stack state :integer) 1))
    	(make-push-instruction state -' [:integer :float] :float)
	(>= (length-of-stack state :integer) 2)
    	(make-push-instruction state -' [:integer :integer] :integer)
	:else	
    	state)) ;; noop

(defn num_*
  "Multiplies the top two numbers and leaves result on the float stack.
  If float and int stacks don't have enough values, noops"
  [state]
  (cond 
	(>= (length-of-stack state :float) 2)
    	(make-push-instruction state *' [:float :float] :float)
	(and (>= (length-of-stack state :float) 1) (>= (length-of-stack state :integer) 1))
    	(make-push-instruction state *' [:integer :float] :float)
	(>= (length-of-stack state :integer) 2)
    	(make-push-instruction state *' [:integer :integer] :integer)
	:else	
    	state)) ;; noop

(defn div-zero-help
  "Replaces first item on  stack with a 1. This is only called when
  the first item is a zero to avoid dividing by 0. Returns the resulting state"
  [state stack]
  (push-to-stack (pop-stack state stack) stack 1))

(defn make-num
  "Converts first element on the stack to an float. Only called 
  when dividing to avoid ratios."
  [state stack]
  (let [new-int (bigdec (peek-stack state stack))] 
	(push-to-stack (pop-stack state stack) stack new-int)))

(defn num_%
  "This instruction implements 'protected division'.
  In other words, it acts like division most of the time, but if the
  denominator is 0, it returns the numerator, to avoid divide-by-zero errors.
  First item in stack is denominator"
  [state]
  (cond 
	(>= (length-of-stack state :float) 2)
	  (if (zero? (peek-stack state :float))
      (make-push-instruction (div-zero-help state :float) / [:float :float] :float)
      (make-push-instruction state / [:float :float] :float))
	(>= (length-of-stack state :integer) 2)
	  (if (zero? (peek-stack state :integer))
      (make-push-instruction (div-zero-help state :integer) / [:integer :integer] :integer)
      (make-push-instruction state / [:float :integer] :float))
    :else 
		state)) ;; noop

(defn exec_dup
  "Implements exec_dup, which duplicates the next instruction on the 
  exec stack"
  [state]
  (if (empty-stack? state :exec)
    state
    (push-to-stack state :exec (first (:exec state)))))

(defn exec-pop
  "Pops element on exec stack"
  [state]
  (pop-stack state :exec))

;;;;;;;;;
;; Interpreter

(defn interpret-one-step
  "Helper function for interpret-push-program.
  Takes a Push state and executes the next instruction on the exec stack,
  or if the next element is a literal, pushes it onto the correct stack.
  Or, if the next element is a nested list, needs to unwrap that list onto
  the exec stack.
  Returns the new Push state."
  [state]
  (let [next-exec (peek-stack state :exec) 
        new-state (pop-stack state :exec)]
  (cond
    ;; if it's a literal push it on the approriate stack
    (boolean? next-exec) (push-to-stack new-state :bool next-exec)
    (float? next-exec) (push-to-stack new-state :float next-exec)
    (int? next-exec) (push-to-stack new-state :float next-exec)
    (string? next-exec) (push-to-stack new-state :string next-exec)
    ;; if it's a keyword (input), get the inputs value and put it on the exec stack
    (keyword? next-exec) (push-to-stack new-state :exec (next-exec (:input new-state)))
    ;; if it's a list then loop through all the items putting them on the exec stack
    (list? next-exec) 
      (loop [items (reverse next-exec) state new-state]
        (if (empty? items) state
          (recur (rest items) (push-to-stack state :exec (first items)))))
    ;; otherwise it must be a function that we need to execute on
    :else ((eval next-exec) new-state))))

(defn program-to-exec
  "Takes a push program and adds all of the instructions to the exec stack
  in reverse order"
  [program state]
  (loop [program (reverse program) 
         state state]
    (if (empty? program) 
      state
      (recur (rest program) (push-to-stack state :exec (first program))))))

(defn interpret-push-program
  "Runs the given program starting with the stacks in start-state. Continues
  until the exec stack is empty. Returns the state of the stacks after the
  program finishes executing.
   To avoid infinite execution, you will need to enforce some maximum number
  of interpreter steps before terminating the program. You can choose this limit."
  [program start-state]
  (loop [state (program-to-exec program start-state) 
         steps 0] 
    (cond 
      (= steps max-steps) state
      (empty-stack? state :exec) state
      :else (recur (interpret-one-step state) (inc steps)))))
  
;;;;;;;;;
;; Translation from Plushy genomes to Push programs

(defn translate-plushy-to-push
  "Returns the Push program expressed by the given plushy representation."
  [plushy]
  (let [opener? #(and (vector? %) (= (first %) 'open))] ;; [open <n>] marks opened-blocks
    (loop [push () ;; iteratively build the Push program from the plushy
           plushy (mapcat #(if-let [n (get opened-blocks %)] [% ['open n]] [%]) plushy)]
      (if (empty? plushy)       ;; maybe we're done?
        (if (some opener? push) ;; done with plushy, but unclosed open
          (recur push '(close)) ;; recur with one more close
          push)                 ;; otherwise, really done, return push
        (let [i (first plushy)]
          (if (= i 'close)
            (if (some opener? push) ;; process a close when there's an open
              (recur (let [post-open (reverse (take-while (comp not opener?)
                                                          (reverse push)))
                           open-index (- (count push) (count post-open) 1)
                           num-open (second (nth push open-index))
                           pre-open (take open-index push)]
                       (if (= 1 num-open)
                         (concat pre-open [post-open])
                         (concat pre-open [post-open ['open (dec num-open)]])))
                     (rest plushy))
              (recur push (rest plushy))) ;; unmatched close, ignore
            (recur (concat push [i]) (rest plushy)))))))) ;; anything else

;;;;;;;;;;
;; The error function
;; The functions below are specific to a particular problem.
;; A different problem would require replacing these functions.
;; Problem: f(x) = x^3 + x + 3

(defn target-function
  "Target function: f(x) = x^3 + x + 3
  Should literally compute this mathematical function."
  [x]
  (+ (* x x x) x 3))

(defn abs-value
  "Returns absolute value of a number. Math/abs was giving me trouble"
  [x]
  (max x (- x)))

(defn not-all-inputs-used?
  "Helper function that returns false if not all inputs are used in a function"
  [program]
  (let [unique-prog (set (flatten program))]
    (loop [inputs (set input-funcs)]
      (cond
        (empty? inputs) false
        (not (contains? unique-prog (first inputs))) true
        :else (recur (rest inputs))))))
 
;; test cases
(def stock-test-cases (get-stock-data "teslaOneYear.csv"))
(def regression-test-cases (map #(- % 25) (range 50)))

(defn regression-error-function
  "Takes an individual and evaluates it on some test cases.
  This will need to translate each individual's Plushy genome into a Push
  program before executing the Push program (see translate-plushy-to-push).
  For each test case,
  runs program with the input set to :in1 in the :input map part of the Push state.
  Then, the output is the integer on top of the integer stack in the Push state
  returned by the interpreter. Computes each error by comparing output of
  the program to the correct output.
  Returns the individual with :errors set to the list of errors on each case,
  and :total-error set to the sum of the errors. You may also want to set
  :program to be the Push program translated from the Plushy genome, though
  this isn't mandatory.
  Note: You must consider what to do if the program doesn't leave anything
  on the integer stack."
  [individual]
  (loop [test-cases regression-test-cases
         errors '[]]
    (if (empty? test-cases) 
      (add-errors individual errors) ;; no more test cases, update individual
      (let [input (first test-cases) ;; else evalute current test case
            eval-state (interpret-push-program (:program individual) 
                          (assign-in empty-state :in1 input))]
          ;; if the program doesn't leave anything on the integer stack, then
          ;; make it's total error infinity
          ;; no value on integer stack should NEVER be chosen as a viable parent
          (recur (rest test-cases) ;; add difference between target and computed 
                 (conj errors (abs-value (- (target-function input) 
                   (if (empty-stack? eval-state :integer) 
                     ##Inf
                     (peek-stack eval-state :integer))))))))))

(defn stock-error-function
  "Takes an individual and evaluates it on some test cases.
  It translates each individual's Plushy genome into a Push
  program before executing the Push program (see translate-plushy-to-push).
  For each test case,
  runs program by setting all 6 inputs in the :input map part of the Push state.
  Then, the output is the float on top of the float stack in the Push state
  returned by the interpreter. 
  Computes error by comparing the predicted price with the price TIME-FRAME
  days in the future.
  Returns the individual with :errors set to the list of errors on each case,
  if there is nothing on the float stack, then it gives that individual an error
  of infinity"
  [individual]
  (let [test-cases stock-test-cases]
    (loop [errors '[]
           test-case 0]
      (if (= (- (count test-cases) test-case) TIME-FRAME) 
        (add-errors individual errors) ;; no more test cases, update individual
        ;; inputs is a list of information about the stock on a given day
        ;; the price of the stock on that day is always the first input
        (let [inputs (nth test-cases test-case)
              eval-state (interpret-push-program (:program individual)
                            (assign-all-in-values empty-state inputs))
              correct-price (second (nth test-cases (+ test-case TIME-FRAME)))]
              ;; check float stack for values
              (recur
                (conj errors (abs-value (- correct-price 
                  (if PENALIZE-NOT-USING-INPUTS ;; global variable
                    (cond
                      (empty-stack? eval-state :float) ##Inf
                      ; penalize program for not using all inputs
                      (not-all-inputs-used? (:program individual)) 
                          (+ 1.0 (peek-stack eval-state :float))
                      :else (peek-stack eval-state :float))
                    (if (empty-stack? eval-state :float)
                      ##Inf
                      (peek-stack eval-state :float))))))
                (inc test-case)))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;        GP          ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;
;; Novelty

(defn euclidean-distance
  "Given two collections of equal length, calculates the euclidean distance"
  [col1 col2]
  (loop [col1 col1
         col2 col2
         errors '()]
    (if (zero? (count col1))
      (Math/sqrt (reduce +' errors)) ;; square root of sum of errors
      (let [diff (- (first col1) (first col2))]
        (recur
          (rest col1)
          (rest col2)
          (conj errors (* diff diff)))))))

(defn k-nearest-neighbors
  "Given an individual returns the euclidean distances of the k-nearest 
  neighbors in terms of total error"
  [ind population]
  (take k (sort < (map #(euclidean-distance (:errors %) (:errors ind)) population))))

(defn get-novelty
  [ind population]
  (assoc ind :novelty (/ (reduce + (k-nearest-neighbors ind population)) k)))

;;;;;;;;
;; variation and selection
    
(defn make-random-plushy-genome
  "Creates and returns a new plushy genome. Takes a list of instructions and
  a maximum initial Plushy genome size."
  [instructions max-initial-plushy-size]
    (let [plushy-size (+ min-initial-plushy-size 
                         (rand-int max-initial-plushy-size))]
      (repeatedly plushy-size #(rand-nth instructions))))

(defn get-individual
  "Returns first individual found with given error"
  [error tournament]
  (loop [tournament tournament]
    (if (= error (:total-error (first tournament))) 
      (first tournament)
      (recur (rest tournament)))))

(defn find-ind-with-better-novelty
  [novelty index population]
  (cond
    (> (:novelty (nth population index)) novelty) (list (nth population index) index)
    (= (+ index 1) (count population)) nil
    :else (recur novelty
                 (inc index)
                 population)))

(defn pareto-selection
  [population]
  (let [sorted-by-error-pop (sort #(< (:total-error %1) (:total-error %2)) population)]
    (loop [current-ind (first sorted-by-error-pop)
           pareto-front '(current-individual)
           index 1]
      (let [next-ind (find-ind-with-better-novelty 
                       (:novelty current-ind) index sorted-by-error-pop)]
        (if (not next-ind)
          (rand-nth pareto-front)
          (recur (first next-ind)
                 (conj pareto-front (first next-ind))
                 (second next-ind)))))))
            
(defn weighted-sum-fitness
  "Simple weighted sum fitness in the form of:
      (a * error) + b * (- novelty)
      where a and b are constants
  Note: this fitness is something that should be minimized"
  [individual]
  (+ 
    (* 1.4 (:total-error individual)) 
    (* 4 (- (:novelty individual)))))

(defn weighted-sum-selection
  [population]
  (let [tournament (repeatedly tournament-size #(rand-nth population))]
    (loop [tournament (rest tournament)
           best-ind (first tournament)
           best-fitness (weighted-sum-fitness best-ind)]
      (if (zero? (count tournament)) 
        best-ind
        (let [curr-ind (first tournament)
              curr-fitness (weighted-sum-fitness curr-ind)]
          (if (< curr-fitness best-fitness)
            (recur 
              (rest tournament)
              curr-ind
              curr-fitness)
            (recur 
              (rest tournament)
              best-ind
              best-fitness)))))))

(defn tournament-selection
  "Selects an individual from the population using a tournament. Returned 
  individual will be a parent in the next generation. Can use a fixed
  tournament size."
  [population]
  (let [tournament (repeatedly tournament-size #(rand-nth population))]
    ;; Of the parents, return the individual with smallest error
    (get-individual (apply min (map #(:total-error %) tournament)) tournament)))


(defn epsilon-lexicase-selection
  "Implements epsilon-lexicase selection for selecting parents
  Randomly shuffles the order that we evaluate test cases, and looks at each 
  individuals error on that test case. If any individual has an error that is 
  epsilon more than the individual with the best error, they are eliminated.
  Returns the individual that survives the most test cases, or if many 
  individuals survive all the test cases, pick a random one."
  [population]
  (loop [population population 
         ;; shuffle the test-cases. Since the errors are already computed,
         ;; just shuffle the indices that we will select errors from
         test-cases (shuffle (range 0 (count (:errors (first population)))))
         curr-case (first test-cases)
         ;; calculate the best error on the current test case
         best-score (apply min (map #(nth (:errors %) curr-case) population))] 
    (cond 
      ;; returns individual that survives the longest
      (= (count population) 1) (first population)
      ;; returns random individual of those that survived all of the test cases
      (= (count test-cases) 1) (rand-nth population)
      :else
		(let [filtered (filter ;; filter those out that have too high an error
                         #(< (abs-value (- best-score (nth (:errors %) curr-case))) 
                             epsilon) 
                         population)] 
          (recur
            (if (= best-score ##Inf) ;; if the best score is inf, then don't filter  
              population
              filtered)
            (rest test-cases)
            (second test-cases)
            (if (= best-score ##Inf) ;; if the best score is inf, then don't filter  
              (apply min (map #(nth (:errors %) (second test-cases)) population))
              (apply min (map #(nth (:errors %) (second test-cases)) filtered))))))))

(defn insert-by-chance
  "Inserts the item into the given collection if a random probability is less
  than the given chance. Else just returns the collection"
  [coll chance item]
  (if (< (rand) chance)
    (conj coll item)
    coll))

(defn crossover
  "Crosses over two Plushy genomes (note: not individuals) using uniform crossover.
  Returns child Plushy genome."
  [prog-a prog-b]
  (loop [prog-a (reverse prog-a) 
         prog-b (reverse prog-b) 
         child '()]
    (cond
      ;; both programs are empty, so return the child
      (and (empty? prog-a) (empty? prog-b)) child
      ;; one of the programs are empty, so take gene from other 50% of the time
      (empty? prog-a) (recur '() (rest prog-b) (insert-by-chance child 0.5 (first prog-b)))
      (empty? prog-b) (recur (rest prog-a) '() (insert-by-chance child 0.5 (first prog-a)))
      :else ;; each parent has a 50% of chance of passing along their genes
        (if (< (rand) 0.5)
          (recur (rest prog-a) (rest prog-b) (conj child (first prog-a)))  
          (recur (rest prog-a) (rest prog-b) (conj child (first prog-b)))))))

(defn uniform-addition
  "Randomly adds new instructions before every instruction (and at the end of
  the Plushy genomes) with some probability. Returns child Plushy genome."
  [prog instructions]
  (loop [child '() 
         prog (reverse prog)]  
    ;; if we are at the end of the genome, add an instruction with probability
    (if (empty? prog) 
      (if (< (rand) add-del-chance)
        (conj child (rand-nth instructions))
        child)
      ;; if we are not at the end, either add a new random instruction and the 
      ;; current instruction in the parent, or just the parent instruction
      (if (< (rand) add-del-chance)
        (recur (conj (conj child (first prog)) (rand-nth instructions)) (rest prog))
        (recur (conj child (first prog)) (rest prog))))))

(defn uniform-replacement
  "Each instruction in the program has an add-del-chance chance of being 
  replaced with a random instruction. Returns the resulting Plushy genome"
  [prog instructions]
  (loop [child '() 
         prog (reverse prog)]  
    (if (empty? prog) 
      child
      (if (< (rand) add-del-chance) 
        ;; replace current instruction with random one
        (recur (conj child (rand-nth instructions)) (rest prog)) ;; replace
        ;; or just keep it
        (recur (conj child (first prog)) (rest prog))))))

(defn uniform-deletion
  "Randomly deletes instructions from Plushy genomes at some rate. Returns
   child Plushy genome."
  [prog]
  (filter #(and (> (rand) add-del-chance) (= % %)) prog))

(defn generate-individual
  "Given a plushy genome, Creates a new individual, translating the push program 
  and calculating the errors"
  [genome error-func]
  (error-func {:genome genome :program (translate-plushy-to-push genome)}))

(defn variation
  [parent-a parent-b error-func instructions]
  (let [choice (rand)]
    (cond  
      (< choice 0.15)
        (generate-individual (uniform-addition (:genome parent-a) instructions) error-func)
      (< choice 0.30) 
        (generate-individual (uniform-replacement (:genome parent-a) instructions) error-func)
      (< choice 0.45)
        (generate-individual (uniform-deletion (:genome parent-a)) error-func) 
      :else         
        (generate-individual (crossover (:genome parent-a) (:genome parent-b)) error-func))))

(defn select-and-vary
  "Selects parent(s) from population and varies them, returning
  a child individual (note: not program/genome). Chooses which genetic operator
  to use probabilistically. Gives 50% chance to crossover,
  25% to uniform-addition, and 25% to uniform-deletion."
  [population instructions error-func]
  (if USE-NOVELTY
    ;; calculate novelty values 
    (variation
      (weighted-sum-selection population)
      (pareto-selection population)
      error-func
      instructions)
    (variation 
      (epsilon-lexicase-selection population)
      (tournament-selection population)
      error-func
      instructions)))

(defn remove-random
  "Given a list remove a random element from it"
  [lst]
  (let [index (rand-int (count lst))]
    (keep-indexed #(if (not= index %1) %2) lst)))

(defn simplify
  "Implements plushy simplification. Iterates simplification-steps number
  of times. Each iteration, randomly removes a small amount of instructions.
  If the resulting program is behaviorally equivalent, replaces the current
  individual with that simplified individual"
  [orig error-func]
  (loop [num-steps simplification-steps
         num-to-remove (inc (rand-int 2))
         ind orig]
          ;; remove between 1 and 5 things
    (if (zero? num-steps) 
      ind
      (let [simp (generate-individual 
                    ;; randomly removes num-to-remove items from the list
                    (nth (iterate remove-random (:genome ind)) num-to-remove)
                    error-func)]
        (recur
          (dec num-steps)
          (inc (rand-int 2))
          ;; if the total error of the simplified program is the same as the
          ;; current individual, replace the current individual with the 
          ;; simplified one. Else just keep the current one
          (if (<= (:total-error simp) (:total-error ind))
            simp
            ind))))))

(defn get-best-individual
  [population]
  (get-individual (apply min (map #(:total-error %) population)) population))

(defn report
  "Reports information on the population each generation. Should look something
  like the following (should contain all of this info; format however you think
  looks best; feel free to include other info).

  -------------------------------------------------------
                 Report for Generation 3
  -------------------------------------------------------
  Best program: ...  
  Best program size: 33
  Best total error: 727
  Best errors: (117 96 77 60 45 32 21 12 5 0 3 4 3 0 5 12 21 32 45 60 77)
  "
  [population generation]
  (let [best (get-best-individual population)]
  (println (str "\n\n----- Report for Generation " generation " -----"))
  (println (str "Best Program: " (list (:program best))))
  (println (str "Best Program size: " (count (flatten (:program best)))))
  (println (str "Best Total Error: " (:total-error best)))
  (println (str "Best Total Error per Day: " (/ (:total-error best) (count (:errors (first population))))))
  (println (str "Average Program Size: " (float (/ 
                                           (apply + (map #(count (flatten (:program %))) population)) 
                                           (count population)))))
  (:total-error best)))

(defn create-initial-population
  "Creates an initial population of random programs. Each program will be
  less than max-size size."
  [pop-size max-size instructions]
  (loop [num-left (dec pop-size) 
         population '()]
    (if (zero? num-left) population
      (let [genome (make-random-plushy-genome instructions max-size)]
      (recur (dec num-left)
             (conj population
                 {:genome genome
                  :program (translate-plushy-to-push genome)}))))))
                  
(defn push-gp
  "Main GP loop. Initializes the population, and then repeatedly
  generates and evaluates new populations. Stops if it finds an
  individual with 0 error (and should return :SUCCESS, or if it
  exceeds the maximum generations (and should return nil). Should print
  report each generation.
  --
  The only argument should be a map containing the core parameters to
  push-gp. The format given below will decompose this map into individual
  arguments. These arguments should include:
   - population-size
   - max-generations
   - error-function
   - instructions (a list of instructions)
   - max-initial-plushy-size (max size of randomly generated Plushy genomes)"
  [{:keys [population-size 
           max-generations 
           error-function 
           instructions 
           max-initial-plushy-size]
    :as argmap}]
  ;; initial population is a randomly generated
  (loop [population (map error-function
          (create-initial-population population-size max-initial-plushy-size instructions))
         generation 0]
    (cond 
      ;; report returns the total error of the best program
      (zero? (report population generation)) :SUCCESS
      (= generation max-generations) nil ;; we exceded generation limit
      :else ;; create a new population and recursively call
        (recur 
          (repeatedly population-size 
            (if SIMPLIFY 
              #(simplify (select-and-vary 
                          (if USE-NOVELTY 
                            (map (fn [ind] (get-novelty ind population)) population)
                            population)
                          instructions error-function)
                         error-function)
              #(select-and-vary 
                 (if USE-NOVELTY
                   (map (fn [ind] (get-novelty ind population)) population) 
                   population)
                 instructions error-function)))
          (inc generation)))))
;;;;;;;;;;
;; Plotting utilities

;;;;;;;;;;
;; The main function call
;; You can call this in a REPL, or alternatively from the command line
;; by running:
;;   lein run

(defn -main
  "Runs push-gp, giving it a map of arguments."
  [& args]
  (binding [*ns* (the-ns 'push307.core)]
    ; The above line is necessary to allow `lein run` to work
    (push-gp {:instructions stock-instructions
              :error-function stock-error-function
              :max-generations 125
              :population-size 300
              :max-initial-plushy-size 100})))

(defn main
  []
  (push-gp {:instructions stock-instructions
            :error-function stock-error-function
            :max-generations max-generations
            :population-size population-size
            :max-initial-plushy-size max-initial-plushy-size}))

(comment
  (oz/view!
    {:data {:values plotting-data}
     :encoding {:x {:field "generation" :type "quantitative"}
                :y {:field "error" :type "quantitative"}
                :color {:field "item" :type "nominal"}}
     :mark "line"}))

(println (main))
