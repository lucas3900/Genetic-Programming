(ns push307.core
  (:gen-class))

(comment
  "NAME: Lucas Barusek
  Implementation of symbolic regression.
  Uses push and plushy genomes to for program representation.
  Uses tournament selection to select parents.
  Implements addition, deletion, and crossover as variatio operators.
  Hyper-Parameters are defined as global constants.
  " 
)

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
   :program  '(in1 in1 in1 integer_* integer_* in1 integer_+ 3 integer_+)}
)


;;;;;;;;;;
; Instructions must all be either functions that take one Push state and return another
; or constant literals.
; The exception is `close`, which is only used in translation from Plushy to Push

(def default-instructions
  (list
   'in1
   'integer_+
   'integer_-
   'integer_*
   'integer_%
   'exec_dup
   'close
   0
   1))

; number of code blocks opened by instructions (default = 0)
(def opened-blocks
  {'exec_dup 1})

;;;;;;;;;
;; Utilities

(def empty-state
  {:exec '()
   :integer '()
   :string '()
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

;;;;;;;;;
;; Instructions

(defn in1
  "Pushes the input labeled :in1 on the inputs map onto the :exec stack.
  Can't use make-push-instruction, since :input isn't a stack, but a map."
  [state]
  (assoc state :exec (conj (:exec state) (:in1 (:input state)))))

(defn integer_+
  "Adds the top two integers and leaves result on the integer stack.
  If integer stack has fewer than two elements, noops."
  [state]
  (if (< (length-of-stack state :integer) 2) state
  (make-push-instruction state +' [:integer :integer] :integer)))

(defn integer_-
  "Subtracts the top two integers and leaves result on the integer stack.
  Note: if `first` is the top integer on the stack, and `second` is the 
  second, the result pushed to the stack should be (second - first)."
  [state]
  (if (< (length-of-stack state :integer) 2) state
  (make-push-instruction state -' [:integer :integer] :integer)))

(defn integer_*
  "Multiplies the top two integers and leaves result on the integer stack."
  [state]
  (if (< (length-of-stack state :integer) 2) state
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
  (if (< (length-of-stack state :integer) 2) state
    (if (zero? (peek-stack state :integer))
      (make-push-instruction (div-zero-help state) / [:integer :integer] :integer)
      (make-int (make-push-instruction state / [:integer :integer] :integer)))))

(defn exec_dup
  "Implements exec_dup, which duplicates the next instruction on the 
  exec stack"
  [state]
  (if (empty-stack? state :exec)
    state
    (push-to-stack state :exec (first (:exec state)))))

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
  (let [next-exec (peek-stack state :exec) new-state (pop-stack state :exec)]
  (cond
    ;; if it's a literal push it on the approriate stack
    (int? next-exec) (push-to-stack new-state :integer next-exec)
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
  (let [reverse-program (reverse program)]
    (loop [program (reverse program) state state]
      (if (empty? program) state
      (recur (rest program) (push-to-stack state :exec (first program)))))))

;; 100% arbitrary
(def max-steps 5000)

(defn interpret-push-program
  "Runs the given program starting with the stacks in start-state. Continues
  until the exec stack is empty. Returns the state of the stacks after the
  program finishes executing.
   To avoid infinite execution, you will need to enforce some maximum number
  of interpreter steps before terminating the program. You can choose this limit."
  [program start-state]
  (loop [state (program-to-exec program start-state) steps 0] 
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
  ;; test cases are from -50 to 50
  (loop [test-cases (map #(- % 50) (range 101)) errors '[]]
    (if (empty? test-cases) 
      (add-errors individual errors) ;; no more test cases, update individual
      (let [input (first test-cases) ;; else evalute current test case
            eval-state (interpret-push-program (:program individual) 
                       (assign-in empty-state :in1 input))]
        (if (empty-stack? eval-state :integer) 
          ;; if the program doesn't leave anything on the integer stack, then
          ;; make it's total error infinity
          ;; no value on integer stack should NEVER be chosen as a viable parent
          (add-errors individual '[##Inf]) 
          (recur (rest test-cases) ;; add difference between target and computed 
                 (conj errors (abs-value (- (target-function input) 
                                 (peek-stack eval-state :integer))))))))))


;;;;;;;;;
;; GP

;; plushy sizes of 1 - 2 seem to small for this problem
(def min-initial-plushy-size 3)
;; chance of adding or deleting an instruction during mutation
(def add-del-chance 0.07)
;; number of individuals selected during parent selection
(def tournament-size 15)

(defn make-random-plushy-genome
  "Creates and returns a new plushy genome. Takes a list of instructions and
  a maximum initial Plushy genome size."
  [instructions max-initial-plushy-size]
    (let [plushy-size (+ min-initial-plushy-size (rand-int max-initial-plushy-size))]
      (repeatedly plushy-size #(rand-nth instructions))))

(defn get-individual
  "Returns first individual found with given error"
  [error tournament]
  (loop [tournament tournament]
    (if (= error (:total-error (first tournament))) (first tournament)
        (recur (rest tournament)))))

(defn tournament-selection
  "Selects an individual from the population using a tournament. Returned 
  individual will be a parent in the next generation. Can use a fixed
  tournament size."
  [population]
  (let [tournament (repeatedly tournament-size #(rand-nth population))]
    ;; Of the parents, return the individual with smallest error
    (get-individual (apply min (map #(:total-error %) tournament)) tournament)))

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
  (loop [prog-a (reverse prog-a) prog-b (reverse prog-b) child '()]
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
  (loop [child '() prog (reverse prog)]  
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

(defn select-and-vary
  "Selects parent(s) from population and varies them, returning
  a child individual (note: not program/genome). Chooses which genetic operator
  to use probabilistically. Gives 50% chance to crossover,
  25% to uniform-addition, and 25% to uniform-deletion."
  [population instructions error-func]
  (let [parent-a (tournament-selection population) 
        parent-b (tournament-selection population) choice (rand)]
    (cond  
      (< choice 0.25) ;; addition 25% of time
        (generate-individual (uniform-addition (:genome parent-a) instructions) error-func)
      (< choice 0.50) ;; deletion 25% of time
        (generate-individual (uniform-deletion (:genome parent-a)) error-func) 
      :else           ;; crossover 50% of time
        (generate-individual (crossover (:genome parent-a) (:genome parent-b)) error-func))))

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
  (let [errors (map #(:total-error %) population) 
        best (get-individual (apply min errors) population)]
  (println (str "\n\n----- Report for Generation " generation " -----"))
  (println (str "Best Program: " (list (:program best))))
  (println (str "Best Program size: " (count (flatten (:program best)))))
  (println (str "Best Total Error: " (:total-error best)))
  (println (str "Best errors: " (:errors best)))
  (:total-error best)))

(defn create-initial-population
  "Creates an initial population of random programs. Each program will be
  less than max-size size."
  [pop-size max-size instructions]
  (loop [num-left (dec pop-size) population '()]
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
  [{:keys [population-size max-generations error-function instructions max-initial-plushy-size]
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
        (recur (repeatedly population-size 
                #(select-and-vary population instructions error-function))
               (inc generation)))))

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
    (push-gp {:instructions default-instructions
              :error-function regression-error-function
              :max-generations 500
              :population-size 200
              :max-initial-plushy-size 50})))

(defn main
  []
  (push-gp {:instructions default-instructions
            :error-function regression-error-function
            :max-generations 500
            :population-size 200
            :max-initial-plushy-size 15}))
