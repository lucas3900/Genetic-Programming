;; Lucas Barusek
;; September 26, 2021

; (ns homework2.core)

;; Question 1

;; infinite sequence of random rolls
(def random-rolls (repeatedly #(+ 1 (rand-int 6))))

(defn index-of-five-6s
  "Given a sequence of integers, returns the first index at which
  a string of 5 6s in a row is found"
  [nums]
  (loop [nums nums num-6s 0 real-index 0 index-6s 0]
    (cond
    (= num-6s 5) index-6s
    (= (first nums) 6) (recur (rest nums) (inc num-6s) (inc real-index) index-6s)
    :else (recur (rest nums) 0 (inc real-index) (inc real-index)))))

(println (index-of-five-6s random-rolls)) 
(println (index-of-five-6s '(1 2 3 4 6 6 6 6 1 2 3 1 2 3 6 6 6 6 6))) 

;; Question 2

(defn get-next-digit
  "Given a dividen and divisor, return the next digit in the decimal
  along with the remainder, just like in long division"
  [dividend divisor]
  (let [digit (int (/ dividend divisor))] 
    ;; return a 2-vector containing the digit and the leftover
    [digit (int (- dividend (* digit divisor)))])) ;; cast to avoid big-ints

(defn get-dec-lazy
  "Recursive function that generate a lazy function of the decimal place,
   one decimal at a time"
  [dividend divisor]
  (let [[next-digit leftover] (get-next-digit dividend divisor)]
	(lazy-seq (cons next-digit (get-dec-lazy (* 10 leftover) divisor)))))

(defn ratio->infinite-seq 
  "given a ratio between 0 and 1, returns the infinite sequence consisting
  of its digits after the decimal, one at a time"
  [ratio]
  (get-dec-lazy (* 10 (numerator ratio)) (denominator ratio)))

(println (take 20 (ratio->infinite-seq 2/7)))
(println (take 20 (ratio->infinite-seq 3/8)))
(println (take 20 (ratio->infinite-seq 1/111)))

;; Question 3

;; Knapsack problem

"
I considered a lot of things when implementing hill climbing for the 
knapsack problem. Firstly, I decided how to determine on whether
one sack if preferable over another. The most obvious hueristic
would be to use total value of the taken items. However this could
run into issues where the algorithm could just greedily pick the best items
by value completely ignoring weight, and inefficiently fill up the sack. The 
other thing I considered was value-per-weight, but this has problems because
a very light object could have a great value-per-weight, causing the algorithm
not to add anything because it would decrease the value-per-weight, even
though the overall value increase. So I decided to implement both and compare 
them. Overall, total-value performed better; it had better min and average
values for the value of the sack, and it had a much smaller standard deviation.
This is probably because this is a very basic implementation of the knapsack
problem, and this algorithm can better handle just maximizing the main goal
of the problem, rather than maximizing something adjacent to it, such 
as value by weight.

The next thing I considered was how to mutate the sack. The three things I 
ended up doing were adding an item, replacing an item, and creating a new
random bag. Adding and replacing an item were primarily used by the algorithm
when eye-balling it. I included creating a random bag to avoid getting stuck
in local optima, and pseudo simulating multi-restart. Since it's a hill
climbing algo, it only chooses the mutated bag if it's better than the 
current bag, so generating a new random bag would likely only get used
in the beginning. Additionally, I chose not to include removing an item,
because this would never have an affect when using total-value as a hueristic,
and generating a new bag would also pseudo act as remove.

For choosing when to stop, I just picked a constant number of iterations
(num-runs). Increasing this number passed 100 didn't seem to make much of
a difference on the examples I tried. Overall the algorithm performs 
pretty well on the sample inputs I gave it, finding the optimal solution
or near the optimal solution with both heuristics. As I previously stated,
total-value outperform value-per-weight in all metrics.
"
(def num-runs 100) ;; step limit for hill climbing

(defn toggle-taken-status
  "toggles whether an item is taken or no"
  [items index]
  (if (empty? items) 
    items ;; if the list is empty, then there are no free items
    (assoc items index (update (nth items index) :taken #(not %)))))

(defn get-taken-items
  "returns a vector containing all taken items"
  [items]
  (vec (filter #(:taken %) items)))

(defn get-free-items
  "returns a vector containing all free items"
  [items]
  (vec (filter #(not (:taken %)) items)))

(defn get-total-value 
  "returns the total value of a given list of items"
  [items] 
  (reduce + (map #(:value %) items)))

(defn get-total-weight 
  "returns the total weight of a given list of items"
  [items] 
  (reduce + (map #(:weight %) items)))

(defn get-value-by-weight
  "Returns the total value / total weight"
  [items]
  (let [weight (get-total-weight items)]
    (if (zero? weight)
      0	
      (double (/ (get-total-value items) weight)))))

(defn add-random-item
  "Adds one random item to the knapsack"
  [items]
  (let [free-items (get-free-items items)]
    (concat (toggle-taken-status free-items (rand-int (count free-items)))
            (get-taken-items items))))

(defn heuristic
  "Heuristic function to determine whether one sack is better than the other
   'func' is either total value of list or total value per weight"
  [items capacity func]
  (if (> (get-total-weight (get-taken-items items)) capacity)
    -1 ;; knapsack over capacity is invalid
    (func (get-taken-items items))))

(defn generate-initial
  "Generates random initial knapsack for start state"
  [items capacity]
  (loop [items items]
    (let [new-sack (add-random-item items)]
      (if (> (get-total-weight  (get-taken-items new-sack)) capacity)
        items ;; return the items once we reach capacity 
        (recur new-sack))))) ;; else keep adding random items

(defn new-sack
  "Creates a new random sack"
  [items capacity]
  (generate-initial (map #(update % :taken (constantly false)) items) capacity))


(defn replace-item
  "Removes one item from the knapsack and adds a new one"
  [items]
  (let [free-items (get-free-items items) taken-items (get-taken-items items)]
    (concat (toggle-taken-status free-items (rand-int (count free-items)))
            (toggle-taken-status taken-items (rand-int (count taken-items))))))

(defn mutate-sack
  "Randomly mutates the sack by either adding a new item, replacing an item
  or creating a new sack all together"
  [items capacity]
  (let [mutation (rand-int 3)]
    (case mutation
      0 (new-sack items capacity)
      1 (replace-item items)
      2 (add-random-item items))))

(defn knapsack-hill-climbing
  "
  items: list of maps, each map having :name, :weight, and :value keys
  capcacity: integer representing the capacity of the knapsack
  func: function used to compare different sacks
  Runs for num-runs amount of steps, mutating the sack at each step. If the 
  mutated sack is better than the current sack, choose it as the new sack. 
  Else keep going with current sack
  "
  [items capacity func]
  (let [items (generate-initial (vec (map #(assoc % :taken false) items)) capacity)]
	(loop [items items steps 0]	
		(let [new-sack  (mutate-sack items capacity)]
			(cond 
				(= steps num-runs) items ;; reached our step limit. return what we have
                ;; return new sack if it's better
				(> (heuristic new-sack capacity func) 
                   (heuristic items capacity func)) (recur new-sack (+ 1 steps)) 
                ;; keep old sack if it's better
				:else (recur items (+ 1 steps)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    Code for Testing Knapsack  ;;;;;;    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn square
  "Squares a number"
  [x]
  (* x x))

(defn mean 
  "Computes mean of a list"
  [lst] 
  (double (/ (reduce + lst) (count lst))))

(defn standard-deviation 
  "Computes standard deviation of a list"
  [lst]
  (let [lst-mean (mean lst)]
    (double (Math/sqrt (/ (reduce + (map #(square (- % lst-mean)) lst))
                          (count lst))))))

(defn sum-stats
  "Prints summary stats for a given list of runs"
  [many-items]
  (let [values (map #(get-total-value (get-taken-items %)) many-items)
        weights (map #(get-total-weight (get-taken-items %)) many-items)]
    (println "STATS FOR VALUES")
    (println (str "Max: " (apply max values)))
    (println (str "Min: " (apply min values)))
    (println (str "Average: " (mean values)))
    (println (str "Std. Deviation: " (standard-deviation values)))

    (println "STATS FOR WEIGHTS")
    (println (str "Max: " (apply max weights)))
    (println (str "Min: " (apply min weights)))
    (println (str "Average: " (mean weights)))
    (println (str "Std. Deviation: " (standard-deviation weights)))))

(defn repeated-knapsack
  "Repeatedly run knapsack, compile all of the results in one list, and then
  print out summary statistics"
  [items capacity func iters]
  (sum-stats (repeatedly iters #(knapsack-hill-climbing items capacity func)))
)

(defn print-knapsack-results
  "Prints results for one complete knapsack run"
  [items]
  (println "Sample one Run")
  (println "\nItems taken:")
  (run! println (get-taken-items items))
  (println "\nItems Not taken:")
  (run! println (get-free-items items))
  (println "\nTotal Weight:" (get-total-weight (get-taken-items items)))
  (println "Total Value:" (get-total-value (get-taken-items items))))

;; optimal solution is 34
(def sample-items
  [{:name :apple :weight 2 :value 1}
   {:name :coinpurse :weight 10 :value 15}
   {:name :candlestick :weight 10 :value 3}
   {:name :bowl :weight 3 :value 2}
   {:name :book :weight 5  :value 4}
   {:name :mirror :weight 8 :value 10}
   {:name :lamp :weight 6  :value 6}
   {:name :chest :weight 20  :value 15}
   {:name :shirt :weight 2  :value 2}
   {:name :bed :weight 50  :value 100}
   {:name :shoes :weight 4  :value 7}
   {:name :rock :weight 15  :value 1}]) 
(def sample-capacity 25) ;; capacity of knapsack 

;; optimal soltution is 60
(def sample-items-2
  [{:name :item1 :weight 1 :value 20}
   {:name :item2 :weight 2 :value 5}
   {:name :item3 :weight 3 :value 10}
   {:name :item4 :weight 8 :value 40}
   {:name :item5 :weight 7 :value 15}
   {:name :item6 :weight 4 :value 25}])
(def sample-capacity-2 10) ;; capacity of knapsack 

(def num-restarts 50) ;; number of times to re-rerun hill climbing to average results

;; single knapsack run
(print-knapsack-results (knapsack-hill-climbing 
  sample-items
  25
  get-total-value))

;; sample-items 1
(println "\n\n*** Running on sample 1: Optimal solution is 34 ****")
(println "Summary Stats with total-value as heuristic")
(repeated-knapsack sample-items sample-capacity get-total-value num-restarts)
(println "\nSummary Stats with value-per-weight as heuristic")
(repeated-knapsack sample-items sample-capacity get-value-by-weight num-restarts)

;; sample-items 2
(println "\n\n*** Running on sample 2: Optimal solution is 60 ***")
(println "Summary Stats with total-value as heuristic")
(repeated-knapsack sample-items-2 sample-capacity-2 get-total-value num-restarts)
(println "\nSummary Stats with value-per-weight as heuristic")
(repeated-knapsack sample-items-2 sample-capacity-2 get-value-by-weight num-restarts)
