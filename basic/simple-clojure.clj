;; Lucas Barusek
;; September 21st, 2021

; (ns homework1.core)

;;;;;;;;;;;;;;;;
;; Question 1 ;;
;;;;;;;;;;;;;;;;

(def times-list
'("03:05:32"
"06:25:34"
"06:35:28"
"09:55:00"
"15:22:01"
"18:07:11"
"20:12:18" "20:21:21"
"22:00:34"
"23:00:28"
"02:05:40"
"03:14:50"
"04:54:51"
"06:44:20"))

(defn get-diff
  "Given a list of seconds with size n, returns a list of size n-1 of
  the consecutive differences between the seconds, while adjusting for
  different days"
  [seconds]
  (loop [seconds seconds diff-list '()]
    (if (= (count seconds) 1)
      diff-list ;; returns the list of differences at the last element
      (let [diff (- (second seconds) (first seconds))]
        (if (< diff 0) 
          ;; if the diff is negative, we are at a new day, so add 24 hours
          (recur (rest seconds) (conj diff-list (+ (* 60 60 24) diff)))
          ;; else just add the diff as normal to the list
          (recur (rest seconds) (conj diff-list diff)))))))

(defn get-decimal
  " Returns a double corresponding to the mantissa of a given double"
  [number]
  (let [str-num (str number)]
    (Double/parseDouble  (subs str-num (clojure.string/index-of str-num \.)))))

(defn pad-zeros
  "Given a number, pad it with a leading zero if the number is less than 10" 
  [number]
  (format "%02d" number))

(defn seconds-to-string
  "Given a integer seconds, convert it to a string in HH:MM:SS format"
  [seconds]
  ;; CITE: https://www.inchcalculator.com/seconds-to-time-calculator/
  ;; DESC: math for how to convert from seconds to HH::MM::SS
  (let [hours-dec (double (/ seconds 3600))
        hours (int hours-dec)
        mins-dec (double (* 60 (get-decimal hours-dec)))
        mins (int mins-dec)
        secs (Math/round (* 60  (get-decimal mins-dec)))
       ]
    ;; returns a string with ':' in between hours, mins, and secs
    (str (pad-zeros hours) \: (pad-zeros mins) \: (pad-zeros secs))))

(defn string-to-seconds
  "Given a string in HH:MM:SS format, returns an int for the number of seconds"
  [string]
  ;; splice hours, mins, and seconds out of string
  (let [hours (Integer/parseInt (subs string 0 2))
        mins (Integer/parseInt (subs string 3 5))
        secs (Integer/parseInt (subs string 6 8))]
    ;; convert mins and hours to secs, and then add all of the values
    (+ secs (* 60 mins) (* 60 60 hours))))

(defn longest-time-gap
  "Given a list of times, return the string representation of the longest
  difference between times"
  [times]
  ;; return the string representation of the max difference of the times
  (seconds-to-string (apply max (get-diff (map string-to-seconds times)))))

(println (longest-time-gap times-list))

;;;;;;;;;;;;;;;;
;; Question 2 ;;
;;;;;;;;;;;;;;;;

(def scrabble-values
  {\a 1
   \b 3
   \c 3
   \d 2
   \e 1
   \f 4
   \g 2
   \h 4
   \i 1
   \j 8
   \k 5
   \l 1
   \m 3
   \n 1
   \o 1
   \p 3
   \q 10
   \r 1
   \s 1
   \t 1
   \u 1
   \v 4
   \w 4
   \x 8
   \y 4
   \z 10})

(defn get-random-word-scrabble
  "returns a randomly generate word of length 7"
  []
  (repeatedly 7 #(rand-nth (keys scrabble-values))))

(defn create-word-and-score
  "returns a map with keys :word and :score, corresponding to random word
  and its score calculated by scrabble rules respectively"
  []
  (let [word (get-random-word-scrabble)] 
        {:word (apply str word) 
         :score (apply + (map #(get scrabble-values %) word))}))

(defn scrabble-population
  "Returns a list of 1000 maps with words and scores"
  []
  (repeatedly 1000 #(create-word-and-score)))

(defn best-scrabble-words
  "Returns a list with word, score maps that have a score greater than 40"
  []
  (filter #(< 40 (:score %)) (scrabble-population)))

(println (best-scrabble-words)) 

;;;;;;;;;;;;;;;;
;; Question 3 ;;
;;;;;;;;;;;;;;;;

(def characters
  '(\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z
    \A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z
    " "  "."))

(defn get-word-distance
  "Given two words of equal length, return how many characters at the same
  index are equivalent between the two strings"
  [correct-word test-word]
  (loop [correct-word correct-word test-word test-word num-correct 0]
    (if (= (count correct-word) 0)
      num-correct
      (if (= (first correct-word) (first test-word))
        (recur (rest correct-word) (rest test-word) (+ num-correct 1))
        (recur (rest correct-word) (rest test-word) num-correct)))))

(defn get-random-word-stochastic
  "Creates a random word with a given length out of the 54 available characters"
  [length]
  (repeatedly length #(rand-nth characters)))

(defn found-correct
  "Prints out when we found the test word with how many steps. Used for testing"
  [test-word steps] 
  (println "Found target:" (str \" test-word \") "in" steps "steps!") 
  steps)

(defn mutate-word
  "Replace a single chracter in a word with another character"
  [word]
  (let [index (rand-int (count word))]
    (str (subs word  0 index) (rand-nth characters) (subs word (+ 1 index)))))

(defn stochastic-hill-climber
  "Main driver of question 3. Given a word, use stochastic hill climbing to
  evolve the given word. We start off with a random word of equivalent length,
  and then mutate that word until we get to the correct word, recording 
  the number of steps it takes to find the correct word" 
  [correct-word]
  ;; we recurse until we have found the correct word
  (loop [test-word (get-random-word-stochastic (count correct-word)) steps 0]
    (let [num-correct (get-word-distance correct-word test-word)
          new-word (mutate-word (apply str test-word))]
      (if (= (count  correct-word) num-correct)
        steps ;; we found the correct solution, return # of steps
        (if (> (get-word-distance correct-word new-word) num-correct)
          ;; return the new word if it's better than the current test word
          ;; else just return the current step word and try again
          (recur new-word (+ steps 1))
          (recur test-word (+ steps 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;
;; Question 3 timing ;;
;;;;;;;;;;;;;;;;;;;;;;;

" 
For timing, I decided that the built-in time function in clojure is terrible.
It times the function, and only prints out the answer, and it is impossible
to get the actual value from 'time'. So I asked Prof. Helmuth if I could steal
some code off the internet and that's what I did! The below function was somebody
creating a macro for timing functions. In their post, they modified the print
statement, but still didn't return the value. I further modified their macro
to make it so it actually returns the number of ms the function takes to run.
This way I could create a function called 'time-hill-climbing' which could
repeatedly time 'stochastic-hill-climbing', and then print out the string
and its length that first took over 200ms.

After running it about 15 times, I found it usually takes a string of around
50-60 characters to first be over 200 ms.
"
(defmacro timed [expr]
  (let [sym (= (type expr) clojure.lang.Symbol)]
    `(let [start# (. System (nanoTime))
           return# ~expr
           res# (if ~sym  
                    (resolve '~expr)  
                    (resolve (first '~expr)))]
       (/ (double (- (. System (nanoTime)) start#)) 1000000.0))))

(defn time-hill-climbing
  "Function to record how long it takes for hill climbing to complete, starting
  with a word of length 1, and going until a word takes more than 200ms. Prints
  the length of the word that first took over 200ms"
  []
  ;; recurse until it takes longer than 200 ms for the function to run
  (loop [length 1]
    (let [word (apply str (get-random-word-stochastic length))
          function-time (timed (stochastic-hill-climber word))]
      (if (> function-time 200)
        ;; if it takes over 200 ms, print out the length of the word
        (println (str "Word (" word  ") with length " length " was first over 200ms!"))
        (recur (+ 1 length))))))

(time-hill-climbing)
