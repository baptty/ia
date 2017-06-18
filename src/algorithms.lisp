;; Generic AI algorithms.
;; To use this algorithms with a data structure interfaces with the following
;; methods must be implemented:
;; 
;; (datastructure-objective board)
;; (datastructure-sucessor board)
;; (datastructure-heuristic board)

(defstruct problem
  "Generic problem structure to be accepted by a algorithm.
This structure has an initial problem state the functions to generate
sucessors, calculate heuristics and check if a table is the objective."
  initial-state
  sucessor
  heuristic
  objective)

(defun problem-set-initial-state ()
  "Set initial-state given a value.")

(defun problem-set-sucessor ()
  "Set sucessor function given a value.")

(defun problem-set-heuristic ()
  "Set heuristic function given a value")

(defun problem-set-objective ()
  "Set objective value given a value")

(defun problem-set ()
  "Initialize problem data structure with initial state and its functions")

(defun hill-climbing ()
  "Implementation of Hill Climbing algorithm for a problem data structure")
