;; ---------------------------------------------------------------------------
;; Generic AI algorithms.
;; To use this algorithms with a data structure interfaces with the following
;; methods must be implemented:
;; ---------------------------------------------------------------------------

(defstruct problem
  "Generic problem structure to be accepted by a algorithm.
This structure has an initial problem state the functions to generate
sucessors, calculate heuristics and check if a table is the objective."
  state
  sucessor
  heuristic
  objective)

(defun problem-create (state sucessor-fun heuristic-fun objective-fun)
  "Create a new problem from a set of problem functions needed to solve
a problem with the hill-climbing algorithm."
  (make-problem
    :state state
    :sucessor sucessor-fun
    :heuristic heuristic-fun
    :objective objective-fun))


;; -----------------------------------------------------------
;; N-Queens structure and auxiliary functions to manipulate it
;; -----------------------------------------------------------

(defstruct nqueen
  "N-Queens problem structure.
The board member is an array of queens where the index is the x coordinate
and the content of an index is the y coordinate"
  size
  board)


(defun nqueen-get (board index)
  "Get a queen at a certain index from a given table"
  (aref (nqueen-board board) index))

(defun nqueen-set (board index value)
  "Set a value to a queen in a given index of a given board"
  (setf (aref (nqueen-board board) index) value))


(defun nqueen-create-empty (size)
  "Create a new nqueen object with all members to zero given a size"
  (make-nqueen
    :size size
    :board (make-array size :initial-element 0)))

(defun nqueen-sequencial-initialize-aux (board index)
  (cond ((= index (nqueen-size board)) board)
        (T (progn
             (nqueen-set board index index)
             (nqueen-sequencial-initialize-aux board (1+ index))))))

(defun nqueen-sequencial-initialize (board)
  "Initialize queens board from 1 to N where N is size of the array"
  (nqueen-sequencial-initialize-aux board 0))


(defun nqueen-swap-two-random (board)
  "Switch position between two random queens in the given table and return
the new board"
  (let* ((queen1 (random (nqueen-size board)))
        (queen2 (random (nqueen-size board)))
        (queen1-value (nqueen-get board queen1))
        (queen2-value (nqueen-get board queen2)))

    (progn
      (nqueen-set board queen1 queen2-value)
      (nqueen-set board queen2 queen1-value)
      board)))

(defun nqueen-n-swaps (board n)
  "Make N swaps to random elements of the N-Queens board array. This is
used for the creation of the root board."
  (cond ((= n 0) board)
        (T (progn
             (nqueen-swap-two-random board)
             (nqueen-n-swaps board (1- n))))))

(defun nqueen-create (size)
  "Create a new N Queen board by generating a sequencial board and then swapping
board-size queens with board-size maximum values"
  (nqueen-n-swaps (nqueen-sequencial-initialize (nqueen-create-empty size))
                  size))

(defun nqueen-replicate-aux (original-board new-board index)
  "Recursive auxiliary function to replicate to a new board from a original"
  (cond ((= index (nqueen-size original-board)) new-board)
        (T (progn
             (nqueen-set new-board index (nqueen-get original-board index))
             (nqueen-replicate-aux original-board new-board (1+ index))))))

(defun nqueen-replicate (board)
  "Create a new nqueen object with the same contents as the original"
  (nqueen-replicate-aux board (nqueen-create-empty (nqueen-size board)) 0))

(defun nqueen-xy-heuristic (board x y)
  "Check if two queens threat eachother. Returns true if they do"
  (or (= (- (nqueen-get board x) x) (- (nqueen-get board y) y))
      (= (+ (nqueen-get board x) x) (+ (nqueen-get board y) y))
      (= (nqueen-get board x) (nqueen-get board y))))


;; Functions to feed to the algorithm

(defun nqueen-heuristic (board)
  "Calculate the cost of a given board based on number of threatened queens on a board"
  (let ((threat-count 0))
      (loop for i from 0 to (nqueen-size board) do
            (loop for j from (1+ i) to (1- (nqueen-size board)) do
                  (if (nqueen-xy-heuristic board i j)
                    (incf threat-count))))
      threat-count))

(defun nqueen-sucessors-list (board)
  "Generate and return a list of new sucessors from a current board"
  (let (list)
    (loop for i from 0 to 200 by 1 do
      (setf list (cons (nqueen-swap-two-random (nqueen-replicate board)) list)))
      list))

(defun nqueen-sucessor (board)
  "Generate the list of sucessors and choose the first with best heuristic found"
  (dolist (elem (nqueen-sucessors-list board))
    (if (<= (nqueen-heuristic elem) (nqueen-heuristic board))
      (return-from nqueen-sucessor elem)))
  nil)

(defun nqueen-objective (board)
  "Verify if a board is an objective, that is, if no queens are threatened"
  (= (nqueen-heuristic board) 0))


(defun objective (board)
  (nqueen-objective board))

(defun sucessor (board)
  (nqueen-sucessor board))

(defun hill-climbing (size flatland-repetitions)
  (let* ((current-state)
         (sucessor-state))

    (loop while t do
          (setf current-state (nqueen-create size))
      
          (loop for i from 0 to flatland-repetitions do
                (setf sucessor-state (sucessor current-state))
                (if sucessor-state
                  (setf current-state sucessor-state)
                  (return))
                (if (objective current-state)
                  (return)))

          (if (objective current-state)
            (return)))
    current-state))

(defun nqueen-solve (size)
  (hill-climbing size (* size 5)))


(defun main ()
  (time (nqueen-solve 200)))

