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
  "Initialize the N Queens board with sequencial diagonal number.
This is done to reduce the search space"
  (cond ((= index (nqueen-size board)) board)
        (T (progn
             (nqueen-set board index index)
             (nqueen-sequencial-initialize-aux board (1+ index))))))

(defun nqueen-sequencial-initialize (board)
  "Initialize queens board from 1 to N where N is size of the array"
  (nqueen-sequencial-initialize-aux board 0))

(defun nqueen-swap-two-random (board index1 index2)
    (rotatef (aref (nqueen-board board) index1)
             (aref (nqueen-board board) index2))
    board)


(defun nqueen-n-swaps (board n)
  "Make N swaps to random elements of the N-Queens board array. This is
used for the creation of the root board."
  (cond ((= n 0) board)
        (T (progn
             (nqueen-swap-two-random  board
                                      (random (nqueen-size board))
                                      (random (nqueen-size board)))
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
  "Check if two queens threat each other. Returns true if they do"
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
    (loop for i from 0 to (* 3 (nqueen-size board)) by 1 do
          (setf list (cons (nqueen-swap-two-random (nqueen-replicate board)
                                                   (random (nqueen-size board))
                                                   (random (nqueen-size board))) list)))
    list))

(defun nqueen-sucessor (board)
  "Generate the list of sucessors and choose the first with best heuristic found"
  (let ((current-heuristic-value (nqueen-heuristic board)))
    (dolist (elem (nqueen-sucessors-list board))
      (if (<= (nqueen-heuristic elem) current-heuristic-value)
        (return-from nqueen-sucessor elem))))
  nil)

(defun nqueen-objective (board)
  "Verify if a board is an objective, that is, if no queens are threatened"
  (= (nqueen-heuristic board) 0))


(defun objective (board)
  "Generic successor function that must be implemented to run the algorithm."
  (nqueen-objective board))

(defun sucessor (board)
  "Generic objective function that must be implemented to run the algorithm."
  (nqueen-sucessor board))

(defun create (board)
  (nqueen-create board))

(defun hill-climbing (size flatland-repetitions)
  "Hill climbing algorithm with restarts implementation"
  (let ((current-state)
        (sucessor-state)
        (restarts 0)
        (iterations 0)
        (start-time (get-universal-time))
        (end-time)) 

    (loop while t do
          (setf current-state (create size))
          (incf restarts)

          (loop for i from 0 to flatland-repetitions do
                (setf sucessor-state (sucessor current-state))
                (incf iterations)

                (if sucessor-state
                  (setf current-state sucessor-state)
                  (return))

                (if (objective current-state)
                  (return)))

          (if (objective current-state)
            (return)))

    (setf end-time (get-universal-time))
    (list current-state size restarts iterations (- end-time start-time))))

(defun nqueen-log (board)
  "Log size, restarts, iterations and times of result of a board"
  (with-open-file (str "results-logs/nqueens.csv"
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
      (format str "~D,~D,~D,~D~%"
              (second board)
              (third board)
              (fourth board)
              (fifth board))))

(defun nqueen-solve (size)
  "Solve a N-Queen problem for a given size."
  (nqueen-log (hill-climbing size (* size 10))))


(defun nqueen-tests (test-values)
  "Make a set of tests for sizes given by test-values list passed as
parameter"
  (dolist (elem test-values)
    (loop for i from 0 to 9 do
          (nqueen-solve elem))))

