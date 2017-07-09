
(defstruct latin
  "Latin Squares structure. This holds the size of the board and 
the board itself, which is a matrix."
  size
  board)


(defun latin-create-empty (size)
  "Create an empty structure of latin square with the given size and
and a board of size by size"
  (make-latin
    :size size
    :board (make-array (list size size))))

(defun latin-get (board line column)
  "Get element of the board by line and column."
  (aref (latin-board board) line column))

(defun latin-set (board line column value)
  "Set a value to an element of a board by line line and column."
  (setf (aref (latin-board board) line column) value))

(defun latin-sequential-initialize (board)
  "Initialize the latin board with sequential numbers from 0 to size in
each line. This is done to optimize the search space of the algorithm.
It only needs to check the columns this way."
  (loop for x from 0 to (1- (latin-size board)) by 1 do
        (loop for y from 0 to (1- (latin-size board)) by 1 do
              (latin-set board x y y)))
  board)

(defun latin-swap-numbers (board line column1 column2)
  "Swap two given elements in a board between columns."
  (rotatef (aref (latin-board board) line column1)
           (aref (latin-board board) line column2))
  board)

(defun latin-n-swaps (board)
  "Make size swaps between two random elements in columns.
To improve the randomization this is done size times size times"
  (loop for j from 0 to (1- (latin-size board)) do
        (loop for i from 0 to (1- (latin-size board)) do
              (latin-swap-numbers board i
                                  (random (latin-size board))
                                  (random (latin-size board)))))
  board)

(defun latin-create (size)
  "Create a latin squares object already randomized from a sequential board"
  (latin-n-swaps (latin-sequential-initialize (latin-create-empty size))))

(defun latin-replicate (board)
  "Replicate a given object by creating a in memory copy of everything
in the struct."
  (let ((new-board (latin-create-empty (latin-size board))))
    (loop for i from 0 to (1- (latin-size board)) do
          (loop for j from 0 to (1- (latin-size board)) do
                (latin-set new-board i j (latin-get board i j))))
    new-board))

(defun latin-xy-heuristic (board x1 x2 y)
  "Check if two elements are repeated in a column."
  (= (latin-get board x1 y) (latin-get board x2 y)))

(defun latin-heuristic (board)
  "Count the number of repetitions in columns of a given board"
  (let ((repetitions-count 0))
    (loop for y from 0 to (1- (latin-size board)) do
          (loop for x1 from 0 to (1- (latin-size board)) do
                (loop for x2 from (1+ x1) to (1- (latin-size board)) do
                      (if (latin-xy-heuristic board x1 x2 y)
                        (incf repetitions-count)))))
    repetitions-count))

(defun latin-objective (board)
  "Check if a given board is objective board."
  (= (latin-heuristic board) 0))

(defun latin-successors-list (board)
  "Generate a list of possible neighbors successors from a given board"
  (let (list)
    (loop for i from 0 to (* 3 (latin-size board)) do
          (setf list (cons  (latin-swap-numbers (latin-replicate board)
                                                (random (latin-size board))
                                                (random (latin-size board))
                                                (random (latin-size board))) list)))
    list))

(defun latin-successor (board)
  "Choose a sucessor from a created latin successors list from
the given board"
  (let ((current-heuristic-value (latin-heuristic board)))
    (dolist (elem (latin-successors-list board))
      (if (<= (latin-heuristic elem) current-heuristic-value)
        (return-from latin-successor elem))))
  nil)

(defun sucessor (board)
  "Generic successor function that must be implemented to run the algorithm."
  (latin-successor board))

(defun objective (board)
  "Generic objective function that must be implemented to run the algorithm."
  (latin-objective board))

(defun create (size)
  "Generic create object function that must be implemented to run the algorithm."
  (latin-create size))

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

(defun latin-log (board)
  "Log size, restarts, iterations and times of result of a board"
  (with-open-file (str "latin.csv"
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
      (format str "~D,~D,~D,~D~%"
              (second board)
              (third board)
              (fourth board)
              (fifth board))))

(defun latin-solve (size)
  "Solve a Latin Squares problem for a given size"
  (latin-log (hill-climbing size (expt size 3))))


(defun latin-tests (test-values)
  "Make a set of tests for sizes given by test-values list passed as
parameter"
  (dolist (elem test-values)
    (loop for i from 0 to 9 do
          (latin-solve elem))))



