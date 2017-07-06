(defstruct latin
  ""
  size
  board)

(defun latin-create-empty (size)
  ""
  (make-latin
    :size size
    :board (make-array (list size size))))

(defun latin-get (board line column)
  ""
  (aref (latin-board board) line column))

(defun latin-set (board line column value)
  ""
  (setf (aref (latin-board board) line column) value))

(defun latin-sequential-initialize (board)
  ""
  (loop for x from 0 to (1- (latin-size board)) by 1 do
        (loop for y from 0 to (1- (latin-size board)) by 1 do
              (latin-set board x y y)))
  board)

(defun latin-swap-numbers (board line column1 column2)
  (rotatef (aref (latin-board board) line column1)
           (aref (latin-board board) line column2))
  board)

(defun latin-n-swaps (board)
  (loop for j from 0 to (1- (latin-size board)) do
        (loop for i from 0 to (1- (latin-size board)) do
              (latin-swap-numbers board i
                                  (random (latin-size board))
                                  (random (latin-size board)))))
  board)

(defun latin-create (size)
  (latin-n-swaps (latin-sequential-initialize (latin-create-empty size))))

(defun latin-replicate (board)
  (let ((new-board (latin-create-empty (latin-size board))))
    (loop for i from 0 to (1- (latin-size board)) do
          (loop for j from 0 to (1- (latin-size board)) do
                (latin-set new-board i j (latin-get board i j))))
    new-board))

(defun latin-xy-heuristic (board x1 x2 y)
  (= (latin-get board x1 y) (latin-get board x2 y)))

(defun latin-heuristic (board)
  (let ((repetitions-count 0))
    (loop for y from 0 to (1- (latin-size board)) do
          (loop for x1 from 0 to (1- (latin-size board)) do
                (loop for x2 from (1+ x1) to (1- (latin-size board)) do
                      (if (latin-xy-heuristic board x1 x2 y)
                        (incf repetitions-count)))))
    repetitions-count))

(defun latin-objective (board)
  (= (latin-heuristic board) 0))

(defun latin-successors-list (board)
  (let (list)
    (loop for i from 0 to 200 do
          (setf list (cons (latin-replicate (latin-swap-numbers board
                                                                (random (latin-size board))
                                                                (random (latin-size board))
                                                                (random (latin-size board)))) list)))
    (format t ".") list))

(defun latin-successor (board)
  (dolist (elem (latin-successors-list board))
    (if (<= (latin-heuristic elem) (latin-heuristic board))
      (return-from latin-successor elem)))
   nil)



(defun latin-solve (size)
  (let* ((current-state)
         (sucessor-state))

    (loop while t do
          (setf current-state (latin-create size))
      
          (loop for i from 0 to size do
                (setf sucessor-state (latin-successor current-state))
                (if sucessor-state
                  (setf current-state sucessor-state)
                  (return))
                (if (latin-objective current-state)
                  (return)))

          (if (latin-objective current-state)
            (return)))
    (print "Restart") current-state))

