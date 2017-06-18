;;; Instituto Politecnico de Portalegre
;;; Escola Superior de Tecnologia e Gestao
;;;
;;; Engenharia Informatica
;;; Inteligencia Artificial 2016/2017
;;; Prof. Luis Baptista
;;; Joao Baptista - 16839

;; -----------------------------------------------------------
;; N-Queens structure and auxiliary functions to manipulate it
;; -----------------------------------------------------------

(load (compile-file "algorithms.lisp"))

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

(defun nqueen-create (size)
  "Create a new nqueen object with random values in queens array"
  (nqueen-sequencial-initialize (nqueen-create-empty size)))

(defun nqueen-randomize-aux (board index)
  "Real randomizer of queens board from a given start index"
  (cond ((= index (nqueen-size board)) board)
        (T (progn
             (nqueen-set board index (random (1+ (nqueen-size board))))
             (nqueen-randomize-aux board (1+ index))))))

(defun nqueen-randomize (board)
  "Randomize positions of queens in a board"
  (nqueen-randomize-aux board 0))

(defun nqueen-replicate-aux (original-board new-board index)
  "Recursive auxiliary function to replicate to a new board from a original"
  (cond ((= index (nqueen-size original-board)) new-board)
        (T (progn
             (nqueen-set new-board index (nqueen-get original-board index))
             (nqueen-replicate-aux original-board new-board (1+ index))))))

(defun nqueen-replicate (board)
  "Create a new nqueen object with the same contents as the original"
  (nqueen-replicate-aux board (nqueen-create-empty (nqueen-size board)) 0))

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

(defun nqueen-heuristic (board)
  "Calculate the cost of a given board based on number of threatened queens"
  board
  )

(defun nqueen-sucessor (board)
  "Generate and return a list of new sucessors from a current board"
  board
  )

(defun nqueen-objective (board)
  "Verify if a board is an objective, that is, if no queens are threatened"
  board
  )

