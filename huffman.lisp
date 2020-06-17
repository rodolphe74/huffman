;;(defparameter pth "~/developpement/lisp/test.txt")
;;(defparameter pthOut "~/developpement/lisp/test.huf")
;;(defparameter uncompPthOut "~/developpement/lisp/test.unhuf")
(defparameter pth "~/developpement/lisp/maupassant.txt")
(defparameter pthOut "~/developpement/lisp/maupassant.huf")
(defparameter uncompPthOut "~/developpement/lisp/maupassant.unhuf")




(defparameter carOcc (make-array '(256)))
(defparameter huffmanTree nil)
(defparameter binTable (make-array '(256)))
(defparameter bitBuffer '())
(defparameter bitBufferSize 0)
(defparameter fileSize 0)
(defparameter flushWhenBufferSizeIsGreaterThan 128) ;must be 8 divisor
(defparameter readAheadBufferSize 32);
(defparameter readAheadBuffer (make-array readAheadBufferSize :fill-pointer 0  :element-type 'integer))
(defparameter decodedBits (make-array (* 8 readAheadBufferSize) :fill-pointer 0 :element-type 'bit))
(defparameter currentNode nil)
(defparameter decodedBitsCount 0)


(defun bv2int(z)
  "convert a bitvector to a positive integer. e.g. #*101 -> 5"
  (reduce #'(lambda(x y) (+ (* 2 x) y)) z))


(defun int2bv(i n)
  "convert a positive integer i to a bit vector of exactly length n"
  (let ((res (make-array n :element-type 'bit)))
    (dotimes (k n) (setf (bit res (- n k 1)) (mod i 2)
                         i (truncate i 2)))
    res))



(defun make-tree (data)
  "Creates a new node that contains 'data' as its data."
  (cons (cons data nil) nil))

(defun first-child (tree)
  "Returns a reference to the first child of the node passed in,
  or nil if this node does not have children."
  (cdr (car tree)))

(defun next-sibling (tree)
  "Returns a reference to the next sibling of the node passed in,
  or nil if this node does not have any siblings."
  (cdr tree))

(defun data (tree)
  "Returns the information contained in this node."
  (car (car tree)))

(defun add-child (tree child)
  "Takes two nodes created with 'make-tree' and adds the
  second node as a child of the first. Returns the first node,
  which will be modified."
  (nconc (car tree) child)
  tree)

(defun traverse (tree &optional (padding 0))
  (when tree
    (format t "~&~v@TData: ~A" padding (data tree))
    (when (first-child tree)
      (format t "  Children: ~A"
              (maplist #'(lambda (x) (data x))
                       (first-child tree))))
    (traverse (first-child tree) (+ padding 3))
    (traverse (next-sibling tree) padding)))



(defun readFileOcc ()
  (let   ((fin (open pth :element-type '(unsigned-byte 8))))
    (when fin
      (loop for c = (read-byte fin nil) until (null c) do
	   (setf (aref carOcc c) (+ 1 (aref carOcc c)))
	   )
      (close fin))))


(defun appendToCompressedFile (b)
  (with-open-file (s pthOut :direction :output :if-exists :append  :element-type 'unsigned-byte)
    (write-byte b s)
    ;;(print b)
    )
  )


(defun appendStringToCompressedFile (str)
  (with-open-file (s pthOut :direction :output :if-exists :append)
    (format s str)
    (format s "~%")
    ))





(defun flushBitsFromBuffer (count)
  (let ((p 0))
    (multiple-value-bind (q r) (floor count 8)

      (if (not (= q 0))
	  (progn
	    (loop for i from 1 to q do
		 (appendToCompressedFile (bv2int   (subseq bitBuffer p (+ p 8))))
		 ;;(print (subseq bitBuffer p (+ p 8)))
		 (setf p (+ p 8))		 )
	    (setf bitBuffer (subseq bitBuffer (* q 8)  (length bitBuffer)))
					;	(print "flushed")
	    ))

      (if (not (= r 0))
	  (progn
;;	    (print "Last bits")
;;	    (print (length bitBuffer))
;;	    (print q)
;;	    (print r)
	    
	    ;; 0 padding until 8 bits
	    (loop for x from 1 to (- 8 r) do
					; (print '0)
		 (setf BITBUFFER (APPEND BITBUFFER (cons 0 nil)))
		 )
;;	    (print (bv2int (subseq bitBuffer 0 8)))
	    (appendToCompressedFile (bv2int   (subseq bitBuffer 0 8)))
	    ))
      )
    ))


(defun writeBitsIntoBuffer ()
  (setf bitBuffer '())
  (let   ((fin (open pth :element-type '(unsigned-byte 8)))
	  (countBytes 0))
    (when fin
      (loop for c = (read-byte fin nil) until (null c) do
	   (let ((b (aref binTable c)))

	     (when (or (> (length bitBuffer) flushWhenBufferSizeIsGreaterThan) (= (length bitBuffer) flushWhenBufferSizeIsGreaterThan))
					; (print "flush")
	       (flushBitsFromBuffer flushWhenBufferSizeIsGreaterThan))

	     
	     (setf bitBuffer (append bitBuffer b))
					;; (print (length bitBuffer))
	     
	     )
	   (setf countBytes (+ 1 countBytes))
	   (multiple-value-bind (qFileSize rFileSize) (floor fileSize 100)
	     (declare (ignore rFileSize))
	     (when (> qFileSize 0)
	       (multiple-value-bind (qPercent rPercent) (floor countBytes qFileSize)
		 ;;(declare (ignore qPercent))
		 (when (= 0 rPercent) (progn (format t "~d..." qPercent) (finish-output))))
	     ))
	   )

      (let ((cf (/ (float bitBufferSize) (float 8)))
      	    (htStr (format nil "~A" huffmanTree)))
      	(format t "~A" #\linefeed)
      	(format t "Original File size : ~d" fileSize)
      	(format t "~A" #\linefeed)
      	(format t "Compressed File size (without dictionary) : ~A" cf)
      	(format t "~A" #\linefeed)
      	(format t "Ratio (without dictionary) : ~A" (/ (float fileSize) cf))
      	(format t "~A" #\linefeed)
      	(format t "Dictionary size : ~d" (length htStr))
      	(format t "~A" #\linefeed)
      	(format t "Compressed File size (with dictionary) : ~d" (+ cf (length htStr)))
      	(format t "~A" #\linefeed)
      	(format t "Ratio (with dictionary) : ~A" (/ (float fileSize) (float (+ cf (length htStr)))))
	(format t "~A" #\linefeed)
	)
      (flushBitsFromBuffer (length bitBuffer)) ; last bits
      (close fin))))


(defun evaluateBitsBufferSize ()
  (let   ((fin (open pth :element-type '(unsigned-byte 8))))
    (when fin
      (loop for c = (read-byte fin nil) until (null c) do
	   (let ((b (aref binTable c)))
	     (setf bitBufferSize (+ bitBufferSize (length b))))
	   (setf fileSize (+ 1 fileSize))))
    (close fin)))


(defun findSmallest ()
  (let ((s 2147483647) (p -1)) 
    (loop for i from 0 to 255 do
	 (if (and (< (aref carOcc i) s)  (> (aref carOcc i) 0))
	     (progn
	       (setf s (aref carOcc i))
	       (setf p (cons s i))
					;  (print p)
	       )))

    (if (= 2147483647 s)
	nil
	(progn
					;(setf (aref carOcc (cadr p)) -1)
	  (setf (aref carOcc (cdr p)) -1)
	  p))
    ))


(defun pairToNode (p)
  (add-child  (make-tree (car p)) (list (list (cdr p)))))

(defun initHuffmanTree ()
  (loop
     (let ( ( tr (findSmallest)   ))
       (when (null tr) (return))
					;(setf huffmanTree (append  huffmantree (make-tree  tr)))
					;(setf huffmanTree (append  huffmantree (list tr)))
       (setf huffmanTree (append  huffmantree (pairtonode tr)))
       )))


(defun huffmanTreeStep ()
  (let ((e1 0) (e2 0) (root 0))
    (progn
      (setf e1 (car huffmantree))
      (setf e2 (car (next-sibling huffmantree)))
      (setf root (make-tree (+ (car e1) (car e2))))
					;(print e1)
					;(print e2)
					;(print root)
      (add-child root (list e1))
      (add-child root (list e2))
      (return-from huffmanTreeStep (append root (next-sibling (next-sibling huffmantree ))))
      )))


;; (defun moveFirstElementAtItsRightPlace (tr)
;;   (let ((v (data tr)) (next tr) (oc 0) (i 0))
;;     (loop while (and (> v oc) (next-sibling next)) do
;; 	 (progn
;; 	   (setf next (next-sibling next))
;; 	   (setf oc (data next))
;; 	   (incf i)))
;;     (append (subseq tr 1 i)  (subseq tr 0 1)  (subseq tr i))))


(defun moveFirstElementAtItsRightPlace (tr)
  (let ((v (data tr)) (next tr) (oc 0) (i 0))
    (loop while (> v oc) do
	 (progn
	   (if (next-sibling next)
	       (progn
		 (setf next (next-sibling next))
		 (setf oc (data next))
		 (incf i))
	       (progn
					; when place is the last sibling element
		 (incf i)
					;(print i)
		 (return-from moveFirstElementAtItsRightPlace (append (subseq tr 1 i)  (subseq tr 0 1)  (subseq tr i)))))))
    
    (append (subseq tr 1 i)  (subseq tr 0 1)  (subseq tr i))
    ))



(defun buildHuffmanTree ()
  (setf carOcc (make-array '(256)))
  (setf huffmanTree nil)
  (readfileocc)
  (inithuffmantree)

  (loop while (> (length huffmantree) 2) do
       (setf huffmantree (movefirstelementatitsrightplace (huffmantreestep)))
					;(print (length huffmantree))
					;(print huffmantree)
					;(read)
       )

  (setf binTable (make-array '(256)))
  (dfs huffmanTree () binTable)
  (reorderBinTable)
  )


(defun dfs (tr st tb)
  (when tr

    (when (null (first-child tr))
					;(format t "~a [~a]=~{~a~}~C" (data tr) (code-char (data tr)) st #\linefeed)
      (setf (aref tb (data tr)) st)
					;(setf st (cdr st))
      (return-from dfs nil)
      )
    
    (when (first-child tr)
					;(print (data tr))
      (push 0 st) ; need reorder after push at beginning of the list
      (dfs (first-child tr) st tb)
      (setf st (cdr st))
      )
    (when (next-sibling tr)
      (push 1 st)
      (dfs (first-child (next-sibling tr)) st tb)
      (setf st (cdr st))
      )
    
    ))


(defun forwardTree (nd b)

    (when (and (null (first-child nd)) (null (first-child (next-sibling nd))))
      (progn
	(return-from forwardTree nil))
      )

    (if (= b 0)
    	(progn
    	  (setf nd (first-child nd))
    	  )
    	(progn
    	  (setf nd (first-child (next-sibling nd)))
    	  )
    	)
    (return-from forwardTree nd))


(defun reorderBinTable ()
  (loop for i from 0 to 255 do
       (let ((l (aref binTable i)))
	 (when (listp l)
	   (setf (aref binTable i) (reverse l))
	   (format t "~a [~a]=~{~a~}~C" i (code-char i) (aref binTable i) #\linefeed)
	   ))))




(defun addBitsSizeToFile()
  (with-open-file (s pthOut :direction :output  :if-exists :append)
    (format s "~a~d" #\linefeed bitBufferSize)
    (close s)))


(defun createCompressedFile ()
  (buildHuffmanTree)
  (setf bitBufferSize 0)
  (setf fileSize 0)
  (evaluateBitsBufferSize)
  ;; (print bitBufferSize)
  (let ((htStr (format nil "~A" huffmanTree)) )
    (with-open-file (s pthOut :direction :output :if-exists :supersede  :element-type 'unsigned-byte)
      (appendStringToCompressedFile (format nil "~A" bitBufferSize))
      (appendStringToCompressedFile  (format nil "~A" (length htStr)))
      (appendStringToCompressedFile htStr)
      (close s)
      ))
  (writeBitsIntoBuffer)
  )


  



(defun uncompressFile ()
  (with-open-file (i pthOut :direction :input :element-type 'unsigned-byte)
    (setf huffmanTree (readHuffmanTreeFromCompressedFile i))
    (setf currentNode huffmanTree)
    (setf decodedBitsCount 0)
    (with-open-file (o uncompPthOut :direction :output :if-exists :supersede :element-type 'unsigned-byte)
      (loop 
	 (readAhead i)
	 ;;(print readAheadBuffer)
	 (decodeBits readAheadBuffer o)
	 (when (= 0 (fill-pointer readAheadBuffer)) (return))
	 )
      (close o))
    (close i)
    ))



(defun decodeBits (b f)
  (let ((cb nil))
    (loop for x from 0 to (- (fill-pointer b) 1) do
					;(print  (int2bv (aref b x) 8))
	 (setf cb (int2bv (aref b x) 8))
	 (loop for y from 0 to 7 do
					;(print (aref cb y))
	      (setf currentNode  (forwardTree currentNode (aref cb y)))
					;(print currentNode)
	      (setf decodedBitsCount (+ 1 decodedBitsCount))
	     ;; (format t "~d : ~d/~d~d" (aref cb y)  decodedBitsCount bitBufferSize #\linefeed)
	      (if (null (first-child currentNode))
		  (progn
	;;	    (format t "~d [~d]~d" (data currentNode) (code-char (data currentNode)) #\linefeed )
		    (write-byte (data currentNode) f)
		    (setf currentNode huffmanTree)))

	      (if (= decodedBitsCount bitBufferSize)
		  (progn
		    (print "Bit buffer size reached")
		    (format t "~A" #\linefeed)
		    (return-from decodeBits nil)
		    )
		  )
	      
	      ))))
  


;; TODO CHECK IF ALL READED BYTES ARE PUSHED IN readAheadBuffer
;; AND REMOVE STOP COUNT
(defun readAhead (inputFile)
  (setf (fill-pointer readAheadBuffer) 0)
  (let ((count 0) (c 0))
    ;; (loop for c = (read-byte inputFile nil) until (or (null c) (= count readAheadBufferSize)) do
    ;; 	 (vector-push c readAheadBuffer)
    ;; 	 (print c)
    ;; 	 (setf count (+ 1 count))
    ;; 	 )
    (loop for x from 1 to readAheadBufferSize do
	 (setf c (read-byte inputFile nil))

	 (when (null c)
	   (return))
	 
    	 (vector-push c readAheadBuffer)
;;    	 (print c)
	 (setf count (+ 1 count))
    	 )

    (return-from readAhead count)))


(defun readHuffmanTreeFromCompressedFile (inputFile)
  (let (
	(bbSizeStr (make-array 0 :fill-pointer 0 :adjustable t :element-type 'character))
	(bbSize 0)
	(htSizeStr (make-array 0 :fill-pointer 0 :adjustable t :element-type 'character))
	(htSize 0)
	(htStr (make-array 0 :fill-pointer 0 :adjustable t :element-type 'character))
	(ht nil)
	(count 0))

    ;; get bits buffer size
    (loop for c = (read-byte inputFile nil) until (= c (char-code #\linefeed)) do
	 (vector-push-extend (code-char c) bbSizeStr)
	 )
    (setf bbSize (parse-integer bbSizeStr))
    (setf bitBufferSize bbSize)
    (print bbSize)
    
    ;; get tree size
    (loop for c = (read-byte inputFile nil) until (= c (char-code #\linefeed)) do
	 (vector-push-extend (code-char c) htSizeStr)
	 )
    (setf htSize (parse-integer htSizeStr))
    (print htSize)
    
    (loop for c = (read-byte inputFile nil) until (= count htSize ) do
	 (vector-push-extend (code-char c) htStr)
	 (setf count (+ 1 count)))
    (setf ht (read-from-string htStr))
					;(traverse ht)
    
    (return-from readHuffmanTreeFromCompressedFile ht)
    
    ))

