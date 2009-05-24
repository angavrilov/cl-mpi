;;;
;;; A CL-MPI implementation of the famous "Evo-Lisa" image compression demo by Roger Alsoing
;;;  http://rogeralsing.com/2008/12/07/genetic-programming-evolution-of-mona-lisa/
;;; The code is derived from Yann N. Dauphin's Clojure code
;;;  http://npcontemplation.blogspot.com/2009/01/clojure-genetic-mona-lisa-problem-in.html
;;;
;;; 
;;;  to convert file to P3 ppm format: convert ml.bmp -compress none ml.ppm
;;; Using cffi and uffi-compat is almost 10x faster than using UFFI!
;;;

(defpackage #:evo-lisa
  (:use #:cl )
  (:export compress compare-images evo-lisa))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cl-mpi)
  (asdf:operate 'asdf:load-op 'par-eval)
  (asdf:operate 'asdf:load-op 'cl-gd)) ; Edi Weitz's wrapper for the GD graphics library

(in-package #:evo-lisa)

(defstruct color
  (red 0 :type (integer 0 255))
  (blue 0 :type (integer 0 255))
  (green 0 :type (integer 0 255))
  (alpha 0 :type fixnum))

(defstruct point
  (x 0 :type fixnum)
  (y 0 :type fixnum))
 
(defstruct polygon
  (color (make-color) :type color)
  (points ))

(defstruct individual
  (polygons nil)
  (score 0 :type fixnum)
  (image nil))

(defparameter *target-image* nil)
(defparameter *num-initial-polygons* 5)
(defparameter *num-points-in-init-polygon* 5)
(defparameter *width* 0 "image width")
(defparameter *height* 0 "image height")
(defparameter *max-polygons* 50)
(defparameter *prob-mutate-color* 0.5)
(defparameter *tournament-size* 5)
(defparameter *parallel* nil "run the MPI parallel GA?")

(defun generate-random-color ()
   (make-color :red (random 256) :green (random 256) :blue (random 256) :alpha (+ 30 (random 50))))

(defun mutate-color (c)
  "Return new color based on old color"
  (make-color :red (max 0 (min 255 (- (color-red c) (floor (* (color-red c) (random 1.0))))))
	      :green (max 0 (min 255 (- (color-green c) (floor (* (color-green c) (random 1.0))))))
	      :blue (max 0 (min 255 (- (color-blue c) (floor (* (color-blue c) (random 1.0))))))
	      :alpha (max 0 (min 255 (- (color-alpha c) (floor (* (color-alpha c) (random 1.0))))))))

(defun generate-random-point ()
  (make-point :x (random *width*) :y (random *height*)))

(defun mutate-point (p)
  "return new point based on p"
  (make-point :x (max 0 (min *width* (- (point-x p) (floor (* (point-x p) (random 1.0))))))
	      :y (max 0 (min *height* (- (point-x p) (floor (* (point-x p) (random 1.0))))))))

(defun mutate-polygon (p)
  "return new polygon based on p"  
  (declare (optimize (debug 3)))
  (cond ((< (random 1.0) *prob-mutate-color*)
	 (make-polygon :color (mutate-color (polygon-color p)) :points (polygon-points p)))
	(t
	 (let ((points-copy (copy-seq (polygon-points p)))
	       (mutate-loc (random (length (polygon-points p)))))
	   (setf (nth mutate-loc points-copy) (mutate-point (elt points-copy mutate-loc)))
	   (make-polygon :color (polygon-color p) :points points-copy)))))

(defun make-random-polygon ()
  (make-polygon :color (generate-random-color)
		:points (loop repeat *num-points-in-init-polygon* collect (generate-random-point))))

(defun ind-add-polygon (ind)
  "adds polygon to ind"
  (make-individual :polygons (cons (make-random-polygon) (copy-seq (individual-polygons ind)))))

(defun ind-remove-polygon (ind)
  "removes polygon from ind"
  (make-individual :polygons (remove (elt (individual-polygons ind) (random (length (individual-polygons ind))))
				     (individual-polygons ind))))

(defun ind-mutate-polygon (ind)
  "Mutates an ind's polygon"
  (let* ((i (random (length (individual-polygons ind))))
	 (polygon-to-mutate (nth i (individual-polygons ind)))
	 (polygons-copy (copy-seq (individual-polygons ind))))
    (setf (nth i polygons-copy) (mutate-polygon polygon-to-mutate))
    (make-individual :polygons polygons-copy)))

(defun generate-random-individual ()
  (make-individual :polygons (loop repeat (1+ (random *num-initial-polygons*)) collect
		   (make-random-polygon))))

(defun mutate-individual (ind)
  (let* ((num-polygons (length (individual-polygons ind)))
	 (method (cond ((null (individual-polygons ind)) 4)
		       ((= 1 num-polygons (if (> (random 0.5) 0.5) 1 4)))
		       ((>= num-polygons *max-polygons*) (random 4))
		       (t (random 5)))))
    (cond ((< method 3) (ind-mutate-polygon ind))
	  ((= method 3) (ind-remove-polygon ind))
	  ((= method 4) (ind-add-polygon ind))
	  (t (assert nil)))))

(defun generate-image (ind)
  (declare (optimize (debug 3) (speed 0)(safety 3)))
  ;;(format t "evaluating ind=~a~%" ind)(force-output t)
  (let ((img (cl-gd:create-image *width* *height* t)))
    (setf (cl-gd:alpha-blending-p img) t)
    (cl-gd:allocate-color 0 0 0 :image img) ; background color
    (loop for polygon in (individual-polygons ind) 
	  for i from 0
	  for points = (loop for point in (polygon-points polygon)
			     append (list (point-x point) (point-y point))) do
	  ;(format t "points=~a~%" points)
	  (cl-gd:draw-polygon  points 
			       :color (cl-gd:allocate-color (color-red (polygon-color polygon))
							    (color-blue (polygon-color polygon))
							    (color-green (polygon-color polygon))
							    :alpha (color-alpha (polygon-color polygon))
							    :image img)
			       :image img :filled t)
	  ;(cl-gd:write-image-to-file (concatenate 'string "test" (format nil "~a" i) ".gif") :image img :if-exists :supersede)
	  )
    img))

(defun square (x) 
  (declare (optimize (debug 0) (speed 3)(safety 0)))
  (declare (type (integer 0 256) x))
  (the fixnum (* x x)))
(declaim (inline square))

(defun evaluate (ind)
  (declare (optimize (debug 0) (speed 3)(safety 0)))
  #+nil
  (if *parallel*
      (mpi:formatp t "evaluate ~a" ind)
      (format t "evaluate ~a" ind))
  (setf (individual-image ind) (generate-image ind))
  (let* ((err 0)
	(ind-image (individual-image ind))
	(ind-img (cl-gd::img ind-image))
	(target-img (cl-gd::img *target-image*)))
    (declare (fixnum err))

    (loop for y fixnum from 0 below *height* do
	  (loop for x fixnum from 0 below *width* do
		(let* ((img-color (cl-gd::gd-image-get-pixel ind-img y x ))
		       (target-color (cl-gd::gd-image-get-pixel target-img y x )))
 		  (incf err (square (- (the fixnum (cl-gd::gd-image-get-red ind-img img-color))
 				       (the fixnum (cl-gd::gd-image-get-red target-img target-color)))))
  		  (incf err (square (- (the fixnum (cl-gd::gd-image-get-blue ind-img img-color))
  				       (the fixnum (cl-gd::gd-image-get-blue target-img target-color)))))
  		  (incf err (square (- (the fixnum (cl-gd::gd-image-get-green ind-img img-color))
  				       (the fixnum (cl-gd::gd-image-get-green target-img target-color)))))
		  ;;slower version, using official API
		  #+nil
		  (progn
		    (incf err (square (- (the fixnum (cl-gd:color-component :red img-color :image ind-image))
					 (the fixnum (cl-gd:color-component :red target-color :image *target-image*)))))
		    (incf err (square (- (the fixnum (cl-gd:color-component :blue img-color :image ind-image))
					 (the fixnum (cl-gd:color-component :blue target-color :image *target-image*)))))
		    (incf err (square (- (the fixnum (cl-gd:color-component :green img-color :image ind-image))
					 (the fixnum (cl-gd:color-component :green target-color :image *target-image*))))))
		  )))
    ;;(cl-gd:write-image-to-file "test.gif" :image (individual-image ind) :if-exists :supersede)
    (cl-gd:destroy-image (individual-image ind))
    (setf (individual-image ind) nil
	  (individual-score ind) (- err))
    ;(format t "score  = ~a~%" (individual-score ind))
    (individual-score ind)))

(defun choose-individual-with-tournament (pop)
  (declare (optimize (speed 0)(debug 3)))
  (assert (<= *tournament-size* (length pop)))
  (loop repeat *tournament-size* 
	with best-score = most-negative-fixnum
	with best-index = nil
	for index = (random (length pop))
	for ind = (aref pop index)
	when (> (individual-score ind) best-score) do
	(setf best-score (individual-score ind)
	      best-index index)
	finally (return (aref pop best-index))))

(defun ga (pop-size num-generations)
  (declare (optimize (debug 3) (speed 0)))
  (assert (= 0 (mod pop-size 2)))
  (let ((pop (make-array pop-size :initial-contents
			 (loop repeat pop-size collect (generate-random-individual))))
	(bsf-score most-negative-fixnum)
	(bsf-individual nil))
    (loop repeat num-generations 
	  for gen from 0 do
	  (format t "begin generation ~a~%" gen)
	  ;(format t "begin generation: pop=~a~%" pop)
	  ;; spawn score computation jobs
	  (let ((scores
		 (if *parallel*
		     (par-eval::par-eval (loop for ind across pop collect `(evaluate ,ind)))
		     (make-array  pop-size :initial-contents (loop for ind across pop collect 
								   (evaluate ind)))
		     )))
	    ;; store scores in ind structures
	    (loop for ind across pop 
		  for i = 0 then (1+ i) do
		  (setf (individual-score ind) (aref scores i))
		  ;; save new best individual, if necessary
		  (when (> (individual-score ind) bsf-score)
		    (let ((img (generate-image ind)))
		      (cl-gd:write-image-to-file "bsf.gif" :image img :if-exists :supersede)
		      (cl-gd:destroy-image img))
		    (setf bsf-score (individual-score ind)
			  bsf-individual ind)
		    (format t "new bsf = ~a~%" bsf-score))
		  )
	    ;; generate new individuals and fill population with new individuals
	    (setf pop (make-array pop-size :initial-contents
				  (loop repeat pop-size collect 
					(mutate-individual  (choose-individual-with-tournament pop)))))
	    ;; replace worst individual with the best-so-far individual 
	    (setf pop (sort pop #'(lambda (x y) (> (individual-score x) (individual-score y)))))
	    (setf (aref pop (1- pop-size)) bsf-individual)
	    ))
    (format t "Finished GA.")
    (format t "Best individual = ~a~%" bsf-individual)
    (format t "Best score = ~a~%" bsf-score)
    ;; regenerate image for this individual
    (let ((img (generate-image bsf-individual)))
      (cl-gd:write-image-to-file "best.gif" :image img :if-exists :supersede)
      (cl-gd:destroy-image img))
    ))

(defun mpi-par-ga (pop-size num-generations )
  (mpi:load-mpi-foreign-libraries)
  (mpi:mpi-init)
  (cond ((= 0 (mpi::mpi-comm-rank))
	 (ga pop-size num-generations)
	 (par-eval::kill-slaves))
	(t
	 (par-eval::slave-server)))
  (mpi:mpi-barrier)
  (mpi:mpi-finalize))

(defun non-mpi-par-ga (pop-size num-generations)
  (ga pop-size num-generations))

(defun compress (filename &key (pop-size 10) (num-generations 10)(seed 1))
  ;(seed-sbcl-rand seed)
  (setf *target-image* (cl-gd:create-image-from-file filename))
  (setf *width* (cl-gd:image-width *target-image*)
	*height* (cl-gd:image-height *target-image*))
  (if *parallel*
      (mpi-par-ga pop-size num-generations)      
      (non-mpi-par-ga pop-size num-generations)))


(defun compare-images (image1-name image2-name)
  "Compare two images"
  (declare (optimize (debug 0) (speed 3)(safety 0)))
  (cl-gd:with-image-from-file (image1 image1-name)
    (cl-gd:with-image-from-file (image2 image2-name)
      (let ((err 0)
	    (height (cl-gd:image-height image1))
	    (width (cl-gd:image-width image1)))
	(declare (fixnum err height width))
	(format t "height1=~a, height2=~a" height width)
	(assert (and (= height (cl-gd:image-height image2)) 
		     (= width (cl-gd:image-width image2))))
	(loop for y fixnum from 0 below height do
	  (loop for x fixnum from 0 below width do
		(let ((img-color (cl-gd:get-pixel y x :image image2))
		      (target-color (cl-gd:get-pixel y x :image image1)))
		  (incf err (square (- (the fixnum(cl-gd:color-component :red img-color :image image2))
				       (the fixnum(cl-gd:color-component :red target-color :image image1)))))
		  (incf err (square (- (the fixnum (cl-gd:color-component :blue img-color :image image2))
				       (the fixnum(cl-gd:color-component :blue target-color :image image1)))))
		  (incf err (square (- (the fixnum(cl-gd:color-component :green img-color :image image2))
				       (the fixnum(cl-gd:color-component :green target-color :image image1))))))))
	err))))

(defun evo-lisa (filename pop-size num-generations)
  (let ((par-eval::*trace-pe* nil)
	(*parallel* t))
    (compress filename :pop-size pop-size :num-generations num-generations)))
