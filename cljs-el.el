;; cljs-el.el --- Enough of the ClojureScript core in EMACS lisp to get test.check working in elisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Matthew O. Smith

;; Author: Matthew O. Smith <m0smith@jacob>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:


;; (defclass cljs-el-lazy-cons-class ()
;;   ((car :initarg :car)
;;    (cdr-fn :initarg :cdr-fn)))

;; (defmethod cljs-el-lazy-cons-car ((obj cljs-el-lazy-cons))
;;   (oref obj :car))

;; (defmethod cljs-el-lazy-cons-cdr ((obj cljs-el-lazy-cons))
;;   (let ((cdr-fn (oref obj :cdr-fn)))
;;     (when cdr-fn
;;       (funcall cdr-fn))))

(defun cljs-el-lazy-cons (name &rest args)
  "Return a vector with ['cljs-el-lazy-cons name car cdr-fn]"
  (let ((l (length args)))
    (assert (or (= l 2) (= l 4)) nil "Args must be 2 or 4 elements")
    (if (= 2 l) 
	(vector 'cljs-el-lazy-cons name (elt args 0) (elt args 1))
      (vector 'cljs-el-lazy-cons name (plist-get args :car) (plist-get args :cdr-fn)))))


(defun cljs-el-lazy-cons-car (cn)
  (elt cn 2))

(defun cljs-el-lazy-cons-cdr (cn)
  (let ((cdr-fn (elt cn 3)))
    (when cdr-fn
      (funcall cdr-fn))))

(defun cljs-el-lazy-cons-p (cn)
  (and (vectorp cn)
       (= 4 (length cn))
       (eq (elt cn 0) 'cljs-el-lazy-cons)))


;;
;;  The CAR of a chunk is a list of values
;;  The CDR is a thunk to the next lazy chunk

;; (defclass cljs-el-lazy-chunk-class (cljs-el-lazy-cons-class)
;;   ((car :initarg :car)
;;    (cdr-fn :initarg :cdr-fn)))

;; (defmethod cljs-el-lazy-cons-car ((obj cljs-el-lazy-chunk-class))
;;   (let ((chunk (oref obj :car)))
;;     (car chunk)))


;; (defmethod cljs-el-lazy-cons-cdr ((obj cljs-el-lazy-chunk-class))
;;   (let ((chunk (oref obj :car)))
;;     (if (cdr chunk)
;; 	(oset obj :car (cdr chunk))
;;       (let ((next-chunk (funcall (oref obj :cdr-fn))))
;; 	(oset obj :car (oref next-chunk :car))
;; 	(oset obj :cdr-fn (oref next-chunk :cdr-fn))))
;;     obj))

;; ;;

	  
(defun cljs-el-lazy-chunk (name &rest args)
  "Return a vector with ['cljs-el-lazy-chunk name car cdr-fn]"
  (let ((l (length args)))
    (assert (or (= l 2) (= l 4)) nil "Args must be 2 or 4 elements")
    (if (= 2 l) 
	(vector 'cljs-el-lazy-chunk name (elt args 0) (elt args 1))
      (vector 'cljs-el-lazy-chunk name (plist-get args :car) (plist-get args :cdr-fn)))))


(defun cljs-el-lazy-chunk-car (obj)
  (let ((chunk (elt obj 2)))
    (car chunk)))

(defmethod cljs-el-lazy-chunk-cdr (obj)
  (let ((chunk (elt obj 2)))
    (if (cdr chunk)
	(aset obj 2 (cdr chunk))
      (let ((next-chunk (funcall (elt obj 3))))
	(aset obj 2 (elt next-chunk 2))
	(aset obj 3 (elt next-chunk 3))))
    obj))

(defun cljs-el-lazy-chunk-p (cn)
  (and (vectorp cn)
       (= 4 (length cn))
       (eq (elt cn 0) 'cljs-el-lazy-chunk)))

	

(defun cljs-el-cons (item coll)
  (cons item coll))

(defun cljs-el-car (coll)
  (cond ((cljs-el-lazy-cons-p coll) (cljs-el-lazy-cons-car coll))	
	((cljs-el-lazy-chunk-p coll) (cljs-el-lazy-chunk-car coll))
	((listp coll) (car coll))
	((arrayp coll) (when (<  0 (length coll)) (elt coll 0)))
	(t (error "Unknown type %s" coll))))

(defun cljs-el-cdr (coll)
  (cond ((cljs-el-lazy-cons-p coll)  (cljs-el-lazy-cons-cdr coll))
	((cljs-el-lazy-chunk-p coll)  (cljs-el-lazy-chunk-cdr coll))
	 ((arrayp coll) (when (< 1 (length coll)) (subseq coll 1)))
	 (t  (cdr coll))))


(defun cljs-el-conj (coll x &rest xs)
  "conj[oin]. Returns a new collection with the xs 'added'. (conj
    nil item) returns (item).  The 'addition' may happen at
    different 'places' depending on the concrete type.

   EMACS lists have the elements added at the first ala `cons' while
   array types have the elements added at the end."
      (cond ((cljs-el-lazy-cons-p coll)  (cljs-el-lazy-chunk "conj" (cons x xs) (elt coll 3)))
	    ((cljs-el-lazy-chunk-p coll) (cljs-el-lazy-chunk "conj" (cons x xs) (elt coll 3)))
	    ((listp coll) (if xs
			      (cljs-el-reduce3 (lambda (c a) (cons a c)) (cons x coll) xs)
			    (cons x coll)))
	    ((vectorp coll) (vconcat coll (vector x) (when xs (cljs-el-vec  xs))))
	    (t (error "Unknown type %s %s" (typeof coll) ))))
    

(defun cljs-el-into (to-coll from-coll)
  "Returns a new coll consisting of TO-COLL with all of the items of
  FROM-COLL conjoined."
    
  (funcall 'cljs-el-conj to-coll (cljs-el-car from-coll) (cljs-el-cdr from-coll)))
  

(defun cljs-el-iterate (f x)
  "Returns a chunked lazy sequence of x, (f x), (f (f x)) etc. f must be free of side-effects"
  (let ((rtnval (list x)))
    (dotimes (_ 32 rtnval)
      (setq rtnval (cons (funcall f (car rtnval)) rtnval)))
    (let ((c (car rtnval)))
      (cljs-el-lazy-chunk "iterate" 
			  :car (nreverse rtnval)
			  :cdr-fn (lambda () (cljs-el-iterate f (funcall f c)))))))


(defun cljs-el-range (&rest args)
  "Returns a lazy seq of nums from start (inclusive) to end
  (exclusive), by step, where start defaults to 0, step to 1, and end to
  infinity. When step is equal to 0, returns an infinite sequence of
  start. When start is equal to end, returns empty list."
  (cond
   ((= (length args) 0)
    (cljs-el-iterate '1+ 0))
   ((= (length args) 2)
    (destructuring-bind (start end) args
      (if (= start end)
	  '()
	(number-sequence start (1- end)))))))

(defun cljs-el-cycle* (coll orig-coll)
  (if (cljs-el-seq coll)
      (cljs-el-lazy-cons "cycle*"
			 :car (cljs-el-car coll)
			 :cdr-fn (lambda () (cljs-el-cycle* (cljs-el-cdr coll) orig-coll)))
    (cljs-el-cycle* orig-coll orig-coll)))

(defun cljs-el-cycle (coll)
  (when (cljs-el-seq coll)
    (cljs-el-cycle* coll coll)))

(defun cljs-el-seq (coll)
  "Returns a something compatible with cljs-el-lazy-cons. If the collection is
    empty, returns nil.  (seq nil) returns nil. "
  (when coll
    (cond ((cljs-el-lazy-cons-p coll) coll)
	  ((cljs-el-lazy-chunk-p coll) coll)
	  ((listp coll) coll)
	  ((and (arrayp coll) (< 0 (length coll))) coll))))
	  
  
(defun cljs-el-take (n coll)
  "Returns a lazy sequence of the first n items in coll, or all items if
  there are fewer than n."
  (when (and (> n 0) (cljs-el-seq coll))
    (cljs-el-lazy-cons "take"
		       :car (cljs-el-car coll)
		       :cdr-fn (lambda () (cljs-el-take (1- n) (cljs-el-cdr coll))))))

(defun cljs-el-take-while (pred coll)
  "Returns a lazy sequence of the first n items in coll, or all items if
  there are fewer than n."
  (when (and pred (cljs-el-seq coll))
    (let ((value (cljs-el-car coll)))
      (when (funcall pred value)
	(cljs-el-lazy-cons "take-while"
			   :car (cljs-el-car coll)
			   :cdr-fn (lambda () (cljs-el-take-while pred (cljs-el-cdr coll))))))))

(defun cljs-el-reduce2 (f coll)
  (let ((value (cljs-el-car coll))
	(rest  (cljs-el-cdr coll)))
    (if (cljs-el-seq rest)
	(cljs-el-reduce3 f value rest)
      value)))

(defun cljs-el-reduce3 (f value coll)
  " f should be a function of 2 arguments. If val is not supplied,
returns the result of applying f to the first 2 items in coll, then
applying f to that result and the 3rd item, etc. If coll contains no
items, f must accept no arguments as well, and reduce returns the
result of calling f with no arguments.  If coll has only 1 item, it
is returned and f is not called.  If val is supplied, returns the
result of applying f to val and the first item in coll, then
applying f to that result and the 2nd item, etc. If coll contains no
items, returns val and f is not called."
  (let ((accum value))
    (while (cljs-el-seq coll)
      (setq accum (funcall f accum (cljs-el-car coll)))
      (setq coll (cljs-el-cdr coll)))
    accum))

(defun cljs-el-vec (coll)
  (apply 'vector  (cljs-el-list coll)))

(defun cljs-el-list (coll)
  (when coll 
    (reverse (cljs-el-reduce3 (lambda (accum val) (cons val accum)) '() coll))))

(defun cljs-el-filter (pred coll)
  (when (and pred (cljs-el-seq coll))
    (let ((the-car (cljs-el-car coll)))
      (while (and (cljs-el-seq coll) (not (funcall pred the-car)))
	(setq coll (cljs-el-cdr coll))
	(setq the-car (cljs-el-car coll)))
      (when (cljs-el-seq coll)
	(cljs-el-lazy-cons "filter"
			   :car the-car
			   :cdr-fn (lambda () (cljs-el-filter pred (cljs-el-cdr coll))))))))


(defun cljs-el-map (f &rest colls)
  (if (< 0 (length colls))
      (destructuring-bind (coll) colls
	(if (cljs-el-seq coll)
	    (cljs-el-lazy-cons "map"
			       :car (funcall f (cljs-el-car coll))
			       :cdr-fn (lambda() (cljs-el-map f (cljs-el-cdr coll))))))))

(provide 'cljs-el)
;;; cljs-el.el ends here
