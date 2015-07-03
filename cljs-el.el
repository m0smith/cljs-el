;;; cljs-el.el --- Enough of the ClojureScript core in EMACS lisp to get test.check working in elisp  -*- lexical-binding: t; -*-

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


(defclass cljs-el-lazy-cons ()
  ((car :initarg :car)
   (cdr-fn :initarg :cdr-fn)))

(defmethod cljs-el-lazy-cons-car ((obj cljs-el-lazy-cons))
  (oref obj :car))

(defmethod cljs-el-lazy-cons-cdr ((obj cljs-el-lazy-cons))
  (let ((cdr-fn (oref obj :cdr-fn)))
    (when cdr-fn
      (funcall cdr-fn))))

(defun cljs-el-cons (item coll)
  (cons item coll))

(defun cljs-el-car (coll)
  (if (cljs-el-lazy-cons-p coll)
      (cljs-el-lazy-cons-car coll)
    (car coll)))

(defun cljs-el-cdr (coll)
  (if (cljs-el-lazy-cons-p coll)
      (cljs-el-lazy-cons-cdr coll)
    (cdr coll)))


(defun cljs-el-iterate (f x)
  "Returns a lazy sequence of x, (f x), (f (f x)) etc. f must be free of side-effects"
  (let ((rtnval (funcall f x)))
    (cljs-el-lazy-cons "iterate" :car rtnval :cdr-fn (lambda () (cljs-el-iterate f rtnval)))))

(defun cljs-el-range (&rest args)
  "Returns a lazy seq of nums from start (inclusive) to end
  (exclusive), by step, where start defaults to 0, step to 1, and end to
  infinity. When step is equal to 0, returns an infinite sequence of
  start. When start is equal to end, returns empty list."
  (cond
   ((= (length args) 0)
    (cljs-el-iterate '1+ 0))))

(defun cljs-el-seq (coll)
  "Returns a something compatible with cljs-el-lazy-cons. If the collection is
    empty, returns nil.  (seq nil) returns nil. "
  (or (cljs-el-lazy-cons-p coll)
      (and (sequencep coll) coll)))
  
(defun cljs-el-take (n coll)
  "Returns a lazy sequence of the first n items in coll, or all items if
  there are fewer than n."
  (when (and (> n 0) (cljs-el-seq coll))
    (cljs-el-lazy-cons "take"
		       :car (cljs-el-car coll)
		       :cdr-fn (lambda () (cljs-el-take (1- n) (cljs-el-cdr coll))))))

(defun cljs-el-reduce (f &rest args)
" f should be a function of 2 arguments. If val is not supplied,
returns the result of applying f to the first 2 items in coll, then
applying f to that result and the 3rd item, etc. If coll contains no
items, f must accept no arguments as well, and reduce returns the
result of calling f with no arguments.  If coll has only 1 item, it
is returned and f is not called.  If val is supplied, returns the
result of applying f to val and the first item in coll, then
applying f to that result and the 2nd item, etc. If coll contains no
items, returns val and f is not called."
  (if (= 1 (length args))
      (let ((coll (car args)))
	(cljs-el-reduce f (cljs-el-car coll) (cljs-el-cdr coll)))
    (destructuring-bind (value coll) args
      (if (cljs-el-seq coll)
	  (cljs-el-reduce f (funcall f value (cljs-el-car coll)) (cljs-el-cdr coll))
	value))))

(provide 'cljs-el)
;;; cljs-el.el ends here
