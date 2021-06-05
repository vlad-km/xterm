;;; -*- mode:lisp; coding:utf-8 -*-

;;; Stream prototype for dumb-terminal
;;; @vlad-km, 2021

(in-package :xterm)

(defclass terminal-stream ()
  ((buffer :initform nil
           :accessor terminal-stream-buffer)
   (buffer-length :initform 0
                 :accessor terminal-stream-buffer-length)
   (dirty :initform nil
          :accessor terminal-stream-buffer-dirty)
   (right-margin :initform nil
                 :accessor terminal-stream-right-margin)
   (line   :initform 0
           :accessor terminal-stream-buffer-line)
   (column :initform 0
           :accessor terminal-stream-buffer-column)
   (length :initform 0
           :accessor terminal-stream-length)))


(defun dirty-terminal-stream-buffer ()
  (setf (terminal-stream-buffer-dirty *tsb*) t))

(defun terminal-stream-buffer-line-at-start ()
  (= (terminal-stream-buffer-column *tsb*) 0))

(defun clear-terminal-stream-buffer ()
  (when (terminal-stream-buffer-dirty *tsb*)
    (setf (terminal-stream-buffer *tsb*)
          (make-array 0 :fill-pointer 0 :element-type 'character)
          (terminal-stream-buffer-dirty *tsb*)
          nil)
    (incf (terminal-stream-length *tsb*)
          (terminal-stream-buffer-length *tsb*))
    (setf (terminal-stream-buffer-length *tsb*) 0))
  (values))


(defun make-string-image (str)
  str)

(defvar *CR* (code-char 13))

(defun terminal-stream-out (str)
  (let ((ch)
        (si (make-string-image str))
        (buffer (terminal-stream-buffer *tsb*))
        (r-margin (terminal-stream-right-margin *tsb*)))
    (when (null buffer)
      (setq buffer (make-array 0 :fill-pointer 0 :element-type 'character))
      (setf (terminal-stream-buffer *tsb*) buffer))
    
    (dotimes (i (length si))
      (setq ch (aref si i))
      (cond  ((eq ch #\newline)
              (incf (terminal-stream-buffer-line *tsb*))
              (setf (terminal-stream-buffer-column *tsb*) 0)
              (vector-push-extend ch buffer)
              (vector-push-extend *CR* buffer))
             ((eq ch *CR*)
              (incf (terminal-stream-buffer-line *tsb*))
              (setf (terminal-stream-buffer-column *tsb*) 0)
              (vector-push-extend #\newline buffer))
             (t
              (vector-push-extend ch buffer)
              (incf (terminal-stream-buffer-column *tsb*)))))
    (dirty-terminal-stream-buffer)
    (values)))

(defun terminal-stream-finish ()
  (when (dirty-terminal-stream-buffer)
    (term-write (terminal-stream-buffer *tsb*))
    #+nil (if (terminal-stream-buffer-line-at-start)
        (term-writeln ""))
    (clear-terminal-stream-buffer)))

(defvar *tsb*)
(defun make-terminal-stream ()
  (make-instance 'terminal-stream))

(defvar stdout
  (jscl::make-stream
   :write-fn (lambda (string)
               (terminal-stream-out string)
               (terminal-stream-finish))))


;;; input system queue

;;; holds esc-seq & chars from keyboard
;;; via xterm:on.key
;;;
(defclass kb-stream ()
  ((head   :initform 0
           :accessor kb-stream-head)
   (tail   :initform 0
           :accessor kb-stream-tail)
   (size   :initform nil
           :initarg :size)
   (buffer :initarg :buffer
           :initform nil
           :accessor kb-stream-buffer)
   (index :initarg :index
          :initform nil
          :accessor kb-stream-index)))

(defun make-kb-stream (&key size)
  (make-instance 'kb-stream
                 :buffer (make-array (list size) :initial-element nil)
                 :size size))

;;; push element
;;; return nil if buffer is full
(defun kb-stream-write (stream value)
    (let ((next (1+ (kb-stream-tail stream))))
        (if (>= next (kb-stream-size stream)) (setq next 0))
        (if (null (= next (kb-stream-head stream)))
            (setf (aref (kb-stream-buffer stream) (kb-stream-tail stream)) value
                  (kb-stream-tail stream) next)
            nil)))

;;; pop element from buffer
;;; return (nil nil) if empty buffer
(defun kb-stream-read (stream)
  (if (not (= (kb-stream-head stream) (kb-stream-tail stream)))
      (let ((value (aref (kb-stream-buffer stream) (kb-stream-head stream)))
            (next (1+ (kb-stream-head stream))))
        (if (>= next (kb-stream-size stream))
            (setq next 0))
        (setf (kb-stream-head stream) next)
        (values value t))
      (values nil nil)))


;;; reset queue
(defun kb-stream-reset (stream)
    (setf (kb-stream-head stream) 0 (kb-stream-tail stream) 0)
    (values))

(in-package :cl-user)
;;;EOF
