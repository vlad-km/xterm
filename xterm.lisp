;;; -*- mode:lisp; coding:utf-8 -*-

;;; 
;;; tiny api for https://github.com/xtermjs/xterm.js
;;;
;;; @vlad-km, 2021
;;;
;;; 

(defpackage #:xterm
  (:use #:cl))

(in-package :xterm)

(defmacro defn- (name lambda &body body)
  `(progn
     (eval-when (:compile-toplevel)
       (jscl::fn-info ',name :defined t))
     (jscl::fset ',name #'(jscl::named-lambda ,name ,lambda ,@body))
     (export '(#::,name))
     ',name))

(defun str.to-lowercase (s)
  (let ((js (jscl::lisp-to-js s)))
    (funcall ((jscl::oget js  "toLowerCase" "bind") js))))


(defn- {new} (&rest kv)
  (let* ((obj (jscl::new))
         (idx 0)
         (key-val))
    (if (oddp (length kv))
        (error "{new}: Too few arguments"))
    (dolist (it kv)
      (cond ((oddp idx)
             (setf (jscl::oget obj key-val) it))
            (t
             (typecase it
               (keyword (setq key-val (str.to-lowercase (symbol-name it))))
               (symbol (setq key-val (symbol-name it)))
               (t (setq key-val it)))))
      (incf idx))
    obj))


(defmacro fork (time &rest forms)
  `(#j:setTimeout
    (lambda ()
      ,@forms)
    ,time))

(defvar +ESC+ (code-char #x1b))
(defvar +CSI+ (code-char #x9b))

(export '(xterm::+esc+ xterm::+csi+))


(defmacro make-esc-seq (code)
  `(jscl::concat +ESC+ ,code))

(export '(xterm::make-esc-seq))

(defun ansi-rgb (r g b)
  (jscl::concat +ESC+ "[38;2;" r ";" g ";" b))

(defvar +bg-black+ (make-esc-seq "[40m"))
(defvar +bg-red+ (make-esc-seq "[41m"))
(defvar +bg-green+ (make-esc-seq "[42m"))
(defvar +bg-yellow+ (make-esc-seq "[43m"))
(defvar +bg-blue+ (make-esc-seq "[44m"))
(defvar +bg-grey+ (make-esc-seq "[100m"))


#+nil (jscl::load-js "node_modules/xterm/lib/xterm.js")
#+nil (jscl::load-css "node_modules/xterm/css/xterm.css")
#+nil (jscl::load-js "node_modules/xterm-addon-fit/out/FitAddon-fit.js")

;;; vieport vars

;;; This ranges between 0 (left side) and Terminal.cols (after last cell of the row)
(defn- buffer.cursor.x nil #j:term:buffer:active:cursorX)

;;; This ranges between 0 (left side) and Terminal.cols (after last cell of the row)
(defn- buffer.cursor-y nil #j:term:buffer:active:cursorY)

;;; The line within the buffer where the top of the bottom page is (when fully scrolled down
(defn- term.buffer.base-y nil #j:term:buffer:active:baseY)

;;; The line within the buffer where the top of the viewport is.
(defn- term.buffer.viewport-y nil #j:term:buffer:active:viewportY)

;;; The amount of lines in the buffer.
;;; Wrong values possible. In some cases lies
(defn- term.buffer.length nil #j:term:buffer:active:length)

;;; The number of rows in the terminal’s viewport.
;;; Use ITerminalOptions.rows to set this in the constructor
;;; and Terminal.resize for when the terminal exists.
(defn- term.rows () #j:term:rows)

;;; The number of columns in the terminal’s viewport.
;;; Use ITerminalOptions.cols to set this in the constructor
;;; and Terminal.resize for when the terminal exists.
(defn- term.cols  #j:term:cols)

;;; internal xterm:active:buffer vars
(defn- buffer.scroll.top nil #j:term:buffer:active:_buffer:scrollTop)
(defn- buffer.scroll.buttom nil #j:term:buffer:active:_buffer:scrollBottom)
(defn- buffer.x nil #j:term:buffer:active:_buffer:x)
(defn- buffer.y nil #j:term:buffer:active:_buffer:y)
(defn- buffer.ybase nil #j:term:buffer:active:_buffer:ybase)
(defn- buffer.ydisp nil #j:term:buffer:active:_buffer:ydisp)

;;; started terminal options
(defvar *theme*
  ({new}  "background" "black"
          "foreground" "green"
          "cursor" "yellow"))

(defvar *options*
  ({new} :rows 20
         :cols 132
         :theme *theme*
         "fontSize" 13
         "rightClickSelectsWord" t
         "rendererType" "canvas"
         "fontFamily" "consolas"))

;;; create terminal instance
(defn- make-terminal (options)
  (setf #j:term
        (jscl::make-new #j:Terminal options)))

;;; open it
(defn- term.open (element)
  (#j:term:open (#j:document:getElementById element)))

(defn- term.dispose nil (#j:term:dispose))

;;; xterm.js api
(defn- on.dispose (d) (funcall (jscl::oget d "dispose")))
(defn- on.key (drv) (#j:term:onKey drv))
(defn- on.data (drv) (#j:term:onData drv))
(defn- term.write (seq) (#j:term:write seq))
(defn- term.writeln (seq) (#j:term:writeln seq))
(defn- term.dispose nil (#j:term:dispose))
(defn- term.blur nil (#j:term:blur))
;;; note: `clear` dirty source code. use `reset`
(defn- term.clear nil (#j:term:clear))
(defn- term.reset nil (#j:term:reset))
(defn- term.focus nil (#j:term:focus))
(defn- term.getOption (k) (#j:term:getOption k))
(defn- term.resize (c r) (#j:term:resize c r))
;;; scrolling
(defn- scroll.lines (n) (#j:term:scrollLines n ))
(defn- scroll.up () (#j:term:scrollLines -1 ))
(defn- scroll.down () (#j:term:scrollLines 1))
(defn- scroll.top () (#j:term:scrollToTop))
(defn- scroll.bottom () (#j:term:scrollToBottom))
(defn- scrollPages (c) (#j:term:scrollPages c))
(defn- scrollPage.up nil (#j:term:scrollPages -1))
(defn- scrollPage.down nil (#j:term:scrollPages 1))
(defn- scrollToLine (n) (#j:term:scrollToLine n))
(defn- setOption (k v) (#j:term:setOption k v))

;;; selection
(defn- select.lines (start end) (#j:term:selectLines start end))
(defn- select (col row length) (#j:term:select col row length))
(defn- clearSelection nil (#j:term:clearSelection))
(defn- hasSelection nil (#j:term:hasSelection))
(defn- getSelectionPosition nil
  (when (hasSelection)
    (let ((r (#j:term:getSelectionPosition)))
      (values (jscl::oget r "startRow")
              (jscl::oget r "startColumn")
              (jscl::oget r "endRow")
              (jscl::oget r "endColumn")) )))

;;; trim	boolean	Whether to trim any whitespace at the right of the line.
;;; start	number	The column to start from (inclusive).
;;; end 	number	The column to end at (exclusive).
(defn- get-buffer-line (num &optional (trim t) start end)
  (#j:term:buffer:active:_buffer:translateBufferLineToString num trim start end))

;;; 
(defn- get-wrapped-region (line)
  (let ((result
          (#j:term:buffer:active:_buffer:getWrappedRangeForLine line)))
    (values (jscl::oget result "first")
            (jscl::oget result "last"))))

;;; SM IRM
(defn- insert.mode.on nil (term.write (jscl::concat +CSI+ 4 "h")))

;;; RM IRM
(defn- replace.mode.on nil (term.write (jscl::concat +CSI+ 4 "l")))

(defn- delete.columns (&optional (col 1)) (term.write (jscl::concat +CSI+ col "'~")))

;;; bad effect
;;; term.buffer._core._coreService.decPrivateModes.origin
;;; #j:term:buffer:_core:_coreService:decPrivateModes:origin
(defn- set-origin-mode () (term.write (jscl::concat +CSI+ "?" 6 "h")))

;;; CSI Ps ; Ps r
;;;          Set Scrolling Region [top;bottom] (default = full size of
;;;          window) (DECSTBM), VT100.
(defn- set-scroll-region (top bottom)
  (term.write (jscl::concat +CSI+ top ";" bottom "r")))

;;; CUP
(defn- cursor.position (&optional (row 0) (col 0))
  (term.write (jscl::concat +CSI+ row ";" col "H")))

;;; CUF
(defn- cursor.forward (&optional (row 1))
  (term.write (jscl::concat +CSI+ row "C")))

;;; CUB
(defn- cursor.backward (&optional (row 1))
  (term.write (jscl::concat +CSI+ row "D")))

;;; CUU
(defn- cursor.up (&optional (col 1))
  (term.write (jscl::concat +CSI+ col "A")))

;;; CPL
(defn- cursor.up.first (&optional (col 1))
  (term.write (jscl::concat +CSI+ col "F")))

;;; CHA
(defn- cursor.horizontal.move (&optional (col 1))
  (term.write (jscl::concat +CSI+ col "G")))

;;; CUD
(defn- cursor.down (&optional (col 1))
  (term.write (jscl::concat +CSI+ col "B")))

;;; VPA	Vertical Position Absolute
;;; CSI Ps d	Move cursor to Ps-th row (default=1).
(defn- cursor.vertical.move (&optional(row 1))
  (term.write (jscl::concat +CSI+ row "d")))

(defn- cursor.save () (term.write (jscl::concat +CSI+ "s")))
(defn- cursor.restore () (term.write (jscl::concat +CSI+ "u")))

;;; ED
;;; 0	Erase from the cursor through the end of the viewport.
;;; 1	Erase from the beginning of the viewport through the cursor.
;;; 2	Erase complete viewport.
;;; 3	Erase scrollback.
(defn- erase.display (&optional (part 0))
  (term.write (jscl::concat +CSI+ part "J")))
  
;;; EL
(defn- erase.line (&optional (part 0))
  (term.write (jscl::concat +CSI+ part "K")))

;;; IL
(defn- insert.line (&optional (rows 1))
  (term.write (jscl::concat +CSI+ rows "L")))

;;; DL
(defn- delete.line (&optional (rows 1))
  (term.write (jscl::concat +CSI+ rows "M")))

;;; DCH
(defn- delete.char (&optional (rows 1))
  (term.write (jscl::concat +CSI+ rows "P")))

;;; ECH
(defn- erase.char (&optional (rows 1))
  (term.write (jscl::concat +CSI+ rows "X")))

;;; ICH
(defn- insert.char (&optional col 1)
  (term.write (jscl::concat +CSI+ col "@")))

(in-package :cl-user)

;;;EOF
