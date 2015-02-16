;;; rpn-calc.el --- quick RPN calculator for hackers

;; Copyright (C) 2015 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/
;; Version: 1.0.0beta
;; Package-Requires: ((popup "0.4"))

;;; Commentary:

;; Try: "M-x rpn-calc[RET] 1 2+3/sin"

;;; Change Log:

;;; Code:

(require 'popup)

(defconst rpn-calc-version "1.0.0beta")

(defgroup rpn-calc nil
  "quick RPN calculator for hackers"
  :group 'emacs)

;; + custom

(defcustom rpn-calc-operator-table
  '(("+" 2 . +)
    ("--" 2 . -)                      ; "10 -10" should be "10 (- 10)"
    ("/" 2 . /)
    ("*" 2 . *)
    ("<<" 2 . ash)
    (">>" 2 . (lambda (a b) (ash a (- b))))
    ("sin" 1 . sin)
    ("cos" 1 . cos)
    ("tan" 1 . tan))
  "list of (NAME ARITY . FUNCTION)."
  :group 'rpn-calc)

(defcustom rpn-calc-incompatible-minor-modes
  '(phi-autopair-mode key-combo-mode)
  "list of minor-modes that should be disabled while RPN calc is
active."
  :group 'rpn-calc)

;; + utils

;; *FIXME* incorrect for huge integers (rpn-calc--int-to-hex ?\xffffff)

(defun rpn-calc--int-to-bin (int)
  (let* ((str (make-string 32 0)))
    (dotimes (n 32)
      (aset str (- 32 1 n) (if (zerop (logand int 1)) ?0 ?1))
      (setq int (ash int -1)))
    (cond ((string-match "^1*1111\\(1\\|[^1].*\\)$" str)
           (concat "..11" (match-string 1 str)))
          ((string-match "^0*\\(0\\|[^0].*\\)$" str)
           (match-string 1 str))
          (t
           str))))

(defun rpn-calc--int-to-hex (int)
  (let* ((str (make-string 8 0)))
    (dotimes (n 8)
      (aset str (- 8 1 n)
            (cl-case (logand int 15)
              ((0) ?0) ((1) ?1) ((2) ?2) ((3) ?3)
              ((4) ?4) ((5) ?5) ((6) ?6) ((7) ?7)
              ((8) ?8) ((9) ?9) ((10) ?a) ((11) ?b)
              ((12) ?c) ((13) ?d) ((14) ?e) ((15) ?f)))
      (setq int (ash int -4)))
    (string-match "^0*\\(0\\|[^0].*\\)$" str)
    (match-string 1 str)))

(defun rpn-calc--float-to-ieee754 (float)
  ;; based on ieee-754.el
  (let (IEEE-sign IEEE-exp IEEE-mantissa exp)
    (if (isnan float) "NaN"
      (when (< float 0)                  ; negative
        (setq IEEE-sign t
              float (- float)))
      (cond ((= float 0)                   ; zero
             (setq IEEE-exp 0
                   IEEE-mantissa 0))
            ((= float 1e+INF)              ; infinite
             (setq IEEE-exp 255
                   IEEE-mantissa 0))
            ((<= (setq exp (floor (log float 2.0))) -127) ; subnormal (very small)
             (setq IEEE-exp 0
                   IEEE-mantissa (round (* float 0.5 (expt 2.0 127) (lsh 1 23)))))
            (t                                      ; normal
             (setq IEEE-exp (+ exp 127)
                   IEEE-mantissa (round (* (- (/ float (expt 2.0 exp)) 1) (lsh 1 23))))))
      (let ((str (format "%x%06x"
                         (+ (if IEEE-sign 128 0) (lsh IEEE-exp -1))
                         (+ (lsh (logand IEEE-exp 1) 23) IEEE-mantissa))))
        (string-match "^0*\\(0\\|[^0].*\\)$" str)
        (match-string 1 str)))))

;; + core

;; *TODO* operator table should be a (patricia)-trie
;; *TODO* maybe apply function if inserted object is a function ?

(defvar rpn-calc--saved-minor-modes nil)
(defvar rpn-calc--temp-buf nil)
(defvar rpn-calc--stack nil)
(defvar rpn-calc--popup nil)

(defconst rpn-calc-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap [remap self-insert-command] 'rpn-calc-self-insert)
    (define-key kmap (kbd "DEL") 'rpn-calc-backspace)
    kmap))

(define-minor-mode rpn-calc
  "quick RPN calculator for hackers"
  :init-value nil
  :keymap rpn-calc-map
  (cond (rpn-calc
         (setq rpn-calc--stack    nil
               rpn-calc--temp-buf (get-buffer-create " *rpn-calc*")
               rpn-calc--saved-minor-modes
               (mapcar (lambda (mode) (when mode (prog1 mode (funcall mode -1))))
                       rpn-calc-incompatible-minor-modes)
               rpn-calc--popup
               (popup-create (point) 60 10 :selection-face 'popup-menu-selection-face))
         (add-hook 'post-command-hook 'rpn-calc--post-command-hook)
         (add-hook 'pre-command-hook 'rpn-calc--pre-command-hook)
         (rpn-calc--post-command-hook))
        (t
         (remove-hook 'post-command-hook 'rpn-calc--post-command-hook)
         (remove-hook 'pre-command-hook 'rpn-calc--pre-command-hook)
         (popup-delete rpn-calc--popup)
         (mapc 'funcall rpn-calc-incompatible-minor-modes)
         (kill-buffer rpn-calc--temp-buf))))

(defun rpn-calc--pre-command-hook ()
  (unless (and (symbolp this-command)
               (string-prefix-p "rpn-" (symbol-name this-command)))
    (rpn-calc -1)))

(defun rpn-calc--post-command-hook ()
  (condition-case err
      (rpn-calc--maybe-commit-current-input)
    (error (message (error-message-string err))))
  (rpn-calc--refresh-popup))

(defun rpn-calc--maybe-commit-current-input ()
  (with-current-buffer rpn-calc--temp-buf
    (let* ((obj (ignore-errors (read (buffer-string))))
           (name (when (symbolp obj) (symbol-name obj))))
      (cond ((null obj)              ; input is unreadable yet -> fail
             nil)
            ((or (and (consp obj) (= (char-after 1) ?\()) ; a complete list
                 (vectorp obj)          ; a complete vector
                 (memql (char-before) '(?\s ?\t ?\n))) ; other complete input
             (erase-buffer)
             (push (eval obj) rpn-calc--stack))
            ((null name)                ; obj is not a symbol -> fail
             nil)
            ((string-match              ; a number
              "^[+-]?[0-9]*\\(?:[0-9]\\|\\.[0-9]+\\)\\(?:e[+-]?\\(?:[0-9]+\\|INF\\)\\)?"
              name)
             (push (read (match-string 0 name)) rpn-calc--stack)
             (erase-buffer)
             (insert (substring name (match-end 0)))
             (rpn-calc--maybe-commit-current-input))
            ((setq obj (let (val)       ; an operator
                         (catch 'ret
                           (dolist (entry rpn-calc-operator-table)
                             (when (string-prefix-p name (car entry))
                               (if (not (string= name (car entry)))
                                   (throw 'ret nil)
                                 (setq val (cdr entry)))))
                           val)))
             (erase-buffer)
             (if (zerop (car obj))
                 (push (funcall (cdr obj)) rpn-calc--stack)
               (let* ((last-cell (nthcdr (1- (car obj)) rpn-calc--stack))
                      (args rpn-calc--stack))
                 (unless last-cell
                   (error "too few arguments for the operator"))
                 (setq rpn-calc--stack (cdr last-cell))
                 (setcdr last-cell nil)
                 (push (apply (cdr obj) (nreverse args)) rpn-calc--stack))))))))

(defun rpn-calc--refresh-popup ()
  (with-current-buffer rpn-calc--temp-buf
    (let* ((num (ignore-errors (read (buffer-string))))
           (head (concat (buffer-string) (rpn-calc--annotation num)))
           (stack (mapcar (lambda (item)
                            (concat (prin1-to-string item) (rpn-calc--annotation item)))
                          rpn-calc--stack)))
      (popup-set-list rpn-calc--popup (cons head stack))
      (popup-draw rpn-calc--popup))))

(defun rpn-calc--annotation (item)
  (cond
   ((integerp item)
    (format " (HEX:%s) (BIN:%s)" (rpn-calc--int-to-hex item) (rpn-calc--int-to-bin item)))
   ((floatp item)
    (format " (IEEE754:%s)" (rpn-calc--float-to-ieee754 item)))))

;; + commands

(defun rpn-calc-self-insert (ch)
  (interactive (list last-input-event))
  (with-current-buffer rpn-calc--temp-buf
    (goto-char (point-max))
    (insert ch)))

(defun rpn-calc-backspace ()
  (interactive)
  (with-current-buffer rpn-calc--temp-buf
    (if (zerop (buffer-size))
        (pop rpn-calc--stack)
      (backward-delete-char 1))))
