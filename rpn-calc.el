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
;; Package-Requires: ((popup "0.4"))

;;; Commentary:

;; Try: "M-x rpn-calc[RET] 1[SPC]2+3/sin'number-to-string[SPC]"

;;; Change Log:

;;; Code:

(require 'popup)
(require 'help-fns)                     ; help-function-arglist

(defgroup rpn-calc nil
  "quick RPN calculator for hackers"
  :group 'emacs)

;; + custom

(defcustom rpn-calc-operator-table
  '(("+"   2 . +)
    ("--"  2 . -)                      ; "10 -10" should be "10 (- 10)"
    ("/"   2 . /)
    ("*"   2 . *)
    ("%"   2 . mod)
    ("&" 2 . logand)
    ("|" 2 . logor)
    ("^" 2 . logxor)
    ("~" 1 . lognot)
    ("<<"  2 . ash)
    (">>"  2 . (lambda (value count) (ash value (- count))))
    ("sin" 1 . sin)
    ("cos" 1 . cos)
    ("tan" 1 . tan)
    ("log" 2 . (lambda (base value) (log value base)))
    ("lg" 1 . log10)
    ("ln" 1 . log))
  "list of (NAME ARITY . FUNCTION)."
  :group 'rpn-calc)

(defcustom rpn-calc-incompatible-minor-modes '()
  "list of minor-modes that should be disabled while RPN calc is
active."
  :group 'rpn-calc)

(defcustom rpn-calc-apply-optional-args nil
  "when non-nil, optional arguments are applied to pushed
  functions."
  :group 'rpn-calc)

(defcustom rpn-calc-apply-rest-args t
  "when non-nil, rest arguments are applied to pushed
  functions."
  :group 'rpn-calc)

;; + utils

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

(defun rpn-calc--function-args (fn)
  "return (ARGS OPTIONAL-ARGS . REST-ARGS) of FN."
  (let ((lst (help-function-arglist fn t))
        args optional-args)
    (while (and lst (not (memq (car lst) '(&optional &rest))))
      (push (pop lst) args))
    (when (eq (car lst) '&optional)
      (pop lst)
      (while (and lst (not (eq (car lst) '&rest)))
        (push (pop lst) optional-args)))
    (cons (nreverse args) (cons (nreverse optional-args) (cadr lst)))))

;; + core

;; *TODO* operator table should be a (patricia)-trie
;; *TODO* display ASCII char ?
;; *TODO* `popup-next' to insert to middle of the stack

(defvar rpn-calc--saved-minor-modes nil)
(defvar rpn-calc--temp-buffer nil)
(defvar rpn-calc--buffer nil)
(defvar rpn-calc--stack nil)
(defvar rpn-calc--popup nil)

(defconst rpn-calc-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap [remap self-insert-command]  'rpn-calc-self-insert)
    (define-key kmap [remap delete-backward-char] 'rpn-calc-backspace)
    (define-key kmap [remap backward-delete-char] 'rpn-calc-backspace)
    (define-key kmap [remap backward-kill-word]   'rpn-calc-backward-kill-word)
    (define-key kmap (kbd "C-n")                  'rpn-calc-next)
    (define-key kmap (kbd "C-p")                  'rpn-calc-previous)
    (define-key kmap (kbd "<left>")               'rpn-calc-next)
    (define-key kmap (kbd "<right>")              'rpn-calc-previous)
    (define-key kmap (kbd "DEL")                  'rpn-calc-backspace)
    (define-key kmap (kbd "RET")                  'rpn-calc-select)
    kmap))

;;;###autoload
(define-minor-mode rpn-calc
  "quick RPN calculator for hackers"
  :init-value nil
  :keymap rpn-calc-map
  (cond (rpn-calc
         (setq rpn-calc--stack    nil
               rpn-calc--buffer (current-buffer)
               rpn-calc--temp-buffer (get-buffer-create " *rpn-calc*")
               rpn-calc--saved-minor-modes
               (mapcar (lambda (mode) (when (and (boundp mode) (symbol-value mode))
                                        (prog1 mode (funcall mode -1))))
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
         (mapc 'funcall rpn-calc--saved-minor-modes)
         (kill-buffer rpn-calc--temp-buffer))))

(defun rpn-calc--pre-command-hook ()
  (unless (and (symbolp this-command)
               (string-prefix-p "rpn-" (symbol-name this-command)))
    (rpn-calc -1)))

(defun rpn-calc--post-command-hook ()
  (condition-case err
      (rpn-calc--maybe-commit-current-input)
    (error (message (error-message-string err))))
  (rpn-calc--refresh-popup))

(defun rpn-calc--take (n)
  "take first N elements from the stack."
  (when (> n 0)
    (let* ((last-cell (nthcdr (1- n) rpn-calc--stack)))
      (unless last-cell (error "stack underflow"))
      (prog1 rpn-calc--stack
        (setq rpn-calc--stack (cdr last-cell))
        (setcdr last-cell nil)))))

(defun rpn-calc--push (obj)
  (with-current-buffer rpn-calc--buffer
    (cond ((and (consp obj) (eq (car obj) 'function)) ; quoted function
           (push (eval obj) rpn-calc--stack))
          ((and (consp obj) (integerp (car obj)) (functionp (cdr obj))) ; RPN operator
           (push (apply (cdr obj) (nreverse (rpn-calc--take (car obj)))) rpn-calc--stack))
          (t                           ; other
           (setq obj (eval obj))
           (if (not (functionp obj))
               (push obj rpn-calc--stack)
             (let* ((arglst (rpn-calc--function-args obj))
                    (args (nreverse (rpn-calc--take (length (car arglst)))))
                    (optional-args (if rpn-calc-apply-optional-args
                                       (nreverse (rpn-calc--take (length (cadr arglst))))
                                     (make-list (length (cadr arglst)) nil)))
                    (rest-args (when (and rpn-calc-apply-rest-args (cddr arglst))
                                 (prog1 (nreverse rpn-calc--stack)
                                   (setq rpn-calc--stack nil)))))
               (push (apply obj (nconc args optional-args rest-args)) rpn-calc--stack)))))))

(defun rpn-calc--maybe-commit-current-input ()
  (with-current-buffer rpn-calc--temp-buffer
    (catch 'read-error
      (let* ((obj  (condition-case nil
                       (read (buffer-string))
                     (error (throw 'read-error nil))))
             (name (when (symbolp obj) (symbol-name obj))))
        (cond ((eq obj ':)             ; command: duplicate
               (erase-buffer)
               (push (car rpn-calc--stack) rpn-calc--stack))
              ((eq obj '\\)            ; command: swap
               (erase-buffer)
               (if (cdr rpn-calc--stack)
                   (let ((tmp (car rpn-calc--stack)))
                     (setcar rpn-calc--stack (cadr rpn-calc--stack))
                     (setcar (cdr rpn-calc--stack) tmp))
                 (error "stack underflow")))
              ((looking-back "\\(^\\|[^\\]\\)[])}\"\s\t\n]") ; complete input
               (erase-buffer)
               (rpn-calc--push (or (when (symbolp obj)
                                     (assoc (symbol-name obj) rpn-calc-operator-table))
                                   obj)))
              ((null name)             ; obj is not a symbol -> fail
               nil)
              ((string-match           ; a number + incomplete symbol
                "^[+-]?[0-9]*\\(?:[0-9]\\|\\.[0-9]+\\)\\(?:e[+-]?\\(?:[0-9]+\\|INF\\)\\)?"
                name)
               (let ((num (read (match-string 0 name))))
                 (erase-buffer)
                 (insert (substring name (match-end 0)))
                 (rpn-calc--push num)
                 ;; recurse
                 (rpn-calc--maybe-commit-current-input)))
              ((setq obj (let (val)    ; a complete operator
                           (catch 'ret
                             (dolist (entry rpn-calc-operator-table)
                               (when (string-prefix-p name (car entry))
                                 (if (not (string= name (car entry)))
                                     (throw 'ret nil)
                                   (setq val (cdr entry)))))
                             val)))
               (erase-buffer)
               (rpn-calc--push obj)))))))

(defun rpn-calc--refresh-popup ()
  (with-current-buffer rpn-calc--temp-buffer
    (let ((head (let ((str (buffer-string)))
                 (popup-make-item
                  (concat str (rpn-calc--annotation (ignore-errors (read str))))
                  :value str)))
          (stack (mapcar (lambda (item)
                           (let ((str (prin1-to-string item)))
                             (popup-make-item
                              (concat str (rpn-calc--annotation item)) :value str)))
                         rpn-calc--stack)))
      (popup-set-list rpn-calc--popup (cons head stack))
      (popup-draw rpn-calc--popup))))

(defun rpn-calc--annotation (item)
  (cond ((integerp item)
         (format " (HEX:%s, BIN:%s)"
                 (rpn-calc--int-to-hex item)
                 (rpn-calc--int-to-bin item)))
        ((floatp item)
         (format " (IEEE754:%s)" (rpn-calc--float-to-ieee754 item)))
        ((and item (symbolp item))
         (and (boundp item)
              (format " (%s)" (prin1-to-string (symbol-value item)))))
        ((and (consp item) (eq (car item) 'quote) (functionp (cadr item)))
         (let ((args (rpn-calc--function-args (cadr item))))
           (format " (%s%s%s)"
                   (mapconcat 'prin1-to-string (car args) " ")
                   (if (not rpn-calc-apply-optional-args) ""
                     (concat " " (mapconcat 'prin1-to-string (cadr args) " ")))
                   (if (not (and rpn-calc-apply-rest-args (cddr args))) ""
                     (concat " . " (prin1-to-string (cddr args)))))))))

;; + commands

(defun rpn-calc-self-insert (n)
  (interactive "P")
  (with-current-buffer rpn-calc--temp-buffer
    (goto-char (point-max))
    (call-interactively 'self-insert-command)))

(defun rpn-calc-backspace (n)
  (interactive "p")
  (with-current-buffer rpn-calc--temp-buffer
    (if (zerop (buffer-size))
        (dotimes (_ n) (pop rpn-calc--stack))
      (backward-delete-char n))))

(defun rpn-calc-backward-kill-word (n)
  (interactive "p")
  (with-current-buffer rpn-calc--temp-buffer
    (backward-kill-word n)))

(defun rpn-calc-next (n)
  (interactive "p")
  (dotimes (_ n)
    (popup-next rpn-calc--popup)))

(defun rpn-calc-previous (n)
  (interactive "p")
  (dotimes (_ n)
    (popup-previous rpn-calc--popup)))

(defun rpn-calc-select ()
  (interactive)
  (let ((return))
    ;; Return the last valid value from the stack
    (while (null return)
      (setq return (popup-item-value (popup-selected-item rpn-calc--popup)))
      (rpn-calc-previous 1))
    (insert return))
  (rpn-calc -1))

;; + provide

(provide 'rpn-calc)

;;; rpn-calc.el ends here
