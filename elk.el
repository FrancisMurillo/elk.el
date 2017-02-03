;;; elk.el --- Emacs Lisp source code parser for code analysis  -*- lexical-binding: t; -*-
;;
;; Filename: elk.el
;; Description: A simple parser for Emacs Lisp
;; Author: Francis Murillo
;; Maintainer: Francis Murillo
;; Created: Tue Aug 23 18:13:37 2016 (+0800)
;; Version: 0.10
;; Package-Requires: ((emacs "24.4"))
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL: https://github.com/FrancisMurillo/elk.el
;; Doc URL:
;; Keywords: tools
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'cl-lib)


;;* Shivs
(unless (fboundp 'string-match-p)
  (defsubst string-match-p (regexp string &optional start)
    "\
Same as `string-match' except this function does not change the match data.
Taken from `subr-x'"
    (let ((inhibit-changing-match-data t))
      (string-match regexp string start))))


(defun elk--mapcar-indexed (f xs)
  "Like `mapcar' but indexed.
Like `-map-indexed' from `dash'"
  (car
   (cl-reduce
    (lambda (x acc)
      (pcase-let ((`(,ys . ,index) acc))
        (cons (cons (funcall f index x) ys) (1- index))))
    xs
    :from-end t
    :initial-value (cons (list) (length xs)))))

(defun elk--flatten (xs)
  "Flatten a list XS.
Like `-flatten' from `dash'"
  (apply #'append
     (mapcar
      (lambda (x)
        (if (listp x) (elk--flatten x) (list x)))
      xs)))

(defun elk--compose (&rest fs)
  "Function composition with FS.
Like `-compose' from `dash'."
  (lambda (&rest args)
    (car (cl-reduce
          (lambda (f xs) (list (apply f xs)))
          fs
          :from-end t
          :initial-value args))))



;;* Package
(defgroup elk nil
  "Parse Emacs Lisp source code"
  :prefix "elk-"
  :group 'tools
  :link (list 'url-link
           :tag "Github" "https://github.com/FrancisMurillo/elk.el"))


;;* Private
(defvar elk--stream-consumers
  (list
   ;; Fillers
   #'elk--consume-whitespace
   #'elk--consume-comment

   ;; Types
   #'elk--consume-text ; Or string if you'd like

   ;; Symbols
   #'elk--consume-quote ; Quoting must come before atoms and expressions
   #'elk--consume-atom

   ;; Matching Braces
   #'elk--consume-expression
   ;; TODO: Does not handle [] or {}
   )
  "Elisp parsing handlers, order is important.")


;;* Stream
(defun elk--text-stream (text)
  "Create a TEXT stream used to tokenize Emacs code."
  (lexical-let ((current-text text)
      (text-length (length text))
      (current-value 'base)
      (index 0)
      (column 0)
      (line 1))
    (lambda (&optional command increment)
      (let* ((incrementer (if (null increment)
                           0
                         (1- increment)))
          (incremented-index (+ index incrementer)))
        (pcase command
          ('peek
           (if (< incremented-index text-length)
               (cons (substring-no-properties current-text
                                           incremented-index
                                           (1+ incremented-index))
                  incremented-index)
             'stop))
          ('current current-value)
          ('line line)
          ('column column)
          (_
           (setf current-value
              (if (< incremented-index text-length)
                  (lexical-let ((this-text
                       (substring-no-properties current-text
                                                incremented-index
                                                (1+ incremented-index))))
                    (prog1
                        (cons this-text incremented-index)
                      (setf index (1+ incremented-index))
                      (pcase this-text
                        ((pred (string-equal "\n"))
                         (setq line (1+ line)
                            column 0))
                        (_
                         (setq column (1+ column))))))
                'stop))
           current-value))))))

(defun elk--started-stream (text)
  "Start a stream so that it has a current TEXT already.  This is for testing."
  (let ((stream (elk--text-stream text)))
    (funcall stream) ;; Consume first to start generator L
    stream))

(defun elk--use-stream (stream command &optional default)
  "Execute STREAM COMMAND safely.  This also includes a DEFAULT  if needed."
  (let ((base-value (funcall stream command)))
    (if (or (eq base-value 'base)
           (eq base-value 'stop))
        (or default
           (cons "" -1))
      base-value)))

(defun elk--stream-next-p (stream)
  "Check if a STREAM is has more values."
  (not (eq (funcall stream 'peek) 'stop)))

(defun elk--stream-stop-p (stream)
  "Check if a STREAM is stopped."
  (eq (funcall stream 'current) 'stop))


;;* Token
(defun elk--create-token (type tokens start-pos end-pos line column)
  "Create a token with a specified token TYPE, TOKENS and START-POS and END-POS range."
  (list
   :type type
   :tokens tokens
   :start-pos start-pos
   :end-pos end-pos
   :line line
   :column column))


;;* Letter Predicates
;;; Letters are just text/strings of length 1
(defun elk--whitespace-p (letter)
  "Is LETTER a whitespace?"
  (or (string-equal letter "")
     (string-equal letter " ")
     (string-equal letter "\n")
     (string-equal letter "\t")))

(defun elk--comment-p (letter)
  "Is LETTER is comment?"
  (string-equal letter ";"))

(defun elk--newline-p (letter)
  "Is LETTER a newline?"
  (string-equal letter "\n"))

(defun elk--quote-p (letter)
  "Is LETTER a quoter?"
  (string-equal letter "'"))

(defun elk--function-quote-p (letter)
  "Is LETTER a function quoter?"
  (string-equal letter "#"))

(defun elk--back-quote-p (letter)
  "Is LETTER a back quoter?"
  (string-equal letter "\`"))

(defun elk--text-quote-p (letter)
  "Is LETTER a text quote?"
  (string-equal letter "\""))

(defun elk--text-escape-p (letter)
  "Is LETTER a text escape?"
  (string-equal letter "\\"))

(defun elk--letter-escape-p (letter)
  "Is LETTER a text escape?"
  (string-equal letter "?"))

(defun elk--expression-start-p (letter)
  "Is LETTER an expression starter?"
  (string-equal letter "("))

(defun elk--expression-close-p (letter)
  "Is LETTER an expression closer?"
  (string-equal letter ")"))

(defun elk--atom-letter-p (letter)
  "Is LETTER an valid atom letter?"
  (string-match-p "[A-z0-9-/$:&<>=+,!%*\\.|\\@?]" letter))


;;* Consumer
(defun elk--consume-whitespace (stream)
  "Consume STREAM for a whitespace.  This takes {\n}, {\t}, { } and {^M}."
  (let ((this-char (elk--use-stream stream 'current)))
    (when (elk--whitespace-p (car this-char))
      (let ((start-pos (cdr this-char))
          (line (funcall stream 'line))
          (column (funcall stream 'column))
          (current-char (elk--use-stream stream nil)))
        (while (elk--whitespace-p (car (elk--use-stream stream 'current)))
          (setf current-char (elk--use-stream stream nil)))
        (elk--create-token 'whitespace (list) start-pos (cdr current-char) line column)))))

(defun elk--consume-comment (stream)
  "Consume STREAM a comment.  This takes {;}comment{\n}."
  (let ((this-char (elk--use-stream stream 'current)))
    (when (elk--comment-p (car this-char))
      (let ((start-pos (cdr this-char))
          (line (funcall stream 'line))
          (column (funcall stream 'column))
          (current-char (elk--use-stream stream nil)))
        (while (and (elk--stream-next-p stream)
                  (not (elk--newline-p (car (elk--use-stream stream 'current)))))
          (setf current-char (elk--use-stream stream nil)))
        (setf current-char (elk--use-stream stream nil)) ; Consume the newline as well
        (elk--create-token 'comment (list) start-pos (cdr current-char) line column)))))

(defun elk--consume-text (stream)
  "Consume STREAM for a text declaration.  This takes {\"}text{\"}."
  (let ((this-char (elk--use-stream stream 'current)))
    (when (elk--text-quote-p (car this-char))
      (let ((start-pos (cdr this-char))
          (line (funcall stream 'line))
          (column (funcall stream 'column))
          (current-char (elk--use-stream stream nil)))
        (while (and (elk--stream-next-p stream)
                  (not (elk--text-quote-p (car (elk--use-stream stream 'current)))))
          (when (elk--text-escape-p (car (elk--use-stream stream 'current)))
            (setf current-char (elk--use-stream stream nil)))
          (setf current-char (elk--use-stream stream nil)))
        (setf current-char (elk--use-stream stream nil))
        (elk--create-token 'text (list) start-pos (cdr current-char) line column)))))

(defun elk--consume-quote (stream)
  "Consume STREAM for an atom name.  This takes {`}, {'} and {#'}."
  (let ((this-char (elk--use-stream stream 'current))
      (quote-text ""))
    (when (or (elk--quote-p (car this-char))
             (elk--back-quote-p (car this-char))
             (and (elk--function-quote-p (car this-char))
                (elk--quote-p (car (elk--use-stream stream 'peek)))))
      (let ((start-pos (cdr this-char))
          (start-letter (car this-char))
          (line (funcall stream 'line))
          (column (funcall stream 'column))
          (next-char (elk--use-stream stream nil)))
        (setf quote-text start-letter)
        (when (elk--quote-p (car next-char))
          (setf quote-text (concat start-letter (car next-char)))
          (setf next-char (elk--use-stream stream nil)))
        (let* ((sub-tokens (list (elk--dispatch-stream-consumers stream)))
            (base-token (elk--create-token 'quote
                                           sub-tokens
                                           start-pos
                                           (cdr (elk--use-stream stream 'current))
                                           line
                                           column)))
          (plist-put base-token :quote-text quote-text))))))

(defun elk--consume-atom (stream)
  "Consume STREAM for an atom name."
  (let ((this-char (elk--use-stream stream 'current)))
    (cond
     ((elk--letter-escape-p (car this-char))
      (let ((start-pos (cdr this-char))
          (line (funcall stream 'line))
          (column (funcall stream 'column))
          (current-char (elk--use-stream stream nil)))
        (when (elk--text-escape-p (car (elk--use-stream stream 'current)))
          (setf current-char (elk--use-stream stream nil)))
        (setf current-char (elk--use-stream stream nil))
        (elk--create-token 'atom (list) start-pos (cdr current-char) line column)))
     ((elk--atom-letter-p (car this-char))
      (let ((start-pos (cdr this-char))
          (line (funcall stream 'line))
          (column (funcall stream 'column))
          (current-char (elk--use-stream stream nil)))
        (while (elk--atom-letter-p (car (elk--use-stream stream 'current)))
          (when (elk--letter-escape-p (car (elk--use-stream stream 'current)))
            (setf current-char (elk--use-stream stream nil)))
          (setf current-char (elk--use-stream stream nil)))
        (elk--create-token 'atom (list) start-pos (cdr current-char) line column)))
     (t nil))))

(defun elk--consume-expression (stream)
  "Consume STREAM for expressions."
  (let ((this-char (elk--use-stream stream 'current)))
    (when (elk--expression-start-p (car this-char))
      (let ((start-pos (cdr this-char))
          (current-char (elk--use-stream stream nil))
          (line (funcall stream 'line))
          (column (funcall stream 'column))
          (expression-tokens (list)))
        (while (and (elk--stream-next-p stream)
                  (not (elk--expression-close-p (car current-char))))
          (push (elk--dispatch-stream-consumers stream) expression-tokens)
          (setf current-char (elk--use-stream stream 'current)))
        (setf current-char (elk--use-stream stream nil))
        (elk--create-token 'expression (reverse expression-tokens) start-pos (cdr current-char) line column)))))


(defun elk--dispatch-stream-consumers (stream)
  "Execute elisp parsing functions over the STREAM consumers."
  (lexical-let ((value nil))
    (mapc (lambda (handler)
            (unless value
              (setf value (funcall handler stream))))
          elk--stream-consumers)
    value))


;;* Pre-parsers
(defun elk--attach-source (text tokens)
  "Label atoms based on their source TEXT and parsed TOKENS."
  (letrec ((recurser
       (lambda (text tokens)
         (mapcar (lambda (token)
                   (let ((type (plist-get token :type)))
                     (pcase type
                       ((or `atom `text `comment `whitespace)
                        (let ((start-pos (plist-get token :start-pos))
                            (end-pos  (plist-get token :end-pos))
                            (new-token (copy-sequence token)))
                          (when (= end-pos -1)
                            (setf end-pos (length text))
                            (plist-put new-token :end-pos end-pos))
                          (plist-put new-token :text
                                     (substring-no-properties
                                      text
                                      start-pos
                                      end-pos))))
                       ((or `expression `quote)
                        (let* ((sub-tokens (plist-get token :tokens)))
                          (plist-put (copy-sequence token) :tokens
                                     (funcall recurser text sub-tokens)))))))
                 tokens))))
    (funcall recurser text tokens )))

(defun elk--attach-level (tokens)
  "Attach a level value for the TOKENS."
  (letrec ((recurser
       (lambda (level tokens)
         "Recurser of elk--attach-level."
         (mapcar (lambda (token)
                   (let ((type (plist-get token :type))
                       (leveled-token (plist-put (copy-sequence token) :level level)))
                     (pcase type
                       ((or `expression `quote)
                        (let ((sub-tokens (plist-get leveled-token :tokens)))
                          (plist-put (copy-sequence leveled-token)
                                     :tokens (funcall recurser (1+ level) sub-tokens))))
                       (_ leveled-token))))
                 tokens))))
    (funcall recurser 1 tokens)))

(defun elk--attach-token-id (tokens)
  "Attach an id for each token, useful when the TOKENS are flattned."
  (letrec ((incremental-sequence
       (lambda (&optional start)
         (lexical-let ((seed (or start 0)))
           (lambda ()
             (prog1
                 seed
               (setf seed (1+ seed)))))))
      (recurser
       (lambda (parent-id generator tokens)
         (mapcar (lambda (token)
                   (let ((type (plist-get token :type))
                       (marked-token (plist-put (copy-sequence token) :id (funcall generator))))
                     (setf marked-token (plist-put marked-token :parent-id parent-id))
                     (pcase type
                       ((or `expression `quote)
                        (let ((sub-tokens (plist-get marked-token :tokens)))
                          (plist-put (copy-sequence marked-token)
                                     :tokens (funcall recurser
                                                (plist-get marked-token :id)
                                                generator sub-tokens))))
                       (_ marked-token))))
                 tokens))))
    (funcall recurser 0 (funcall incremental-sequence 1) tokens)))

(defun elk--attach-expression-index (tokens)
  "Attach indices to TOKENS to determine what position it is in."
  (letrec ((recurser
       (lambda (tokens)
         (elk--mapcar-indexed
          (lambda (index token)
            (let ((type (plist-get token :type))
                (indexed-token (plist-put (copy-sequence token) :index index)))
              (pcase type
                ((or `expression `quote)
                 (let ((sub-tokens (plist-get indexed-token :tokens)))
                   (plist-put (copy-sequence indexed-token)
                              :tokens (funcall recurser sub-tokens))))
                (_ indexed-token))))
          tokens))))
    (funcall recurser tokens)))

(defun elk--attach-atom-type (tokens)
  "Attach atom data type to TOKENS if it has one."
  (letrec ((typer
       (lambda (text)
         (cond
          ((string-equal text "0") 'number)
          ((not (zerop (string-to-number text))) 'number)
          (t 'symbol))))
      (recurser
       (lambda (tokens)
         (mapcar (lambda (token)
                   (let ((type (plist-get token :type))
                       (new-token (copy-sequence token)))
                     (when (eq type 'atom)
                       (plist-put new-token :data-type
                                  (funcall typer (plist-get token :text))))
                     (let ((sub-tokens (plist-get token :tokens)))
                       (plist-put new-token
                                  :tokens (funcall recurser sub-tokens)))
                     new-token))
                 tokens))))
    (funcall recurser tokens)))


;;* Api
(defun elk--parsing (text)
  "Parse source TEXT into a stream/generator yielding tokens.  Lazy loading in other ways."
  (lexical-let ((stream (elk--text-stream text)))
    (elk--use-stream stream nil) ; Start stream
    (lambda ()
      (if (not (elk--stream-stop-p stream))
          (elk--dispatch-stream-consumers stream)
        'stop))))


(defun elk--parse (text)
  "Tokenize an elisp source TEXT."
  (lexical-let ((tokens (list))
      (parsing (elk--parsing text))
      (token nil))
    (while (not (eq token 'stop))
      (setf token (funcall parsing))
      (when (not (eq token 'stop))
        (push token tokens)))
    (funcall (elk--compose
        #'elk--attach-atom-type ; needs source text first
        (apply-partially #'elk--attach-source text)
        #'elk--attach-token-id
        #'elk--attach-level
        #'elk--attach-expression-index
        #'reverse)
       tokens)))

(defun elk--codify (tokens)
  "Convert TOKENS into source code."
  (letrec ((texify
       (lambda (tokens)
         (lexical-let ((recurser texify))
           (mapcar (lambda (token)
                     (let* ((type (plist-get token :type))
                         (text-tokens
                          (pcase type
                            (`quote
                             (let ((sub-tokens (plist-get token :tokens))
                                 (quote-text (plist-get token :quote-text)))
                               (list quote-text (funcall recurser sub-tokens))))
                            (`expression
                             (let ((sub-tokens (plist-get token :tokens)))
                               (list "(" (funcall recurser sub-tokens) ")")))
                            (_ (plist-get token :text)))))
                       text-tokens))
                   tokens)))))
    (funcall (elk--compose
        (apply-partially #'s-join "")
        #'-flatten
        texify)
       tokens)))

(defvar elk-current-tokens (list)
  "Current tokens parsed.  For ease of use.")


;;* Interface
(defun elk-parse (&optional text)
  "Tokenize the TEXT and save in elk-current-tokens.  If there is no argument specified it takes the region or the buffer."
  (interactive)
  (letrec ((source-text (cond
                    ((not (null text)) text)
                    ((region-active-p)
                     (buffer-substring-no-properties (region-beginning) (region-end)))
                    (t  (buffer-substring-no-properties (point-min) (point-max))))))
    (setq elk-current-tokens (elk--parse source-text))
    elk-current-tokens))

(defun elk-parse-buffer (&optional buffer)
  "Using `elk-parse' with BUFFER."
  (interactive)
  (elk-parse
   (with-current-buffer (or buffer (current-buffer))
     (buffer-substring-no-properties (point-min) (point-max)))))


(provide 'elk)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; elk.el ends here
