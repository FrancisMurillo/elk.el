;;; elk.el --- Emacs Lisp source code parser for code analysis  -*- lexical-binding: t; -*-
;;
;; Filename: elk.el
;; Description: A simple parser for Emacs Lisp
;; Author: Francis Murillo
;; Maintainer: Francis Murillo
;; Created: Tue Aug 23 18:13:37 2016 (+0800)
;; Version: 0.10
;; Package-Requires: (dash s)
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

(require 'dash)
(require 's)

;;* Package
(defgroup elk nil
  "Parse Emacs Lisp source code"
  :prefix "elk"
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
                (index 0))
    (lambda (&optional command increment)
      (let* ((incrementer (if (null increment)
                              0
                            (1- increment)))
             (incremented-index (+ index incrementer)))
        (cond
         ((eq command 'peek)
          (if (< incremented-index text-length)
              (cons (substring-no-properties current-text
                                             incremented-index
                                             (1+ incremented-index))
                    incremented-index)
            'stop))
         ((eq command 'current) current-value)
         (t
          (setf current-value
                (if (< incremented-index text-length)
                    (prog1
                        (cons (substring-no-properties current-text
                                                       incremented-index
                                                       (1+ incremented-index))
                              incremented-index)
                      (setf index (1+  incremented-index)))
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
(defun elk--create-token (type tokens start-pos end-pos)
  "Create a token with a specified token TYPE, TOKENS and START-POS and END-POS range."
  (list
   :type type
   :tokens tokens
   :start-pos start-pos
   :end-pos end-pos))


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
  (s-matches-p "[A-z0-9-/$:&<>=+,!%*\\.|\\@?]" letter))


;;* Consumer
(defun elk--consume-whitespace (stream)
  "Consume STREAM for a whitespace.  This takes {\n}, {\t}, { } and {^M}."
  (let ((this-char (elk--use-stream stream 'current)))
    (when (elk--whitespace-p (car this-char))
      (let ((start-pos (cdr this-char))
            (current-char (elk--use-stream stream nil)))
        (while (elk--whitespace-p (car (elk--use-stream stream 'current)))
          (setf current-char (elk--use-stream stream nil)))
        (elk--create-token 'whitespace (list) start-pos (cdr current-char))))))

(defun elk--consume-comment (stream)
  "Consume STREAM a comment.  This takes {;}comment{\n}."
  (let ((this-char (elk--use-stream stream 'current)))
    (when (elk--comment-p (car this-char))
      (let ((start-pos (cdr this-char))
            (current-char (elk--use-stream stream nil)))
        (while (and (elk--stream-next-p stream)
                    (not (elk--newline-p (car (elk--use-stream stream 'current)))))
          (setf current-char (elk--use-stream stream nil)))
        (setf current-char (elk--use-stream stream nil)) ; Consume the newline as well
        (elk--create-token 'comment (list) start-pos (cdr current-char))))))

(defun elk--consume-text (stream)
  "Consume STREAM for a text declaration.  This takes {\"}text{\"}."
  (let ((this-char (elk--use-stream stream 'current)))
    (when (elk--text-quote-p (car this-char))
      (let ((start-pos (cdr this-char))
            (current-char (elk--use-stream stream nil)))
        (while (and (elk--stream-next-p stream)
                    (not (elk--text-quote-p (car (elk--use-stream stream 'current)))))
          (when (elk--text-escape-p (car (elk--use-stream stream 'current)))
            (setf current-char (elk--use-stream stream nil)))
          (setf current-char (elk--use-stream stream nil)))
        (setf current-char (elk--use-stream stream nil))
        (elk--create-token 'text (list) start-pos (cdr current-char))))))

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
            (next-char (elk--use-stream stream nil)))
        (setf quote-text start-letter)
        (when (elk--quote-p (car next-char))
          (setf quote-text start-letter)
          (setf next-char (elk--use-stream stream nil)))
        (let* ((sub-tokens (list (elk--dispatch-stream-consumers stream)))
               (base-token (elk--create-token 'quote
                                              sub-tokens
                                              start-pos
                                              (cdr (elk--use-stream stream 'current)))))
          (plist-put base-token :quote-text quote-text))))))

(defun elk--consume-atom (stream)
  "Consume STREAM for an atom name."
  (let ((this-char (elk--use-stream stream 'current)))
    (cond
     ((elk--letter-escape-p (car this-char))
      (let ((start-pos (cdr this-char))
            (current-char (elk--use-stream stream nil)))
        (when (elk--text-escape-p (car (elk--use-stream stream 'current)))
          (setf current-char (elk--use-stream stream nil)))
        (setf current-char (elk--use-stream stream nil))
        (elk--create-token 'atom (list) start-pos (cdr current-char))))
     ((elk--atom-letter-p (car this-char))
      (let ((start-pos (cdr this-char))
            (current-char (elk--use-stream stream nil)))
        (while (elk--atom-letter-p (car (elk--use-stream stream 'current)))
          (when (elk--letter-escape-p (car (elk--use-stream stream 'current)))
            (setf current-char (elk--use-stream stream nil)))
          (setf current-char (elk--use-stream stream nil)))
        (elk--create-token 'atom (list) start-pos (cdr current-char))))
     (t nil))))

(defun elk--consume-expression (stream)
  "Consume STREAM for expressions"
  (let ((this-char (elk--use-stream stream 'current)))
    (when (elk--expression-start-p (car this-char))
      (let ((start-pos (cdr this-char))
            (current-char (elk--use-stream stream nil))
            (expression-tokens (list)))
        (while (and (elk--stream-next-p stream)
                    (not (elk--expression-close-p (car current-char))))
          (push (elk--dispatch-stream-consumers stream) expression-tokens)
          (setf current-char (elk--use-stream stream 'current)))
        (setf current-char (elk--use-stream stream nil))
        (elk--create-token 'expression (seq-reverse expression-tokens) start-pos (cdr current-char))))))


(defun elk--dispatch-stream-consumers (stream)
  "Execute elisp parsing functions over the STREAM consumers."
  (lexical-let ((value nil))
    (mapc (lambda (handler)
            (unless value
              (setf value (funcall handler stream))))
          elk--stream-consumers)
    value))


(defun elk--discard-filler (tokens)
  "Disregard comments and whitespace with the tokens"
  (let* ((filterer (lambda (token)
                     (let ((type (plist-get token :type)))
                       (pcase type
                         ((or `whitespace `comment) nil)
                         (_ t)))))
         (recurser (lambda (token)
                     (let ((type (plist-get token :type)))
                       (pcase type
                         ((or `expression `quote)
                          (let* ((sub-tokens (plist-get token :tokens)))
                            (plist-put (-copy token) :tokens (elk--discard-filler sub-tokens))))
                         (_ token)))))
         (pipeline (-compose
                    (-partial #'-map recurser)
                    (-partial #'-filter filterer))))
    (funcall pipeline tokens)))


(defun elk--attach-source (text tokens)
  "Label atoms based on their source text"
  (lexical-let* ((source-text text)
                 (recurser (lambda (token)
                             (let ((type (plist-get token :type)))
                               (pcase type
                                 ((or `atom `text `comment `whitespace)
                                  (let ((start-pos (plist-get token :start-pos))
                                        (end-pos  (plist-get token :end-pos))
                                        (new-token (-copy token)))
                                    (when (= end-pos -1)
                                      (setf end-pos (length source-text))
                                      (plist-put new-token :end-pos end-pos))
                                    (plist-put new-token :text
                                              (substring-no-properties source-text
                                                                       start-pos
                                                                       end-pos))))
                                 ((or `expression `quote)
                                  (let* ((sub-tokens (plist-get token :tokens)))
                                    (plist-put(-copy token) :tokens (elk--attach-source source-text sub-tokens))))
                                 (_ token))))))
    (-map recurser tokens)))

(defun elk--leveler (level tokens)
  "Recurser of elk--attach-level"
  (-map (lambda (token)
          (let ((type (plist-get token :type))
                (leveled-token (plist-put (-copy token) :level level)))
            (pcase type
              ((or `expression `quote)
               (let ((sub-tokens (plist-get leveled-token :tokens)))
                 (plist-put (-copy leveled-token)
                            :tokens (elk--leveler (1+ level) sub-tokens))))
              (_ leveled-token))))
        tokens))

(defun elk--attach-level (tokens)
  "Attach a level value for the"
  (elk--leveler 0 tokens))

(defun elk--incremental-sequence (&optional start)
  "An quick implementation of an increasing sequence"
  (lexical-let ((seed (or start 0)))
    (lambda ()
      (prog1
          seed
        (setf seed (1+ seed))))))

(defun elk--marker (parent-id generator tokens)
  "Recurser of elk--attach-token-id"
  (-map (lambda (token)
          (let ((type (plist-get token :type))
                (marked-token (plist-put (-copy token) :id (funcall generator))))
            (setf marked-token (plist-put marked-token :parent-id parent-id))
            (pcase type
              ((or `expression `quote)
               (let ((sub-tokens (plist-get marked-token :tokens)))
                 (plist-put (-copy marked-token)
                            :tokens (elk--marker
                                     (plist-get marked-token :id)
                                     generator sub-tokens))))
              (_ marked-token))))
        tokens))

(defun elk--attach-token-id (tokens)
  "Attach an id for each token, useful when the tokens are flattned"
  (elk--marker 0 (elk--incremental-sequence 1) tokens))

(defun elk--indexer (tokens)
  "Recurser of elk--attach-expression-index"
  (-map-indexed (lambda (index token)
                  (let ((type (plist-get token :type))
                        (indexed-token (plist-put (-copy token) :index index)))
                    (pcase type
                      ((or `expression `quote)
                       (let ((sub-tokens (plist-get indexed-token :tokens)))
                         (plist-put (-copy indexed-token)
                                    :tokens (elk--indexer sub-tokens))))
                      (_ indexed-token))))
                tokens))

(defun elk--attach-expression-index (tokens)
  "Attach indices to expression to determine what position it is in"
  (elk--indexer tokens))


(defun elk--flatten-tokens (tokens)
  "Flatten nested tokens as one token list"
  (funcall (-compose
            (-partial #'apply #'append)
            (-partial #'-map (lambda (token)
                               (let ((type (plist-get token :type)))
                                 (pcase type
                                   ((or `expression `quote)
                                    (let ((sub-tokens (plist-get token :tokens)))
                                      (elk--flatten-tokens sub-tokens)))
                                   (_ (list token)))))))
           tokens))

(defun elk--select-type (type tokens)
  "Filter tokens by a specified type"
  (funcall (-compose
            (-partial #'-filter
                      (lambda (token)
                        (eq (plist-get token :type) type)))
            #'elk--flatten-tokens)
           tokens))

(defun elk--extract-atoms (tokens)
  "Get atoms in tokens"
  (funcall (-compose
            (-partial #'-map (-rpartial #'plist-get :text))
            (-partial #'elk--select-type 'atom))
           tokens))

(defun elk--extract-text (tokens)
  "Get text in tokens"
  (funcall (-compose
            (-partial #'-map (-rpartial #'plist-get :text))
            (-partial #'elk--select-type 'text))
           tokens))

(defun elk--default-atom-filter (atom)
  "Filter an atom if it is not built-in or redundant"
  (funcall (-andfn (-not
                    (-compose
                     #'special-form-p
                     #'intern-soft))
                   (-not (-compose
                          #'subrp
                          #'symbol-function
                          #'intern-soft)))
           atom))


(defun elk--summarize-atoms (tokens)
  "Report what atoms are used more likely"
  (funcall (-compose
            (-partial #'-sort (-on #'> #'cdr))
            (-partial #'-map (lambda (repeating-tokens)
                               (cons (-first-item repeating-tokens)
                                     (1- (length repeating-tokens)))))
            (-partial #'-group-by #'identity)
            (-partial #'-filter #'elk--default-atom-filter)
            #'elk--extract-atoms)
           tokens))


(defun elk--tokenize (text)
  "Tokenize an elisp source TEXT."
  (lexical-let ((tokens (list))
                (stream (elk--text-stream text)))
    (elk--use-stream stream nil)
    (while (elk--stream-next-p stream)
      (push (elk--dispatch-stream-consumers stream) tokens))
    (funcall (-compose
              (-partial #'elk--attach-source text)
              #'elk--attach-token-id
              #'elk--attach-level
              #'elk--attach-expression-index
              #'seq-reverse)
             tokens)))

(defun elk--codify (tokens)
  "Convert TOKENS into source code"
  (letrec ((texify (lambda (tokens)
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
    (funcall (-compose
              (-rpartial #'string-join "")
              #'-flatten
              texify
              )
             tokens)))

(defvar elk-current-tokens (list)
  "Current tokens parsed")


(defun elk-tokenize (&optional text)
  "Tokenize the text and save in current tokens"
  (interactive)
  (let ((source-text (cond
                      ((not (null text)) text)
                      ((region-active-p) (buffer-substring-no-properties (region-beginning) (region-end)))
                      (t  (buffer-substring-no-properties (point-min) (point-max))))))
    (setq elk-current-tokens (elk--tokenize source-text))
    elk-current-tokens))


(defun elk--nearest-top-expression-at-point ()
  "Get token expression that is nearest to the highest point"
  (interactive)
  (let* ((source-text (buffer-substring-no-properties (point-min) (point-max)))
         (tokens (elk--tokenize source-text))
         (expression-token (-first (lambda (token)
                                     (and (= (plist-get token :level) 0)
                                          (eq (plist-get token :type) 'expression)
                                          (<= (plist-get token :start-pos) (point))
                                          (>= (plist-get token :end-pos) (point))))
                                   tokens)))
    (if expression-token
        (goto-char (1+ (plist-get expression-token :start-pos)))
      (message "No near top level expression at point"))))


(provide 'elk)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; elk.el ends here
