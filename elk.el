(require 'dash)
(require 's)
(require 'concurrent)


(defun elk--text-stream (text)
  "Create a text stream used to tokenize lisp"
  (lexical-let ((current-text text)
                (text-length (length text))
                (current-value 'base)
                (index 0))
    (lambda (&optional command)
      (cond
       ((eq command 'peek) (if (< index text-length)
                                (cons (substring-no-properties current-text index (1+ index)) index)
                              'stop))
       ((eq command 'current) current-value)
       (t
        (setf current-value (if (< index text-length)
                                (prog1
                                    (cons (substring-no-properties current-text index (1+ index)) index)
                                  (setf index (1+ index)))
                              'stop))
        current-value)))))

(defun elk--test-stream (text)
  "For testing purposes"
  (let ((stream (elk--text-stream text)))
    (funcall stream) ;; Consume first to start generator L
    stream))


(defun elk--use-stream (stream command &optional default)
  "Execute stream commands safely"
  (let ((base-value (funcall stream command)))
    (if (or (eq base-value 'base)
            (eq base-value 'stop))
        (or default
            (cons "" -1))
      base-value)))

(defun elk--stream-next-p (stream)
  "Check if a stream is stoppped"
  (not (eq (funcall stream 'peek) 'stop)))


(defun elk--create-token (type tokens start-pos end-pos)
  "Create a token with a specified token type, token value and start and end range"
  (list
   :type type
   :tokens tokens
   :start-pos start-pos
   :end-pos end-pos))


(defun elk--whitespace-p (letter)
  "Is letter a whitespace"
  (and (not (string-equal letter ""))
       (string-equal (s-trim letter) "")))

(defun elk--comment-p (letter)
  "Is letter is comment"
  (string-equal letter ";"))

(defun elk--newline-p (letter)
  "Is letter a newline"
  (string-equal letter "\n"))

(defun elk--quote-p (letter)
  "Is letter a quoter"
  (or (string-equal letter "#")
      (string-equal letter "\`")
      (string-equal letter "'")))

(defun elk--text-quote-p (letter)
  "Is letter a text quote"
  (string-equal letter "\""))

(defun elk--text-escape-p (letter)
  "Is letter a text escape"
  (string-equal letter "\\"))

(defun elk--expression-start-p (letter)
  "Is letter an expression starter"
  (string-equal letter "("))

(defun elk--expression-close-p (letter)
  "Is letter an expression closer"
  (string-equal letter ")"))

(defun elk--atom-letter-p (letter)
  "Is letter an valid atom letter"
  (s-matches-p "[A-z0-9-/:&<>=+,!%*?\\.|)(\\]" letter))


(defun elk--consume-whitespace (stream)
  "Consume whitespace"
  (let ((this-char (elk--use-stream stream 'current)))
    (when (elk--whitespace-p (car this-char))
      (let ((start-pos (cdr this-char))
            (current-char (elk--use-stream stream nil)))
        (while (elk--whitespace-p (car (elk--use-stream stream 'peek)))
          (setf current-char (elk--use-stream stream nil)))
        (elk--create-token 'whitespace (list) start-pos (cdr current-char))))))

(defun elk--consume-comment (stream)
  "Consume a comment"
  (let ((this-char (elk--use-stream stream 'current)))
    (when (elk--comment-p (car this-char))
      (let ((start-pos (cdr this-char))
            (current-char (elk--use-stream stream nil)))
        (while (and (elk--stream-next-p stream)
                    (not (elk--newline-p (car (elk--use-stream stream 'peek)))))
          (setf current-char (elk--use-stream stream nil)))
        (setf current-char (elk--use-stream stream nil current-char))
        (elk--create-token 'comment (list) start-pos (cdr current-char))))))

(defun elk--consume-atom (stream)
  "Consume an atom name"
  (let ((this-char (elk--use-stream stream 'current)))
    (when (elk--atom-letter-p (car this-char))
      (let ((start-pos (cdr this-char))
            (current-char (elk--use-stream stream nil)))
        (while (elk--atom-letter-p (car (elk--use-stream stream 'peek)))
          (setf current-char (elk--use-stream stream nil)))
        (elk--create-token 'atom (list) start-pos (cdr current-char))))))

(defun elk--consume-text (stream)
  "Consume a text declaration"
  (let ((this-char (elk--use-stream stream 'current)))
    (when (elk--text-quote-p (car this-char))
      (let ((start-pos (cdr this-char))
            (current-char (elk--use-stream stream nil)))
        (while (and (elk--stream-next-p stream)
                    (not (elk--text-quote-p (car (elk--use-stream stream 'current)))))
          (when (elk--text-escape-p (car (elk--use-stream stream 'peek)))
            (setf current-char (elk--use-stream stream nil))
            (setf current-char (elk--use-stream stream nil)))
          (setf current-char (elk--use-stream stream nil))
          )
        (setf current-char (elk--use-stream stream nil))
        (elk--create-token 'text (list) start-pos (cdr current-char))))))

(defun elk--consume-expression (stream)
  "Consume an expression"
  (let ((this-char (elk--use-stream stream 'current)))
    (when (elk--expression-start-p (car this-char))
      (let ((start-pos (cdr this-char))
            (current-char (elk--use-stream stream nil))
            (expression-tokens (list)))
        (while (and (elk--stream-next-p stream)
                    (not (elk--expression-close-p (car current-char))))
          (push-end (elk--dispatch-stream-handlers stream) expression-tokens)
          (setf current-char (elk--use-stream stream 'current)))
        (setf current-char (elk--use-stream stream nil))
        (elk--create-token 'expression expression-tokens start-pos (cdr current-char))))))

(defun elk--consume-quote (stream)
  "Consume an atom name"
  (let ((this-char (elk--use-stream stream 'current)))
    (when (elk--quote-p (car this-char))
      (let ((start-pos (cdr this-char))
            (next-char (elk--use-stream stream nil)))
        (when (elk--quote-p (car next-char))
          (setf next-char (elk--use-stream stream nil)))
        (elk--create-token 'quote (elk--dispatch-stream-handlers stream) start-pos (cdr next-char))))))


(defvar elk--stream-handlers
    (list
     #'elk--consume-whitespace
     #'elk--consume-comment
     #'elk--consume-quote
     #'elk--consume-atom
     #'elk--consume-text
     #'elk--consume-expression)
  "Elisp parsing handlers")


(defun elk--dispatch-stream-handlers (stream)
  "Execute elisp parsing functions"
  (lexical-let ((value nil))
    (mapc (lambda (handler)
            (unless value
              (setf value (funcall handler stream))))
          elk--stream-handlers)
    value))


(defun elk--tokenize (text)
  "Tokenize an elisp text"
  (lexical-let ((tokens (list))
                (stream (elk--text-stream text)))
    (elk--use-stream stream nil)
    (while (elk--stream-next-p stream)
      (push-end (elk--dispatch-stream-handlers stream) tokens))
    tokens))
