;;; elk-api-test.el ---
;;
;; Filename: elk-api-test.el
;; Description:
;; Author: Francis Murillo
;; Maintainer:
;; Created: Wed Aug 24 13:57:24 2016 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL:
;; Doc URL:
;; Keywords:
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

(ert-deftest elk-test/parsing ()
  (let* ((source-code
       (s-trim-left "
    ; A comment
      Four
        Whitespace
  \"Worker bees\"   #'func-quote (1 2 (3 ;meow
))"))
      (parsing (elk--parsing source-code))
      (check-token
       (lambda (value)
         (should (not (and (eq value 'stop) (null value))))
         (car (elk--attach-source source-code (list value)))))
      (current-token nil)
      (tokens nil))
    (setf current-token (funcall check-token (funcall parsing)))
    (should (eq (plist-get current-token :type) 'comment))

    (setf current-token (funcall check-token (funcall parsing)))
    (should (eq (plist-get current-token :type) 'whitespace))

    (setf current-token (funcall check-token (funcall parsing)))
    (should (eq (plist-get current-token :type) 'atom))
    (should (string-equal (plist-get current-token :text) "Four"))

    (setf current-token (funcall check-token (funcall parsing)))
    (should (eq (plist-get current-token :type) 'whitespace))

    (setf current-token (funcall check-token (funcall parsing)))
    (should (eq (plist-get current-token :type) 'atom))
    (should (string-equal (plist-get current-token :text) "Whitespace"))

    (setf current-token (funcall check-token (funcall parsing)))
    (should (eq (plist-get current-token :type) 'whitespace))

    (setf current-token (funcall check-token (funcall parsing)))
    (should (eq (plist-get current-token :type) 'text))
    (should (string-equal (plist-get current-token :text) "\"Worker bees\""))

    (setf current-token (funcall check-token (funcall parsing)))
    (should (eq (plist-get current-token :type) 'whitespace))


    (setf current-token (funcall check-token (funcall parsing)))
    (should (eq (plist-get current-token :type) 'quote))
    (should (not (null (plist-get current-token :tokens))))
    (should (string-equal (plist-get current-token :quote-text) "#'"))

    (setf current-token (car (plist-get current-token :tokens)))
    (should (eq (plist-get current-token :type) 'atom))
    (should (string-equal (plist-get current-token :text) "func-quote"))


    (setf current-token (funcall check-token (funcall parsing)))
    (should (eq (plist-get current-token :type) 'whitespace))

    (setf current-token (funcall check-token (funcall parsing)))
    (should (eq (plist-get current-token :type) 'expression))
    (should (not (null (plist-get current-token :tokens))))


    (setf tokens (plist-get current-token :tokens))
    (should (= (length tokens) 5))

    (setf current-token (car tokens))
    (setf tokens (cdr tokens))
    (should (eq (plist-get current-token :type) 'atom))
    (should (string-equal (plist-get current-token :text) "1"))

    (setf current-token (car tokens))
    (setf tokens (cdr tokens))
    (should (eq (plist-get current-token :type) 'whitespace))

    (setf current-token (car tokens))
    (setf tokens (cdr tokens))
    (should (eq (plist-get current-token :type) 'atom))
    (should (string-equal (plist-get current-token :text) "2"))

    (setf current-token (car tokens))
    (setf tokens (cdr tokens))
    (should (eq (plist-get current-token :type) 'whitespace))

    (setf current-token (car tokens))
    (should (eq (plist-get current-token :type) 'expression))
    (should (not (null (plist-get current-token :tokens))))

    (setf tokens (cdr tokens))
    (should (null tokens))


    (setf tokens (plist-get current-token :tokens))
    (should (= (length tokens) 3))

    (setf current-token (car tokens))
    (setf tokens (cdr tokens))
    (should (eq (plist-get current-token :type) 'atom))
    (should (string-equal (plist-get current-token :text) "3"))

    (setf current-token (car tokens))
    (setf tokens (cdr tokens))
    (should (eq (plist-get current-token :type) 'whitespace))

    (setf current-token (car tokens))
    (setf tokens (cdr tokens))
    (should (eq (plist-get current-token :type) 'comment))
    (should (string-equal (plist-get current-token :text) ";meow\n"))

    (setf tokens (cdr tokens))
    (should (null tokens))


    (should (eq (funcall parsing) 'stop))))

(ert-deftest elk-test/identity ()
  (let* ((package-file (symbol-file 'elk))
      (source-code (f-read-text package-file)))
    (should (string-equal
             (funcall (-compose
                       #'elk--codify
                       #'elk--parse)
                      source-code)
             source-code))))

(provide 'elk-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; elk-api-test.el ends here
