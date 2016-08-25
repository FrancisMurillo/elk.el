;;; elk-consume-test.el --- elk
;;
;; Filename: elk-consume-test.el
;; Description:
;; Author: Francis Murillo
;; Maintainer:
;; Created: Tue Aug 23 21:51:24 2016 (+0800)
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


(require 's)
(require 'subr-x)


;;* consume-whitespace
(ert-deftest elk--consume-whitespace-test/base ()
  (let* ((source-code " \n\t  ")
         (source-stream (elk--started-stream source-code))
         (token (elk--consume-whitespace source-stream)))
    (should (eq 'whitespace
                (plist-get token :type)))
    (should (null (plist-get token :tokens)))
    (should (= 0
               (plist-get token :start-pos)))
    (should (= -1
               (plist-get token :end-pos)))

    (should-not (elk--stream-next-p source-stream))))

(ert-deftest elk--consume-whitespace-test/partial ()
  (let* ((source-code " \n|\t  ")
         (source-stream (elk--started-stream source-code))
         (token (elk--consume-whitespace source-stream)))
    (should (eq 'whitespace
                (plist-get token :type)))
    (should (null (plist-get token :tokens)))
    (should (= 0
               (plist-get token :start-pos)))
    (should (= (s-index-of "|" source-code)
               (plist-get token :end-pos)))

    (should (elk--stream-next-p source-stream))))

(ert-deftest elk--consume-whitespace-test/nil ()
  (let* ((source-code "STOP")
         (source-stream (elk--started-stream source-code))
         (token (elk--consume-whitespace source-stream)))
    (should (null token))

    (should (elk--stream-next-p source-stream))))

(ert-deftest elk--consume-whitespace-test/consuming ()
  (mapc (lambda (whitespace-character)
          (let* ((source-code " ")
                 (source-stream (elk--started-stream source-code))
                 (token (elk--consume-whitespace source-stream)))
            (should-not (null token))
            (should (elk--stream-stop-p source-stream))))
        (list " " "\n" "\t")))


;;* consume-comment
(ert-deftest elk--consume-comment-test/base ()
  (let* ((source-code "; Meow \n")
         (source-stream (elk--started-stream source-code))
         (token (elk--consume-comment source-stream)))
    (should (eq 'comment
                (plist-get token :type)))
    (should (null (plist-get token :tokens)))
    (should (= 0
               (plist-get token :start-pos)))
    (should (= -1
               (plist-get token :end-pos)))

    (should-not (elk--stream-next-p source-stream))))

(ert-deftest elk--consume-comment-test/partial ()
  (let* ((source-code "; \tHalf \nEmpty")
         (source-stream (elk--started-stream source-code))
         (token (elk--consume-comment source-stream)))
    (should (eq 'comment
                (plist-get token :type)))
    (should (null (plist-get token :tokens)))
    (should (= 0
               (plist-get token :start-pos)))
    (should (= (1+ (s-index-of "\n" source-code))
               (plist-get token :end-pos)))

    (should (elk--stream-next-p source-stream))))

(ert-deftest elk--consume-comment-test/nil ()
  (let* ((source-code "NO;pe")
         (source-stream (elk--started-stream source-code))
         (token (elk--consume-comment source-stream)))
    (should (null token))
    (should (elk--stream-next-p source-stream))))

(ert-deftest elk--consume-comment-test/consuming ()
  (mapc (lambda (code)
          (let* ((source-code code)
                 (source-stream (elk--started-stream source-code))
                 (token (elk--consume-comment source-stream)))
            (should-not (null token))
            (should (elk--stream-stop-p source-stream))))
        (list ";" ";\n")))


;;* consume-text
(ert-deftest elk--consume-text-test/base ()
  (let* ((source-code "\"Meow\"")
         (source-stream (elk--started-stream source-code))
         (token (elk--consume-text source-stream)))
    (should (eq 'text
                (plist-get token :type)))
    (should (null (plist-get token :tokens)))
    (should (= 0
               (plist-get token :start-pos)))
    (should (= -1
               (plist-get token :end-pos)))

    (should-not (elk--stream-next-p source-stream))))

(ert-deftest elk--consume-text-test/partial ()
  (let* ((source-code "\" Cartoon \"|; Saloon")
         (source-stream (elk--started-stream source-code))
         (token (elk--consume-text source-stream)))
    (should (eq 'text
                (plist-get token :type)))
    (should (null (plist-get token :tokens)))
    (should (= 0
               (plist-get token :start-pos)))
    (should (= (s-index-of "|" source-code)
               (plist-get token :end-pos)))

    (should (elk--stream-next-p source-stream))))

(ert-deftest elk--consume-text-test/nil ()
  (let* ((source-code "(What the hell)")
         (source-stream (elk--started-stream source-code))
         (token (elk--consume-text source-stream)))
    (should (null token))
    (should (elk--stream-next-p source-stream))))

(ert-deftest elk--consume-text-test/consuming ()
  (mapc (lambda (code)
          (let* ((source-code code)
                 (source-stream (elk--started-stream source-code))
                 (token (elk--consume-text source-stream)))
            (should-not (null token))
            (should (elk--stream-stop-p source-stream))))
        (list "\"\"" "\"\ \"")))


;;* consume-quote
(ert-deftest elk--consume-quote-test/base ()
  (mapc (lambda (code)
          (let* ((source-code code)
                 (source-stream (elk--started-stream source-code))
                 (token (elk--consume-quote source-stream)))
            (should (eq 'quote
                        (plist-get token :type)))
            (should-not (null (plist-get token :tokens)))
            (should (= 0
                       (plist-get token :start-pos)))
            (should (= -1
                       (plist-get token :end-pos)))

            (should-not (elk--stream-next-p source-stream))))
        (list "'quote" "#'function" "`back-quote" "'spa\ ced")))

(ert-deftest elk--consume-quote-test/partial ()
  (mapc (lambda (code)
          (let* ((source-code code)
                 (source-stream (elk--started-stream source-code))
                 (token (elk--consume-quote source-stream)))
            (should (eq 'quote
                        (plist-get token :type)))
            (should-not (null (plist-get token :tokens)))
            (should (= 0
                       (plist-get token :start-pos)))
            (should (= (s-index-of " " source-code)
                       (plist-get token :end-pos)))

            (should (elk--stream-next-p source-stream))))
        (list "'quote atom" "#'function atom" "`back-quote atom")))

(ert-deftest elk--consume-quote-test/nil ()
  (mapc (lambda (code)
          (let* ((source-code code)
                 (source-stream (elk--started-stream source-code))
                 (token (elk--consume-quote source-stream)))
            (should (null token))
            (should (elk--stream-next-p source-stream))))
        (list "atom 'quote" "atom #'function" "atom `back-quote" "(expressive)" "#incomplete")))

(ert-deftest elk--consume-quote-test/consuming ()
  ;; Edge case of nothing being actually quoted but still consumed
  (mapc (lambda (code)
          (let* ((source-code code)
                 (source-stream (elk--started-stream source-code))
                 (token (elk--consume-quote source-stream)))
            (should-not (null token))
            (should (elk--stream-stop-p source-stream))))
        (list "'" "#'" "`")))


;;* consume-atom
(ert-deftest elk--consume-atom-test/base ()
  (mapc (lambda (code)
          (let* ((source-code code)
                 (source-stream (elk--started-stream source-code))
                 (token (elk--consume-atom source-stream)))
            (should (eq 'atom
                        (plist-get token :type)))
            (should (null (plist-get token :tokens)))
            (should (= 0
                       (plist-get token :start-pos)))
            (should (= -1
                       (plist-get token :end-pos)))

            (should-not (elk--stream-next-p source-stream))))
        (list "simple-name" "abc-and-123" "namespaced/function" "colonized:$callback" "my\ space"
              "?Q" "?\Q")))

(ert-deftest elk--consume-atom-test/partial ()
  (mapc (lambda (code)
          (let* ((source-code code)
                 (source-stream (elk--started-stream source-code))
                 (token (elk--consume-atom source-stream)))
            (should (eq 'atom
                        (plist-get token :type)))
            (should (null (plist-get token :tokens)))
            (should (= 0
                       (plist-get token :start-pos)))
            (should (= (s-index-of " " source-code)
                       (plist-get token :end-pos)))

            (should (elk--stream-next-p source-stream))))
        (list "oneword (1)" "$xyz +123" "a\ b\ c again" "?\P ; Meow" "?Q ; Purr}")))

(ert-deftest elk--consume-atom-test/nil ()
  (mapc (lambda (code)
          (let* ((source-code code)
                 (source-stream (elk--started-stream source-code))
                 (token (elk--consume-atom source-stream)))
            (should (null token))
            (should (elk--stream-next-p source-stream))))
        (list " legit token" "\nreally whitespace" "\tmorestuff" "(-:" ")-(")))

(ert-deftest elk--consume-atom-test/consuming ()
  (mapc (lambda (code)
          (let* ((source-code code)
                 (source-stream (elk--started-stream source-code))
                 (token (elk--consume-atom source-stream)))
            (should-not (null token))
            (should (elk--stream-stop-p source-stream))))
        (list "a" "?b" "?\c")))


;;* consume-expression
(ert-deftest elk--consume-expression-test/base ()
  (mapc (lambda (code)
          (let* ((source-code code)
                 (source-stream (elk--started-stream source-code))
                 (token (elk--consume-expression source-stream)))
            (should (eq 'expression
                        (plist-get token :type)))
            (should-not (null (plist-get token :tokens)))
            (should (= 0
                       (plist-get token :start-pos)))
            (should (= -1
                       (plist-get token :end-pos)))

            (should-not (elk--stream-next-p source-stream))))
        (list "(1)" "(a\ b axel)" "(a $ b)" "(a ?a ?\) d)")))

(ert-deftest elk--consume-expression-test/complete ()
  (let* ((source-code "(1 a ;meow\n 'b #'c `d ,e (\"meopw\") ?\)  )")
         (source-stream (elk--started-stream source-code))
         (token (elk--consume-expression source-stream)))
    (should (eq 'expression
                (plist-get token :type)))
    (should-not (null (plist-get token :tokens)))))

(ert-deftest elk--consume-expression-test/partial ()
  (mapc (lambda (code)
          (let* ((source-code code)
                 (source-stream (elk--started-stream source-code))
                 (token (elk--consume-expression source-stream)))
            (should (eq 'expression
                        (plist-get token :type)))
            (should-not (null (plist-get token :tokens)))
            (should (= 0
                       (plist-get token :start-pos)))
            (should (= (s-index-of "|" source-code)
                       (plist-get token :end-pos)))

            (should (elk--stream-next-p source-stream))))
        (list "('a)|'b " "('a ?\))|?\)")))

(ert-deftest elk--consume-expression-test/nil ()
  (mapc (lambda (code)
          (let* ((source-code code)
                 (source-stream (elk--started-stream source-code))
                 (token (elk--consume-expression source-stream)))
            (should (null token))
            (should (elk--stream-next-p source-stream))))
        (list "axel" "?b" "?( ?)")))

(ert-deftest elk--consume-expression-test/consuming ()
  (mapc (lambda (code)
          (let* ((source-code code)
                 (source-stream (elk--started-stream source-code))
                 (token (elk--consume-expression source-stream)))
            (should-not (null token))
            (should (elk--stream-stop-p source-stream))))
        (list "()" "(   )")))


(provide 'elk-consume-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; elk-consume-test.el ends here
