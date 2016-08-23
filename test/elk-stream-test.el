;;; elk.el-test.el --- elk: main test  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Francis Murillo

;; Author: Francis Murillo <francisavmurillo@gmail.com>
;; Keywords:

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

(require 's)


(ert-deftest elk--text-stream-test/usage ()
  (let* ((text "abcde")
         (stream (elk--text-stream text)))
    ;; Not started test
    (should (eq 'base
                   (funcall stream 'current)))

    ;; Actual iteration
    (dotimes (index (length text))
      (should (equal-pair (char-at index text)
                          (funcall stream 'peek)))
      (should (equal-pair (char-at index text)
                          (funcall stream 'next)))
      (should (equal-pair (char-at index text)
                          (funcall stream 'current))))

    ;; End of iteration
    (should (eq 'stop
                (funcall stream 'peek)))
    (should (eq 'stop
                (funcall stream 'next)))
    (should (eq 'stop
                (funcall stream 'current)))

    ;; Idempotence
    (should (eq 'stop
                (funcall stream 'next)))))

(ert-deftest elk--text-stream-test/empty ()
  (let* ((text "")
         (stream (elk--text-stream text)))
    ;; Should be base, not stop
    (should (eq 'base
                (funcall stream 'current)))

    ;; Should say stop on next values
    (should (eq 'stop
                (funcall stream 'peek)))
    (should (eq 'stop
                (funcall stream 'next)))))


(ert-deftest elk--started-stream-test/usage ()
  (let* ((text "abc")
         (stream (elk--started-stream text)))
    ;; Should start at the first character
    (should-not (eq 'base
                    (funcall stream 'current)))

    (should (equal-pair (char-at 0 text)
                        (funcall stream 'current)))
    (should (equal-pair (char-at 1 text)
                        (funcall stream 'peek)))))


(ert-deftest elk--use-stream-test/usage ()
  (let* ((text "z")
         (stream (elk--text-stream text))
         (default-pair (cons "" -1)))
    ;; Not started test
    (should (eq default-pair
                (elk--use-stream stream 'current default-pair)))

    ;; Should work normally
    (should (equal-pair (char-at 0 text)
                        (elk--use-stream stream 'peek default-pair)))
    (should (equal-pair (char-at 0 text)
                        (elk--use-stream stream 'next default-pair)))
    (should (equal-pair (char-at 0 text)
                        (elk--use-stream stream 'current default-pair)))

    ;; Should default
    (should (equal-pair default-pair
                        (elk--use-stream stream 'peek default-pair)))
    (should (equal-pair default-pair
                        (elk--use-stream stream 'next default-pair)))))


(provide 'elk-stream-test)

;;; elk.el-test.el ends here
