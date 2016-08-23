;;; elk.el-test.el --- elk: main test

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


(defun equal-pair (expected-pair actual-pair)
  (and (string-equal (car expected-pair) (car actual-pair))
       (= (cdr expected-pair) (cdr actual-pair))))

(defun char-at (n text)
  (cons (substring-no-properties text n (1+ n)) n))

(ert-deftest elk--text-stream-test/usage ()
  (let* ((text "abcde")
         (stream (elk--text-stream text)))
    (should (equal 'base
                   (funcall stream 'current)))

    (dotimes (index (length text))
      (should (equal-pair (char-at index text)
                          (funcall stream 'peek)))
      (should (equal-pair (char-at index text)
                          (funcall stream 'next)))
      (should (equal-pair (char-at index text)
                          (funcall stream 'current))))

    (should (eq 'stop
                (funcall stream 'peek)))
    (should (eq 'stop
                (funcall stream 'next)))
    (should (eq 'stop
                (funcall stream 'current)))

    (should (eq 'stop
                (funcall stream 'next)))))


(provide 'elk-stream-test)

;;; elk.el-test.el ends here
