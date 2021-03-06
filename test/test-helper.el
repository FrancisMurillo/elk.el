;;; test-helper.el --- elk: test helper              -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Francis Murillo

;; Author: Francis Murillo <francisavmurillo@gmail.com>

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

(eval-when-compile (require 'cl))


(require 'dash)
(require 'dash-functional)

(require 'elk (expand-file-name "elk" "./"))


;; Stream
(defun equal-pair (expected-pair actual-pair)
  "Check if EXPECTED-PAIR is equql to ACTUAL-PAIR."
  (and (string-equal (car expected-pair) (car actual-pair))
       (= (cdr expected-pair) (cdr actual-pair))))

(defun char-at (n text)
  "Get character at N of TEXT as a pair."
  (cons (substring-no-properties text n (1+ n)) n))


(provide 'test-helper)
;;; test-helper.el ends here
