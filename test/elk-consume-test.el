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
               (plist-get token :end-pos)))))

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
               (plist-get token :end-pos)))))

(ert-deftest elk--consume-whitespace-test/nil ()
  (let* ((source-code "STOP")
         (source-stream (elk--started-stream source-code))
         (token (elk--consume-whitespace source-stream)))
    (should (null token))))


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
               (plist-get token :end-pos)))))

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
               (plist-get token :end-pos)))))

(ert-deftest elk--consume-comment-test/nil ()
  (let* ((source-code "NO;pe")
         (source-stream (elk--started-stream source-code))
         (token (elk--consume-comment source-stream)))
    (should (null token))))


(provide 'elk-consume-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; elk-consume-test.el ends here
