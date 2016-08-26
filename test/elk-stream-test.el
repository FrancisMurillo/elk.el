;;; elk-stream-test.el ---
;;
;; Filename: elk-stream-test.el
;; Description:
;; Author: Francis Murillo
;; Maintainer:
;; Created: Tue Aug 23 21:51:46 2016 (+0800)
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

(ert-deftest elk--text-stream-test/incremented ()
  ;; TODO: Feature might not be needed
  (let* ((text "+123-")
         (stream (elk--text-stream text)))
    ;; Not started test
    (should (eq 'base
                (funcall stream 'current)))
    (should (equal-pair (char-at 0 text)
                        (funcall stream 'next)))

    (should (equal-pair (char-at 1 text)
                        (funcall stream 'peek)))
    (should (equal-pair (char-at 1 text)
                        (funcall stream 'peek 1)))

    (should (equal-pair (char-at 2 text)
                        (funcall stream 'peek 2)))
    (should (equal-pair (char-at 2 text)
                        (funcall stream 'next 2)))

    (should (equal-pair (char-at 3 text)
                        (funcall stream 'peek)))))

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


(ert-deftest elk--stream-next-p-test/usage ()
  (let* ((text "me")
         (stream (elk--text-stream text)))
    ;; Base test
    (should (elk--stream-next-p stream))
    (funcall stream 'next)

    (should (elk--stream-next-p stream))
    (funcall stream 'next)

    (should-not (elk--stream-next-p stream))))


(provide 'elk-stream-test)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; elk-stream-test.el ends here
