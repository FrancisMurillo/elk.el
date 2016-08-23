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

(require 'f)
(require 'elk (f-expand "elk" (f-parent (f-parent (f-this-file)))))

(provide 'test-helper)
;;; test-helper.el ends here