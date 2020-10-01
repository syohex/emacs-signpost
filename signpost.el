;;; signpost.el --- Emacs Lisp binding of macOS signpost log

;; Copyright (C) 2020 Shohei YOSHIDA

;; Author: Shohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-signpost
;; Package-Requires: ((emacs "27"))
;; Version: 0.01

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

;;; Code:

(require 'cl-lib)
(require 'signpost-core)

;;;###autoload
(defun signpost-log-create (sub-system)
  "Create a custom log object whose subsystem name is `sub-system'"
  (signpost-core-log-create sub-system))

(defun signpost-begin (log-obj)
  (signpost-core-begin log-obj))

(defun signpost-end (log-obj)
  (signpost-core-end log-obj))

(defmacro with-signpost (log-obj &rest body)
  (declare (indent 1) (debug t))
  `(progn
     (signpost-core-begin ,log-obj)
     ,@body
     (signpost-core-end ,log-obj)))

(provide 'signpost)

;;; signpost.el ends here
