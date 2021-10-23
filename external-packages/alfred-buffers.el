;;; alfred-buffers.el --- Switch buffers from Alfred  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Dale Sedivec

;; Author: Dale Sedivec <dale@codefu.org>
;; Keywords: tools, convenience
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Your Emacs must load this file so that Alfred can call the
;; functions in it.  An easy way is to just drop the file somewhere on
;; your `load-path' and then `require' it.  Alternatively, you can use
;; the autoloads defined here; if you `package-install-file' (or
;; `package-install-from-buffer') this file, Emacs should set those
;; autoloads up for you.

;;; Code:

(require 'json)
(require 'subr-x)

(defvar alfred-buffers-buffer-list-function #'alfred-buffers-buffer-list
  "Returns a list of buffers to be offered as completions.")

(defvar alfred-buffers-display-buffer-function #'display-buffer
  "Function to display a buffer in some window given its name.

The function must return the window where BUFFER-NAME is
displayed, just like `display-buffer'.")

(defun alfred-buffers-buffer-list ()
  "Return buffers to be offered for completion in Alfred.
Includes all buffers except those that begin with a space."
  (seq-remove (lambda (buf) (string-match-p "^ " (buffer-name buf)))
              (buffer-list)))

;;;###autoload
(defun alfred-buffers-produce-items ()
  "Produce a JSON list of buffers for use in an Alfred script filter."
  (let ((buffers (mapcar (lambda (buf)
                           (let ((name (buffer-name buf)))
                             `((title . ,name)
                               (arg . ,(format "%S" name)))))
                         (funcall alfred-buffers-buffer-list-function))))
    (json-encode `((items . ,(seq-into buffers 'vector))))))

;;;###autoload
(defun alfred-buffers-switch-buffer (buffer-name)
  "Select a window showing BUFFER-NAME, then raise and focus Emacs."
  (let ((win (display-buffer buffer-name)))
    (select-window win)
    (select-frame-set-input-focus (window-frame win))))

(provide 'alfred-buffers)
;;; alfred-buffers.el ends here
