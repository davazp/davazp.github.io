#!/usr/bin/emacs --script
;;; generate.el --- Generate "dynamic" content of the site

;; Copyright (C) 2012  David Vazquez

;; Author: David Vazquez <davazp@gmail.com>
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

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'org)

(defvar blog-root-directory
  (expand-file-name "~/Projects/davazp.github.com/"))

(defvar blog-org-options
  (list :table-of-contents nil
        :section-numbers nil
        :publishing-directory blog-root-directory))

(defun blog-new (title)
  "Create a new post with the current date."
  (interactive "MTitle: ")
  (let ((dashed-title (replace-regexp-in-string "\s" "-" title)))
    (find-file (concat blog-root-directory
                       "org/"
                       (format-time-string "%Y-%m-%d-")
                       dashed-title ".org"))
    (insert "#+TITLE: " title "\n\n")))

(defun page-generate (input-file output-file layout)
  (let ((org-export-html-toplevel-hlevel 3)
        (output-buffer (generate-new-buffer-name "*blog-generate*")))
    (when (or (not (file-exists-p output-file))
              (< (float-time (nth 6 (file-attributes output-file)))
                 (float-time (nth 6 (file-attributes input-file)))))
      (let (title keywords)
        (with-current-buffer (find-file-noselect input-file)
          (goto-char (point-min))
          (let ((case-fold-search t))
            (re-search-forward "^#\\+TITLE:\s?\\(.*\\)$"))
          (setq title (match-string 1))

          (goto-char (point-min))
          (let ((case-fold-search t))
            (when (re-search-forward "^#\\+KEYWORDS:\s?\\(.*\\)$" nil t)
              (setq keywords (split-string (match-string 1) "," t))))

          (org-export-as-html 3 blog-org-options output-buffer t))
        (with-current-buffer (get-buffer output-buffer)
          (goto-char (point-min))
          (insert "---\n")
          (insert "title: " title "\n")
          (insert "tags: [")
          (while (cdr keywords)
            (insert (car keywords) ",")
            (setq keywords (cdr keywords)))
          (when (car keywords)
            (insert (car keywords)))
          (insert "]\n")
          (insert "layout: " layout "\n")
          (insert "---\n")
          (write-file output-file))))))

(defun blog-generate ()
  "Generate the HTML files from the org-mode posts."
  (interactive)
  (let* ((count 0)
         (posts (reverse (directory-files (concat blog-root-directory "org/") t "\\.org$")))
         (pages (directory-files blog-root-directory t "\\.org$"))
         (reporter (make-progress-reporter "Generating blog..." 0 (+ (length posts) (length pages))))
         (post-directory (concat blog-root-directory "_posts/")))
    (let ((org-link-abbrev-alist '(("img" . "file:../../../img/"))))
      (dolist (post posts)
        (page-generate post (concat post-directory (file-name-base post) ".html") "post")
        (progress-reporter-do-update reporter (incf count))))
    (let ((org-link-abbrev-alist '(("img" . "file:img/"))))
      (dolist (page pages)
        (page-generate page (concat blog-root-directory (file-name-base page) ".html") "article")
        (progress-reporter-do-update reporter (incf count))))
    (progress-reporter-done reporter)))


;;; If this script is loaded non-interactively, then generate the
;;; blog. Otherwise, we are supposed to be in emacs, so just provide
;;; the useful commands to the user.
(when noninteractive
  (blog-generate))

;;; generate.el ends here
