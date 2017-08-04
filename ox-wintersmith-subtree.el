;;; ox-wintersmith-subtree.el --- Extension to ox-wintersmith for better export of subtrees   -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Artur Malabarba

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; Keywords: hypermedia

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
;; Extension to ox-wintersmith for better export of subtrees. This is only
;; possible thanks to `ox-wintersmith`, from the
;; [org-octopress](https://github.com/yoshinari-nomura/org-octopress)
;; repo (a copy is provided in this repo).
;;
;; *Please note, this is not a package, this is a script. Feel free to
;;  submit issues if you run into problems, just be aware that this does
;;  not fully conform with usual package standards.*
;;
;;; Usage
;;
;; Place this in your `load-path`, add the following lines to your init file, and invoke `M-x nitelite/export-to-blog` to export a subtree as a blog post.
;;
;; ```
;; (autoload 'nitelite/export-to-blog "wintersmith-once")
;; (setq org-wintersmith-use-src-plugin t)
;;
;; ;; Obviously, these two need to be changed for your blog.
;; (setq nitelite/blog-base-url "https://nitelite.io/")
;; (setq nitelite/blog-dir (expand-file-name "~/projects/byronsanchez/nitelite.io/contents/"))
;; ```

;;; Code:

(require 'org)
(require 'ox-wintersmith)
(require 'subr-x)

(defcustom nitelite/blog-dir (expand-file-name "~/projects/byronsanchez/nitelite.io/contents/")
  "Directory to save posts."
  :type 'directory
  :group 'nitelite)

(defcustom nitelite/blog-base-url "https://nitelite.io/"
  "Base URL of the blog.
Will be stripped from links addresses on the final HTML."
  :type 'string
  :group 'nitelite)

(defcustom nitelite/export-all-constraint "+TODO=\"DONE\""
  "An org-mode search constraint to determine which headlines in a file get exported when using `nitelite/export-all`"
  :group 'org-export-wintersmith
  :type 'string)

(defun nitelite/export-to-blog (dont-show &optional dont-validate)
  "Exports current subtree as wintersmith html and copies to blog.
Posts need very little to work, most information is guessed.
Scheduled date is respected and heading is marked as DONE.

Pages are marked by a \":EXPORT_WINTERSMITH_TEMPLATE: page\" property,
and they also need a :filename: property. Schedule is then
ignored, and the file is saved inside `nitelite/blog-dir'.

The filename property is not mandatory for posts. If present, it
will used exactly (no sanitising will be done). If not, filename
will be a sanitised version of the title, see
`nitelite/sanitise-file-name'."
  (interactive "P")
  (save-excursion
    ;; Actual posts NEED a TODO state. So we go up the tree until we
    ;; reach one.
    (while (null (org-entry-get (point) "TODO" nil t))
      (outline-up-heading 1 t))
    (org-entry-put (point) "EXPORT_WINTERSMITH_TEMPLATE"
                   (org-entry-get (point) "EXPORT_WINTERSMITH_TEMPLATE" t))
    (let* (
           ;; custom-id takes precedence over generated ids
           (custom-id (org-entry-get (point) "CUSTOM_ID" t))
           (id (org-id-get-create))
           ;; Try inactive timestamp first
           ;; THEN:
           ;; Try the closed stamp to make sure we don't set the front
           ;; matter to 00:00:00 which moves the post back a day
           (closed-stamp (org-entry-get (point) "CLOSED" t))
           (ia-stamp (org-entry-get (point) "TIMESTAMP_IA" t))
           (date (if ia-stamp
                     (date-to-time ia-stamp)
                   (if closed-stamp
                     (date-to-time closed-stamp)
                    (org-get-scheduled-time (point) nil))
                   ))
           (tags (nreverse (org-get-tags-at)))
           (meta-title (org-entry-get (point) "meta_title"))
           (is-page (string= (org-entry-get (point) "EXPORT_WINTERSMITH_TEMPLATE") "page"))
           (name (org-entry-get (point) "filename"))
           (title (org-get-heading t t))
           (series (org-entry-get (point) "series" t))
           (org-wintersmith-categories
            (mapconcat
             (lambda (tag) (nitelite/convert-tag tag))
             tags " "))
           (org-export-show-temporary-export-buffer nil))

      (unless date
        (org-schedule nil ".")
        (setq date (current-time)))

      ;; For pages, demand filename.
      (if is-page
          (if (null name)
              (error "Pages need a :filename: property"))
        ;; For posts, guess some information that wasn't provided as
        ;; properties.
        ;; Define a name, if there isn't one.
        (unless name
          (setq name (concat (format-time-string "%Y-%m-%d" date) "-" (nitelite/handleize title)))
          (org-entry-put (point) "filename" name))
        (org-todo 'done))

      (let ((subtree-content
             (save-restriction
               (org-narrow-to-subtree)
               (unless dont-validate
                 (ignore-errors (ispell-buffer))
                 )
               (buffer-string)))
            (header-content
             (nitelite/get-org-headers))
            (reference-buffer (current-buffer)))
        (with-temp-buffer
          (nitelite/prepare-input-buffer
           header-content subtree-content reference-buffer)

          ;; Export and then do some fixing on the output buffer.
          (org-wintersmith-export-as-html nil t nil nil nil)
          (with-current-buffer "*Org Wintersmith HTML Export*"
            (goto-char (point-min))
            ;; Configure the wintersmith header.
            (search-forward "\n---\n")
            (goto-char (1+ (match-beginning 0)))
            (when series
              (insert "series: \"" series "\"\n"))
            (when meta-title
              (insert "meta_title: \"" (format meta-title title) "\"\n"))
            (search-backward-regexp "\ndate *:\\(.*\\)$")
            (if is-page
                ;; Pages don't need a date field.
                (replace-match "" :fixedcase :literal nil 0)
              (replace-match (concat " " (format-time-string "%Y-%m-%d %T" date)) :fixedcase :literal nil 1))

            ;; Save the final file.
            (nitelite/clean-output-links)
            (let ((out-file
                   (expand-file-name (concat (if is-page "" "notebooks/") name ".html")
                                     nitelite/blog-dir)))
              (write-file out-file)
              (unless dont-show
                (find-file-other-window out-file)))

            ;; In case we commit, lets push the message to the kill-ring
            (kill-new (concat "UPDATE: " title))
            (kill-new (concat "POST: " title))))))))

(defun nitelite/get-org-headers ()
  "Return everything above the first headline of current buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp "^\\*+ ")
    (buffer-substring-no-properties (point-min) (match-beginning 0))))

(defconst nitelite/base-regexp
  (macroexpand `(rx (or ,nitelite/blog-base-url ,nitelite/blog-dir))))

(defun nitelite/clean-output-links ()
  "Strip `nitelite/blog-base-url' and \"file://\" from the start of URLs. "
  ;; Fix org's stupid filename handling.
  (goto-char (point-min))
  (while (search-forward-regexp "\\(href\\|src\\)=\"\\(file://\\)/" nil t)
    (replace-match "" :fixedcase :literal nil 2))
  ;; Strip base-url from links
  (goto-char (point-min))
  (while (search-forward-regexp
          (concat "href=\"" nitelite/base-regexp)
          nil t)
    (replace-match "href=\"/" :fixedcase :literal))
  (goto-char (point-min)))

;; TODO: Fix the bug here where if there's no newline after a link, it crashes
(defun nitelite/prepare-input-buffer (header content reference-buffer)
  "Insert content and clean it up a bit."
  (insert header content)
  (goto-char (point-min))
  (org-mode)
  (outline-next-heading)
  (let ((this-filename (org-entry-get nil "filename" t))
        target-filename)
    (while (progn (org-next-link)
                  (not org-link-search-failed))
      (cond
       ((looking-at (format "\\[\\[\\(file:%s\\)"
                            (regexp-quote (abbreviate-file-name nitelite/blog-dir))))
        (replace-match "file:/" nil nil nil 1)
        (goto-char (match-beginning 0))
        (when (looking-at (rx "[[" (group "file:/images/" (+ (not space))) "]]"))
          (goto-char (match-end 1))
          (forward-char 1)
          (insert "[" (match-string 1) "]")
          (forward-char 1)))
       ((looking-at "\\[\\[\\(\\*[^]]+\\)\\]")
        ;; Find the blog post to which this link points.
        (setq target-filename
              (save-excursion
                (save-match-data
                  (let ((point (with-current-buffer reference-buffer
                                 (point))))
                    (org-open-at-point t reference-buffer)
                    (with-current-buffer reference-buffer
                      (prog1 (url-hexify-string (org-entry-get nil "filename" t))
                        (goto-char point)))))))
        ;; We don't want to replace links inside the same post. Org
        ;; handles them better than us.
        (when (and target-filename
                   (null (string= target-filename this-filename)))
          (replace-match
           (format "/%s.html" (nitelite/strip-date-from-filename target-filename))
           :fixedcase :literal nil 1))))))
  (goto-char (point-min))
  (outline-next-heading))

(defun nitelite/strip-date-from-filename (name)
  (replace-regexp-in-string "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]-" "" name))

(defun nitelite/convert-tag (tag)
  "Overcome org-mode's tag limitations."
  (replace-regexp-in-string
   "_" "-"
   (replace-regexp-in-string "__" "." tag)))

(defun nitelite/sanitise-file-name (name)
  "Make NAME safe for filenames.
Removes any occurrence of parentheses (with their content),
Trims the result,
And transforms anything that's not alphanumeric into dashes."
  (require 'url-util)
  (require 'subr-x)
  (url-hexify-string
   (downcase
    (replace-regexp-in-string
     "[^[:alnum:]]+" "-"
     (string-trim
      (replace-regexp-in-string
       "(.*)" "" name))))))

(defun nitelite/handleize(name)
  "Make NAME safe for filenames.
Removes any occurrence of parentheses (with their content),
Trims the result,
And transforms anything that's not alphanumeric into dashes."
  (require 'url-util)
  (require 'subr-x)
  (url-hexify-string
   (downcase
    ;; Remove any remaining not word characters
    (replace-regexp-in-string
     "[^[:word:]-]" ""
     ;; replace all whitespace with a dash
      (replace-regexp-in-string
       "[[:space:]]+" "-"
        ;; Remove all punctuation since quotes won't be removed by word class
        (replace-regexp-in-string
         "[[:punct:]]+" ""
         ;; trim
         (string-trim
          (replace-regexp-in-string
           "(.*)" "" name))))))))

(defun nitelite/export-all ()
  "Export all subtrees that are *not* tagged with :noexport: to
separate files.

Subtrees that do not have the :EXPORT_FILE_NAME: property set
are exported to a filename derived from the headline text."
  (interactive)
  (save-excursion
    ;; (set-mark (point-min))
    ;;  (goto-char (point-max))
    (org-map-entries
     (lambda ()
         (funcall 'nitelite/export-to-blog t t)
         ) nitelite/export-all-constraint nil)))

; (defun nitelite/org-export-all (backend blog-tag)
;   "Export all subtrees that are *not* tagged with :noexport: to
; separate files.
;
; Subtrees that do not have the :EXPORT_FILE_NAME: property set
; are exported to a filename derived from the headline text."
;     (save-excursion
;       ;; (set-mark (point-min))
;       ;;  (goto-char (point-max))
;       (org-map-entries
;        (lambda ()
;          (let ((export-file (org-entry-get (point) "EXPORT_FILE_NAME")))
;            (unless export-file
;              (org-set-property
;               "EXPORT_FILE_NAME"
;               ;; This function generates the export filename by replacing spaces
;               ;; with _ in the headline text.
;               ;;
;               ;; You can change =replace-regexp-in-string= to whatever you like
;               ;; (eg. your own handleize function)
;               (replace-regexp-in-string " " "_" (nth 4 (org-heading-components)))))
;            (funcall fn nil t)
;            (unless export-file (org-delete-property "EXPORT_FILE_NAME"))
;            (set-buffer-modified-p modifiedp)))
;        blog-tag nil)
;
;       ))

; (defun nitelite/export-nitelite (backend)
;   "Export all subtrees that are *not* tagged with :noexport: to
; separate files.
;
; Subtrees that do not have the :EXPORT_FILE_NAME: property set
; are exported to a filename derived from the headline text."
;   (interactive "sEnter backend: ")
;   (let ((fn (cond ((equal backend "html") 'org-html-export-to-html)
;                   ((equal backend "markdown") 'org-md-export-to-markdown)
;                   ((equal backend "latex") 'org-latex-export-to-latex)
;                   ((equal backend "pdf") 'org-latex-export-to-pdf)))
;         (modifiedp (buffer-modified-p)))
;     (nitelite/org-export-all backend "NITELITE & EXPORT")))

(provide 'ox-wintersmith-subtree)
;;; ox-wintersmith-subtree.el ends here
