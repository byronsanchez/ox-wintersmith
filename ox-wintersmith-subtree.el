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
;; Place this in your `load-path`, add the following lines to your init file,
;; and invoke `M-x nitelite/export-to-blog` to export a subtree as a blog post.
;;
;; ```
;; (autoload 'nitelite/export-to-blog "wintersmith-once")
;; (setq org-wintersmith-use-src-plugin t)
;;
;; ;; Obviously, these two need to be changed for your blog.
;; (setq nitelite/blog-base-url "https://nitelite.io/")
;; (setq nitelite/blog-dir (expand-file-name "~/projects/hackBytes/byronsanchez/nitelite.io/contents/"))
;; ```

;;; Code:

(require 'org)
(require 'ox-wintersmith)
(require 'subr-x)

;;
;; niteLite.io - music
;;

(defcustom nitelite/asset-dir (expand-file-name "~/Dropbox/hackBytes/byronsanchez/blogs/blogs-nitelite/contents/assets/")
  "Directory to store attachment assets."
  :type 'directory
  :group 'nitelite)

(defcustom nitelite/blog-dir (expand-file-name "~/Dropbox/hackBytes/byronsanchez/blogs/blogs-nitelite/contents/")
  "Directory to save posts."
  :type 'directory
  :group 'nitelite)

(defcustom nitelite/blog-base-url "https://nitelite.io/"
  "Base URL of the blog.
Will be stripped from links addresses on the final HTML."
  :type 'string
  :group 'nitelite)

(defcustom nitelite/export-all-constraint "+BLOG+NITELITE+EXPORT_WINTERSMITH_PUBLISHED=\"t\""
  "An org-mode search constraint to determine which headlines in a file get exported when using `wintersmith/export-all`"
  :group 'org-export-wintersmith
  :type 'string)

(defconst nitelite/base-regexp
  (macroexpand `(rx (or ,nitelite/blog-base-url ,nitelite/blog-dir))))

(defun nitelite/export-to-blog (dont-show &optional dont-validate)
  (interactive "P")
  (wintersmith/export-to-blog dont-show nitelite/asset-dir nitelite/blog-dir nitelite/blog-base-url nitelite/base-regexp dont-validate)
  )

(defun nitelite/export-all (dont-show &optional dont-validate)
  (interactive "P")
  (wintersmith/export-all dont-show nitelite/asset-dir nitelite/blog-dir nitelite/blog-base-url nitelite/base-regexp nitelite/export-all-constraint dont-validate)
  )

;;
;; hackBytes.io - software
;;

(defcustom hackbytes/asset-dir (expand-file-name "~/Dropbox/hackBytes/byronsanchez/blogs/blogs-hackbytes/contents/assets/")
  "Directory to store attachment assets."
  :type 'directory
  :group 'nitelite)

(defcustom hackbytes/blog-dir (expand-file-name "~/Dropbox/hackBytes/byronsanchez/blogs/blogs-hackbytes/contents/")
  "Directory to save posts."
  :type 'directory
  :group 'nitelite)

(defcustom hackbytes/blog-base-url "https://hackbytes.io/"
  "Base URL of the blog.
Will be stripped from links addresses on the final HTML."
  :type 'string
  :group 'nitelite)

(defcustom hackbytes/export-all-constraint "+BLOG+HACKBYTES+EXPORT_WINTERSMITH_PUBLISHED=\"t\""
  "An org-mode search constraint to determine which headlines in a file get exported when using `wintersmith/export-all`"
  :group 'org-export-wintersmith
  :type 'string)

(defconst hackbytes/base-regexp
  (macroexpand `(rx (or ,hackbytes/blog-base-url ,hackbytes/blog-dir))))

(defun hackbytes/export-to-blog (dont-show &optional dont-validate)
  (interactive "P")
  (wintersmith/export-to-blog dont-show hackbytes/asset-dir hackbytes/blog-dir hackbytes/blog-base-url hackbytes/base-regexp dont-validate)
  )

(defun hackbytes/export-all (dont-show &optional dont-validate)
  (interactive "P")
  (wintersmith/export-all dont-show hackbytes/asset-dir hackbytes/blog-dir hackbytes/blog-base-url hackbytes/base-regexp hackbytes/export-all-constraint dont-validate)
  )

;;
;; Plugin
;;

(defun wintersmith/export-to-blog (dont-show wintersmith/asset-dir wintersmith/blog-dir wintersmith/blog-base-url wintersmith/base-regexp &optional dont-validate)
  "Exports current subtree as wintersmith html and copies to blog.
Posts need very little to work, most information is guessed.
Scheduled date is respected and heading is marked as DONE.

Pages are marked by a \":EXPORT_WINTERSMITH_TEMPLATE: page\" property,
and they also need a :filename: property. Schedule is then
ignored, and the file is saved inside `wintersmith/blog-dir'.

The filename property is not mandatory for posts. If present, it
will used exactly (no sanitising will be done). If not, filename
will be a sanitised version of the title, see
`wintersmith/sanitise-file-name'."
  (interactive "P")
  (save-excursion
    ;; Actual posts NEED a PUBLISHED state. We move up to the 4th headline and
    ;; check for "PUBLISHED" state. We stop at the 4th headline, because using
    ;; file+datetree, the third headline is an actual date headline of the form
    ;; YYYY-MM-DD, so it stops being titles there, at least the way I use
    ;; org-mode for blogging.
    ;;
    ;; If there is no published state by the 4th-level headline, we assume it's
    ;; a draft and skip it.
    ;;
    ;; We don't inherit properties with org-entry-get, because we're manually
    ;; checking up the tree via while loop iteration
    ;;
    ;; UPDATE: didn't implement the last comment, but keeping it there for
    ;; reference in case what I currently have isn't enough.

    ;; get literal nil as string, so we can break this loop if we find an
    ;; explicit setting that signals the post is a draft
    ;;
    ;; basically, just keep going up hierarchies (that way we don't publish a
    ;; subheading eg. h3's and h4's) til we find an explicitly set PUBLISHED
    ;; state. This state signals that it's the top-level post headline. From
    ;; there, we read the value as a string (whether or not it's nil) and can
    ;; programattically decide what to do with nil (drafts) or t (published)
    ;; later.
    (while (null (org-entry-get (point) "EXPORT_WINTERSMITH_PUBLISHED" nil t))
      (outline-up-heading 1 t))

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
             (lambda (tag) (wintersmith/convert-tag tag))
             tags " "))
           (org-export-show-temporary-export-buffer nil))

      ;; DO NOT publish drafts
      ;; Only run the export process if a headline has been explicitly marked as published
      ;;
      ;; /now/ we get the literal nil as nil instead of string, to treat the
      ;; post as a draft
      (if (null (org-entry-get (point) "EXPORT_WINTERSMITH_PUBLISHED" nil))
          ;; IF NOT PUBLISHED, SET DRAFT TAGS
          (progn
            (message (concat "NOT PUBLISHED: " title))
            ;; Update tags for quick view of blog post state
            (org-toggle-tag "DRAFT" 'on)
            (org-toggle-tag "PUBLISHED" 'off))

        ;; IF IT'S PUBLISHED, THEN PUBLISH
        (unless date
          (org-schedule nil ".")
          (setq date (current-time)))

        ;; For pages, demand filename.
        (if is-page
            (if (null name)
                (error "Pages need a :filename: property"))

          ;; PERMALINK
          ;;
          ;; For posts, guess some information that wasn't provided as
          ;; properties.
          ;; Define a name, if there isn't one.
          ;;
          ;; If you go with a flat permalink structure (no YYYY-MM-dd), you can
          ;; hold off on implementing a unique fallback (appending number
          ;; incrementing if a collision occurs) until you actually run into
          ;; that situation multiple times
          ;;
          ;; Until then, you can leverage the :filename: property if you need to
          ;; as a fallback for the occasional collision that doesn't have to be
          ;; automated yet.
          (unless name
            (setq name (concat
                        ;;(format-time-string "%Y-%m-%d" date)
                        ;;"-"
                        (wintersmith/handleize title)))
            (org-entry-put (point) "filename" name))
          ;;(org-todo 'done)
          )

        ;; ATTACHMENTS
        ;;
        ;; Copy over attachments
        ;;
        ;; 1 - preserve modification times
        ;; 2 - create parent directories if they don't exist (ala mkdir -p)
        ;; 3 - copy contents into the target directory (ala rsync merge/ dst)
        ;;
        ;; This logic should be here because the target-directory will be
        ;; determined by which blog instance is doing the exporting.
        (unless (or (null wintersmith/asset-dir) (null (org-attach-dir)))

          ;; the directory the file resides in
          ;; the suffix of the attachment's unique id directory
          (setq wintersmith/attachments-suffix (org-attach-dir))
          ;; the prefix of the attachment's unique id directory
          (setq wintersmith/attachments-prefix (file-name-directory (directory-file-name wintersmith/attachments-suffix)))
          ;; the top-level attachment directory name (eg. "/data")
          (setq wintersmith/attachments-top-level (file-name-directory (directory-file-name wintersmith/attachments-prefix)))

          (setq wintersmith/target-asset-dir (concat wintersmith/asset-dir
                                         "/" (wintersmith/basename wintersmith/attachments-top-level)
                                         "/" (wintersmith/basename wintersmith/attachments-prefix)
                                         "/" (wintersmith/basename wintersmith/attachments-suffix)))

          (message (concat "Copying " wintersmith/attachments-suffix " to " wintersmith/target-asset-dir))
          (copy-directory wintersmith/attachments-suffix wintersmith/target-asset-dir t t t))

        (let ((subtree-content
               (save-restriction
                 (message (concat "Exporting: " name))
                 (org-narrow-to-subtree)
                 (unless dont-validate
                   (ignore-errors (ispell-buffer))
                   )
                 (buffer-string)))
              (header-content
               (wintersmith/get-org-headers))
              (reference-buffer (current-buffer)))
          (with-temp-buffer
            (wintersmith/prepare-input-buffer
             header-content subtree-content reference-buffer wintersmith/blog-dir)

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
              (wintersmith/clean-output-links wintersmith/base-regexp)
              (let ((out-file
                     (expand-file-name (concat (if is-page "" "notebooks/") name ".html")
                                       wintersmith/blog-dir)))
                (write-file out-file)
                (unless dont-show
                  (find-file-other-window out-file)))

              ;; In case we commit, lets push the message to the kill-ring
              (kill-new (concat "UPDATE: " title))
              (kill-new (concat "POST: " title)))))

        ;; Update tags for quick view of blog post state
        (org-toggle-tag "DRAFT" 'off)
        (org-toggle-tag "PUBLISHED" 'on)))))

(defun wintersmith/basename (path)
  (file-name-nondirectory (directory-file-name path)))

(defun wintersmith/get-org-headers ()
  "Return everything above the first headline of current buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp "^\\*+ ")
    (buffer-substring-no-properties (point-min) (match-beginning 0))))

(defun wintersmith/clean-output-links (wintersmith/base-regexp)
  "Strip `wintersmith/blog-base-url' and \"file://\" from the start of URLs. "
  ;; Fix org's stupid filename handling.
  (goto-char (point-min))
  (while (search-forward-regexp "\\(href\\|src\\)=\"\\(file://\\)/" nil t)
    (replace-match "" :fixedcase :literal nil 2))
  ;; Strip base-url from links
  (goto-char (point-min))
  (while (search-forward-regexp
          (concat "href=\"" wintersmith/base-regexp)
          nil t)
    (replace-match "href=\"/" :fixedcase :literal))
  (goto-char (point-min)))

;; TODO: Fix the bug here where if there's no newline after a link, it crashes
(defun wintersmith/prepare-input-buffer (header content reference-buffer wintersmith/blog-dir)
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
                            (regexp-quote (abbreviate-file-name wintersmith/blog-dir))))
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
           (format "/%s.html" (wintersmith/strip-date-from-filename target-filename))
           :fixedcase :literal nil 1))))))
  (goto-char (point-min))
  (outline-next-heading))

(defun wintersmith/strip-date-from-filename (name)
  (replace-regexp-in-string "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]-" "" name))

(defun wintersmith/convert-tag (tag)
  "Overcome org-mode's tag limitations."
  (replace-regexp-in-string
   "_" "-"
   (replace-regexp-in-string "__" "." tag)))

(defun wintersmith/sanitise-file-name (name)
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

(defun wintersmith/handleize(name)
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
         ;; Some of the puncutation /should/ be converted to dashes. We'll
         ;; convert them to spaces here, so that higher nested ops (punct
         ;; replacement) don't remove them completed.
         ;;
         ;; Spaces will get converted to dashes later on, so by converting them
         ;; here, we guarantee they will become dashes.
         (replace-regexp-in-string
          "[-_]" " "
          ;; trim
          (string-trim
           (replace-regexp-in-string
            "(.*)" "" name)))))))))

(defun wintersmith/export-all (dont-show wintersmith/asset-dir wintersmith/blog-dir wintersmith/blog-base-url wintersmith/base-regexp wintersmith/export-all-constraint &optional dont-validate)
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
       (funcall 'wintersmith/export-to-blog dont-show wintersmith/asset-dir wintersmith/blog-dir wintersmith/blog-base-url wintersmith/base-regexp dont-validate
        )
         ) wintersmith/export-all-constraint nil)))

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
