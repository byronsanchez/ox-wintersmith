;;; ox-wintersmith.el --- Export Wintersmith articles using org-mode.

;; Copyright (C) 2013  Yoshinari Nomura

;; Author: Yoshinari Nomura <nom@quickhack.net>
;; Author: Justin Gordon <justin.gordon@gmail.com>
;; Keywords: org, wintersmith
;; Version: 0.1

;;; Commentary:

;; This library implements a Wintersmith-style html backend for
;; Org exporter, based on `html' back-end.
;;
;; It provides two commands for export, depending on the desired
;; output: `org-jkl-export-as-html' (temporary buffer) and
;; `org-jkl-export-to-html' ("html" file with YAML front matter).
;;
;; For publishing, `org-wintersmith-publish-to-html' is available.
;; For composing, `org-wintersmith-insert-export-options-template' is available.

;;; Code:

;;; Dependencies

(require 'ox-html)
(defvar org-html-display-buffer-mode)
;;; User Configurable Variables

(defgroup org-export-wintersmith nil
  "Options for exporting Org mode files to wintersmith HTML."
  :tag "Org Wintersmith"
  :group 'org-export
  :version "24.2")

(defcustom org-wintersmith-include-yaml-front-matter t
  "If true, then include yaml-front-matter when exporting to html.

If false, then you should include the yaml front matter like this at the top of the file:

#+BEGIN_HTML
---
layout: post
title: \"Upgrading Octopress\"
date: 2013-09-15 22:08
comments: true
categories: [octopress, rubymine]
keywords: Octopress
description: Instructions on Upgrading Octopress
---
#+END_HTML"
  :group 'org-export-wintersmith
  :type 'boolean)

(defcustom org-wintersmith-layout "post"
  "Default layout used in Wintersmith article."
  :group 'org-export-wintersmith
  :type 'string)

(defcustom org-wintersmith-categories ""
  "Default space-separated categories in Wintersmith article."
  :group 'org-export-wintersmith
  :type 'string)

(defcustom org-wintersmith-published "true"
  "Default publish status in Wintersmith article."
  :group 'org-export-wintersmith
  :type 'string)

(defcustom org-wintersmith-comments ""
  "Default comments (disqus) flag in Wintersmith article."
  :group 'org-export-wintersmith
  :type 'string)

(defcustom org-wintersmith-use-src-plugin nil
  "If t, org-wintersmith exporter eagerly uses plugins instead of
org-mode's original HTML stuff. For example:

   #+BEGIN_SRC ruby
     puts \"Hello world\"
   #+END_SRC

makes:

  {% highlight ruby %}
  puts \"Hello world\"
  {% endhighlight %}"
  :group 'org-export-wintersmith-use-src-plugin
  :type 'boolean)


;;; Define Back-End

(org-export-define-derived-backend 'wintersmith 'html
  :export-block '("HTML" "WINTERSMITH")
  :menu-entry
  '(?j "Wintersmith: export to HTML with YAML front matter."
       ((?H "As HTML buffer" org-wintersmith-export-as-html)
        (?h "As HTML file" org-wintersmith-export-to-html)))
  :translate-alist
  '((template . org-wintersmith-template) ;; add YAML front matter.
    (src-block . org-wintersmith-src-block)
    (inner-template . org-wintersmith-inner-template)) ;; force body-only
  :options-alist
  '((:wintersmith-layout "WINTERSMITH_LAYOUT" nil org-wintersmith-layout)
    (:wintersmith-categories "WINTERSMITH_CATEGORIES" nil org-wintersmith-categories)
    (:wintersmith-published "WINTERSMITH_PUBLISHED" nil org-wintersmith-published)
    (:wintersmith-comments "WINTERSMITH_COMMENTS" nil org-wintersmith-comments)))


;;; Internal Filters
(defun org-wintersmith-src-block (src-block contents info)
  "Transcode SRC-BLOCK element into wintersmith code template format
if `org-wintersmith-use-src-plugin` is t. Otherwise, perform as
`org-html-src-block`. CONTENTS holds the contents of the item.
INFO is a plist used as a communication channel."
  (if org-wintersmith-use-src-plugin
      (let ((language (org-element-property :language src-block))
            (value (org-remove-indentation
                    (org-element-property :value src-block))))
        (format "{%% highlight %s %%}\n%s{%% endhighlight %%}"
                (replace-regexp-in-string
                 "emacs-lisp" "cl"
                 (format "%s" language)) value))
    (org-export-with-backend 'html src-block contents info)))

;;; Template
(defun org-wintersmith-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string. INFO is a plist
holding export options."
  (if org-wintersmith-include-yaml-front-matter
      (concat
       (org-wintersmith--yaml-front-matter info)
       contents)
    contents))

(defun org-wintersmith-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth (org-html-toc depth info)))
   ;; ;; PREVIEW mark on the top of article.
   ;; (unless (equal "true" (plist-get info :wintersmith-published))
   ;;   "<span style=\"background: red;\">PREVIEW</span>")
   ;; Document contents.
   contents
   ;; Footnotes section.
   (org-html-footnote-section info)))

;;; YAML Front Matter
(defun org-wintersmith--get-option (info property-name &optional default)
  (let ((property (org-export-data (plist-get info property-name) info)))
    (format "%s" (or property default ""))))

(defun org-wintersmith--yaml-front-matter (info)
  (let ((title
         (org-wintersmith--get-option info :title))
        (date
         (org-wintersmith--get-option info :date))
        (layout
         (org-wintersmith--get-option info :wintersmith-layout org-wintersmith-layout))
        (categories
         (org-wintersmith--get-option info :wintersmith-categories org-wintersmith-categories)))
    (concat
     "---"
     "\ntitle: \""    title
     "\"\ndate: "     date
     "\nlayout: "     layout
     "\ntags: " categories
     "\n---\n")))

;;; Filename and Date Helper
(defun org-wintersmith-date-from-filename (&optional filename)
  (let ((fn (file-name-nondirectory (or filename (buffer-file-name)))))
    (if (string-match "^[0-9]+-[0-9]+-[0-9]+" fn)
        (match-string 0 fn)
      nil)))

(defun org-wintersmith-property-list (&optional filename)
  (let ((backend 'wintersmith) plist)
    (if filename
        (with-temp-buffer
          (insert-file-contents filename)
          (org-mode)
          (setq plist (org-export-get-environment backend))
          (setq plist (plist-put plist :input-file filename)))
      (setq plist (org-export-backend-options backend))
      plist)))

(defun org-wintersmith-property (keys &optional filename)
  (let ((plist (org-wintersmith-property-list filename)))
    (mapcar (lambda (key) (org-export-data-with-backend (plist-get plist key) 'wintersmith plist))
            keys)))

(defun org-wintersmith-date-from-property (&optional filename)
  (let ((plist (org-wintersmith-property filename)))
    (org-read-date
     nil nil
     (org-export-data-with-backend (plist-get plist :date) 'wintersmith plist))))

(defun org-wintersmith-create-filename ()
  (let ((date (org-wintersmith-date-from-property))
        (file (file-name-nondirectory (buffer-file-name)))
        (dir  (file-name-directory (buffer-file-name))))
    (expand-file-name
     (replace-regexp-in-string "^[0-9]+-[0-9]+-[0-9]+" date file)
     dir)))

;;; End-User functions
;;;###autoload
(defun org-wintersmith-export-subtree-as-html
    (&optional async visible-only body-only ext-plist)
  "Export current subtree to a HTML buffer adding some YAML front matter."
  (interactive)
  )

;;;###autoload
(defun org-wintersmith-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a HTML buffer adding some YAML front matter."
  (interactive)
  (if async
      (org-export-async-start
          (lambda (output)
            (with-current-buffer (get-buffer-create "*Org Wintersmith HTML Export*")
              (erase-buffer)
              (insert output)
              (goto-char (point-min))
              (funcall org-html-display-buffer-mode)
              (org-export-add-to-stack (current-buffer) 'wintersmith)))
        `(org-export-as 'wintersmith ,subtreep ,visible-only ,body-only ',ext-plist))
    (let ((outbuf (org-export-to-buffer
                      'wintersmith "*Org Wintersmith HTML Export*"
                    nil subtreep visible-only body-only ext-plist)))
      ;; Set major mode.
      (with-current-buffer outbuf (set-auto-mode t))
      (if org-export-show-temporary-export-buffer
          (switch-to-buffer-other-window outbuf)
        outbuf))))

;;;###autoload
(defun org-wintersmith-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a HTML file adding some YAML front matter."
  (interactive)
  (let* ((extension (concat "." org-html-extension))
         (file (org-export-output-file-name extension subtreep))
         (org-export-coding-system org-html-coding-system))
    (if async
        (org-export-async-start
            (lambda (f) (org-export-add-to-stack f 'wintersmith))
          (let ((org-export-coding-system org-html-coding-system))
            `(expand-file-name
              (org-export-to-file
                  'wintersmith ,file ,subtreep ,visible-only ,body-only ',ext-plist))))
      (let ((org-export-coding-system org-html-coding-system))
        (org-export-to-file
            'wintersmith file nil subtreep visible-only body-only ext-plist)))))

;;;###autoload
(defun org-wintersmith-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML with YAML front matter.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'wintersmith filename ".html" plist pub-dir))

;;;###autoload
(defun org-wintersmith-insert-export-options-template
    (&optional title date setupfile categories published layout)
  "Insert a settings template for Wintersmith exporter."
  (interactive)
  (let ((layout     (or layout org-wintersmith-layout))
        (published  (or published org-wintersmith-published))
        (categories (or categories org-wintersmith-categories)))
    (save-excursion
      (insert (format (concat
                       "#+TITLE: "             title
                       "\n#+DATE: "              date
                       "\n#+SETUPFILE: "         setupfile
                       "\n#+WINTERSMITH_LAYOUT: "     layout
                       "\n#+WINTERSMITH_CATEGORIES: " categories
                       "\n#+WINTERSMITH_PUBLISHED: "  published
                       "\n\n* \n\n{{{more}}}"))))))

;;; provide

(provide 'ox-wintersmith)

;;; ox-wintersmith.el ends here
