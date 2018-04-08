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
template: article.jade
title: \"Upgrading Wintersmith\"
date: 2017-01-28 22:08
comments_enabled: true
category: Technology
tags: [wintersmith, nodejs]
publised: true
description: Instructions on Upgrading Octopress
---
#+END_HTML"
  :group 'org-export-wintersmith
  :type 'boolean)

(defcustom org-wintersmith-template "layouts/article.jade"
  "Default template used in Wintersmith article."
  :group 'org-export-wintersmith
  :type 'string)

(defcustom org-wintersmith-category ""
  "Default space-separated category in Wintersmith article."
  :group 'org-export-wintersmith
  :type 'string)

(defcustom org-wintersmith-tags ""
  "Default space-separated tags in Wintersmith article."
  :group 'org-export-wintersmith
  :type 'string)

(defcustom org-wintersmith-published "0"
  "Default publish status in Wintersmith article."
  :group 'org-export-wintersmith
  :type 'string)

(defcustom org-wintersmith-comments "0"
  "Default comments (disqus) flag in Wintersmith article."
  :group 'org-export-wintersmith
  :type 'string)

(defcustom org-wintersmith-description "0"
  "A one-liner description to display in place of summary text or the full post on index pages."
  :group 'org-export-wintersmith
  :type 'string)

;; Path for highlight command name
(defvar hljs-path "hljs")

;; TODO: This is pretty much useless right now, since I'm converting org-mode to
;; HTML and not markdown. Maybe I'll find something I wanna use, so I'm leaving
;; this here for now. But it's also likely that I end up using org-mode's export
;; since wintersmith doesn't really have highlight plugins like jekyll does (ala
;; Liquid).
;;
;; What jekyll does is it passes highlight blocks to a distinct parser seperate
;; from the markdown parser. Markdown can go to the markdown parser, while
;; highlight codeblocks can go to the Liquid extension. Thus, you can use
;; markdown + liquid or html + liquid.
;;
;; Wintersmith, on the other hand, provides syntax highlighting via a markdown
;; parse plugin. The nodejs plugin for markdown calls highlight.js and passes
;; markdown codeblocks to it. Anything crazier goes to the markdown parser
;; directly.
;;
;; Since we're parsing org-mode to HTML, it'd probably be easiest to just have
;; org-export render codeblocks for me. But just in case I hate the output, I'm
;; keeping all this info here as backup.
(defcustom org-wintersmith-use-src-plugin nil
  "If t, org-wintersmith exporter eagerly uses plugins instead of
org-mode's original HTML stuff. For example:

   #+BEGIN_SRC ruby
     puts \"Hello world\"
   #+END_SRC

makes:

  ```ruby
  puts \"Hello world\"
  ```"
  :group 'org-export-wintersmith-use-src-plugin
  :type 'boolean)


;;; Define Back-End

(org-export-define-derived-backend 'wintersmith 'html
  :menu-entry
  '(?j "Wintersmith: export to HTML with YAML front matter."
       ((?H "As HTML buffer" org-wintersmith-export-as-html)
        (?h "As HTML file" org-wintersmith-export-to-html)))
  :translate-alist
  '((template . org-wintersmith-template) ;; add YAML front matter.
    (src-block . org-wintersmith-src-block)
    (inner-template . org-wintersmith-inner-template)) ;; force body-only
  :options-alist
  '(
    (:wintersmith-template "WINTERSMITH_TEMPLATE" nil org-wintersmith-template)
    (:wintersmith-category "WINTERSMITH_CATEGORY" nil org-wintersmith-category)
    (:wintersmith-tags "WINTERSMITH_TAGS" nil org-wintersmith-tags)
    (:wintersmith-published "WINTERSMITH_PUBLISHED" nil org-wintersmith-published)
    (:wintersmith-comments "WINTERSMITH_COMMENTS" nil org-wintersmith-comments)
    (:wintersmith-description "WINTERSMITH_DESCRIPTION" nil org-wintersmith-description)
    ))


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
        (format "```%s\n%s\n```"
                (replace-regexp-in-string
                 "emacs-lisp" "cl"
                 (format "%s" language)) value))
    (hljs-org-html-code src-block contents info)
    ))

;;(org-export-with-backend 'html src-block contents info)


;; Pipe org-mode codeblocks/src-blocks to highlight.js instead of the native
;; org-mode syntax highlighter
;;
;; source: https://linevi.ch/en/org-pygments.html
;; adapted to highlight.js
(defun hljs-org-html-code (code contents info)
  ;; Generating tmp file path.
  ;; Current date and time hash will ideally pass our needs.
  (setq temp-source-file (format "/tmp/hljs-%s.txt"(md5 (current-time-string))))
  ;; default language to plain text
  (setq hljs-lang (or (org-element-property :language code)
                      "text"))

  ;; determine whether the codeblock is plain-text or code for the wrapper tag
  (setq shouldOnlyHighlight (cond
                              ((string= hljs-lang "d") t)
                              ((string= hljs-lang "hh") t)
                              ((string= hljs-lang "ld") t)
                              ((string= hljs-lang "fa") t)
                              ((string= hljs-lang "nd") t)
                              ((string= hljs-lang "out") t)
                              ((string= hljs-lang "in") t)
                              ((string= hljs-lang "sa") t)
                              ((string= hljs-lang "t") t)
                              (t nil)))

  (if shouldOnlyHighlight
      (setq hljs-code-block (concat
                             (format "<div class=\"highlight %s\">" hljs-lang)
                             (org-element-property :value code)
                             "</div>"
                             ))
    (setq hljs-code-block (concat
                           (format "<div class=\"highlight %s\"><pre><code class=\"%s\">" hljs-lang hljs-lang)
                           (org-element-property :value code)
                           "</code></pre></div>"
                           )))

  ;; Writing block contents to the file.
  (with-temp-file temp-source-file (insert hljs-code-block))
  ;; Exectuing the shell-command an reading an output
  (shell-command-to-string (format "%s < %s"
				                   hljs-path
				                   temp-source-file)))

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
  (let ((property
         (org-export-data (plist-get info property-name) info)))
    (format "%s" (or property default ""))))

;; Get the id to pass to wintersmith front-matter, to establish a UID for the
;; blog post (can be used with disqus! or for anything else with requiring
;; identification of posts; because we're using org-mode properties, it will
;; always be the same after export!)
(defun org-wintersmith--get-id ()
  (org-entry-get (point) "ID" nil t)
  )

;; Dig throw the parse-tree in the info plist and find the node-property
;; containing the ID property for the article
;;
;; UPDATE: Renamed, because it appears to have broken with the emacs 26 and
;; org-mode 9.1 update. Manually going through the parse tree reveals it's not
;; accessible there.
;; (defun org-wintersmith--old-get-id (info &optional default)
;;   (let ((the-parse-tree (plist-get info :parse-tree)))
;;     (let ((vals (org-element-map the-parse-tree 'node-property
;;                   (lambda (np)
;;                     (if (string= (org-element-property :key np) "id")
;;                         (org-element-property :value np)
;;                       ;;(format "%s" (or (org-element-property :value np) default ""))
;;                       )))))

;;       ;; org-element-map gives us a list of values
;;       ;; since if there are multiple ids, they'll all be the same, and since
;;       ;; we only limit 1 id per article, the first element of the list is what
;;       ;; we pick.
;;       (format "%s" (or (car vals) default "")))))

(defun org-wintersmith--yaml-front-matter (info)
  (let (
        (title
         (org-wintersmith--get-option info :title))
        (date
         (org-wintersmith--get-option info :date))
        (author
         (org-wintersmith--get-option info :author))
        (template
         (org-wintersmith--get-option info :wintersmith-template org-wintersmith-template))
        (category
         (org-wintersmith--get-option info :wintersmith-category org-wintersmith-category))
        (tags
         (org-wintersmith--get-option info :wintersmith-tags org-wintersmith-tags))
        (published
         (org-wintersmith--get-option info :wintersmith-published org-wintersmith-published))
        (comments
         (org-wintersmith--get-option info :wintersmith-comments org-wintersmith-comments))
        (description
         (org-wintersmith--get-option info :wintersmith-description org-wintersmith-description))
        (id
         (org-wintersmith--get-id))
        )

    ;; Translate org-mode/elisp values to wintersmith-compatible values (t -> 1; nil-> 0)
    ;;
    ;; nil values get skipped (handled by using our defaults defined here), so
    ;; no validation required!
    ;;
    ;; However, translate some org-mode/elisp conventions to wintersmith
    ;; conventions (eg. t to 1)
    (if (string= published "t")
        (progn
          (setq published "1"))
        )

    (if (string= comments "t")
        (progn
          (setq comments "1"))
      )

    (concat
     "---"
     "\ndate: "     date
     "\ntitle: \""    title
     ;; This is NOT a typo. It's quoting title.
     "\"\nauthor: " author
     "\ncategory: " category
     "\ntags: " tags
     "\npublished: " published
     "\ncomments_enabled: " comments
     "\ntemplate: "     template
     "\ndescription: " description
     "\nid: " id
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
    (&optional title date setupfile category tags published template)
  "Insert a settings template for Wintersmith exporter."
  (interactive)
  (let ((template     (or template org-wintersmith-template))
        (published  (or published org-wintersmith-published))
        (category (or category org-wintersmith-category))
        (tags  (or published org-wintersmith-tags)))
    (save-excursion
      (insert (format (concat
                       "#+TITLE: "             title
                       "\n#+DATE: "              date
                       "\n#+SETUPFILE: "         setupfile
                       "\n#+WINTERSMITH_TEMPLATE: "     template
                       "\n#+WINTERSMITH_CATEGORY: " category
                       "\n#+WINTERSMITH_TAGS: " tags
                       "\n#+WINTERSMITH_PUBLISHED: "  published
                       "\n\n* \n\n{{{more}}}"))))))

;;; provide

(provide 'ox-wintersmith)

;;; ox-wintersmith.el ends here
