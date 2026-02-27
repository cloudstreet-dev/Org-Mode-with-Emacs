;;; publish.el — Org-Mode with Emacs book publisher
;;; Run with: emacs --batch -l publish.el --funcall org-publish-all

(require 'org)
(require 'ox-html)
(require 'ox-publish)

;; ──────────────────────────────────────────────
;; Appearance settings
;; ──────────────────────────────────────────────

(setq org-html-doctype "html5"
      org-html-html5-fancy t
      org-html-head-include-default-style nil
      org-html-head-include-scripts nil
      org-html-validation-link nil
      org-html-use-infojs nil
      org-export-with-smart-quotes t
      org-export-with-sub-superscripts nil
      ;; Never evaluate code blocks during export — this is a book,
      ;; not a notebook. Blocks are displayed verbatim.
      org-export-use-babel nil
      ;; Convert file:chapter.org links → chapter.html in HTML export
      org-html-link-org-files-as-html t
      ;; Example links in the text (e.g. [[*Project Alpha]]) won't
      ;; resolve — skip them rather than crashing.
      org-export-with-broken-links t)

;; Use htmlize for syntax highlighting when available
(when (require 'htmlize nil t)
  (setq org-html-htmlize-output-type 'css))

;; ──────────────────────────────────────────────
;; Chapter metadata
;; ──────────────────────────────────────────────

(defvar book-chapters
  '(("index"                           "Home")
    ("01-introduction"                 "1. Introduction")
    ("02-getting-started"              "2. Getting Started")
    ("03-document-structure"           "3. Document Structure")
    ("04-tables"                       "4. Tables")
    ("05-links-and-images"             "5. Links and Images")
    ("06-todo-items"                   "6. TODO Items")
    ("07-tags-and-properties"          "7. Tags and Properties")
    ("08-agenda-views"                 "8. Agenda Views")
    ("09-clocking-time-tracking"       "9. Clocking & Time Tracking")
    ("10-capture-templates"            "10. Capture Templates")
    ("11-code-blocks-literate-programming" "11. Literate Programming")
    ("12-export-publishing"            "12. Export & Publishing")
    ("13-advanced-features"            "13. Advanced Features"))
  "Ordered list of (slug title) for all book chapters.")

(defun book-sidebar-html ()
  "Return the HTML string for the sidebar nav."
  (concat
   "<nav id=\"org-nav\">\n"
   "  <a class=\"nav-title\" href=\"index.html\">Org-Mode<br>with Emacs</a>\n"
   "  <ol>\n"
   (mapconcat
    (lambda (chapter)
      (let ((slug  (car chapter))
            (title (cadr chapter)))
        (if (string= slug "index")
            ""                          ; skip index in the chapter list
          (format "    <li><a href=\"%s.html\">%s</a></li>\n" slug title))))
    book-chapters "")
   "  </ol>\n"
   "  <div class=\"nav-index\"><a href=\"index.html\">&larr; Table of Contents</a></div>\n"
   "</nav>\n"))

(defun book-chapter-nav-html (current-slug)
  "Return prev/next chapter navigation HTML for CURRENT-SLUG."
  (let* ((slugs (mapcar #'car book-chapters))
         (pos   (cl-position current-slug slugs :test #'string=))
         (prev  (when (and pos (> pos 0))
                  (nth (1- pos) book-chapters)))
         (next  (when (and pos (< pos (1- (length book-chapters))))
                  (nth (1+ pos) book-chapters))))
    (concat
     "<div class=\"chapter-nav\">\n"
     (if prev
         (format "  <a href=\"%s.html\">&larr; %s</a>\n"
                 (car prev) (cadr prev))
       "  <span></span>\n")
     (if next
         (format "  <a href=\"%s.html\">%s &rarr;</a>\n"
                 (car next) (cadr next))
       "  <span></span>\n")
     "</div>\n")))

(defun book-preamble (info)
  "Generate HTML preamble using INFO plist from ox-html."
  (let* ((file    (plist-get info :input-file))
         (slug    (when file
                    (file-name-sans-extension
                     (file-name-nondirectory file)))))
    (concat
     (book-sidebar-html)
     "<div id=\"preamble\">\n"
     (book-chapter-nav-html (or slug ""))
     "</div>\n")))

(defun book-postamble (_info)
  "Generate HTML postamble."
  "<div id=\"postamble\">
  <span>Org-Mode with Emacs — CC0 1.0 Universal</span>
  <span>Built with <a href=\"https://orgmode.org\">Org-Mode</a> &amp; Emacs</span>
</div>")

;; ──────────────────────────────────────────────
;; Publish project definition
;; ──────────────────────────────────────────────

(setq org-publish-project-alist
      `(("org-book-html"
         :base-directory       "."
         :base-extension       "org"
         :exclude              "setup\\.org"
         :publishing-directory "./public"
         :recursive            nil
         :publishing-function  org-html-publish-to-html
         :headline-levels      4
         :section-numbers      t
         :with-toc             t
         :with-author          nil
         :with-creator         nil
         :with-date            nil
         :html-doctype         "html5"
         :html-html5-fancy     t
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-preamble        book-preamble
         :html-postamble       book-postamble)

        ("org-book-static"
         :base-directory       "."
         :base-extension       "css\\|js\\|png\\|jpg\\|gif\\|svg\\|ico\\|webp"
         :publishing-directory "./public"
         :recursive            nil
         :publishing-function  org-publish-attachment)

        ("book"
         :components ("org-book-html" "org-book-static"))))

;; Force republish everything (no cache in CI)
(setq org-publish-use-timestamps-flag nil)
(setq org-publish-timestamp-directory "./.org-timestamps/")

;;; End of publish.el
