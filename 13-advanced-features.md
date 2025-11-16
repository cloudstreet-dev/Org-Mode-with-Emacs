# Chapter 13: Advanced Features and the Org Ecosystem

You've learned the essentials. You could stop here and have a productive Org-Mode workflow. But Org-Mode goes deeper. Much deeper.

This chapter explores the advanced features and extensions that make Org-Mode practically inexhaustible.

## Advanced Agenda Features

### Stuck Projects

Identify projects without next actions:

```elisp
(setq org-stuck-projects
      '("+PROJECT/-DONE" ("TODO" "NEXT") nil ""))
```

`C-c a #` - Show stuck projects

Projects are headings tagged `PROJECT` without any TODO/NEXT items. These need attention—no next action defined.

### Custom Agenda Commands: Advanced

Block agendas with search:

```elisp
(setq org-agenda-custom-commands
      '(("w" "Work Dashboard"
         ((agenda "" ((org-agenda-span 1)))
          (tags-todo "+work+PRIORITY=\"A\"")
          (tags-todo "+work-PRIORITY=\"A\"")
          (tags "+work+SCHEDULED>=\"<today>\"")))))
```

Combine agenda views, searches, filters—build mission control dashboards.

### Agenda Skip Functions

Programmatically exclude items:

```elisp
(defun my-skip-unless-high-priority ()
  (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
    (if (not (string= (org-get-priority (current-buffer)) "A"))
        next-headline
      nil)))

(setq org-agenda-custom-commands
      '(("h" "High Priority"
         tags-todo "work"
         ((org-agenda-skip-function 'my-skip-unless-high-priority)))))
```

Ultimate filtering control.

### Calendar Integration

Sync with external calendars:

```elisp
(setq org-agenda-include-diary t)
```

Org agenda shows diary entries (birthdays, holidays, appointments).

## Encryption with org-crypt

Encrypt sensitive headings:

```elisp
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance '("crypt"))
(setq org-crypt-key "your-gpg-key-id")
```

Tag heading with `:crypt:`:

```org
* Passwords                                      :crypt:
  MyBank: password123
  Email: supersecret456
```

Save file → heading encrypts automatically:

```org
* Passwords                                      :crypt:
-----BEGIN PGP MESSAGE-----
[encrypted content]
-----END PGP MESSAGE-----
```

Open file → Org decrypts on demand. Plain text security.

## Attachments

Attach files to headings:

`C-c C-a a` - Attach file

Org creates directory structure:

```
~/org/data/
  └── ab/
      └── 123abc-def4-5678-90ab-cdef12345678/
          ├── document.pdf
          └── image.png
```

Unique ID per heading. Files travel with your org files.

`C-c C-a o` - Open attachment
`C-c C-a f` - Browse attachment directory

Reference attachments in exports. Everything in one ecosystem.

## Advanced Properties and Column View

### Formulas in Column View

Remember column view? Add calculations:

```org
#+COLUMNS: %25ITEM %ESTIMATE{+} %ACTUAL{+} %REMAINING
```

The `{+}` sums values. Parent headings show totals.

### Dynamic Blocks from Properties

Create reports:

```org
#+BEGIN: columnview :hlines 1 :id local :format "%ITEM %EFFORT{+} %CLOCKSUM{+}"
#+END:
```

Generates effort vs. actual time reports. Track estimation accuracy.

## Org-Babel: Advanced Techniques

### Remote Code Execution

Execute code on remote machines:

```org
#+BEGIN_SRC shell :dir /ssh:user@remote:/path/to/dir
ls -la
#+END_SRC
```

Tramp integration. Document remote operations.

### Data Pipeline

Chain multiple languages:

```org
#+BEGIN_SRC shell :results output
curl -s https://api.example.com/data.json
#+END_SRC

#+BEGIN_SRC python :var data=previous-block
import json
parsed = json.loads(data)
return [[item['name'], item['value']] for item in parsed['items']]
#+END_SRC

#+BEGIN_SRC R :var dataset=previous-block
plot(dataset)
#+END_SRC
```

Fetch with shell → Parse with Python → Visualize with R. All in one document.

### Unit Tests in Documentation

```org
* Function: calculate_tax

#+NAME: tax-function
#+BEGIN_SRC python
def calculate_tax(income, rate=0.2):
    return income * rate
#+END_SRC

* Tests

#+BEGIN_SRC python :var func=tax-function
assert calculate_tax(100, 0.2) == 20
assert calculate_tax(0) == 0
assert calculate_tax(50) == 10
print("All tests passed!")
#+END_SRC
```

Tests live with documentation. Update function → re-run tests.

## Org-Roam: Zettelkasten for Org

For serious knowledge management, add `org-roam`:

```elisp
(use-package org-roam
  :custom
  (org-roam-directory "~/org-roam/")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-db-autosync-mode))
```

Features:
- Bidirectional links (see what links to current note)
- Network graph of knowledge
- Daily notes
- Tag-based organization

Roam transforms org files into a personal Wikipedia.

## Org-Agenda + Email Integration

With `mu4e` or `notmuch` email:

```elisp
(setq org-capture-templates
      '(("e" "Email TODO" entry (file "~/org/inbox.org")
         "* TODO Reply to %:from about %:subject\n  %a\n  %i")))
```

Reading email, press capture key → creates TODO with link back to email. Never lose track of email actions.

## Habit Tracking: Advanced

Track multiple habits with different intervals:

```org
* TODO Exercise                                  :health:
  SCHEDULED: <2025-01-15 Wed .+1d>
  :PROPERTIES:
  :STYLE: habit
  :END:

* TODO Weekly Review                             :gtd:
  SCHEDULED: <2025-01-15 Wed ++1w>
  :PROPERTIES:
  :STYLE: habit
  :END:

* TODO Meditation                                :wellness:
  SCHEDULED: <2025-01-15 Wed .+1d>
  :PROPERTIES:
  :STYLE: habit
  :LAST_REPEAT: [2025-01-14 Tue 18:30]
  :END:
```

Agenda shows consistency graphs for each. Visual accountability.

## Mobile Org

Access org files on mobile:

**Options:**
- **Beorg** (iOS) - Native org-mode app
- **Orgzly** (Android) - Full-featured org client
- **Dropbox/Syncthing** - Sync files, edit in plain text apps

Some sync via Dropbox/Git, others via MobileOrg protocol.

Capture on phone → appears in Emacs. Seamless mobile integration.

## Version Control Integration

Org files are plain text. Use git:

```bash
cd ~/org
git init
git add .
git commit -m "Initial commit"
```

Now:
- Track changes
- Revert mistakes
- Sync across machines
- Collaborate with others

Org + Git = distributed, versioned knowledge base.

## Archiving Strategies

Advanced archiving:

```elisp
(setq org-archive-location "archive/%s_archive::")
```

Creates `archive/` directory with per-file archives.

Or archive to date trees:

```elisp
(setq org-archive-location "~/org/archive.org::datetree/")
```

All archived items in one file, organized by date.

## Org-Protocol: Browser Integration

Capture from browser:

1. Set up org-protocol handler
2. Create bookmarklet
3. Click on any web page → Emacs capture opens

Instant web-to-org pipeline.

Example bookmarklet:

```javascript
javascript:location.href='org-protocol://capture?template=w&url='+encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title)+'&body='+encodeURIComponent(window.getSelection())
```

Read article → click → captured with link and selected text.

## Custom Link Types: Real Examples

**Jira integration:**

```elisp
(org-link-set-parameters "jira"
  :follow (lambda (issue)
            (browse-url (concat "https://jira.mycompany.com/browse/" issue))))
```

Use: `[[jira:PROJ-123][Bug fix]]`

**Local file with line number:**

```elisp
(org-link-set-parameters "line"
  :follow (lambda (path)
            (let ((parts (split-string path ":")))
              (find-file (car parts))
              (goto-line (string-to-number (cadr parts))))))
```

Use: `[[line:/path/to/file.py:42][Implementation]]`

**Shell command links:**

```elisp
(org-link-set-parameters "cmd"
  :follow (lambda (command)
            (async-shell-command command)))
```

Use: `[[cmd:docker ps][Check containers]]`

Infinitely extensible.

## Effort and Clocksum Reporting

Generate effort reports:

```org
#+BEGIN: columnview :hlines 2 :id local :format "%50ITEM(Task) %10EFFORT(Estimated){:} %10CLOCKSUM(Actual){:}"
#+END:
```

Compare estimated vs. actual across projects. Improve estimates over time.

## Org-Edna: Task Dependencies

Complex task dependencies:

```elisp
(use-package org-edna
  :config
  (org-edna-mode))
```

Define:

```org
* TODO Task A
  :PROPERTIES:
  :TRIGGER: Task B TODO
  :END:

* NEXT Task B
  :PROPERTIES:
  :BLOCKER: Task A
  :END:
```

Complete Task A → Task B auto-changes to TODO. Task B can't complete until Task A done.

Advanced workflows in plain text.

## Presentations with Org-Tree-Slide

Present directly from org:

```elisp
(use-package org-tree-slide)
```

`M-x org-tree-slide-mode`

Each top-level heading becomes a slide. Navigate with arrow keys. Simple presentations without leaving Emacs.

## Org-Drill: Spaced Repetition

Flashcards in org:

```org
* Question                                       :drill:
  What is the capital of France?

** Answer
   Paris
```

`M-x org-drill` - Start drill session

Spaced repetition learning from plain text.

## Custom TODO State Triggers

Auto-actions on state changes:

```elisp
(setq org-todo-state-tags-triggers
      '(("CANCELLED" ("CANCELLED" . t))
        ("WAITING" ("WAITING" . t))
        (done ("WAITING"))
        ("TODO" ("WAITING") ("CANCELLED"))
        ("DONE" ("WAITING") ("CANCELLED"))))
```

Change to WAITING → auto-adds :WAITING: tag. Change to DONE → removes :WAITING:.

## Bibliography Management

With `org-ref`:

```elisp
(use-package org-ref)
```

Cite papers:

```org
* Introduction
  Recent studies show cite:knuth1984literate that literate programming...

* Bibliography
  bibliography:~/papers/references.bib
```

Export to LaTeX → citations formatted properly.

## The Org Ecosystem: Notable Packages

- **org-roam** - Zettelkasten knowledge base
- **org-super-agenda** - Enhanced agenda grouping
- **org-brain** - Concept mapping
- **org-noter** - Annotate PDFs within org
- **org-transclusion** - Embed content from other files
- **org-download** - Drag-and-drop images
- **org-pomodoro** - Pomodoro timer with org integration
- **org-journal** - Journaling system
- **org-chef** - Recipe manager
- **org-timeline** - Visual timeline of tasks

The ecosystem is vast.

## Configuration: Going Further

A mature config might include:

```elisp
;; Custom agenda views for different contexts
;; Project-specific capture templates
;; Custom link types for your tools
;; Export templates for different document types
;; Keyboard shortcuts for common operations
;; Integration with email, calendar, chat
;; Automated archiving and cleanup
;; Custom functions for your workflow
```

Org-Mode grows with you. Start simple, add complexity as needed.

## The Org Manual

We've covered a lot. The official manual covers more:

- Advanced exporting
- Special blocks
- Macro expansion
- Publishing customization
- Internal linking
- Complex property searches
- And hundreds of configuration options

Access in Emacs: `C-h i m org`

Or online: https://orgmode.org/manual/

## When Org-Mode Isn't the Answer

Org-Mode is powerful but not universal:

**Not ideal for:**
- Real-time collaboration (conflicts in plain text)
- Heavy multimedia (images, video editing)
- WYSIWYG requirements
- Non-technical team members
- Mobile-first workflows

**Excels at:**
- Personal knowledge management
- Technical writing
- Project planning
- Research and notes
- Reproducible research
- Text-oriented workflows

Use the right tool for the job.

## Your Journey Continues

You've learned:
- Document structure
- Task management
- Time tracking
- Capture workflows
- Literate programming
- Publishing systems
- Advanced features

But Org-Mode is infinite. People use it for:
- Running businesses
- Writing books (like this one)
- Managing PhDs
- Tracking fitness
- Planning meals
- Managing finances
- Running D&D campaigns
- And everything in between

## Final Exercise

Build your system:

1. Decide on core workflows (GTD? Custom? Hybrid?)
2. Set up agenda files structure
3. Create capture templates for common inputs
4. Configure custom agenda views
5. Set up time tracking if needed
6. Choose tags and properties schema
7. Configure export for your needs
8. Iterate and refine

Start small. Add features as you need them. Your Org-Mode system should grow organically.

## The Philosophy: Embrace the Journey

Org-Mode has a learning curve. That's not a bug—it's a feature. Tools that do everything for everyone do nothing particularly well.

Org-Mode provides primitives. You compose them. The result is a system that matches your brain, not someone else's productivity philosophy.

There's always more to learn. That's exciting, not overwhelming. Each new feature is a tool you can choose to adopt or ignore.

## Parting Thoughts

You started this book as an Org-Mode beginner. You've learned the syntax, the workflows, the power. But the real learning happens in practice.

Your first week: things will be slow. Keybindings feel awkward. You'll reach for the mouse.

Your first month: muscle memory forms. Capture becomes natural. Agenda views make sense.

Your first year: Org-Mode becomes an extension of thought. You'll wonder how you managed before.

Welcome to the community of people who manage their lives in plain text. We're a peculiar bunch, but we're productive.

Now close this book. Open Emacs. Create an org file.

Your journey begins.

---

## Appendix: Quick Reference

**Essential Keybindings:**

- `TAB` - Fold/unfold heading
- `S-TAB` - Fold/unfold all
- `M-RET` - New heading/item
- `M-left/right` - Promote/demote
- `M-up/down` - Move heading
- `C-c C-t` - Cycle TODO state
- `C-c C-s` - Schedule
- `C-c C-d` - Deadline
- `C-c C-c` - Context-aware command
- `C-c C-e` - Export dispatcher
- `C-c a` - Agenda dispatcher
- `C-c c` - Capture (if configured)
- `C-c C-x C-i` - Clock in
- `C-c C-x C-o` - Clock out
- `C-c C-l` - Insert link
- `C-c C-o` - Open link
- `C-c '` - Edit code block

**Resources:**

- Official Manual: https://orgmode.org/manual/
- Org-Mode Website: https://orgmode.org
- Community: r/orgmode on Reddit
- Mailing List: emacs-orgmode@gnu.org
- Worg: Community-driven wiki at https://orgmode.org/worg/

**The End... and the Beginning**
