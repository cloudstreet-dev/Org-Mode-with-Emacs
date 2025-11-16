# Chapter 10: Capture Templates - Frictionless Note-Taking

Brilliant ideas strike at inconvenient times. You're mid-conversation, mid-code, mid-commute. You need to capture the thought NOW—before it evaporates—without derailing your current focus.

Enter Org-Mode's capture system: instant note-taking from anywhere, filed automatically to the right place, with zero friction.

## The Basic Capture Workflow

Bind a global capture key (add to your config):

```elisp
(global-set-key (kbd "C-c c") 'org-capture)
```

Now from anywhere in Emacs:

1. `C-c c` - Opens capture menu
2. Choose template (we'll define these)
3. Type note
4. `C-c C-c` - Save and return to what you were doing

The note files automatically to your inbox. You barely broke focus.

## Your First Capture Template

Add to config:

```elisp
(setq org-capture-templates
      '(("t" "TODO" entry (file+headline "~/org/inbox.org" "Tasks")
         "* TODO %?\n  %i\n  %a")))
```

Let's decode:

- `"t"` - The key to trigger this template
- `"TODO"` - Description shown in capture menu
- `entry` - Create a new heading
- `(file+headline "~/org/inbox.org" "Tasks")` - File location and heading
- `"* TODO %?\n  %i\n  %a"` - The template text

The `%` symbols are placeholders:

- `%?` - Cursor position after template expands
- `%i` - Initial region content (if you selected text before capturing)
- `%a` - Link to where you were when you captured

Press `C-c c t`, type your task, press `C-c C-c`. Boom. Captured.

## Template Placeholders: The Full Arsenal

### Position and Content

- `%?` - Cursor position
- `%i` - Initial content (selected region)
- `%a` - Link to current location
- `%A` - Like `%a`, but only if called from agenda

### Timestamps

- `%t` - Timestamp (date only)
- `%T` - Timestamp (date and time)
- `%u` - Inactive timestamp (date only)
- `%U` - Inactive timestamp (date and time)
- `%^t` - Prompt for date
- `%^T` - Prompt for date and time

### Context

- `%f` - Current file name
- `%F` - Current file name (full path)
- `%n` - Your user name
- `%c` - Kill ring head (clipboard content)
- `%x` - X clipboard content

### Prompts

- `%^{Prompt}` - Prompt for text
- `%^{Prompt|default|choice1|choice2}` - Prompt with completion
- `%^g` - Prompt for tags
- `%^G` - Prompt for tags (with allowed tags completion)

### Special

- `%l` - Link to current location (like `%a` but no description)
- `%(elisp expression)` - Evaluate elisp, insert result

## Practical Capture Templates

### Quick Task

```elisp
("t" "Task" entry (file+headline "~/org/inbox.org" "Tasks")
 "* TODO %?\n  %U\n  %a")
```

Creates:
```org
* TODO Fix the bug I just noticed
  [2025-01-15 Wed 14:23]
  [[file:code.py::42][code.py:42]]
```

### Meeting Notes

```elisp
("m" "Meeting" entry (file+datetree "~/org/meetings.org")
 "* %^{Meeting Title}\n  %U\n  Attendees: %^{Attendees}\n\n  %?")
```

Prompts for title and attendees, files by date:

```org
* 2025
** 2025-01 January
*** 2025-01-15 Wednesday
**** Client kickoff meeting
     [2025-01-15 Wed 14:23]
     Attendees: Alice, Bob, Carol

     [Your notes here]
```

### Journal Entry

```elisp
("j" "Journal" entry (file+datetree "~/org/journal.org")
 "* %U\n  %?")
```

Simple dated entry. Daily journaling made effortless.

### Idea Capture

```elisp
("i" "Idea" entry (file "~/org/ideas.org")
 "* %^{Idea Title}\n  %U\n  %?")
```

Prompts for title, adds timestamp, positions cursor for details.

### Link Repository

```elisp
("l" "Link" entry (file+headline "~/org/links.org" "Links")
 "* %a\n  %U\n  %?")
```

Capturing a web article? Link auto-captured with timestamp and space for notes.

### Code TODO

```elisp
("c" "Code TODO" entry (file+headline "~/org/code-todos.org" "Code Tasks")
 "* TODO %?\n  %a\n  %i")
```

Browsing code, spot something that needs work? Capture it with link to exact location and selected code snippet.

### Email Follow-up

```elisp
("e" "Email" entry (file+headline "~/org/inbox.org" "Emails")
 "* TODO Reply to %:from about %:subject\n  %a\n  %U")
```

When using Emacs for email, `%:from` and `%:subject` pull email metadata. Instant email TODO with link back to message.

## Advanced Target Locations

### File+Headline

```elisp
(file+headline "~/org/projects.org" "Project Alpha")
```

Captures under specific heading.

### File+Olp (Outline Path)

```elisp
(file+olp "~/org/projects.org" "Work" "Project Alpha" "Tasks")
```

Captures under `Work > Project Alpha > Tasks` hierarchy.

### File+Datetree

```elisp
(file+datetree "~/org/journal.org")
```

Files by date hierarchy (year > month > day).

### File+Function

```elisp
(file+function "~/org/inbox.org" my-custom-function)
```

Custom function determines location. Ultimate flexibility.

### Clock

```elisp
(clock)
```

Captures under currently clocked task. Perfect for adding notes while working.

### Current File

```elisp
(file+headline buffer-file-name "Notes")
```

Captures in current file. Useful for project-specific captures.

## Template Types

### Entry (Heading)

```elisp
entry (file "~/org/inbox.org")
"* TODO %?"
```

Creates a new heading.

### Item (List Item)

```elisp
item (file+headline "~/org/shopping.org" "Groceries")
"- %?"
```

Creates a list item:
```org
* Groceries
  - Milk
  - Eggs
  - [Your new item]
```

### Checkitem (Checkbox)

```elisp
checkitem (file+headline "~/org/packing.org" "Packing List")
"- [ ] %?"
```

Creates checkbox item.

### Table-line

```elisp
table-line (file+headline "~/org/habits.org" "Exercise Log")
"| %U | %^{Exercise} | %^{Duration} |"
```

Adds row to table:

```org
* Exercise Log
  | Date              | Exercise | Duration |
  |-------------------+----------+----------|
  | [2025-01-15 Wed]  | Running  | 30 min   |
  | [Your new entry]  |          |          |
```

### Plain Text

```elisp
plain (file+headline "~/org/notes.org" "Quick Notes")
"%?"
```

Inserts plain text (no heading, no list marker).

## Template Options

Customize template behavior:

```elisp
("t" "Task" entry (file+headline "~/org/inbox.org" "Tasks")
 "* TODO %?\n  %U"
 :empty-lines 1        ; Add blank line after entry
 :clock-in t           ; Start clock when capturing
 :clock-resume t       ; Resume previous clock when done
 :immediate-finish t   ; Don't prompt for editing
 :jump-to-captured t   ; Jump to captured entry when done
 :kill-buffer t        ; Kill capture buffer when done
 :prepend t            ; Insert at beginning of target
 :unnarrowed t)        ; Show full file, not just target
```

Mix and match options for each template.

## Multi-Template Menus

Group related templates:

```elisp
(setq org-capture-templates
      '(("w" "Work")
        ("wt" "Work Task" entry (file+headline "~/org/work.org" "Tasks")
         "* TODO %?")
        ("wm" "Work Meeting" entry (file+datetree "~/org/work.org")
         "* %^{Meeting} %U\n  %?")

        ("p" "Personal")
        ("pt" "Personal Task" entry (file+headline "~/org/personal.org" "Tasks")
         "* TODO %?")
        ("pj" "Journal" entry (file+datetree "~/org/journal.org")
         "* %U\n  %?")))
```

`C-c c w` shows work options, `C-c c p` shows personal options.

## Context-Aware Capture

Use elisp to make templates context-aware:

```elisp
("t" "Task" entry
 (file+headline
  (lambda () (if (string-match "work" (or (buffer-file-name) ""))
                 "~/org/work.org"
               "~/org/personal.org"))
  "Tasks")
 "* TODO %?")
```

Capturing from work files? Goes to work.org. From personal files? Goes to personal.org.

## Capture Hooks

Run code before/after capture:

```elisp
(add-hook 'org-capture-before-finalize-hook
          (lambda ()
            (save-excursion
              (org-back-to-heading)
              (org-set-tags ":captured:"))))
```

Auto-tag all captured items.

Or:

```elisp
(add-hook 'org-capture-after-finalize-hook
          (lambda ()
            (shell-command "notify-send 'Org Capture' 'Item captured!'")))
```

Desktop notification on capture.

## Aborting Captures

Started capture but changed your mind?

- `C-c C-k` - Kill capture (nothing saved)
- `C-c C-w` - Refile instead (save but different location)

## Capture from Outside Emacs

Want to capture from your browser, terminal, or anywhere?

Install `org-protocol` and set up browser bookmarklet:

```javascript
javascript:location.href='org-protocol://capture?template=l&url='+encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title)
```

Click bookmarklet → Emacs pops up → capture template → back to browser. Web to Org in seconds.

Or use `emacsclient`:

```bash
#!/bin/bash
emacsclient -n "org-protocol://capture?template=t&body=$1"
```

Terminal to Org.

## Practical Capture Systems

### The GTD Inbox

```elisp
(setq org-capture-templates
      '(("i" "Inbox" entry (file "~/org/inbox.org")
         "* %?\n  %U")))
```

Everything goes to inbox. Process later. Never lose a thought.

### The Cornell Note-taking System

```elisp
("n" "Note" entry (file+datetree "~/org/notes.org")
 "* %^{Topic}\n  %U\n\n** Cues\n   %?\n\n** Notes\n\n** Summary\n")
```

Structured note template.

### The Zettelkasten

```elisp
("z" "Zettel" entry (file "~/org/zettelkasten.org")
 "* %^{Title}\n  :PROPERTIES:\n  :ID: %(org-id-uuid)\n  :END:\n  %U\n\n  %?")
```

Each note gets unique ID for linking.

## Your Exercise

1. Set up basic capture templates (task, note, journal at minimum)
2. Bind `C-c c` globally to `org-capture`
3. Practice capturing from different contexts (agenda, code files, etc.)
4. Create at least one template with prompts (`%^{...}`)
5. Set up a datetree-based template
6. Try capturing with selected text (region)
7. Experiment with different template options (`:clock-in`, `:empty-lines`, etc.)

## The Philosophy of Capture

The best capture system is invisible. You have a thought, press two keys, type briefly, done. Back to work. No context switches, no app switching, no "where should this go?"

Org-Mode's capture templates remove friction from note-taking. Every thought can be preserved without disrupting flow.

The question isn't "should I capture this?" It's "why wouldn't I?"

## Next: Code Blocks and Literate Programming

You've organized notes, tracked time, captured thoughts. Now let's add executable code to your documents. Yes, your plain text notes can run programs. Welcome to literate programming with Org-Mode.
