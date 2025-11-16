# Chapter 7: Tags and Properties - The Metadata Layer

You've organized your tasks. Now let's make them searchable, filterable, and queryable. Tags and properties are Org-Mode's metadata system—the layer that transforms static lists into a dynamic, searchable knowledge base.

## Tags: Categorization with Colons

Tags attach labels to headings:

```org
* TODO Fix authentication bug                    :urgent:bug:security:
* Meeting notes from client call                 :meeting:client:acme:
* Ideas for next sprint                          :planning:ideas:
* DONE Implement user dashboard                  :feature:frontend:done:
```

Tags appear at the end of the heading line, wrapped in colons. One tag looks like `:tag:`, multiple tags like `:tag1:tag2:tag3:`.

### Adding Tags

Don't type them manually. Press `C-c C-c` on a heading:

1. Org prompts: `Tags:`
2. Type tag names (with completion if configured)
3. Press Enter

Or use `C-c C-q` for the same effect (mnemonic: "q" for taQ).

Tags are case-sensitive. `:Work:` and `:work:` are different. Choose a convention and stick with it (lowercase recommended).

### Tag Inheritance

By default, tags inherit. A child heading has all its parent's tags:

```org
* Project Alpha                                  :work:client_a:
** TODO Design phase                             :design:
** TODO Development phase                        :development:
```

"Design phase" effectively has tags `:work:client_a:design:`. "Development phase" has `:work:client_a:development:`. You don't repeat common tags—inheritance handles it.

Disable inheritance if needed:

```elisp
(setq org-use-tag-inheritance nil)
```

Or disable for specific tags:

```elisp
(setq org-tags-exclude-from-inheritance '("project" "personal"))
```

### Predefined Tags

Typing tags gets tedious. Predefine common ones:

```elisp
(setq org-tag-alist '(("work" . ?w)
                      ("personal" . ?p)
                      ("urgent" . ?u)
                      ("email" . ?e)
                      ("phone" . ?h)
                      ("meeting" . ?m)))
```

Now when you press `C-c C-c` to add tags, Org shows a selection interface. Press `w` for "work", `u` for "urgent", etc. Much faster than typing.

Group related tags:

```elisp
(setq org-tag-alist '((:startgroup . nil)
                      ("@office" . ?o)
                      ("@home" . ?h)
                      ("@errands" . ?e)
                      (:endgroup . nil)

                      (:startgroup . nil)
                      ("urgent" . ?u)
                      ("someday" . ?s)
                      (:endgroup . nil)))
```

Groups are mutually exclusive. Adding `@office` removes `@home` and `@errands`. Perfect for context tags (GTD-style).

### Per-File Tags

Set tags available in specific files:

```org
#+TAGS: work(w) personal(p) urgent(u)
#+TAGS: { @office(o) @home(h) @errands(e) }
```

The `{}` creates a mutually exclusive group (same as `:startgroup:` in elisp).

### File-Level Tags

Apply tags to the entire file:

```org
#+FILETAGS: :project_alpha:confidential:
```

Every heading in the file inherits these tags. Useful for project-specific files.

## Searching by Tags

The power of tags emerges when searching.

### Tag Sparse Trees

`C-c / m` - Show headings matching tag query

Examples:
- `work` - All headings tagged "work"
- `work+urgent` - Both "work" AND "urgent"
- `work|personal` - Either "work" OR "personal"
- `work-boss` - Tagged "work" but NOT "boss"
- `work+urgent-meeting` - Work AND urgent, but NOT meeting

The document folds to show only matches. Combined with tag inheritance, this is phenomenally powerful.

### Tag Agenda Searches

From the agenda (covered in Chapter 8):

`C-c a m` - Match tags across all agenda files

Your entire Org system becomes searchable by arbitrary tag combinations.

## Properties: Structured Metadata

Tags are simple labels. Properties are key-value pairs for structured data:

```org
* TODO Implement new feature
  :PROPERTIES:
  :EFFORT: 4:00
  :CLIENT: ACME Corp
  :PROJECT: Website Redesign
  :PRIORITY: High
  :ASSIGNED: Alice
  :DUE_DATE: <2025-01-31 Wed>
  :END:
```

Properties live in a `:PROPERTIES:` drawer. Each property is a key-value pair.

### Adding Properties

`C-c C-x p` - Set a property

Org prompts for property name, then value. The property drawer auto-creates if needed.

### Special Properties

Some properties have special meaning:

- `EFFORT` - Time estimate (used in agenda views)
- `CATEGORY` - Category for agenda display
- `ORDERED` - If t, children must be completed in order
- `CUSTOM_ID` - For stable linking (Chapter 5)
- `ID` - Unique identifier

Others are custom metadata you define.

### Property Inheritance

Like tags, properties can inherit:

```org
* Project: Website Redesign
  :PROPERTIES:
  :CLIENT: ACME Corp
  :PROJECT_CODE: WEB-2025-01
  :END:

** TODO Design mockups
** TODO Implement frontend
```

If `CLIENT` is set to inherit, all subtasks automatically have `CLIENT: ACME Corp`.

Configure inheritance:

```elisp
(setq org-use-property-inheritance '("CLIENT" "PROJECT_CODE"))
```

Or inherit all properties (be careful—this can slow down large files):

```elisp
(setq org-use-property-inheritance t)
```

### Allowed Property Values

Constrain property values for consistency:

```org
#+PROPERTY: ASSIGNED_ALL Alice Bob Carol Dave
#+PROPERTY: STATUS_ALL Open In-Progress Blocked Completed
#+PROPERTY: PRIORITY_ALL Low Medium High Critical
```

Now when setting these properties, Org offers completion from the allowed values. No typos, no inconsistent data.

In elisp:

```elisp
(setq org-global-properties
      '(("ASSIGNED_ALL" . "Alice Bob Carol Dave")
        ("STATUS_ALL" . "Open In-Progress Blocked Completed")))
```

### Column View: Spreadsheet of Properties

View properties as a table:

`C-c C-x C-c` - Enable column view

Org displays headings with their properties in columns:

```
ITEM              | ASSIGNED | EFFORT | STATUS
---------------------------------------------------
Implement feature | Alice    | 4:00   | In-Progress
Write tests       | Bob      | 2:00   | Open
Deploy            | Carol    | 1:00   | Blocked
```

Edit properties directly in the column view. Press `q` to exit.

Define which columns to show:

```org
#+COLUMNS: %25ITEM %ASSIGNED %EFFORT %STATUS
```

Or globally:

```elisp
(setq org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS %EFFORT")
```

Column view transforms Org files into databases with editable spreadsheet views.

### Dynamic Blocks: Property Tables

Create property summaries:

```org
#+BEGIN: columnview :hlines 1 :id local
#+END:
```

Place cursor on `#+BEGIN:` line and press `C-c C-c`. Org generates a table from properties:

```org
#+BEGIN: columnview :hlines 1 :id local
| ITEM              | ASSIGNED | EFFORT | STATUS      |
|-------------------+----------+--------+-------------|
| Implement feature | Alice    | 4:00   | In-Progress |
| Write tests       | Bob      | 2:00   | Open        |
| Deploy            | Carol    | 1:00   | Blocked     |
#+END:
```

Update properties and regenerate with `C-c C-c`. The table auto-updates.

Generate reports this way:
- Task assignment by person
- Effort summaries by project
- Status overviews

Plain text becoming a database before your eyes.

## Property Searches

Search by property values:

`C-c / p` - Property sparse tree

Enter property name and value. Org shows matching headings.

### Advanced Property Searches

Tag/property search syntax (`C-c / m` or `C-c a m`):

```
+work-boss+CLIENT="ACME Corp"+EFFORT>2:00
```

Translation: Tagged "work", not "boss", CLIENT property equals "ACME Corp", and EFFORT greater than 2 hours.

Operators:
- `=` - Equals
- `<>` - Not equal
- `<` - Less than
- `>` - Greater than
- `<=` - Less than or equal
- `>=` - Greater than or equal

For numbers, times, and dates. For strings, use `=`.

Regular expressions:

```
CLIENT={acme|widgets}
```

Matches CLIENT containing "acme" or "widgets".

This is query language built into plain text.

## Combining Tags and Properties

Tags for broad categories, properties for specific metadata:

```org
* TODO Implement payment processing              :feature:backend:urgent:
  SCHEDULED: <2025-01-20 Mon>
  :PROPERTIES:
  :CLIENT: FinTech Startup
  :EFFORT: 8:00
  :ASSIGNED: Alice
  :COMPLEXITY: High
  :SECURITY_REVIEW: Required
  :END:
```

Now you can query:
- "All backend features" (tag search)
- "All tasks for FinTech Startup" (property search)
- "Urgent tasks assigned to Alice requiring more than 4 hours" (combined search)

The metadata layer makes everything discoverable.

## Practical Tag Systems

### GTD-Style Contexts

```elisp
(setq org-tag-alist '((:startgroup)
                      ("@office" . ?o)
                      ("@home" . ?h)
                      ("@computer" . ?c)
                      ("@phone" . ?p)
                      ("@errands" . ?e)
                      (:endgroup)

                      ("work" . ?w)
                      ("personal" . ?r)))
```

Tag tasks by where they can be done. Filter your task list by current context.

### Project-Based

```org
#+FILETAGS: :project_alpha:
#+PROPERTY: CLIENT ACME Corp
#+PROPERTY: PROJECT_CODE ALPHA-2025

* Tasks
** TODO Design
   :PROPERTIES:
   :PHASE: Planning
   :END:
```

All tasks inherit project context.

### Energy Levels

```elisp
(setq org-tag-alist '((:startgroup)
                      ("@high_energy" . ?h)
                      ("@low_energy" . ?l)
                      (:endgroup)))
```

Tag tasks by required energy. When you're tired, filter for low-energy tasks.

### Work Breakdown Structure

```org
* Project: Website Redesign
  :PROPERTIES:
  :PROJECT_ID: WEB-2025-01
  :BUDGET: 50000
  :END:

** Phase 1: Discovery                            :phase:discovery:
   :PROPERTIES:
   :BUDGET_ALLOCATED: 5000
   :END:

** Phase 2: Design                               :phase:design:
   :PROPERTIES:
   :BUDGET_ALLOCATED: 15000
   :END:

** Phase 3: Development                          :phase:development:
   :PROPERTIES:
   :BUDGET_ALLOCATED: 25000
   :END:
```

Track budgets, phases, and allocation through properties. Report with column view.

## Property Clocking Summaries

Combined with time tracking (Chapter 9), properties enable powerful reports:

```org
#+BEGIN: clocktable :scope file :maxlevel 2 :properties ("CLIENT" "PROJECT")
#+END:
```

Generate time reports grouped by client or project. Send invoices from plain text files. It's absurd and brilliant.

## Bulk Property Operations

Need to tag 20 items at once?

1. In agenda view (Chapter 8), mark multiple items
2. `B` for bulk action
3. `s` to add tags or properties
4. Enter tags/properties
5. All marked items update

Metadata at scale.

## Your Exercise

1. Create a file with at least 10 tasks
2. Define a set of common tags (work, personal, contexts, etc.)
3. Tag all your tasks appropriately
4. Add properties to at least 5 tasks (effort, assignment, client, etc.)
5. Practice tag searches with `C-c / m` (try AND, OR, NOT combinations)
6. Try property searches with `C-c / p`
7. Enable column view and edit properties in the spreadsheet interface
8. Create a dynamic property table with columnview

## The Philosophy of Metadata

Good metadata is the difference between a pile of notes and a knowledge system. Tags and properties transform your Org files from documents into databases—queryable, filterable, reportable.

The discipline is adding metadata consistently. The reward is finding exactly what you need, when you need it, across thousands of entries.

Start simple. Add a few tags. As your system grows, add more structure. The beauty of plain text: you can always add metadata retroactively.

## Next: Agenda Views

You've got TODOs, tags, properties, and timestamps. Now let's pull it all together. The Agenda is where Org-Mode's true power manifests—a dynamic, customizable view of your entire task universe. Prepare to achieve inbox zero (well, maybe).
