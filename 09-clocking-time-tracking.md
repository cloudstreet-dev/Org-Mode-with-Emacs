# Chapter 9: Clocking and Time Tracking - Where Your Hours Go

Ever wonder where your day went? Org-Mode's time tracking answers that question with frightening precision. Clock in, clock out, generate reports—all in plain text that will outlive any time tracking SaaS.

Warning: You might discover you spend way more time on email than you thought. The truth can be uncomfortable.

## Basic Clocking

Position cursor on a heading and:

- `C-c C-x C-i` - Clock in (start timer)
- `C-c C-x C-o` - Clock out (stop timer)

Org adds a CLOCK entry:

```org
* TODO Write documentation
  :LOGBOOK:
  CLOCK: [2025-01-15 Wed 09:15]--[2025-01-15 Wed 11:30] =>  2:15
  :END:
```

That's it. You just tracked 2 hours and 15 minutes on "Write documentation."

### The Running Clock

When clocked in, the mode line shows:

```
[Write documentation 1:23]
```

A live timer counting up. Gentle reminder that the meter's running.

### Clocking from Anywhere

You don't need to be in the org file to clock:

- From agenda: `I` to clock in, `O` to clock out
- From anywhere: `C-c C-x C-j` jumps to currently clocked task

Clock in from your agenda in the morning, work all day, clock out whenever. The entry tracks your time.

## Clock History and Jumping

Org maintains a clock history. Recently clocked items are accessible:

`C-c C-x C-i` (clock in) shows recent items:

```
1. Write documentation
2. Code review
3. Team meeting
4. Email processing
```

Type the number to clock into that task instantly. No navigating—instant time tracking.

## The Clock Table

Generate time reports:

```org
#+BEGIN: clocktable :scope file :maxlevel 2 :block thisweek
#+END:
```

Put cursor on `#+BEGIN:` line, press `C-c C-c`:

```org
#+BEGIN: clocktable :scope file :maxlevel 2 :block thisweek
| Headline               |   Time |      |
|------------------------+--------+------|
| *Total time*           | *8:30* |      |
|------------------------+--------+------|
| Write documentation    |   4:15 |      |
| Code reviews           |   2:30 |      |
| Meetings               |   1:45 |      |
#+END:
```

Boom. Time report. Update anytime with `C-c C-c`.

### Clock Table Options

**Scope:**
- `:scope file` - Current file only
- `:scope agenda` - All agenda files
- `:scope subtree` - Current subtree only
- `:scope nil` - Current subtree
- `:scope ("file1.org" "file2.org")` - Specific files

**Time range:**
- `:block today` - Today only
- `:block thisweek` - This week
- `:block thismonth` - This month
- `:block 2025-01` - January 2025
- `:tstart "<2025-01-01>" :tend "<2025-01-31>"` - Custom range

**Detail level:**
- `:maxlevel 2` - Show subtrees up to level 2
- `:maxlevel 4` - Show more detail

**Formatting:**
- `:formula %` - Add percentage column
- `:link t` - Make headlines clickable links
- `:hidefiles t` - Don't show file names in multi-file reports

### Practical Clock Table Examples

**Weekly time report:**

```org
#+BEGIN: clocktable :scope agenda :maxlevel 3 :block thisweek :link t
#+END:
```

**Monthly client billing:**

```org
#+BEGIN: clocktable :scope agenda :maxlevel 2 :block thismonth :tags "+CLIENT=\"ACME\"" :formula %
#+END:
```

**Daily summary:**

```org
#+BEGIN: clocktable :scope file :maxlevel 2 :block today
#+END:
```

Update any table with `C-c C-c`. Your time logs become invoices, reports, analytics.

## Effort Estimates vs. Actual Time

Remember effort estimates from Chapter 6?

```org
* TODO Implement feature
  :PROPERTIES:
  :EFFORT: 4:00
  :END:
  :LOGBOOK:
  CLOCK: [2025-01-15 Wed 09:00]--[2025-01-15 Wed 13:30] =>  4:30
  CLOCK: [2025-01-14 Tue 14:00]--[2025-01-14 Tue 16:00] =>  2:00
  :END:
```

Estimated: 4 hours. Actual: 6.5 hours. Oops.

Track this systematically and your estimates improve. Data beats guesswork.

### Effort Sum

In column view, see total effort vs. actual time:

```
ITEM              | EFFORT | CLOCKSUM
---------------------------------------------
Project tasks     | 20:00  | 24:30
```

You estimated 20 hours. You've spent 24.5. Time to have a conversation with stakeholders.

## Clocking Idle Time

Stepped away and forgot to clock out? Org catches this:

```elisp
(setq org-clock-idle-time 15)  ; Idle timeout in minutes
```

If Emacs detects 15 minutes of inactivity, it asks:

```
You have been idle for 15 minutes. Clock out from <task>?
- Keep the time (K)
- Subtract idle time (S)
- Cancel (C)
```

Never lose time to forgotten clock-outs again.

## Clock Resolution

Multiple clocks running? (Shouldn't happen, but life gets chaotic.)

`M-x org-resolve-clocks` finds overlapping or unclosed clocks and helps fix them.

## Clocking Out Automatically

Clock out when marking DONE:

```elisp
(setq org-clock-out-when-done t)
```

Mark task complete, clock automatically stops. One less thing to remember.

## Clock Persistence

Emacs crashed? Computer restarted? Don't lose your running clock:

```elisp
(setq org-clock-persist t)
(org-clock-persistence-insinuate)
```

Org saves clock state. Restart Emacs, resume where you left off.

## Modeline Display

Customize the running clock display:

```elisp
(setq org-clock-clocked-in-display 'mode-line)  ; Show in modeline
(setq org-clock-string-limit 20)  ; Truncate long task names
```

Or show in frame title:

```elisp
(setq org-clock-clocked-in-display 'frame-title)
```

Constant, gentle reminder of what you're supposed to be doing.

## Clock Drawers

By default, clock entries go in `:LOGBOOK:` drawer:

```org
* TODO Task
  :LOGBOOK:
  CLOCK: [2025-01-15 Wed 09:00]--[2025-01-15 Wed 10:30] =>  1:30
  CLOCK: [2025-01-14 Tue 14:00]--[2025-01-14 Tue 15:00] =>  1:00
  :END:
```

Configure drawer name:

```elisp
(setq org-log-into-drawer t)  ; Use LOGBOOK drawer
(setq org-clock-into-drawer "CLOCKING")  ; Custom drawer name
```

Keeps your headings clean. Press TAB on drawer to collapse.

## Detailed Time Reports

Want more than basic clock tables? Combine with properties:

```org
#+BEGIN: clocktable :scope agenda :maxlevel 2 :block thismonth :properties ("CLIENT" "PROJECT")
| Headline      | Time  | CLIENT      | PROJECT          |
|---------------+-------+-------------+------------------|
| Feature work  |  8:30 | ACME Corp   | Website Redesign |
| Bug fixes     |  4:15 | TechStart   | Mobile App       |
| Meetings      |  3:00 | Internal    | N/A              |
#+END:
```

Group time by client, project, or any custom property. Invoice generation in plain text.

## Daily/Weekly Clocking Workflow

A productive workflow:

**Morning:**
1. `C-c a a` - Open agenda
2. Navigate to first task
3. `I` - Clock in

**Throughout day:**
1. Switch tasks: `C-c C-x C-i` (clock into new task, auto-clocks out of current)
2. Or from agenda: navigate to task, press `I`

**End of day:**
1. `C-c C-x C-o` - Clock out
2. Generate daily report:

```org
#+BEGIN: clocktable :scope agenda :block today :maxlevel 3
#+END:
```

**Weekly review:**
```org
#+BEGIN: clocktable :scope agenda :block thisweek :maxlevel 2
#+END:
```

See where your week went. Adjust next week accordingly.

## Integrating with Task States

Log time when states change:

```elisp
(setq org-clock-in-switch-to-state "IN-PROGRESS")
```

Clock in → task automatically switches to IN-PROGRESS. Visual indicator that you're actively working on it.

Clock out when done:

```elisp
(setq org-clock-out-when-done '("DONE" "CANCELLED"))
```

Mark task DONE or CANCELLED → automatic clock out.

## Budget Warnings

Warn when exceeding time budgets:

```org
* TODO Project work
  :PROPERTIES:
  :EFFORT: 8:00
  :END:
```

After clocking 8+ hours, Org can warn you (with appropriate configuration) that you've exceeded estimate.

## Column View for Time Tracking

Enable column view in agenda to see time at a glance:

```elisp
(setq org-columns-default-format "%50ITEM %10CLOCKSUM %16TIMESTAMP_IA %TODO %TAGS")
```

In agenda, press `C-c C-x C-c`:

```
ITEM                  | CLOCKSUM | TODO | TAGS
---------------------------------------------------------
Write documentation   |     4:15 | DONE | work
Code review           |     2:30 | DONE | work:code
Team meeting          |     1:00 | DONE | meeting
```

Spreadsheet view of your time.

## Clocking and Pomodoro

The Pomodoro Technique: work in 25-minute focused blocks. Org-Mode can help:

```elisp
(setq org-clock-sound "~/sounds/bell.wav")  ; Sound when timer ends
```

Use `org-timer`:
1. `C-c C-x ;` - Start countdown timer (enter duration)
2. Work until sound plays
3. Take break
4. Repeat

Or use the `org-pomodoro` package for full Pomodoro integration (auto-breaks, statistics, etc.).

## Exporting Time Reports

Share time reports with non-Org users:

1. Generate clock table
2. `C-c C-e` - Export
3. Choose format (HTML, PDF, CSV)

Your time logs become professional reports.

Or export just the table:

1. Place cursor in clock table
2. `M-x org-table-export`
3. Choose CSV or other format

Import into spreadsheets, billing systems, whatever you need.

## Time Tracking for Billing

Professional time tracking setup:

```org
* Client: ACME Corp
  :PROPERTIES:
  :CLIENT: ACME Corp
  :RATE: 150
  :END:

** TODO Feature implementation
   :LOGBOOK:
   CLOCK: [2025-01-15 Wed 09:00]--[2025-01-15 Wed 13:00] =>  4:00
   :END:

** TODO Bug fixes
   :LOGBOOK:
   CLOCK: [2025-01-15 Wed 14:00]--[2025-01-15 Wed 16:30] =>  2:30
   :END:
```

Generate invoice:

```org
#+BEGIN: clocktable :scope tree :maxlevel 3 :block thismonth :formula %
| Headline                | Time  |    % |
|-------------------------+-------+------|
| *Total time*            | *6:30*|      |
|-------------------------+-------+------|
| Feature implementation  |  4:00 | 61.5 |
| Bug fixes               |  2:30 | 38.5 |
#+TBLFM: ...
```

Add cost column with table formulas (Chapter 4):

```org
| Headline                | Time  | Rate | Cost    |
|-------------------------+-------+------+---------|
| Feature implementation  |  4:00 |  150 |  600.00 |
| Bug fixes               |  2:30 |  150 |  375.00 |
|-------------------------+-------+------+---------|
| Total                   |  6:30 |      |  975.00 |
#+TBLFM: ...
```

Plain text time tracking → billable invoices.

## Your Exercise

1. Clock in and out of at least 5 different tasks
2. Let some time accumulate (work on actual tasks, or just let timers run)
3. Generate a clock table for today
4. Generate a clock table for the week
5. Try clocking from the agenda view
6. Set up clock persistence and idle time detection
7. Create a task hierarchy and see cumulative time in parent headings
8. Export a time report to HTML or CSV

## The Philosophy of Time Tracking

What gets measured gets managed. Time tracking reveals truths:

- Tasks take longer than you think
- Interruptions destroy productivity
- Some "urgent" work isn't actually valuable
- Your energy patterns throughout the day

Org-Mode's time tracking is lightweight enough to actually use. No fancy apps, no cloud services, no vendor lock-in. Just plain text timestamps that aggregate into insights.

Track time for a week. The patterns that emerge will surprise you.

## Next: Capture Templates

You're tracking time, managing tasks, building agendas. But where do new ideas go? How do you capture thoughts without disrupting flow? Chapter 10 introduces Capture Templates—frictionless note-taking that ensures nothing gets lost.
