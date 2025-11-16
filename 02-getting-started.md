# Chapter 2: Getting Started - Your First Steps in Org-Mode

The journey of a thousand productive hours begins with a single `.org` file. Let's get you set up.

## Prerequisites: You Need Emacs

This might seem obvious given the book's title, but let's be explicit: Org-Mode runs in Emacs. While there are ports and implementations for other editors, you're reading this book to learn the real thing—Org-Mode as it was meant to be experienced.

### Installing Emacs

**macOS:**
```bash
brew install emacs
```

Or download from https://emacsformacosx.com/

**Linux:**
```bash
# Debian/Ubuntu
sudo apt install emacs

# Fedora
sudo dnf install emacs

# Arch (you probably already have it)
sudo pacman -S emacs
```

**Windows:**
Download from https://www.gnu.org/software/emacs/download.html

### Modern Emacs is Beautiful, Actually

If you last tried Emacs in 1999, prepare to be pleasantly surprised. Modern Emacs can be gorgeous, with themes, proper font rendering, and yes, even icons if you're into that sort of thing.

## Org-Mode: Already Included

Here's the good news: if you're running Emacs 22.1 or later (and you definitely are), Org-Mode is already installed. No package manager wrestling required for basic functionality.

That said, you might want to update to the latest version:

```elisp
;; Add to your ~/.emacs or ~/.emacs.d/init.el
(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
```

Then `M-x package-install RET org RET`

(Translation for the Emacs-uninitiated: Hold Alt, press X, type "package-install", press Enter, type "org", press Enter. You'll see this `M-x` notation a lot. M stands for Meta, which is Alt on modern keyboards.)

## Your First Org File

Let's create your first `.org` file. In Emacs:

1. `C-x C-f` (Control-x, then Control-f) to open a file
2. Type `~/notes.org` and press Enter
3. Emacs will create the file and automatically enable Org-Mode

Congratulations! You're now in Org-Mode. Let's type something:

```org
* Welcome to Org-Mode

This is a top-level heading. The asterisk at the start makes it special.

** This is a subheading

You can nest headings as deep as you need. Each additional asterisk creates
a deeper level.

*** Getting deeper
**** Even deeper
***** We must go deeper
****** Is this deep enough?

Probably. Let's come back up.

** Regular text

Everything that's not a heading is just regular text. You can write paragraphs,
thoughts, notes—whatever you need.

* Another top-level heading

Org files are organized as an outline. This structure is the foundation of
everything else.
```

Type this into your file (or something like it—creativity encouraged).

## Essential Keybindings: The First Five

You could learn hundreds of Org-Mode keybindings, but let's start with the five that matter most:

### 1. `TAB` - Fold/Unfold Headings

Place your cursor on a heading and press TAB. The heading collapses, hiding its content. Press TAB again—it expands to show immediate children. Once more—it shows everything. This is how you navigate large documents without drowning in detail.

This simple feature changes everything. Suddenly your 10,000-line document is manageable because you only see what you need.

### 2. `S-TAB` - Fold/Unfold Everything

(That's Shift-TAB)

Press this to cycle through views of your entire document:
- Everything collapsed (just top-level headings)
- One level expanded
- Everything visible

It's like having x-ray vision for your document structure.

### 3. `M-RET` - New Heading

(Alt-Enter, or Option-Enter on Mac)

With your cursor on a heading, press `M-RET` and Org creates a new heading at the same level. No manually typing asterisks, no worrying about the correct number—Org handles it.

### 4. `M-left/right` - Promote/Demote Headings

Move headings up or down the hierarchy. Made a subheading that should be top-level? `M-left` (Alt-LeftArrow) promotes it. Need to demote something? `M-right`.

### 5. `M-up/down` - Move Headings

Reorganizing your outline? `M-up` and `M-down` move entire heading trees up or down, preserving their structure.

## Try It Now

Go to your `notes.org` file and:

1. Create a heading called "My Projects"
2. Create three subheadings under it (use `M-RET`)
3. Add some text under each subheading
4. Practice folding and unfolding with TAB
5. Rearrange the subheadings with `M-up` and `M-down`
6. Promote one subheading to be a sibling of "My Projects" instead

Feel that? That's the feeling of direct manipulation of information structure. It's addictive.

## A Minimal Config to Get Started

While Org-Mode works out of the box, a few settings will improve your experience. Add this to your Emacs config file (`~/.emacs.d/init.el` or `~/.emacs`):

```elisp
;; Org-Mode configuration
(require 'org)

;; Set the directory where you'll keep org files
(setq org-directory "~/org")

;; Enable visual line mode for easier editing
(add-hook 'org-mode-hook 'visual-line-mode)

;; Make org files look prettier
(setq org-hide-leading-stars t
      org-startup-indented t)

;; Set syntax highlighting in code blocks
(setq org-src-fontify-natively t)
```

Don't panic if elisp is unfamiliar. You're not expected to become an Emacs Lisp programmer (though you might accidentally become one—it happens).

## Understanding Org File Structure

An Org file is fundamentally an outline with superpowers:

```org
* Level 1
** Level 2
*** Level 3
**** Level 4

Back to level 1
** Another Level 2
```

That's it. That's the core structure. Everything else in Org-Mode—todos, timestamps, tags, properties, tables, code blocks—gets added to this outline foundation.

The asterisks must be at the start of the line, followed by a space, followed by the heading text. Org-Mode is generally forgiving, but this rule is sacred.

## Working with Multiple Files

You don't need to put everything in one file (though some people do, and more power to them). A typical setup might look like:

```
~/org/
  ├── inbox.org          # Quick captures
  ├── projects.org       # Active projects
  ├── someday.org        # Ideas for later
  ├── reference.org      # Reference material
  └── archive.org        # Completed stuff
```

Org-Mode makes it easy to link between files, search across them, and build agenda views that aggregate from multiple sources. We'll cover all of that later.

## Your Assignment

Before moving to the next chapter:

1. Create an `~/org` directory
2. Create at least one `.org` file
3. Write an outline of something—maybe your current projects, or plans for world domination, or a recipe collection
4. Practice the five essential keybindings until they feel natural
5. Fold and unfold your document until the structure feels intuitive

## The Path Ahead

You now know enough to use Org-Mode as a basic outliner. But that's like knowing how to make a campfire when you're sitting on an oil field. In the next chapters, we'll ignite the real power: TODO items, timestamps, agenda views, and the features that transform Org-Mode from "nice outliner" to "life management system."

Ready? Let's continue.
