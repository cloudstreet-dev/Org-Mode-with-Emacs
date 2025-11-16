# Chapter 5: Links and Images - Connecting Your Knowledge

Your notes don't exist in isolation. They reference other notes, websites, files, emails, and that brilliant idea you had three months ago. Org-Mode's linking system transforms isolated documents into an interconnected knowledge graph.

And yes, you can even embed images. In a text editor. Let that sink in.

## The Basic Link Syntax

Links in Org-Mode look like this:

```org
[[link-target][description]]
```

Or without a description:

```org
[[link-target]]
```

The target can be almost anything: URLs, files, headings, custom IDs, emails, code references, or even custom link types you define yourself.

## Web Links

The simplest case:

```org
[[https://orgmode.org][Org-Mode Official Site]]
```

Displays as a clickable link: "Org-Mode Official Site"

Without a description:
```org
[[https://orgmode.org]]
```

Displays the URL itself.

Press `C-c C-o` (Control-c Control-o) on a link to open it. Web links open in your browser. That's it.

### Angle Bracket Links

For quick URL insertion without formatting:

```org
<https://example.com>
```

Automatically becomes a clickable link. Useful for quick notes where you don't need descriptive text.

## File Links

Link to other files:

```org
[[file:~/org/projects.org][My Projects]]
[[file:./meeting-notes.org][Today's Meeting]]
[[file:/path/to/document.pdf]]
```

Relative paths work. Absolute paths work. `~/` expansion works. Press `C-c C-o` on a file link and Org opens it—in Emacs for text files, external programs for PDFs, images, etc.

### Linking to Specific Locations in Files

Here's where it gets powerful:

```org
[[file:projects.org::*Project Alpha][Project Alpha Details]]
```

The `::*Project Alpha` part jumps directly to the heading named "Project Alpha" in that file.

Other location specifiers:
```org
[[file:code.py::42][Line 42 of code.py]]
[[file:document.org::#custom-id][Heading with custom ID]]
[[file:notes.org::/regex search/][Search results]]
```

### Linking to Headings in the Current File

```org
* Project Ideas
** TODO Implement feature X
   See the [[*Research Notes][research section]] for background.

* Research Notes
  Background information here...
```

The `*` prefix links to a heading by name in the current file. Click it, jump instantly. This is how you build internal document structure.

### Using Custom IDs

For stable links that survive heading renames:

```org
* Important Section
  :PROPERTIES:
  :CUSTOM_ID: important-section
  :END:

  Critical information here.

* Other Section
  For details, see [[#important-section][the important section]].
```

Rename "Important Section" to anything—the link still works because it targets the custom ID, not the heading text.

## ID Links: The Professional Approach

For serious knowledge management, use IDs:

```org
* Meeting Notes
  :PROPERTIES:
  :ID: 8A7F6B2C-3D4E-5F6A-9B8C-1D2E3F4A5B6C
  :END:
```

Generate an ID with `M-x org-id-get-create`. Then link to it:

```org
[[id:8A7F6B2C-3D4E-5F6A-9B8C-1D2E3F4A5B6C][Meeting Notes]]
```

IDs are globally unique. Move the heading to another file? The link follows. Rename the heading? Link still works. This is how you build a personal knowledge base that survives reorganization.

Enable ID tracking in your config:

```elisp
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
```

## Creating Links Easily

Don't memorize syntax. Use `C-c C-l`:

1. Press `C-c C-l`
2. Start typing a link type (file, http, id, etc.)
3. Use completion to select
4. Enter the target
5. Enter an optional description

Org writes the link for you.

### Storing and Inserting Links

Even better workflow:

1. Navigate to the target (a heading, file, URL in your browser)
2. Press `C-c l` - Org stores the link
3. Go to where you want the link
4. Press `C-c C-l` - Org offers the stored link
5. Add a description if desired

Store multiple links, then insert them wherever needed. It's like a clipboard for references.

## Link Abbreviations

Tired of typing long URLs? Define abbreviations:

```elisp
(setq org-link-abbrev-alist
      '(("wiki" . "https://en.wikipedia.org/wiki/")
        ("rfc"  . "https://www.rfc-editor.org/rfc/rfc%s.txt")
        ("gh"   . "https://github.com/%s")))
```

Then use:
```org
[[wiki:Org-mode][Org-Mode Wikipedia]]
[[rfc:2616][HTTP/1.1 Specification]]
[[gh:org-mode/org-mode][Org-Mode GitHub]]
```

Much cleaner.

## Images: Yes, Really

Org-Mode can display images inline:

```org
[[file:~/Pictures/diagram.png]]
```

With a description:
```org
[[file:./screenshot.png][Project Screenshot]]
```

To actually see images in your Emacs buffer:
- `C-c C-x C-v` - Toggle inline images

Suddenly your text editor shows images. Welcome to the future of 1990s technology.

### Image Attributes

Control image display:

```org
#+ATTR_HTML: :width 300px
#+ATTR_LATEX: :width 0.5\textwidth
[[file:diagram.png]]
```

Different attributes for different export formats. The image adapts.

### Image Links

Make an image clickable:

```org
[[https://example.com][file:thumbnail.png]]
```

The image becomes a link. Click it, go to the URL.

## Email Links

If you use Emacs for email (with mu4e, notmuch, or similar):

```org
[[mailto:alice@example.com][Email Alice]]
```

Or link to specific emails (with appropriate email client):
```org
[[mu4e:msgid:abc123@example.com][Project Discussion Thread]]
```

Your email archive becomes part of your knowledge base.

## Shell and Elisp Links

Execute commands:

```org
[[shell:ls -la ~][List home directory]]
[[elisp:(message "Hello from Org-Mode")][Run elisp code]]
```

`C-c C-o` on these runs the command. Use with caution—shell links can do anything your user account can do.

Practical use:
```org
[[elisp:(org-agenda nil "a")][Open Agenda]]
[[shell:open ~/Documents][Open Documents Folder]]
```

## Search Links

Link to search results:

```org
[[grep:TODO][Find all TODOs]]
[[occur:IMPORTANT][Find IMPORTANT in buffer]]
```

Not standard, but you can define custom link types that perform searches.

## Custom Link Types

Define your own link types:

```elisp
(org-link-set-parameters "jira"
                         :follow (lambda (path)
                                   (browse-url
                                    (concat "https://jira.mycompany.com/browse/" path)))
                         :export (lambda (path desc backend)
                                   (format "<a href='https://jira.mycompany.com/browse/%s'>%s</a>"
                                           path desc)))
```

Now use:
```org
[[jira:PROJ-1234][Bug Report]]
```

Links become infinitely extensible. Point them at internal databases, APIs, custom tools—whatever you need.

## Footnote Links

Remember footnotes from Chapter 3? They're really just links:

```org
The Org manual[fn:1] contains everything, though it's dense[fn::inline footnote here].

* Footnotes
[fn:1] Available at https://orgmode.org/manual/
```

`C-c C-c` on the footnote reference jumps to the definition. Press again to return.

## Link Appearance

By default, links show their description text. To see the underlying link:

- `C-c C-x C-v` - Toggle link display (show raw links vs. descriptions)

Useful when editing or debugging links.

## Broken Link Detection

Check for broken links:

```elisp
M-x org-lint
```

Reports problems in your document, including broken links, malformed structures, etc.

## Links and Export

When you export to HTML, LaTeX, or other formats (Chapter 12), links convert appropriately:

- Web links become hyperlinks
- File links become relative paths or references
- Images embed in the output
- Custom links export according to their export function

One source, many outputs, links preserved.

## Building a Personal Wiki

With file links, custom IDs, and inline images, you can build a personal wiki:

```
~/wiki/
  ├── index.org          # Main entry point
  ├── projects.org       # Project documentation
  ├── notes.org          # Random notes
  ├── people.org         # People and contacts
  └── images/
      └── diagrams/
```

In `index.org`:
```org
* My Wiki

Welcome to my personal knowledge base.

** Quick Links
- [[file:projects.org][Projects]]
- [[file:notes.org][Notes]]
- [[file:people.org][People]]

** Recent Additions
- [[file:projects.org::*Project Alpha][Project Alpha launched]]
- [[file:notes.org::#idea-123][New product idea]]
```

Cross-reference freely. The links maintain structure even as content grows.

## Backlinks: The Missing Piece

Standard Org-Mode doesn't track backlinks (what links TO this heading), but extensions exist:

- `org-roam` - Full Roam Research-style backlinks and knowledge graph
- `org-brain` - Concept mapping with bidirectional links

If you're building a serious knowledge base, explore these packages. They supercharge Org-Mode's linking.

## Practical Patterns

### Project Documentation

```org
* Project: New Website
  :PROPERTIES:
  :ID: project-website-2025
  :END:

** Links
- [[file:~/code/website/README.md][Codebase README]]
- [[https://github.com/myorg/website][GitHub Repository]]
- [[file:designs/mockup.png][Design Mockup]]
- [[id:meeting-2025-01-15][Kickoff Meeting Notes]]
```

### Research Notes

```org
* Paper: "Effective Note-Taking"
  :PROPERTIES:
  :ID: paper-effective-notes
  :CUSTOM_ID: paper-notes
  :END:

** Summary
[[https://example.com/paper.pdf][Full PDF]]

Key findings discussed in [[#my-analysis][my analysis]].

** My Analysis
   :PROPERTIES:
   :CUSTOM_ID: my-analysis
   :END:

The authors claim...
```

### Tutorial/Documentation

```org
* Setup Instructions

1. Download the [[file:installer.dmg][installer]]
2. Follow the [[file:setup-guide.org::*Installation][installation guide]]
3. Configure using [[file:config-example.txt][this example config]]
4. See [[file:screenshots/dashboard.png][expected result]]
```

## Your Exercise

1. Create at least three org files
2. Add links between them (heading links, file links, custom IDs)
3. Add some web links to external resources
4. Include at least one image and enable inline display
5. Practice `C-c C-o` to follow links and `C-c C-l` to create them
6. Store links with `C-c l` and insert them elsewhere

Build a small interconnected set of documents. Feel the knowledge web forming.

## The Philosophy of Linking

Links transform documents into networks. A single isolated note has limited value. That same note, connected to related ideas, becomes part of a larger understanding.

Org-Mode makes linking frictionless enough to actually do it. Most systems make linking just annoying enough that people don't bother. The accumulated result? Isolated notes that rot in isolation.

Link liberally. Your future self will thank you.

## Next: TODO Items and Workflow

You've built document structure, created tables, and connected ideas. Now let's turn Org-Mode into a task management powerhouse. TODO items await.
