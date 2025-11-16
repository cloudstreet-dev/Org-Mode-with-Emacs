# Chapter 11: Code Blocks and Literate Programming - Executable Documents

Remember in Chapter 3 when we mentioned code blocks? Time to unlock their full power. Org-Mode code blocks aren't just syntax highlighting—they're executable. Your documentation can run programs, generate output, produce graphs, and manipulate data.

Welcome to literate programming: where code and documentation are one.

## The Basic Code Block

We've seen this syntax:

```org
#+BEGIN_SRC python
def hello():
    print("Hello from Org-Mode!")
#+END_SRC
```

But here's the magic. Put cursor in the block and press `C-c C-c`:

```org
#+RESULTS:
: Hello from Org-Mode!
```

The code executed. The output appeared. In your plain text document.

Let that sink in.

## Enabling Code Execution

First, allow languages you'll use:

```elisp
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (emacs-lisp . t)
   (js . t)
   (ruby . t)
   (sql . t)
   (sqlite . t)
   (dot . t)
   (R . t)))
```

Enable what you need. Org supports dozens of languages.

For security, confirm before execution:

```elisp
(setq org-confirm-babel-evaluate t)  ; Prompt before running code
```

Or disable for trusted files:

```elisp
(setq org-confirm-babel-evaluate nil)  ; Run without prompting (careful!)
```

Or per-language:

```elisp
(setq org-confirm-babel-evaluate
      (lambda (lang body)
        (not (member lang '("python" "emacs-lisp")))))
```

Prompt for most languages, but not Python or Elisp.

## Editing Code Blocks

Press `C-c '` (Control-c single-quote) inside a code block.

A new buffer opens with:
- Full language mode activated (Python mode, JS mode, etc.)
- Syntax highlighting
- Language-specific features (indentation, completion, linting)
- All your normal coding tools

Edit comfortably. Press `C-c '` again—changes transfer back to org file.

This is how you write real code in org documents without losing coding conveniences.

## Code Block Headers

Control execution with header arguments:

```org
#+BEGIN_SRC python :results output :exports both
print("Line 1")
print("Line 2")
print("Line 3")
#+END_SRC
```

Execute with `C-c C-c`:

```org
#+RESULTS:
: Line 1
: Line 2
: Line 3
```

### Essential Header Arguments

**:results** - How to handle output:
- `:results output` - Capture stdout (print statements)
- `:results value` - Return value of last expression
- `:results silent` - Execute but don't show results
- `:results replace` - Replace existing results (default)
- `:results append` - Add to existing results

**:exports** - What to export in documents:
- `:exports code` - Only code
- `:exports results` - Only results
- `:exports both` - Code and results
- `:exports none` - Neither (execute but hide)

**:session** - Persistent environment:
- `:session` - Use default session
- `:session mysession` - Named session
- No session - Fresh environment each time

**:var** - Pass variables:
- `:var x=5` - Variable from value
- `:var data=table-name` - Variable from org table
- `:var x=other-block` - Variable from another block's output

More headers coming...

## Session-Based Execution

Without sessions, each block runs in isolation:

```org
#+BEGIN_SRC python
x = 42
#+END_SRC

#+BEGIN_SRC python
print(x)  # Error: x not defined
#+END_SRC
```

With sessions, state persists:

```org
#+BEGIN_SRC python :session
x = 42
#+END_SRC

#+BEGIN_SRC python :session
print(x)  # Works! Outputs: 42
#+END_SRC
```

Same Python interpreter, same namespace. Variables, functions, imports—all persist.

Multiple named sessions:

```org
#+BEGIN_SRC python :session project-a
x = 1
#+END_SRC

#+BEGIN_SRC python :session project-b
x = 2
#+END_SRC
```

Separate environments, same document.

## Passing Data Between Blocks

### Variables from Values

```org
#+BEGIN_SRC python :var name="Alice" age=30
print(f"{name} is {age} years old")
#+END_SRC

#+RESULTS:
: Alice is 30 years old
```

### Variables from Tables

```org
#+NAME: sales-data
| Product | Q1 | Q2 | Q3 | Q4 |
|---------+----+----+----+----|
| Widget  | 10 | 15 | 12 | 18 |
| Gadget  | 20 | 25 | 30 | 28 |

#+BEGIN_SRC python :var data=sales-data
import pandas as pd
df = pd.DataFrame(data[1:], columns=data[0])
print(df.sum(numeric_only=True))
#+END_SRC

#+RESULTS:
: Q1    30
: Q2    40
: Q3    42
: Q4    46
```

Table becomes 2D list. Process with any language.

### Variables from Block Output

```org
#+NAME: generate-data
#+BEGIN_SRC python
return [1, 2, 3, 4, 5]
#+END_SRC

#+BEGIN_SRC python :var numbers=generate-data
print(sum(numbers))
#+END_SRC

#+RESULTS:
: 15
```

Chain blocks together. One block's output feeds another's input.

## Returning Results to Tables

Code can generate tables:

```org
#+BEGIN_SRC python :results value table
return [
    ['Name', 'Age', 'City'],
    ['Alice', 28, 'Portland'],
    ['Bob', 35, 'Seattle'],
    ['Carol', 42, 'Vancouver']
]
#+END_SRC

#+RESULTS:
| Name  | Age | City      |
|-------+-----+-----------|
| Alice |  28 | Portland  |
| Bob   |  35 | Seattle   |
| Carol |  42 | Vancouver |
```

Generate org tables from code. Add formulas. Instant data reports.

## Graphics and Plots

Generate graphs:

```org
#+BEGIN_SRC python :results file
import matplotlib.pyplot as plt
import numpy as np

x = np.linspace(0, 10, 100)
y = np.sin(x)

plt.plot(x, y)
plt.title('Sine Wave')
plt.savefig('sine.png')
return 'sine.png'
#+END_SRC

#+RESULTS:
[[file:sine.png]]
```

The plot appears in your document. Update code, re-execute, plot updates.

With R:

```org
#+BEGIN_SRC R :results graphics file :file plot.png
plot(cars)
#+END_SRC
```

With gnuplot, ditaa, graphviz—dozens of options.

## Literate Programming: The Full Vision

Donald Knuth's original idea: write programs as explanations, with code woven throughout.

```org
* Calculate Fibonacci Numbers

The Fibonacci sequence starts with 0 and 1. Each subsequent number is the
sum of the previous two.

First, we'll implement a recursive version:

#+NAME: fib-recursive
#+BEGIN_SRC python :session fib
def fib_recursive(n):
    """Recursive Fibonacci (inefficient but clear)"""
    if n <= 1:
        return n
    return fib_recursive(n-1) + fib_recursive(n-2)
#+END_SRC

This is elegant but slow due to repeated calculations. Let's test it:

#+BEGIN_SRC python :session fib
print([fib_recursive(i) for i in range(10)])
#+END_SRC

#+RESULTS:
: [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

For larger numbers, we need a better approach. Here's an iterative version:

#+NAME: fib-iterative
#+BEGIN_SRC python :session fib
def fib_iterative(n):
    """Iterative Fibonacci (efficient)"""
    a, b = 0, 1
    for _ in range(n):
        a, b = b, a + b
    return a
#+END_SRC

Let's time both approaches:

#+BEGIN_SRC python :session fib :results output
import time

n = 30

start = time.time()
result1 = fib_recursive(n)
time1 = time.time() - start

start = time.time()
result2 = fib_iterative(n)
time2 = time.time() - start

print(f"Recursive: {result1} in {time1:.4f}s")
print(f"Iterative: {result2} in {time2:.6f}s")
print(f"Speedup: {time1/time2:.0f}x")
#+END_SRC
```

Code, explanation, tests, benchmarks—all in one document. Export to PDF for sharing, execute for verification.

## Tangling: Extract Source Files

Write code in org, export to source files:

```org
#+BEGIN_SRC python :tangle fib.py
def fibonacci(n):
    """Calculate nth Fibonacci number"""
    if n <= 1:
        return n
    a, b = 0, 1
    for _ in range(n):
        a, b = b, a + b
    return a

if __name__ == "__main__":
    print([fibonacci(i) for i in range(10)])
#+END_SRC
```

Run `M-x org-babel-tangle` or `C-c C-v t`. Org extracts code to `fib.py`.

Multiple blocks can contribute to one file:

```org
#+BEGIN_SRC python :tangle program.py
import sys

#+END_SRC

#+BEGIN_SRC python :tangle program.py
def main():
    print("Hello, World!")

#+END_SRC

#+BEGIN_SRC python :tangle program.py
if __name__ == "__main__":
    main()
#+END_SRC
```

Tangle creates `program.py` with all blocks combined in order.

### Tangle with Noweb References

Reference other blocks by name:

```org
#+NAME: imports
#+BEGIN_SRC python :tangle no
import sys
import os
#+END_SRC

#+NAME: functions
#+BEGIN_SRC python :tangle no
def helper():
    return 42
#+END_SRC

#+BEGIN_SRC python :tangle program.py :noweb yes
<<imports>>

<<functions>>

def main():
    print(helper())

if __name__ == "__main__":
    main()
#+END_SRC
```

`<<blockname>>` inserts that block's content. Compose programs from pieces.

## Inline Code

Execute code inline with results in text:

```org
The sum of 2 + 2 is src_python[:exports results]{return 2+2} {{{results(=4=)}}}.
```

Exports as: "The sum of 2 + 2 is 4."

Live calculations in prose.

## SQL and Database Queries

Query databases directly:

```org
#+BEGIN_SRC sqlite :db mydatabase.db
SELECT name, age FROM users WHERE age > 25;
#+END_SRC

#+RESULTS:
| name  | age |
|-------+-----|
| Alice |  28 |
| Bob   |  35 |
| Carol |  42 |
```

Results as org tables. Add formulas, analyze data.

With PostgreSQL:

```org
#+BEGIN_SRC sql :engine postgresql :database mydb :user myuser
SELECT * FROM orders WHERE status = 'pending';
#+END_SRC
```

Your documentation queries production databases. Be careful, but powerful.

## Shell Scripts

```org
#+BEGIN_SRC shell
ls -la ~/org | head -10
#+END_SRC

#+RESULTS:
: total 128
: drwxr-xr-x   12 user  staff   384 Jan 15 09:30 .
: drwxr-xr-x+  87 user  staff  2784 Jan 15 09:15 ..
: -rw-r--r--    1 user  staff  2048 Jan 14 15:20 inbox.org
: ...
```

Document system administration with executable examples.

## Advanced Header Arguments

**:dir** - Working directory:
```org
#+BEGIN_SRC shell :dir ~/projects/myapp
ls
#+END_SRC
```

**:file** - Output file:
```org
#+BEGIN_SRC dot :file diagram.png
digraph { A -> B; B -> C; }
#+END_SRC
```

**:cache** - Cache results:
```org
#+BEGIN_SRC python :cache yes
# Expensive computation
import time
time.sleep(5)
return "Done"
#+END_SRC
```

Re-run only if code changes.

**:cmdline** - Command-line arguments:
```org
#+BEGIN_SRC shell :cmdline -n 5
head
#+END_SRC
```

**:stdin** - Provide input:
```org
#+BEGIN_SRC python :stdin example-data
import sys
print(sys.stdin.read().upper())
#+END_SRC
```

## Document-Wide Defaults

Set defaults for entire file:

```org
#+PROPERTY: header-args :session mysession :results output
```

All blocks in file use these settings unless overridden.

Per-subtree:

```org
* Python Examples
  :PROPERTIES:
  :header-args:python: :session py :results output
  :END:

** Example 1
#+BEGIN_SRC python
print("Uses :session py :results output")
#+END_SRC
```

## Reproducible Research

Org-Mode + code blocks = reproducible research:

1. Gather data (code block)
2. Clean data (code block)
3. Analyze (code block)
4. Visualize (code block)
5. Export paper (org export)

Anyone can re-run analysis. Change assumption? Re-execute. New data? Re-run pipeline.

Science that actually reproduces.

## Org-Mode as Jupyter Alternative

Jupyter notebooks but:
- Plain text (version control friendly)
- Multiple languages in one document
- Emacs editing power
- Outliner structure
- Offline, no server required
- Export to any format

Not for everyone, but for text-oriented programmers? Paradise.

## Your Exercise

1. Enable at least 3 languages in `org-babel-load-languages`
2. Write a code block in your favorite language and execute it
3. Create a code block that reads from an org table
4. Generate a table from code output
5. Try session-based execution (multiple blocks sharing state)
6. Create a simple plot/graph
7. Experiment with noweb references
8. Tangle code to a source file

## The Philosophy of Literate Programming

Knuth argued programs should be written for humans first, computers second. Org-Mode makes this practical.

Your documentation isn't separate from your code—it IS your code. The explanation and implementation are one artifact.

Update the code? Update the explanation. They're the same file.

This is documentation that can't become outdated because it's executable. Change the code, re-run, results update.

## Next: Export and Publishing

You've built amazing documents with structure, data, code, and results. Now let's share them. Chapter 12 covers exporting to HTML, PDF, LaTeX, Markdown, and more. Your plain text becomes beautiful documents.
