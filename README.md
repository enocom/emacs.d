# Emacs configuration

Here is what I consider a reasonable Emacs config with a focus on Clojure
development.

## Getting Started

This guide assumes a person is using macOS (previously OS X).

1. Install the macOS build of Emacs found at [this
url](https://emacsformacosx.com).

2. Move any previous Emacs config out of the way (or delete it if you prefer):

    mv ~/.emacs.d ~/.emacs.d.old

3. Clone this repo as follows:

    git clone https://github.com/enocom/emacs.d ~/.emacs.d

4. Open Emacs and start exploring!

If you've never used Emacs before, going through the interactive tutorial is
nice. Start the tutorial with `C-h t` (i.e., `control-h` then `t`).

Note: you'll often see key bindings described like: `C-c M-j`. That means press
the control key with the "c" key and then press the option key (otherwise known
as "meta," hence the "M") and j. Any keyboard shortcut invokes an Emacs Lisp
function. Likewise, a person may use `M-x` to open a fuzzy finder of all the
available functions in Emacs. The command is a good way to discover new things
about Emacs, or at least call desired functions.

Once you're comfortable moving around Emacs and doing basic editing, try reading
through [init.el](init.el), which is copiously annotated with explanations of what
each piece of configuration does.

## Installed Plugins

This Emacs config attempts to be as minimal as possible while also preserving
some creature-comforts of development. A more fully featured Emacs config worth
trying is [Emacs Prelude](https://github.com/bbatsov/prelude). If you prefer a
mind-bending and maximalist config, check out 
[Emacs Live](https://github.com/overtone/emacs-live), a config built to support
writing and performing music in Clojure.

Here are the installed plugins:

- [ace-window](https://github.com/abo-abo/ace-window)
- [ag](https://github.com/Wilfred/ag.el) (Requires [The Silver Searcher](https://github.com/ggreer/the_silver_searcher))
- [browse-kill-ring](https://github.com/browse-kill-ring/browse-kill-ring)
- [CIDER](https://cider.mx)
- [clojure-mode](https://github.com/clojure-emacs/clojure-mode/)
- [company](http://company-mode.github.io)
- [expand-region](https://github.com/magnars/expand-region.el) (See [Emacs Rocks Episode 9](http://emacsrocks.com/e09.html))
- [ido-completing-read+](https://github.com/DarwinAwardWinner/ido-completing-read-plus)
- [magit](https://magit.vc) (See [Emacs Rocks Episode 17](http://emacsrocks.com/e17.html))
- [paredit](https://www.emacswiki.org/emacs/ParEdit) (See [Emacs Rocks Episode 14](http://emacsrocks.com/e14.html))
- [projectile](https://www.projectile.mx/en/latest/)
- [rainbow-delimiters](https://github.com/Fanael/rainbow-delimiters)
- [smex](https://github.com/nonsequitur/smex)
- [yaml-mode](https://github.com/yoshiki/yaml-mode)

## Where to Go Next

- [Emacs is Sexy](https://emacs.sexy)
- [Emacs Rocks](http://emacsrocks.com)
- [Mastering Emacs](https://www.masteringemacs.org)
- [Emacs Official Manual](https://www.gnu.org/software/emacs/manual/html_node/emacs/index.html)
- [Emacs Wiki](https://www.emacswiki.org)
