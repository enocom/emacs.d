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

## Running in iTerm2

First, rather than installing the GUI version above, install emacs with:

```
brew install emacs
```

From there, it's possible to start emacs with just the command:

```
emacs # no -nw here
```

It is possible to run emacs in a terminal with nice looking colors.

To do so, you need to configure TrueColor support.

See [this question and answer][truecolor-q] and the associated [Emacs commit][emacs-truecolor].

For some context on TrueColor and its support, see [this gist][truecolor-gist].

[truecolor-q]: https://emacs.stackexchange.com/questions/32506/conditional-true-color-24-bit-color-support-for-iterm2-and-terminal-app-in-osx
[emacs-truecolor]: https://github.com/emacs-mirror/emacs/commit/e463e5762bbe628be3d15da066a90f079a8468b3
[truecolor-gist]: https://gist.github.com/XVilka/8346728

## Where to Go Next

- [Emacs is Sexy](https://emacs.sexy)
- [Emacs Rocks](http://emacsrocks.com)
- [Mastering Emacs](https://www.masteringemacs.org)
- [Emacs Official Manual](https://www.gnu.org/software/emacs/manual/html_node/emacs/index.html)
- [Emacs Wiki](https://www.emacswiki.org)

## Addendum

If Magit runs slow, try setting the full path to the git executable.

```
M-x customize-var RET magit-git-executable RET
```

If initial installation fails with:

```
Package `queue-0.2' is unavailable
```

try looking through the associated [issue](https://github.com/melpa/melpa/issues/2005)
on Melpa.

When running GUI emacs on macOS Catalina, one must permit emacs access to folders like
`Desktop`, `Documents`, and so on.
See [the answer here](https://emacs.stackexchange.com/a/53037) for how to do so.
