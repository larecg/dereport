# Daily Report Utility

## Usage

## Installation

## Development

Download project in the `~/common-lisp` folder

```sh
git clone git@github.com:larecg/dereport.git ~/common-lisp/dereport
```

Install dependencies using [roswell](https://roswell.github.io)

In [roswell](https://roswell.github.io)

```lisp
(ql:quickload :dereport)
```

And change to the same package:

```lisp
(in-package :dereport)
```

Happy hacking! :rocket:

## CLI

### Building CLI

```sh
ros build.lisp
```

The new binary is ready:

```sh
./dereport
```

## Config file

Optionally, to change the options, you can configure the file: `~/.config/dereport/cli.lisp`

```lisp
(in-package :dereport)

(setf *prefixes-per-category* '(("Done" . ("*** DONE" "*** WAITING" "*** DELEGATED"))
                     ("Discarded" . ("*** CANCELLED" "*** FORWARDED"))
                     ("Doing" . ("*** DOING"))
                     ("Next" . ("*** TODO" "*** NEXT"))))

(setf *regex-replacements* '((" :[\\w:]+:$" . "") ; Org-mode tags
                             ("\\[\\[(\\w+)\\]\\[(\\w+)]\\]" . "\\2"))) ; Org-mode links
```

## License

Licensed under the LLGPL License.
