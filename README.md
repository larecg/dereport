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

## License

Licensed under the LLGPL License.
