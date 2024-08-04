# README

## 1. Check `home` in REPL

```bash
$ sbcl
* ql:*quicklisp-home*
#P"/home/<my-username>/.quicklisp/"
nil
* (quit)
```

## 2. Create new project in `~/.quicklisp/local-projects/`

```bash
.
├── my-project
│   ├── my-project.asd
│   ├── README.markdown
│   ├── README.org
│   ├── src
│   │   └── main.lisp
│   └── tests
│       └── main.lisp
```

## 3. Register local projects

```bash
$ sbcl
* (ql:register-local-projects)
nil
* (quit)
```

the file `system-index.txt` will be created just like this

```bash
$ cat system-index.txt
my-project/my-project.asd
```

## 4. Load and run your project

```bash
$ sbcl
* (ql:quickload "my-porject")
To load "my-project":
  Load 1 ASDF system:
    my-project
; Loading "my-project"

("my-project")
* (my-project:hello-world)()
Hello, World!
nil
* nil
```

## References

- <https://qiita.com/tamurashingo@github/items/0284c086c51e12e29240>
