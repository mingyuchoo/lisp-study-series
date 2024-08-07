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

Let's name the project we are going to create `my-project`.

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

### Note: other ways to register local projects

Quicklisp의 local-projects 메커니즘 사용

```
ln -s $(pwd) ~/quicklisp/local-projects/
```

ASDF의 central-registry 사용

```
(push (truename ".") asdf:*central-registry*)
```

ASDF 소스 레지스트리 설정 파일 사용

```
(:directory (:home "path/to/your/project"))
```

Quicklisp의 local-project-directories 변수 사용

```
(push (truename ".") ql:*local-project-directories*)
(ql:register-local-projects)
```

## 4. Load and run your project

```bash
$ sbcl


CL-USER> (ql:quickload :my-porject)
To load "my-project":
  Load 1 ASDF system:
    my-project
; Loading "my-project"
(:MY-PROJECT)


CL-USER> (in-package :my-project)
#<PACKAGE "MY-PROJECT">


MY-PROJECT> (hello-world)
Hello, World!
NIL
```

## 5. Build binary

```bash
./build.sh my-project
```

## References

- <https://qiita.com/tamurashingo@github/items/0284c086c51e12e29240>
