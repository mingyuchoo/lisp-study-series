# README

## 1. Create a project with ASDF

### CASE 1) Create a project with `quickproject`

```bash
$ sbcl
```

```lisp
CL-USER> (ql:quickload "quickproject")
CL-USER> (quickproject:make-project #p"~/quicklisp/local-projects/{project-name}" :name "{project-name}")
CL-USER> (ql:quickload "{project-name}")
CL-USER> (in-package "{project-name}")
```

Check `home` in REPL

```bash
$ sbcl
CL-USER> ql:*quicklisp-home*
CL-USER> (quit)
```

### CASE 2) Create a project from scatch

Create new project `my-project` in `~/quicklisp/local-projects/`

```bash
$ cd
$ cd quicklisp/local-projects
```

Let's name the project we are going to create `my-project`.

```bash
$ mkdir my-project
$ tree 
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

Register local projects: `my-project`

```bash
$ sbcl
CL-USER> (ql:register-local-projects)
CL-USER> (quit)
```

the file `system-index.txt` will be created just like this

```bash
$ cat system-index.txt
my-project/my-project.asd
```

### NOTE: other ways to register local projects

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

## 2. Load and run your project

```bash
$ sbcl
CL-USER> (ql:quickload :my-porject)
CL-USER> (in-package :my-porject)
CL-USER> (hello-world)
Hello, World!
```
 ## 3. Build binary

```bash
./build.sh my-project
```

## References

- <https://qiita.com/tamurashingo@github/items/0284c086c51e12e29240>
