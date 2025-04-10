# README

## 1. Create a project from scatch locally

Create new project `{project-name}` in `~/another-path/` and download `quicklisp.lisp` here.

```bash
$ cd
$ cd another-path

$ curl -O https://beta.quicklisp.org/quicklisp.lisp
$ sbcl --load ./quicklisp.lisp

CL-USER> (quicklisp-quickstart:install :path "./quicklisp")
CL-USER> (ql:add-to-init-file)
CL-USER> (ql:quickload :quicklisp-slime-helper)
CL-USER> (quit)
```

Let's name the project we are going to create `{project-name}`.

```bash
$ mkdir {project-name}
$ tree
.
├── {project-name}
│   ├── {project-name}.asd
│   ├── README.markdown
│   ├── README.org
│   ├── src
│   │   └── main.lisp
│   └── tests
│       └── main.lisp
```

Load local projects: `{project-name}`

```bash
sbcl
CL-USER> (load #p"./quicklisp/setup.lisp")
CL-USER> (ql:quickload :quickproject)
CL-USER> (quickproject:make-project #p"./" :name "{project-name}")
CL-USER> (ql:quickload :{project-name})
CL-USER> (in-package :{project-name})
CL-USER> (quit)

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

CL-USER> (ql:quickload :{project-name})
CL-USER> (in-package :{project-name})
CL-USER> (hello-world)
Hello, World!
```
 ## 3. Build binary

```bash
./build.sh {project-name}
```

## References

- <https://qiita.com/tamurashingo@github/items/0284c086c51e12e29240>
