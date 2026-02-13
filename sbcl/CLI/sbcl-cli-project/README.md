# README

## 1. 로컬에서 프로젝트 처음부터 생성하기

`~/another-path/`에 새 프로젝트 `{project-name}`을 생성하고 여기에 `quicklisp.lisp`를 다운로드합니다.

### Quicklisp 설치

먼저 홈 디렉토리로 이동하여 Quicklisp를 설치합니다:

```bash
cd
sbcl --script setup-quicklisp.lisp
```

### 프로젝트 생성

생성할 프로젝트의 이름을 `{project-name}`으로 정합니다.

```bash
$ mkdir {project-name}
$ tree
.
├── {project-name}.asd
├── README.markdown
├── README.org
├── src
│   └── main.lisp
└── tests
    └── main.lisp
```

### 로컬 프로젝트 로드

SBCL REPL에서 다음 명령어를 실행하여 프로젝트를 생성하고 로드합니다:

```bash
sbcl
CL-USER> (load #p"./quicklisp/setup.lisp")
CL-USER> (ql:quickload :quickproject)
CL-USER> (quickproject:make-project #p"./" :name "{project-name}")
CL-USER> (ql:quickload :{project-name})
CL-USER> (in-package :{project-name})
CL-USER> (quit)

```

**설명:**

- `quickproject`는 Common Lisp 프로젝트의 기본 구조를 자동으로 생성해주는 도구입니다
- `.asd` 파일은 ASDF(Another System Definition Facility) 시스템 정의 파일입니다
- `src/` 디렉토리에는 소스 코드를, `tests/` 디렉토리에는 테스트 코드를 작성합니다

### 참고: 로컬 프로젝트를 등록하는 다른 방법들

#### 1. Quicklisp의 local-projects 메커니즘 사용

프로젝트 디렉토리를 Quicklisp의 local-projects에 심볼릭 링크로 연결합니다:

```bash
ln -s $(pwd) ~/quicklisp/local-projects/
```

#### 2. ASDF의 central-registry 사용

현재 디렉토리를 ASDF의 중앙 레지스트리에 추가합니다:

```lisp
(push (truename ".") asdf:*central-registry*)
```

#### 3. ASDF 소스 레지스트리 설정 파일 사용

`~/.config/common-lisp/source-registry.conf.d/` 디렉토리에 설정 파일을 생성합니다:

```lisp
(:directory (:home "path/to/your/project"))
```

#### 4. Quicklisp의 local-project-directories 변수 사용

프로젝트 디렉토리를 Quicklisp의 로컬 프로젝트 디렉토리 목록에 추가합니다:

```lisp
(push (truename ".") ql:*local-project-directories*)
(ql:register-local-projects)
```

## 2. 프로젝트와 함께 REPL 실행하기

### 기본 REPL 실행

가장 기본적인 SBCL REPL을 실행합니다:

```bash
sbcl
```

### 프로젝트가 로드된 상태로 REPL 실행

프로젝트를 자동으로 로드하여 REPL을 시작합니다:

```bash
$ sbcl --eval "(load \"quicklisp/setup.lisp\")" \
       --eval "(push (uiop:getcwd) asdf:*central-registry*)" \
       --eval "(ql:quickload :sbcl-cli-project)"
```

**설명:**

- `--eval` 옵션으로 SBCL 시작 시 자동으로 명령어를 실행합니다
- `uiop:getcwd`는 현재 작업 디렉토리를 가져옵니다
- 프로젝트가 자동으로 로드되어 바로 사용할 수 있습니다

### REPL에서 단계별로 실행

REPL을 시작한 후 수동으로 프로젝트를 로드하고 실행합니다:

```bash
$ sbcl

CL-USER> (load "quicklisp/setup.lisp")           ; Quicklisp 로드
CL-USER> (push (uiop:getcwd) asdf:*central-registry*)  ; 현재 디렉토리를 ASDF에 등록
CL-USER> (ql:quickload :sbcl-cli-project)        ; 프로젝트 로드
CL-USER> (in-package :sbcl-cli-project)          ; 프로젝트 패키지로 전환
SBCL-CLI-PROJECT> (main)                         ; main 함수 실행
SBCL-CLI-PROJECT> (quit)                         ; REPL 종료
```

**설명:**

- `load`: Lisp 파일을 로드합니다
- `push`: 리스트의 앞에 요소를 추가합니다
- `ql:quickload`: Quicklisp를 통해 시스템을 로드합니다
- `in-package`: 현재 패키지를 변경합니다

## 3. 프로젝트 로드 및 실행

REPL에서 프로젝트를 로드하고 함수를 실행하는 방법입니다:

```bash
$ sbcl

CL-USER> (ql:quickload :{project-name})          ; 프로젝트 로드
CL-USER> (in-package :{project-name})            ; 프로젝트 패키지로 전환
{PROJECT-NAME}> (hello-world)                    ; 함수 실행
Hello, World!
```

**설명:**

- Quicklisp를 통해 프로젝트와 모든 의존성이 자동으로 로드됩니다
- 패키지를 전환하면 프로젝트의 함수들을 직접 호출할 수 있습니다
- 패키지 이름은 대소문자를 구분하지 않습니다

## 4. 실행 파일 빌드

프로젝트를 독립 실행 가능한 바이너리 파일로 컴파일합니다:

```bash
./build.sh {project-name}
```

**설명:**

- `build.sh` 스크립트는 SBCL의 `save-lisp-and-die` 함수를 사용하여 실행 파일을 생성합니다
- 생성된 바이너리는 Lisp 런타임 없이도 독립적으로 실행할 수 있습니다
- 빌드된 실행 파일은 배포가 용이하며 시작 시간이 빠릅니다

### 빌드 후 실행

```bash
./{project-name}
```

## 5. 추가 정보

### 프로젝트 구조

```
{project-name}/
├── {project-name}.asd      # ASDF 시스템 정의 파일
├── README.md               # 프로젝트 문서
├── src/
│   └── main.lisp          # 메인 소스 코드
├── tests/
│   └── main.lisp          # 테스트 코드
├── quicklisp/             # Quicklisp 설치 디렉토리
└── build.sh               # 빌드 스크립트
```

### 유용한 REPL 명령어

```lisp
; 시스템 정보 확인
(lisp-implementation-type)
(lisp-implementation-version)

; 로드된 시스템 확인
(ql:system-apropos "keyword")

; 패키지의 모든 심볼 나열
(do-external-symbols (s :package-name) (print s))

; 함수 문서 확인
(documentation 'function-name 'function)

; 디버깅
(trace function-name)    ; 함수 호출 추적
(untrace function-name)  ; 추적 해제
```

### 일반적인 문제 해결

**문제: 프로젝트를 찾을 수 없음**

```lisp
; 해결: ASDF 레지스트리에 프로젝트 경로 추가
(push (truename ".") asdf:*central-registry*)
```

**문제: 의존성 로드 실패**

```lisp
; 해결: Quicklisp 업데이트
(ql:update-client)
(ql:update-all-dists)
```

## 참고 자료

- [Quicklisp 공식 문서](https://www.quicklisp.org/)
- [ASDF 매뉴얼](https://asdf.common-lisp.dev/)
- [SBCL 사용자 매뉴얼](http://www.sbcl.org/manual/)
- [Common Lisp HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Front/index.htm)
- [Qiita 튜토리얼](https://qiita.com/tamurashingo@github/items/0284c086c51e12e29240)
