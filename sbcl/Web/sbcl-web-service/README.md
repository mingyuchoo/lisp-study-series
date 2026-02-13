# SBCL 웹 서비스

이 프로젝트는 Steel Bank Common Lisp (SBCL)를 사용하여 구축된 웹 서비스입니다. 모듈성과 사용 편의성에 중점을 둔 웹 애플리케이션 생성 및 관리를 위한 프레임워크를 제공합니다.

## 프로젝트 구조

- `src/`: 웹 서비스의 소스 코드를 포함합니다.
  - `main.lisp`: 웹 서비스의 진입점으로, 서버와 애플리케이션 로직을 초기화합니다.
  - `routes.lisp`: 라우팅 로직을 정의하고 다양한 HTTP 라우트를 처리합니다.
  - `utils.lisp`: 데이터 검증 및 포맷팅을 위한 유틸리티 함수를 포함합니다.
  
- `tests/`: 웹 서비스의 테스트 스위트를 포함합니다.
  - `test-suite.lisp`: 주요 애플리케이션 로직과 라우트에 대한 테스트 케이스를 포함합니다.

- `.gitignore`: Git에서 무시할 파일 및 디렉토리를 지정합니다.

- `README.md`: 프로젝트 문서입니다.

- `sbcl-web-service.asd`: 프로젝트를 로드하고 컴파일하기 위한 ASDF 시스템 정의 파일입니다.

## 설치 방법

1. 시스템에 SBCL이 설치되어 있는지 확인하세요.
2. 저장소를 클론합니다:

   ```
   git clone <repository-url>
   ```

3. 프로젝트 디렉토리로 이동합니다:

   ```
   cd sbcl-web-service
   ```

4. Quicklisp를 설치합니다 (최초 1회만):

   ```bash
   sbcl --load setup-quicklisp.lisp --quit
   ```

   이 명령은 프로젝트 디렉토리에 Quicklisp를 로컬로 설치하고 필요한 모든 의존성을 다운로드합니다.

5. ASDF를 사용하여 프로젝트를 로드하고 실행합니다:

   ```lisp
   (push "/<절대-경로>/sbcl-web-service/" asdf:*central-registry*)
   (asdf:load-system :sbcl-web-service)
   ```

## 사용 예제

### 방법 1: SBCL REPL에서 웹 서비스 시작하기

웹 서비스를 시작하려면 SBCL REPL에서 다음 명령을 실행하세요:

```bash
sbcl
```

```lisp
;; (선택사항) `~/.sbclrc`에 의해 이미 로드되지 않은 경우, Quicklisp를 로드합니다
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; 의존성을 로드합니다
(ql:quickload "hunchentoot")

;; 애플리케이션 파일을 로드합니다
(load "src/main.lisp")

;; (선택적 과정) 서버가 자동으로 시작되지 않으면 수동으로 시작할 수 있습니다:
(in-package :sbcl-web-service)
(start-server 8080) ;; 8080 포트가 이미 사용 중이면 다른 포트를 사용하세요

;; 웹 서버를 중지합니다
(stop-server)
```

### 방법 2: 쉘에서 웹 서비스 시작하기

웹 서비스를 시작하려면 쉘에서 다음 명령을 실행하세요:

```bash
./run-server.sh
# 또는
sbcl --load initialize.lisp
```

그런 다음 `http://localhost:8080`에서 웹 서비스에 접속할 수 있습니다.

## SBCL REPL에서 테스트 실행하기

테스트를 실행하려면 SBCL REPL에서 다음 명령을 실행하세요:

```lisp
;; (선택사항) `~/.sbclrc`에 의해 이미 로드되지 않은 경우, Quicklisp를 로드합니다
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; 테스트 의존성을 로드합니다
(ql:quickload '(:fiveam :drakma))

;; 테스트 시스템을 로드합니다
(ql:quickload :sbcl-web-service/tests)

;; 테스트를 실행합니다
(sbcl-web-service.tests:run-tests)

;; 더 자세한 출력을 원하는 경우
(sbcl-web-service.tests:run-tests-interactive)
```
