# Tests for sbcl-cli-project

이 디렉터리는 sbcl-cli-project의 단위 테스트를 포함합니다.

## 테스트 프레임워크

이 프로젝트는 [FiveAM](https://github.com/sionescu/fiveam) 테스트 프레임워크를 사용합니다.

## 테스트 실행 방법

### REPL에서 실행

```lisp
;; Quicklisp을 사용하여 FiveAM 설치 (처음 한 번만)
(ql:quickload :fiveam)

;; 테스트 시스템 로드
(ql:quickload :sbcl-cli-project-tests)

;; 테스트 실행
(asdf:test-system :sbcl-cli-project-tests)

;; 또는 직접 실행
(sbcl-cli-project-tests:run-tests)
```

### 커맨드 라인에서 실행

```bash
sbcl --eval "(ql:quickload :sbcl-cli-project-tests)" \
     --eval "(asdf:test-system :sbcl-cli-project-tests)" \
     --quit
```

또는 제공된 스크립트 사용:

```bash
./run-tests.sh
```

## 테스트 구조

- `main-tests.lisp`: main 함수에 대한 단위 테스트

## 테스트 추가하기

새로운 테스트를 추가하려면:

1. `tests/` 디렉터리에 새 테스트 파일 생성 (예: `new-feature-tests.lisp`)
2. `sbcl-cli-project-tests.asd`에 파일 추가
3. FiveAM의 `def-suite`와 `test` 매크로를 사용하여 테스트 작성

예제:

```lisp
(in-package :sbcl-cli-project-tests)

(test my-new-test
  "Test description"
  (is (= 2 (+ 1 1))))
```
