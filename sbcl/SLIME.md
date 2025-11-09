# SLIME

## Key bindings

| 명령          | 설명                                             |
| ------------- | ------------------------------------------------ |
| `M-x slime`   | SLIME 시작                                       |
| `C-c C-c`     | 현재 함수/표현식 컴파일                          |
| `C-c C-k`     | 현재 버퍼 전체 컴파일                            |
| `C-c C-z`     | REPL로 전환                                      |
| `C-c C-p`     | 이전 SLIME 프로세스로 전환                       |
| `C-x C-e`     | 커서 앞의 표현식을 평가                          |
| `,` (콤마)    | REPL 명령 프롬프트 진입 (`,quit`, `,restart` 등) |

## Examples

```lisp
(defun calc (op a b)
  (case op
    (+ (+ a b))
    (- (- a b))
    (* (* a b))
    (/ (/ a b))
    (t (format t "Unknown operator: ~a~%" op))))

(format t "Result: ~a~%" (calc '+ 3 7))
```

`C-c C-c`, `C-c C-z`

```lisp
CL-USER> (calc '* 9 6)
54
```

## 유용한 SLIME 확장

- `slime-fancy` – 디버거, 문서 검색, 자동 완성 등 활성화
- `slime-company` – Company-mode 기반 자동 완성
- `slime-repl-ansi-color` – REPL 컬러 지원
- `paredit` – 괄호 구조 편집 지원 (Lisp 프로그래밍 거의 필수)
