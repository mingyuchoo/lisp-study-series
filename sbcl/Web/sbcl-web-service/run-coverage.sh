#!/bin/bash
# SBCL 코드 커버리지 측정 스크립트
# sb-cover를 사용하여 테스트 커버리지 리포트를 HTML로 생성합니다.

cd "$(dirname "$0")"

REPORT_DIR="coverage-report"

# 이전 리포트 정리
rm -rf "$REPORT_DIR"
mkdir -p "$REPORT_DIR"

echo "=== SBCL 코드 커버리지 측정 시작 ==="
echo ""

sbcl --noinform \
     --eval "(require :asdf)" \
     --eval "(require :sb-cover)" \
     --eval "(push (truename \".\") asdf:*central-registry*)" \
     --eval "(load (merge-pathnames \"quicklisp/setup.lisp\" (truename \".\")))" \
     --eval "(declaim (optimize sb-cover:store-coverage-data))" \
     --eval "(ql:quickload '(:fiveam :drakma) :silent t)" \
     --eval "(asdf:load-system :sbcl-web-service :force t)" \
     --eval "(asdf:load-system :sbcl-web-service/tests :force t)" \
     --eval "(sbcl-web-service.tests:run-tests)" \
     --eval "(sb-cover:report \"$REPORT_DIR/\")" \
     --eval "(format t \"~%=== 커버리지 리포트 생성 완료 ===~%\")" \
     --eval "(format t \"리포트 위치: $(pwd)/$REPORT_DIR/cover-index.html~%\")" \
     --eval "(declaim (optimize (sb-cover:store-coverage-data 0)))" \
     --eval "(sb-cover:reset-coverage)" \
     --quit

echo ""
echo "브라우저에서 열기: file://$(pwd)/$REPORT_DIR/cover-index.html"
