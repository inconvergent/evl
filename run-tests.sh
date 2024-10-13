#!/bin/bash

set -e
sbcl --version
echo '#### running SBCL tests:'
touch ./evl.asd
time sbcl --noinform  --quit \
     --eval '(ql:quickload :prove)'\
     --eval '(handler-case (ql:quickload :evl :verbose nil)
                           (error (c) (format t "STAGE1FAIL: ~a" c)
                                      (uiop:quit 2)))'\
     --eval '(handler-case (progn (asdf:test-system :evl))
                           (error (c) (format t "STAGE2FAIL: ~a" c)
                                      (uiop:quit 3)))'

cd .. ; touch ./evl.asd

