;;; Caries
;;;  - Copyright(C) 2012 Kazh. All Rights Reserved.

(use srfi-1)
(use text.tree)
(use text.html-lite)
(use gauche.parseopt)
(use gauche.test)

(define inline #/^([^;]*);/)

(define *title* "Untitled")


(define (call-paragraph str)
  (let ((str (regexp-replace-all* str
               #/^\s+/ ""
               #/\s+$/ ""
               #/\s+/ " ")))
    (if (string=? str "") '()
      (html:p str))))

(define (call-function name arg)
  (cond
    ((string=? name "title")
     (set! *title* arg)
     (html:h1 arg))
    ((string=? name "section") (html:h2 arg))
    ((string=? name "subsection") (html:h3 arg))
    (else (error "Unknown function"))))


(define (parse str)
  (or (and-let* ((m (#/@\?=|@\?|@@|@|\n\n/ str))
                 (t (m 0))
                 (a (rxmatch-after m)))
        (cons (call-paragraph (rxmatch-before m))
              (cond
                ((string=? t "@?=")
                 (cons "@?" (parse-dispemb a)))
                ((string=? t "@?")
                 (cons t (parse-inline a)))
                ((string=? t "@@")
                 (cons "@" (parse a)))
                ((string=? t "@")
                 (parse-functions a))
                ((string=? t "\n\n")
                 (parse a)))))
      (list (call-paragraph str))))

(define (parse-dispemb str)
  (cond
    ((inline str) =>
     (lambda (m)
       (cons* "(display" (m 1) "); " (parse (rxmatch-after m)))))
    (else (error "Semicolon required (3)"))))

(define (parse-inline str)
  (cond
    ((inline str) =>
     (lambda (m)
       (cons (m 0) (parse (rxmatch-after m)))))
    (else (error "Semicolon required (2)"))))

(define (parse-functions str)
  (cond
    ((#/^\s*;/ str) =>
     (lambda (m) (parse (rxmatch-after m))))
    ((#/^\s*(\w+)\s*\(([^)]*)\)/ str) =>
     (lambda (m)
       (cons (call-function (m 1) (m 2))
             (parse-functions (rxmatch-after m)))))
    (else (error "Semicolon required"))))


;; ヘッダとフッタを負荷し，HTML 文書を生成する．
(define (generate-page body)
  (tree->string
    (list
      (html-doctype)
      (html:html
        (html:head
          (html:title *title*))
        (html:body body)))))

;; 埋め込み記号を処理し，テキストを生成する．
(define (embed-page page)
  (tree->string
    (cons "(begin"
      (let recur ((page page))
        (or (and-let* ((m (#/@\?/ page))
                       (n (inline (rxmatch-after m))))
              (cons
                (format #f " (display ~S) ~D"
                        (rxmatch-before m) (n 1))
                (recur (rxmatch-after n))))
            (format #f " (display ~S))" page))))))


;; 入力ポートをinp に，出力ポートをoutp に設定してプログラムを始動する．
(define (process inp outp)
  (standard-output-port outp)
  (eval (read-from-string (embed-page (generate-page
          (parse (port->string inp)))))
        (interaction-environment)))

;; ヴァージョン情報を表示する．
(define (show-version)
  (display "Caries Ver.0.01
Copyright(C) 2012 Kazh. All Rights Reserved.

"))

;; コマンドの使用方法を表示する．
(define (show-help)
  (show-version)
  (display "利用可能引数:

-v, --version   バージョン情報を表示して終了します．
-h, --help      コマンドの使用方法を表示して終了します．
"))

;; ユニットテストを実行する．
(define (run-test)
  (test-start "Unit Test")
  ; TODO
  (test-end)
  )


(define (main args)
  (let/cc ret (let-args (cdr args)
    ((ver   "v|version" => (lambda () (show-version)  (ret)))
     (help  "h|help"    => (lambda () (show-help)     (ret)))
     (utes  "t|test"    => (lambda () (run-test)      (ret))))
     (process (standard-input-port) (standard-output-port))))
  0)

