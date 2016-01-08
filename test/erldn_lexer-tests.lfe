(defmodule erldn_lexer-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest integer ()  (check "1" '#(integer 1 1)))

(deftest integer-big () (check "1234" '#(integer 1 1234)))

(deftest float () (check "1.3" '#(float 1 1.3)))

(deftest float-big () (check "1.234" '#(float 1 1.234)))

(deftest bool-true () (check "true" '#(boolean 1 true)))

(deftest bool-false () (check "false" '#(boolean 1 false)))

(deftest bool-nil () (check "nil" '#(nil 1 nil)))

(deftest string () (check "\"hello\"" '#(string 1 #"hello")))

(deftest empty-string () (check "\"\"" '#(string 1 #"")))

(deftest sharp () (check "#" '#(sharp 1 |#|)))

(deftest simple-symbol () (check "foo" '#(symbol 1 foo)))

(deftest slash-symbol () (check "/" '#(symbol 1 /)))

(deftest start-with-slash-symbol () (check "/foo" '#(symbol 1 /foo)))

(deftest ns-keyword () (check ":ns/foo" '#(keyword 1 ns/foo)))

(deftest ns1-keyword ()
  (check ":org.quasiquoting/lfedn" '#(keyword 1 org.quasiquoting/lfedn)))

(deftest char () (check "\\c" '#(char 1 #\c)))

(deftest char1 () (check "\\D" '#(char 1 #\D)))

(deftest char2 () (check "\\$" '#(char 1 #\$)))

(deftest char-newline () (check "\\newline" '#(char 1 10)))

(deftest char-tab () (check "\\tab" '#(char 1 9)))

(deftest char-space () (check "\\space" '#(char 1 32)))

(deftest char-return () (check "\\return" '#(char 1 13)))

(deftest char-in-a-string () (check "\"hi \\c !\"" '#(string 1 #"hi \\c !")))

(deftest ignore-token () (check "#_" '#(ignore 1 |#_|)))

(deftest comment (check "1 ; this is a comment \n" '#(integer 1 1)))

(deftest comment-inside-string ()
  (check "\"; this is NOT a comment\n\""
         '#(string 1 #"; this is NOT a comment\n")))

(defun check (str expected)
  (let ((`#(ok [,result] ,_) (lfedn:lex-str str)))
    (is-equal expected result)))
