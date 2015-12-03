(defmodule erldn_parser-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest integer ()  (check "1" 1))

(deftest integer-big () (check "1234" 1234))

(deftest float () (check "1.3" '1.3))

(deftest float-big () (check "1.234" 1.234))

(deftest bool-true () (check "true" 'true))

(deftest bool-false () (check "false" 'false))

(deftest bool-nil () (check "nil" 'nil))

(deftest string () (check "\"hello\"" #"hello"))

(deftest empty-string () (check "\"\"" #""))

(deftest empty-list () (check "()" '[]))

(deftest one-item-list () (check "(1)" '[1]))

(deftest two-item-list () (check "(1 true)" '[1 true]))

(deftest three-item-list () (check "(1 true nil)" '[1 true nil]))

(deftest two-item-list-with-commas () (check "(1, true)" '[1 true]))

(deftest three-item-list-with-commas () (check "(1, true nil)" '[1 true nil]))

(deftest nested-list () (check "(1, (true, nil), 1.2)" '[1 [true nil] 1.2]))

(deftest empty-vector () (check "[]" '#(vector [])))

(deftest one-item-vector () (check "[1]" '#(vector [1])))

(deftest two-item-vector () (check "[1 true]" '#(vector [1 true])))

(deftest three-item-vector () (check "[1 true nil]" '#(vector [1 true nil])))

(deftest two-item-vector-with-commas () (check "[1, true]" '#(vector [1 true])))

(deftest three-item-vector-with-commas ()
  (check "[1, true nil]" '#(vector [1 true nil])))

(deftest nested-vector ()
  (check "[1, (true, nil), 1.2]" '#(vector [1 [true nil] 1.2])))

(deftest empty-map () (check "{}" '#(map [])))

(deftest one-item-map () (check "{1 true}" '#(map [#(1 true)])))

(deftest two-item-map ()
  (check "{1 true, false nil}"
         '#(map [#(1 true) #(false nil)])))

(deftest three-item-map ()
  (check "{1 true, false nil, \"key\" 42}"
         '#(map [#(1 true) #(false nil) #(#"key" 42)])))

(deftest nested-map ()
  (check "{1 (true), false [nil], \"key\" #{42}}"
         '#(map [#(1      [true])
                 #(false  #(vector [nil]))
                 #(#"key" #(set [42]))])))

(deftest simple-symbol () (check "foo" '#(symbol foo)))

(deftest slash-symbol () (check "/" '#(symbol /)))

(deftest start-with-slash-symbol () (check "/foo" '#(symbol /foo)))

(deftest ns-symbol () (check "ns/foo" '#(symbol ns/foo)))

(deftest ns1-symbol () (check "org.quasiquoting/lfedn"
                              '#(symbol org.quasiquoting/lfedn)))

(deftest simple-keyword () (check ":foo" 'foo))

(deftest nil-keyword () (check ":nil" '#(keyword nil)))

(deftest stash-keyword () (check ":/" '/))

(deftest start-with-slash-keyword () (check ":/foo" '/foo))

(deftest ns-keyword () (check ":ns/foo" 'ns/foo))

(deftest ns1-keyword ()
  (check ":org.quasiquoting/lfedn" 'org.quasiquoting/lfedn))

(deftest simple-tag () (check "#answer 42" '#(tag answer 42)))

(deftest map-tag ()
  (check "#myapp/Person {:first \"John\" :last \"McCarthy\"}"
         '#(tag myapp/Person #(map [#(first #"John")
                                    #(last  #"McCarthy")]))))

(deftest instant ()
  (check "#inst \"1985-04-12T23:20:50.52Z\""
         '#(tag inst #"1985-04-12T23:20:50.52Z")))

(deftest uuid ()
  (check "#uuid \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\""
         '#(tag uuid #"f81d4fae-7dec-11d0-a765-00a0c91e6bf6")))

(deftest ignore-next () (check "#_ 4" '#(ignore 4)))

(deftest ignore-next-number-no-space ()  (check "#_4" '#(ignore 4)))

(deftest ignore-next-atom-no-space () (check "#_foo" '#(ignore #(symbol foo))))

(deftest char () (check "\\c" '#(char #\c)))

(deftest char1 () (check "\\D" '#(char #\D)))

(deftest char-newline () (check "\\newline" '#(char 10)))

(deftest char-tab () (check "\\tab" '#(char 9)))

(deftest char-space () (check "\\space" '#(char 32)))

(deftest char-return () (check "\\return" '#(char 13)))

(deftest comment () (check "{1 ; comment \n true}" '#(map [#(1 true)])))

(defun check (str expected)
  (let ((`#(ok ,result) (lfedn:parse-str str)))
    (is-equal expected result)))
