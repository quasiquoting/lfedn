(defmodule unit-lfedn-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest char->string () (check '#(char #\$) "\\$"))

(deftest nil-keyword->string () (check '#(keyword nil) ":nil"))

(deftest keyword->string () (check 'foo ":foo"))

(deftest true->string () (check 'true "true"))

(deftest false->string () (check 'false "false"))

(deftest nil->string () (check 'nil "nil"))

(deftest integer->string () (check 42 "42"))

(deftest float->string () (check 42.4 "42.4"))

(deftest string->string () (check #"hello" "\"hello\""))

(deftest string-with-escapes->string () (check #"h\n\r\"\\ello"
                                               "\"h\\n\\r\\\"\\\\ello\""))

(deftest symbol->string () (check '#(symbol foo) "foo"))

(deftest tagged-value->string () (check '#(tag foo 42) "#foo 42"))

(deftest empty-vector->string () (check '#(vector []) "[]"))

(deftest empty-list->string () (check '() "()"))

(deftest empty-set->string () (check '#(set []) "#{}"))

(deftest empty-map->string () (check '#(map []) "{}"))

(deftest singleton-map->string () (check '#(map [#(true 1)]) "{true 1}"))

(deftest singleton-vector->string () (check '#(vector [1]) "[1]"))

(deftest singleton-list->string () (check '[1] "(1)"))

(deftest singleton-set->string () (check '#(set [1]) "#{1}"))

(deftest vector->string () (check '#(vector [1 foo nil]) "[1 :foo nil]"))

(deftest list->string () (check '[1 foo nil] "(1 :foo nil)"))

(deftest set->string () (check '#(set [1 foo nil]) "#{1 :foo nil}"))

(deftest map->string () (check '#(map [#(1 foo) #(2 bar) #(3 nil)])
                               "{1 :foo 2 :bar 3 nil}"))

(deftest char->lfe () (ctl '#(char #\a) #"a"))

(deftest integer->lfe () (ctl 41 41))

(deftest float->lfe () (ctl 41.2 41.2))

(deftest keyword->lfe () (ctl 'foo 'foo))

(deftest nil->lfe () (ctl 'nil 'nil))

(deftest true->lfe () (ctl 'true 'true))

(deftest false->lfe () (ctl 'false 'false))

(deftest string->lfe () (ctl #"asd" #"asd"))

(deftest list->lfe () (ctl '[] '[]))

(deftest list1->lfe () (ctl '[1] '[1]))

(deftest list2->lfe () (ctl '[1 foo] '[1 foo]))

(deftest list-nested->lfe () (ctl '[1 #(char #\a)] '[1 #"a"]))

(deftest vector->lfe () (ctl '#(vector []) '[]))

(deftest vector1->lfe ()(ctl '#(vector [1]) '[1]))

(deftest vector2->lfe () (ctl '#(vector [1 foo]) '[1 foo]))

(deftest vector-nested->lfe () (ctl '#(vector [1 #(char #\a)]) '[1 #"a"]))

(deftest set-nested->lfe () (ctl '#(set [1 #(char #\a) 1]) '[1 #"a"]
                                 #'sets:to_list/1))

(deftest map->lfe () (ctl '#(map [#(1 #(char #\a))]) '[#(1 #"a")]
                          #'maps:to_list/1))

(deftest nil-keyword->lfe () (ctl '#(keyword nil) 'nil))

(deftest symbol->lfe () (ctl '#(symbol foo) '#(symbol foo)))

(deftest tag->lfe ()
  (let ((result (lfedn:to-lfe '#(tag foo 42)
                              `[#(foo ,(lambda (_ val _) (+ 1 val)))])))
    (is-equal 43 result)))

(deftest unknown-tag-raises-in-to-lfe ()
  (try
      (progn (lfedn:to-lfe '#(tag bar 42)
                           `[#(foo ,(lambda (_ val _) (+ val 1)))])
             (throw 'should-fail))
    (catch (`#(,_ #(handler-not-found-for-tag bar) ,_) 'ok))))

(defun check (val str)
  (let ((result (lists:flatten (lfedn:to-string val))))
    (is-equal str result)))

(defun ctl (val expected)
  (let ((result (lfedn:to-lfe val)))
    (is-equal expected result)))

(defun ctl (val expected transformer)
  (let ((result (funcall transformer (lfedn:to-lfe val))))
    (is-equal expected result)))
