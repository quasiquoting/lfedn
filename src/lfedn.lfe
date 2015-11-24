(defmodule lfedn
  (doc "edn ⟷ lfe")
  (export (lex-str 1) (parse-str 1) (to-lfe 1) (to-lfe 2) (to-string 1)))

;;;===================================================================
;;; API
;;;===================================================================

(defun lex-str (str)
  "TODO: write docstring"
  (erldn_lexer:string str))

(defun parse-str (str)
  "Parse a given edn string into an LFE data structure, maintaining all detail."
  (case (lex-str str)
    (`#(ok ,tokens ,_)
     (case (erldn_parser:parse tokens)
       (`#(ok ,tree)             `#(ok ,tree))
       (`#(ok ,tree ,_warns)     `#(ok ,tree))
       (`#(error ,error)         `#(error ,error nil))
       (`#(error ,warns ,errors) `#(error ,errors ,warns))))
    (error error)))

(defun to-lfe (val)
  "Convert the result from [`parse-str/1`](#func-parse-str.2F1)
into an LFE-friendly version"
  (to-lfe val '[]))

(defun to-lfe
  "Like [`to-lfe/1`](#func-to-lfe.2F1), but given property list, `handlers`,
of the form `[#(tag (lambda (tag val other-handlers) ...)) ...]`
as the second argument, call `(handler tag val handlers)`
where `(= handler (proplists:get_value tag handlers))`."
  ([`#(char    ,char)  _]        (unicode:characters_to_binary `(,char) 'utf8))
  (['#(keyword nil)    _]        'nil)
  ([`#(vector  ,items) handlers] (to-lfe items handlers))
  ([`#(set     ,items) handlers] (sets:from_list (to-lfe items handlers)))
  ([`#(map     ,kvs)   handlers] (maps:from_list (kvs->lfe kvs handlers)))
  ([vals               handlers] (when (is_list vals))
   (lists:map (lambda (val) (to-lfe val handlers)) vals))
  ([`#(tag ,tag ,val)  handlers]
   (case (proplists:get_value tag handlers)
     ('undefined (throw `#(handler-not-found-for-tag ,tag)))
     (handler    (funcall handler tag val handlers))))
  ([val _] val))

(defun to-string (edn)
  "Convert the result from [`parse-str/1`](#func-parse-str.2F1)
into an edn string."
  (lists:reverse (to-string edn '[])))


;;;===================================================================
;;; Internal functions
;;;===================================================================

(defun to-string
  ([val acc] (when (is_binary val))
   `("\"" ,(escape-string (binary_to_list val)) "\"" . ,acc))
  ([`#(symbol  ,symbol)   acc] `(,(atom_to_list symbol)          . ,acc))
  (['#(keyword nil)       acc] `(":nil"                          . ,acc))
  ([`#(char   ,c)         acc] `(("\\" . (,c))                   . ,acc))
  ([`#(vector ,items)     acc] `("]" ,(items->string items)  "[" . ,acc))
  ([`#(set    ,items)     acc] `("}" ,(items->string items) "#{" . ,acc))
  ([`#(map    ,items)     acc] `("}" ,(kvs->string items)    "{" . ,acc))
  ([items acc] (when (is_list items)) `(")" ,(items->string items) "(" . ,acc))
  (['true      acc] `("true"  . ,acc))
  (['false     acc] `("false" . ,acc))
  (['nil acc] `("nil"   . ,acc))
  ([item acc] (when (is_atom item)) `(,(atom_to_list item) ":" . ,acc))
  ([`#(tag ,tag ,val) acc]
   `(,(to-string val) " " ,(atom_to_list tag) "#" . ,acc))
  ([val acc] `(,(lfe_io_pretty:term val) . ,acc)))

(defun escape-string (str)
  (lists:map
    (match-lambda
      ([#\\] '[#\\ #\\])
      ([#\"] '[#\\ #\"])
      ([10]  '[#\\ #\n])
      ([13]  '[#\\ #\r])
      ([9]   '[#\\ #\t])
      ([char]  char))
    str))

(defun kvs->lfe
  ([`#(,k ,v) handlers] `#(,(to-lfe k handlers) ,(to-lfe v handlers)))
  ([kvs handlers] (when (is_list kvs))
   (lists:map (lambda (kv) (kvs->lfe kv handlers)) kvs)))

(defun kvs->string
  ([`#(,k ,v)] (lists:map #'to-string/1 `(,k ,v)))
  ([kvs] (when (is_list kvs))
   (string:join (lists:flatmap #'kvs->string/1 kvs) " ")))

(defun items->string (items) (string:join (lists:map #'to-string/1 items) " "))
