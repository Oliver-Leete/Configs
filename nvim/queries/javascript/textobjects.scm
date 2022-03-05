; inherits: (jsx)
(function_declaration
  body: (statement_block) @function.inner) @function.outer

(export_statement
  (function_declaration) @function.outer) @function.outer.start

(arrow_function
  body: (_) @function.inner) @function.outer

(method_definition
  body: (statement_block) @function.inner) @function.outer

(class_declaration
  body: (class_body) @function.inner) @function.outer

(export_statement
  (class_declaration) @function.outer) @function.outer.start

(for_in_statement
  body: (_)? @block.inner) @block.outer

(while_statement
  body: (_)? @block.inner) @block.outer

(do_statement
  body: (_)? @block.inner) @block.outer

(if_statement
  consequence: (_)? @block.inner
  alternative: (_)? @block.inner) @block.outer

(switch_statement
  body: (_)? @block.inner) @block.outer

(call_expression) @call.outer
(call_expression (arguments) @call.inner)

;; blocks
(_ (statement_block) @block.inner) @block.outer

;; parameters
; function ({ x }) ...
; function ([ x ]) ...
; function (v = default_value)
(formal_parameters
  "," @_start .
  (_) @parameter.inner
 (#make-range! "parameter.outer" @_start @parameter.inner))
(formal_parameters
  . (_) @parameter.inner
  . ","? @_end
 (#make-range! "parameter.outer" @parameter.inner @_end))

; If the array/object pattern is the first parameter, treat its elements as the argument list
(formal_parameters 
  . (_ 
    [(object_pattern "," @_start .  (_) @parameter.inner)
    (array_pattern "," @_start .  (_) @parameter.inner)]
    ) 
 (#make-range! "parameter.outer" @_start @parameter.inner))
(formal_parameters 
  . (_ 
    [(object_pattern . (_) @parameter.inner . ","? @_end)
    (array_pattern . (_) @parameter.inner . ","? @_end)]
    )
 (#make-range! "parameter.outer" @parameter.inner @_end))


;; arguments
(arguments
  "," @_start .
  (_) @parameter.inner
 (#make-range! "parameter.outer" @_start @parameter.inner))
(arguments
  . (_) @parameter.inner
  . ","? @_end
 (#make-range! "parameter.outer" @parameter.inner @_end))
