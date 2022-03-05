(class_declaration
  body: (class_body) @function.inner) @function.outer

(method_declaration
  body: (_) @function.inner) @function.outer

(constructor_declaration) @function.outer
(constructor_body) @function.inner

(for_statement
  body: (_)? @block.inner) @block.outer

(enhanced_for_statement
  body: (_)? @block.inner) @block.outer

(while_statement
  body: (_)? @block.inner) @block.outer

(do_statement
  body: (_)? @block.inner) @block.outer

(if_statement
  condition: (_ (parenthesized_expression) @block.inner)  @block.outer)

(if_statement
  consequence: (_)? @block.inner
  alternative: (_)? @block.inner
  ) @block.outer

(switch_expression
  body: (_)? @block.inner) @block.outer

;; blocks
(block) @block.outer


(method_invocation) @call.outer
(method_invocation (argument_list) @call.inner)

;; parameters
(formal_parameters    
  "," @_start .
  (formal_parameter) @parameter.inner
 (#make-range! "parameter.outer" @_start @parameter.inner))
(formal_parameters    
  . (formal_parameter) @parameter.inner
  . ","? @_end
 (#make-range! "parameter.outer" @_start @parameter.inner))

