(class_declaration 
  body: (declaration_list) @function.inner) @function.outer

(struct_declaration
  body: (declaration_list) @function.inner) @function.outer

(method_declaration
  body: (block) ? @function.inner) @function.outer

(lambda_expression 
  body: (_) @function.inner) @function.outer

;; loops
(for_statement
  body: (_) @block.inner) @block.outer

(for_each_statement
  body: (_) @block.inner) @block.outer

(do_statement
  (block) @block.inner) @block.outer

(while_statement
  (block) @block.inner) @block.outer

;; conditionals
(if_statement
  consequence: (_)? @block.inner
  alternative: (_)? @block.inner) @block.outer

(switch_statement
  body: (switch_body) @block.inner) @block.outer

;; calls
(invocation_expression 
  (argument_list) @call.inner) @call.outer

;; blocks
(_ (block) @block.inner) @block.outer

;; parameters
((parameter_list
  "," @_start . (parameter) @parameter.inner)
 (#make-range! "parameter.outer" @_start @parameter.inner)) 

((parameter_list
  . (parameter) @parameter.inner . ","? @_end)
 (#make-range! "parameter.outer" @parameter.inner @_end)) 

((argument_list
  "," @_start . (argument) @parameter.inner)
 (#make-range! "parameter.outer" @_start @parameter.inner)) 

((argument_list
  . (argument) @parameter.inner . ","? @_end)
 (#make-range! "parameter.outer" @parameter.inner @_end)) 

;; comments
(comment) @comment.outer
