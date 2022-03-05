(function_definition 
 body: (compound_statement) @function.inner) @function.outer

(method_declaration
  body: (compound_statement) @function.inner) @function.outer

(class_declaration
  body: (declaration_list) @function.inner) @function.outer

(foreach_statement
  body: (_)? @block.inner) @block.outer

(while_statement
  body: (_)? @block.inner) @block.outer

(do_statement
  body: (_)? @block.inner) @block.outer

(switch_statement
  body: (_)? @block.inner) @block.outer

;;blocks
(_ (switch_block) @block.inner) @block.outer

;; parameters
(formal_parameters
  (simple_parameter) @parameter.inner)

(arguments
  (argument) @parameter.inner)
