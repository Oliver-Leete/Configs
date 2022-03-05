[
  (loop_generate_construct)
  (loop_statement)
] @block.outer

[
  (conditional_statement)
  (case_item)
] @block.outer

(comment) @comment.outer

(function_declaration) @function.outer

(always_construct) @block.outer

[
  (module_declaration)
] @function.outer
