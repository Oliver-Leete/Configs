(function_definition) @function.outer

(for_statement
  (_) @block.inner) @block.outer

(while_statement
  condition: (command)
  (command) @block.inner) @block.outer

(if_statement
  (command) @block.inner) @block.outer

(switch_statement 
  (_) @block.inner) @block.outer

(comment) @comment.outer
