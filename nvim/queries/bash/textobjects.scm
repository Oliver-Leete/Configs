(function_definition
  (_) @function.inner ) @function.outer

(case_statement) @block.outer

(if_statement
  (_) @block.inner ) @block.outer

(for_statement
 (_) @loop.inner ) @loop.outer
(while_statement
  (_) @loop.inner ) @loop.outer

(comment) @comment.outer
