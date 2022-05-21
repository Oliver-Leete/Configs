((module_definition
   name: (identifier) @module-name) @scope-root)

((struct_definition
   name: (identifier) @class-name) @scope-root)

((function_definition
   name: (identifier) @function-name) @scope-root)

((macro_definition
   name: (identifier) @macro-name) @scope-root)

((assignment_expression
   (call_expression
    (identifier) @function-name)) @scope-root)

((assignment_expression
   (typed_expression
    (identifier) @container-name)) @scope-root)

((assignment_expression
   (identifier) @container-name) @scope-root)

(call_expression
  (identifier)
  (argument_list
      (named_argument
        (identifier) @container-name
        (identifier)) @scope-root))
