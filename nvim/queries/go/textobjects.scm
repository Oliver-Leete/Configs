;; function textobject
(function_declaration
  body: (block)? @function.inner) @function.outer

;; function literals
(func_literal
	(_)? @function.inner) @function.outer

;; method as function textobject
(method_declaration
  body: (block)? @function.inner) @function.outer

;; struct and interface declaration as class textobject?
(type_declaration
    (type_spec (type_identifier) (struct_type (field_declaration_list (_)?) @function.inner))) @function.outer

(type_declaration
  (type_spec (type_identifier) (interface_type (method_spec_list (_)?) @function.inner))) @function.outer

;; struct literals as class textobject
(composite_literal
  (type_identifier)?
  (struct_type (_))?
  (literal_value (_)) @function.inner) @function.outer

;; conditionals
(if_statement
  alternative: (_ (_) @block.inner)?) @block.outer

(if_statement
  consequence: (block)? @block.inner)

(if_statement
  condition: (_) @block.inner)

;; loops
(for_statement
  body: (block)? @block.inner) @block.outer

;; blocks
(_ (block) @block.inner) @block.outer

;; statements
(block (_) @statement.outer)

;; comments
(comment) @comment.outer

;; calls
(call_expression (_)? @call.inner) @call.outer

;; parameters
(parameter_list
  "," @_start .
  (parameter_declaration) @parameter.inner
 (#make-range! "parameter.outer" @_start @parameter.inner))
(parameter_list
  . (parameter_declaration) @parameter.inner
  . ","? @_end
 (#make-range! "parameter.outer" @_start @parameter.inner))

(parameter_declaration
  (identifier)
  (identifier) @parameter.inner)

(parameter_declaration
  (identifier) @parameter.inner
  (identifier))

(parameter_list
  "," @_start .
  (variadic_parameter_declaration) @parameter.inner
 (#make-range! "parameter.outer" @_start @parameter.inner))

;; arguments
(argument_list
  "," @_start .
  (_) @parameter.inner
 (#make-range! "parameter.outer" @_start @parameter.inner))
(argument_list
  . (_) @parameter.inner
  . ","? @_end
 (#make-range! "parameter.outer" @_start @parameter.inner))
