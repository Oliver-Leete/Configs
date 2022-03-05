;; @functions
(
  [
    (method
      . name: (identifier) 
      !parameters 
      . (_) @_start (_) @_end .)

    (method
      . name: (identifier) 
      . parameters: (method_parameters) 
      . (_) @_start (_) @_end .)

    (singleton_method
      . object: (_)
      . name: (identifier) 
      !parameters 
      . (_) @_start (_) @_end .)

    (singleton_method
      . object: (_)
      . name: (identifier) 
      . parameters: (method_parameters) 
      . (_) @_start (_) @_end .)
  ] @function.outer
   (#make-range! "function.inner" @_start @_end))

[
  (method
    . name: (identifier) 
    !parameters 
    . (_) @function.inner .)

  (method
    . name: (identifier) 
    . parameters: (method_parameters) 
    . (_) @function.inner .)

  (singleton_method
    . object: (_)
    . name: (identifier) 
    !parameters 
    . (_) @function.inner .)

  (singleton_method
    . object: (_)
    . name: (identifier) 
    . parameters: (method_parameters) 
    . (_) @function.inner .)
] @function.outer

;; @blocks
(
  [
    (call
      block: (_
        !parameters
        . (_) @_start (_) @_end .)
      )

    (call
      block: (_
        . parameters: (block_parameters)
        . (_) @_start (_) @_end .)
      )
  ] @block.outer
   (#make-range! "block.inner" @_start @_end))

[
  (call
    block: (_
      !parameters
      . (_) @block.inner .))

  (call
    block: (_
      . parameters: (block_parameters)
      . (_) @block.inner .))
] @block.outer

((lambda
  body: (_
    . (_) @_start (_) @_end .)
  ) @block.outer
 (#make-range! "block.inner" @_start @_end))

(lambda
  body: (_
    . (_) @block.inner .) @block.outer)

;; calls
(call
  method: (_) @call.inner ) @call.outer

;; classes
(
  [
    (class
      . name: (_) 
      . superclass: (_)
      . (_) @_start (_) @_end .)

    (class
      . name: (_) 
      !superclass 
      . (_) @_start (_) @_end .)

    (module
      . name: (_) 
      . (_) @_start (_) @_end .)

    (singleton_class
      . value: (_) 
      . (_) @_start (_) @_end .)
  ] @function.outer
 (#make-range! "function.inner" @_start @_end))

[
  ;; match against classes with and without parrents
  (class
    . name: (_)
    . superclass: (_)
    . (_) @function.inner .)

  (class
    . name: (_)
    !superclass
    . (_) @function.inner .)

  (module
    . name: (_)
    . (_) @function.inner .)

  (singleton_class
    . value: (_)
    . (_) @function.inner .)
] @function.outer

;; comments
(comment) @comment.outer

;; conditionals
[
  (if
   consequence: (_) @block.inner
   alternative: (_) @block.inner)

  (if_modifier
    condition: (_) @block.inner)

  (until_modifier
     condition: (_) @block.inner)

  (unless
   consequence: (_) @block.inner
   alternative: (_) @block.inner)

  (case
    value: (_)
    (_) @block.inner)
]  @block.outer

;; loops
[
  (while
    body: (_) @block.inner)

  (while_modifier
    condition: (_) @block.inner)

  (until
    body: (_) @block.inner)

  (until_modifier
    condition: (_) @block.inner)

  (for
    body: (_) @block.inner)
] @block.outer

;; parameters
[
 (block_parameters (_) @parameter.inner)

 (method_parameters (_) @parameter.inner)

 (lambda_parameters (_) @parameter.inner)
] @parameter.outer
