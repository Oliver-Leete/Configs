;; extends

; Tests
(macrocall_expression
   (macro_identifier (identifier) @macro_name)
   (#match? @macro_name "testitem")
   (macro_argument_list (string_literal) @test.name
   (compound_statement (_)) @test.inner)
) @test.outer

; key/value

(binary_expression
  (_) @key.inner
  (_) @pairopp
  (#eq? @pairopp "=>")
  (_) @value.inner
) @key.outer @value.outer
