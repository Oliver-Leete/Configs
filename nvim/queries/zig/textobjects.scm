;; "Classes"
(VarDecl 
  (_ (_ (ContainerDecl (ContainerMembers)? @function.inner)))) @function.outer

;; functions
(_ 
  (FnProto)
  (Block) @function.inner) @function.outer

;; loops
(_
  (ForPrefix)
  (_) @block.inner) @block.outer

(_
  (WhilePrefix)
  (_) @block.inner) @block.outer

;; blocks
(_ (Block) @block.inner) @block.outer

;; statements
(Statement) @statement.outer

;; parameters
((ParamDeclList 
  "," @_start . (ParamDecl) @parameter.inner)
 (#make-range! "parameter.outer" @_start @parameter.inner)) 
((ParamDeclList
  . (ParamDecl) @parameter.inner . ","? @_end)
 (#make-range! "parameter.outer" @parameter.inner @_end)) 

;; arguments
((FnCallArguments
  "," @_start . (_) @parameter.inner)
 (#make-range! "parameter.outer" @_start @parameter.inner)) 
((FnCallArguments
  . (_) @parameter.inner . ","? @_end)
 (#make-range! "parameter.outer" @parameter.inner @_end)) 

;; comments
(doc_comment) @comment.outer
(line_comment) @comment.outer

;; conditionals
(_
  (IfPrefix)
  (_) @block.inner) @block.outer

((SwitchExpr
  "{" @_start "}" @_end)
  (#make-range! "block.inner" @_start @_end))  @block.outer

;; calls
(_ (FnCallArguments) @call.inner) @call.outer
