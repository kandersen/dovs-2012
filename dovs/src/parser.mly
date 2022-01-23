/* File parser.mly */
%{

  let make_identifier pos i =
    { Ast.identifier_pos=pos; Ast.identifier=i }

  let make_type_decl pos t =
    { Ast.type_decl_pos=pos; Ast.type_decl=t }

  let make_decl pos d =
    { Ast.decl_pos=pos; Ast.decl=d }

  let make_name pos n =
    { Ast.name_pos=pos; Ast.name=n }

  let make_typeexp pos t =
    { Ast.typeexp_pos=pos; Ast.typeexp=t }

  let make_stm pos s =
    { Ast.stm_pos=pos; Ast.stm=s }

  let make_exp pos e =
    { Ast.exp_pos=pos; Ast.exp=e }

  let make_lvalue pos l =
    { Ast.lvalue_pos=pos; Ast.lvalue=l }

  let make_import_decl pos id =
    { Ast.import_decl_pos=pos; Ast.import_decl=id }

  let make_field npos (access,static,final,texp,name,init) =
    { Ast.field_access = access;
      Ast.field_static = static;
      Ast.field_final  = final;
      Ast.field_type   = texp;
      Ast.field_name   = make_identifier npos name;
      Ast.field_init   = init }

  let make_method npos (access,static,final,abs,texp,name,formals,throws,body) =
    { Ast.method_access   = access;
      Ast.method_static   = static;
      Ast.method_final    = final;
      Ast.method_abstract = abs;
      Ast.method_result   = texp;
      Ast.method_name     = make_identifier npos name;
      Ast.method_formals  = formals;
      Ast.method_throws   = throws;
      Ast.method_body     = body }

  let make_constructor npos (access,name,formals,throws,body) =
    { Ast.constructor_access  = access;
      Ast.constructor_name    = make_identifier npos name;
      Ast.constructor_formals = formals;
      Ast.constructor_throws  = throws;
      Ast.constructor_body    = body }

  let make_class (final,abs,name,exts,impl,decls) =
    { Ast.class_final      = final;
      Ast.class_abstract   = abs;
      Ast.class_name       = name;
      Ast.class_extends    = exts;
      Ast.class_implements = impl;
      Ast.class_members    = decls }

  let make_interface (name,exts,decls) =
    { Ast.interface_name       = name;
      Ast.interface_extends    = exts;
      Ast.interface_members    = decls }

  let make_source_file pos (pack,imps,decl) =
    let file_name = pos.Lexing.pos_fname in
    { Ast.source_file_name    = file_name;
      Ast.source_file_package = pack;
      Ast.source_file_imports = imps;
      Ast.source_file_decl    = decl }

  let make_desugared_for_loop startpos for_init_var while_inner_raw while_inner_raw_pos for_update for_update_pos condition condition_pos = 
    let for_update_var = make_stm for_update_pos (Ast.Block [for_update]) in
    let while_inner_body =
      match while_inner_raw with
	| { Ast.stm = Ast.Block _ } ->
	  while_inner_raw
	| _ -> 
	  make_stm while_inner_raw_pos (Ast.Block [while_inner_raw]) in
    let while_body = make_stm while_inner_raw_pos (Ast.Block [while_inner_body;for_update_var]) in
    let while_exp = make_stm condition_pos (Ast.While(condition,while_body)) in
    make_stm startpos (Ast.Block [for_init_var; while_exp])

  let rec make_tarray_expression startpos base_typeexp dims = 
    match dims with
    | 0 -> base_typeexp
    | _ -> make_typeexp startpos (Ast.TArray (make_tarray_expression startpos base_typeexp (dims - 1)))
 
      
%}
%token EOF
/* Keywords */
%token ABSTRACT
%token BOOLEAN
%token BYTE
%token CHAR
%token CLASS
%token ELSE
%token EXTENDS
%token FINAL
%token FOR
%token IF
%token IMPLEMENTS
%token IMPORT
%token INSTANCEOF
%token INT
%token INTERFACE
%token NEW
%token PACKAGE
%token PROTECTED
%token PUBLIC
%token RETURN
%token SHORT
%token STATIC
%token SUPER
%token THIS
%token THROW
%token THROWS
%token VOID
%token WHILE

%token <string>KEYWORD

%token TRUE FALSE
%token NULL

/* Delimiters */
%token L_PAREN R_PAREN
%token L_BRACE R_BRACE
%token L_BRACKET R_BRACKET
%token SEMICOLON
%token COMMA
%token DOT

/* Assignment and logic */
%token ASSIGN
%token COMPLEMENT
%token AND_AND OR_OR

/* Comparison */
%token LT GT EQ
%token LTEQ GTEQ NEQ

/* Arithmetic */
%token PLUS MINUS STAR DIV MOD
%token AND OR XOR
%token PLUS_PLUS MINUS_MINUS

/* Literals and identifiers */
%token <string>INTEGER_LITERAL
%token <string>CHAR_LITERAL
%token <string>STRING_LITERAL
%token <string>IDENTIFIER

%start <Ast.source_file> goal    /* the entry point */
%%

goal :  opt_package_declaration import_declaration* type_declaration EOF
        { make_source_file $startpos ($1,$2,$3) }
;

/* ********** Package and imports *********** */

opt_package_declaration
  :  
     { None }
  |  PACKAGE name SEMICOLON
     { Some $2 }
  ;

import_declaration
  :  IMPORT name DOT STAR SEMICOLON
    { make_import_decl $startpos (Ast.OnDemand($2)) }
  |  IMPORT name DOT IDENTIFIER SEMICOLON
    { make_import_decl  $startpos
	(Ast.Single($2,make_identifier $startpos($4) $4)) }
  ;

/* ********** Type declarations *********** */

class_extends_decl
  :
     { make_name $startpos
         (Ast.Qualified [make_identifier $startpos "java";
                         make_identifier $startpos "lang";
                         make_identifier $startpos "Object"]) }
  |  EXTENDS name
     { $2 }
  ;

implements_decl
  :
     { [] }
  |  IMPLEMENTS typename_list
     { List.rev $2 }
  ;

typename_list
  :  name
     { [$1] }
  |  typename_list COMMA name
     { $3 :: $1 }
  ;

type_declaration
  :  class_declaration
     { make_type_decl $startpos (Ast.Class($1)) }
  |  interface_declaration
     { make_type_decl $startpos (Ast.Interface($1)) }
  ;

class_declaration
  :  PUBLIC FINAL? opt_abstract CLASS IDENTIFIER class_extends_decl implements_decl class_body
  { make_class ($2 <> None,$3,make_identifier $startpos $5,$6,$7,$8) }
  ;

class_body
  :  L_BRACE member_declaration* R_BRACE
     { $2 }
  ;

member_declaration
  :  method_declaration
     { make_decl $startpos (Ast.Method($1)) }
  |  constructor_declaration
     { make_decl $startpos (Ast.Constructor($1)) }
  | field_declaration
      { make_decl $startpos (Ast.Field($1)) }
  ;

access
  :  PUBLIC
     { Ast.Public }
  |  PROTECTED
     { Ast.Protected }
  ;

/* ********** Interface declarations *********** */

interface_declaration
  : PUBLIC INTERFACE IDENTIFIER interface_extends_decl interface_members
  { let ext_decl = List.rev $4 in
    make_interface (make_identifier $startpos $3, ext_decl, $5) }
  ;

interface_extends_decl
  :
   { [] }
  | EXTENDS name
     { [$2] }
  | interface_extends_decl COMMA name
     { $3 :: $1 } 
  ;

interface_members
  :  L_BRACE interface_member_declaration* R_BRACE
     { $2 }
  ;

interface_member_declaration
  :  interface_method_declaration
     { make_decl $startpos (Ast.Method($1)) }
  | interface_field_declaration
      { make_decl $startpos (Ast.Field($1)) }
  ;

interface_field_declaration
  : opt_interface_access FINAL STATIC typeexp IDENTIFIER ASSIGN expression SEMICOLON
    { make_field $startpos($5) ($1,true,true,$4,$5,Some $7) }
  | opt_interface_access STATIC FINAL typeexp IDENTIFIER ASSIGN expression SEMICOLON
    { make_field $startpos($5) ($1,true,true,$4,$5,Some $7) }
;

interface_method_declaration
  :  opt_interface_access typeexp IDENTIFIER method_params throws_clause SEMICOLON
      { make_method $startpos($3) ($1,false,false,true,$2,$3,$4,$5,None) }
  |  opt_interface_access ABSTRACT typeexp IDENTIFIER method_params throws_clause SEMICOLON
      { make_method $startpos($4) ($1,false,false,true,$3,$4,$5,$6,None) }
  |  opt_interface_access VOID IDENTIFIER method_params throws_clause SEMICOLON
      { 
	let void = make_typeexp $startpos (Ast.Base Ast.Void) in
	make_method $startpos($3) ($1,false,false,true,void,$3,$4,$5,None) }
  |  opt_interface_access ABSTRACT VOID IDENTIFIER method_params throws_clause SEMICOLON
      { 	
	let void = make_typeexp $startpos (Ast.Base Ast.Void) in
	make_method $startpos($4) ($1,false,false,true,void,$4,$5,$6,None) }
;

opt_interface_access
  : 
    { Ast.Public }
  | PUBLIC
    { Ast.Public }


/* ********** Method declarations ********** */

method_declaration
  :  access modifiers typeexp IDENTIFIER method_params throws_clause opt_method_body
      { let (stat, fin) = $2 in
        make_method $startpos($4) ($1,stat,fin,false,$3,$4,$5,$6,$7) }
  |  access typeexp IDENTIFIER method_params throws_clause opt_method_body
      { make_method $startpos($3) ($1,false,false,false,$2,$3,$4,$5,$6) }
  |  access modifiers VOID IDENTIFIER method_params throws_clause opt_method_body
      { let (stat, fin) = $2 in
	let void = make_typeexp $startpos (Ast.Base Ast.Void) in
	make_method $startpos($4) ($1,stat,fin,false,void,$4,$5,$6,$7) }
  |  access VOID IDENTIFIER method_params throws_clause opt_method_body
      {let void = make_typeexp $startpos (Ast.Base Ast.Void) in
     make_method $startpos($3) ($1,false,false,false,void,$3,$4,$5,$6) }
  |  access ABSTRACT typeexp IDENTIFIER method_params throws_clause opt_method_body
      { make_method $startpos($4) ($1,false,false,true,$3,$4,$5,$6,$7) }
  |  access ABSTRACT VOID IDENTIFIER method_params throws_clause opt_method_body
      {let void = make_typeexp $startpos (Ast.Base Ast.Void) in
     make_method $startpos($4) ($1,false,false,true,void,$4,$5,$6,$7) }
;

modifiers
  : FINAL
    {false, true}
  | STATIC
    {true, false}
  | FINAL STATIC 
      {true, true}
  | STATIC FINAL
      {true, true}
;

opt_abstract
  : 
     { false }
  |  ABSTRACT
     { true }
  ;

method_params
  :  L_PAREN formal_parameter_list R_PAREN
     { $2 }
  ;

formal_parameter_list
  :  
     { [] }
  |  formal_parameter_list_nonempty
     { List.rev $1 }
  ;

formal_parameter_list_nonempty
  :  formal_parameter
     { [$1] }
  |  formal_parameter_list_nonempty COMMA formal_parameter
     { $3 :: $1 }
  ;

formal_parameter
  :  typeexp IDENTIFIER
     { ($1,make_identifier $startpos($2) $2) }
  ;

throws_clause
  :
     { [] }
  |  THROWS typename_list
     { List.rev $2 }
  ;

method_body
  :  block
     { $1 }
  ;

opt_method_body
  :  SEMICOLON
     { None }
  |  method_body
     { Some $1 }
  ;

/* ********** Constructor declarations ********** */

constructor_declaration
  :  access IDENTIFIER method_params throws_clause constructor_body
     { make_constructor $startpos($2) ($1,$2,$3,$4,$5) }
  ;

constructor_body
  :  L_BRACE statement_or_declaration* R_BRACE
     { $2 }
  |  L_BRACE explicit_constructor_invocation statement_or_declaration* R_BRACE
     { $2 :: $3 }
  ;

explicit_constructor_invocation
  : THIS L_PAREN argument_list R_PAREN SEMICOLON
    { make_stm $startpos (Ast.ThisCall $3) }
  | SUPER L_PAREN argument_list R_PAREN SEMICOLON
    { make_stm $startpos (Ast.SuperCall $3) }
;      


/* ********** Field declarations ********** */

field_declaration
  :  access typeexp IDENTIFIER SEMICOLON
    { make_field $startpos($3) ($1,false,false,$2,$3,None) }
  | access typeexp IDENTIFIER ASSIGN expression SEMICOLON
    { make_field $startpos($3) ($1,false,false,$2,$3,Some $5)}
  | access modifiers typeexp IDENTIFIER SEMICOLON
    { let (static, final) = $2 in 
      make_field $startpos($4) ($1,static,final,$3,$4,None)}
  | access modifiers typeexp IDENTIFIER ASSIGN expression SEMICOLON
    { let (static, final) = $2 in 
      make_field $startpos($4) ($1,static,final,$3,$4,Some $6)}
;


/* ********** Types ********** */

typeexp
  :  primitive_type
     { $1 }
  |  name
     { make_typeexp $startpos (Ast.Named $1) }
  |  array_type
     { $1 }
  ;

complex_type
  :  name
     { make_typeexp $startpos (Ast.Named $1) }
  |  array_type
     { $1 }
;

element_type
  :  primitive_type
     { $1 }
  |  name
     { make_typeexp $startpos (Ast.Named $1) }
  ;

array_type 
  :  primitive_type dims
     { make_tarray_expression $startpos $1 $2}
  |  name dims
     { make_tarray_expression $startpos (make_typeexp $startpos (Ast.Named $1)) $2 }
  ;

dims
  : L_BRACKET R_BRACKET
   { 1 }
  | dims L_BRACKET R_BRACKET
    { $1 + 1 }
;

primitive_type
  : primitive_type_inner
     { make_typeexp $startpos (Ast.Base $1) }
  ;

primitive_type_inner
  :  BOOLEAN
     { Ast.Boolean }
  |  BYTE
     { Ast.Byte }
  |  SHORT
     { Ast.Short }
  |  CHAR
     { Ast.Char }
  |  INT
     { Ast.Int }
  ;

/* ********** Blocks and statements ********** */

block
  :  L_BRACE statement_or_declaration* R_BRACE
     { $2 }
  ;

local_variable_declaration
  :  typeexp IDENTIFIER opt_variable_initializer
     { ($1,make_identifier $startpos($2) $2,$3) }
  ;

opt_variable_initializer
  :
     { None }
  |  ASSIGN expression
     { Some $2 }
  ;

statement_or_declaration
  :  statement
     { $1 }
  |  local_variable_declaration_statement
     { $1 }
  ;

statement
  :  statement_without_trailing_substatement
     { $1 }
  |  if_then_statement
     { $1 }
  |  if_then_else_statement
     { $1 }
  |  while_statement
     { $1 }
  |  for_statement
     { $1 }
  ;

statement_no_short_if
  :  statement_without_trailing_substatement
     { $1 }
  |  if_then_else_statement_no_short_if
     { $1 }
  |  while_statement_no_short_if
     { $1 }
  |  for_statement_no_short_if
     { $1 }
  ;

statement_without_trailing_substatement
  :  block
     { make_stm $startpos (Ast.Block $1) }
  |  empty_statement
     { make_stm $startpos (Ast.Empty) }
  |  expression_statement
     { $1 }
  |  return_statement
     { $1 }
  |  throw_statement
     { $1 }
  ;

local_variable_declaration_statement
  :  local_variable_declaration SEMICOLON
     { let (ty,id,exp) = $1 in
       make_stm $startpos (Ast.LocalDecl (ty,id,exp)) }
  ;

return_statement
  :  RETURN SEMICOLON
     { make_stm $startpos (Ast.VoidReturn) }
  |  RETURN expression SEMICOLON
     { make_stm $startpos (Ast.ValueReturn $2) }
  ;

throw_statement
  : THROW expression SEMICOLON
     { make_stm $startpos (Ast.Throw $2) }
;  

empty_statement
  :  SEMICOLON
     { make_stm $startpos (Ast.Empty) }
  ;

expression_statement
  :  statement_expression SEMICOLON
     { make_stm $startpos (Ast.Exp $1) }
  ;

if_then_statement
  :  IF L_PAREN expression R_PAREN statement
     { make_stm $startpos (Ast.IfThen($3,$5)) }
  ;

if_then_else_statement
  :  IF L_PAREN expression R_PAREN statement_no_short_if ELSE statement
     { make_stm $startpos (Ast.IfThenElse($3,$5,$7)) }
  ;

if_then_else_statement_no_short_if
  :  IF L_PAREN expression R_PAREN statement_no_short_if ELSE statement_no_short_if
     { make_stm $startpos (Ast.IfThenElse($3,$5,$7)) }
  ;

while_statement
  :  WHILE L_PAREN expression R_PAREN statement
     { make_stm $startpos (Ast.While($3,$5)) }
  ;

while_statement_no_short_if
  :  WHILE L_PAREN expression R_PAREN statement_no_short_if
     { make_stm $startpos (Ast.While($3,$5)) }
  ;

for_statement
  :  FOR L_PAREN for_init SEMICOLON for_condition SEMICOLON for_update R_PAREN statement 
    {  make_desugared_for_loop $startpos $3 $9 $startpos($9) $7 $startpos($7) $5 $startpos($5) }
  ;

for_statement_no_short_if
  :  FOR L_PAREN for_init SEMICOLON for_condition SEMICOLON for_update R_PAREN statement_no_short_if 
    { make_desugared_for_loop $startpos $3 $9 $startpos($9) $7 $startpos($7) $5 $startpos($5) }	       
  ;

for_init
  : statement_expression
    { make_stm $startpos (Ast.Exp $1) }
  | local_variable_declaration
    { let (ty,id,exp) = $1 in
       make_stm $startpos (Ast.LocalDecl (ty,id,exp)) }
  | 
    { make_stm $startpos (Ast.Empty) }
;

for_condition
  : 
    { make_exp $startpos (Ast.BooleanConst true) }
  | expression
    { $1 }
;

for_update
  : 
    { make_stm $startpos (Ast.Empty) }
  | statement_expression
    { make_stm $startpos (Ast.Exp $1) }
;




/* ********** Literals and names ********** */

literal 
  :  INTEGER_LITERAL
     { make_exp $startpos (Ast.IntConst $1) }
  |  CHAR_LITERAL
     { make_exp $startpos (Ast.CharConst $1) }
  |  boolean_literal
     { make_exp $startpos (Ast.BooleanConst $1) }
  |  STRING_LITERAL
     { make_exp $startpos (Ast.StringConst $1) }
  |  null_literal
     { make_exp $startpos (Ast.Null) }
  ;

boolean_literal
  :  TRUE
     { true }
  |  FALSE
     { false }
  ;

null_literal
  :  NULL
     {}
  ;

name
  :  IDENTIFIER
     { make_name $startpos (Ast.Simple(make_identifier $startpos $1)) }
  |  name DOT IDENTIFIER
     { match ($1).Ast.name with
       | Ast.Simple id -> 
	 make_name $startpos (Ast.Qualified ([id;make_identifier $startpos($3) $3]))
       | Ast.Qualified ids -> 
	 make_name $startpos (Ast.Qualified (ids @ [make_identifier $startpos($3) $3])) }
  ;

/* ********** Expressions ********** */

statement_expression
  :  assignment
     { $1 }
  |  method_invocation
     { $1 }
  |  class_instance_creation_expression
     { $1 }
  |  pre_increment_expression
     { $1 }
  |  pre_decrement_expression
     { $1 }
  |  post_increment_expression
     { $1 }
  |  post_decrement_expression
     { $1 }
  ;

primary
  :  literal
     { $1 }
  |  THIS
     { make_exp $startpos (Ast.This) }
  |  left_hand_side
     { make_exp $startpos (Ast.Lvalue($1)) }
  |  L_PAREN expression R_PAREN
     { make_exp $startpos (Ast.InnerExp $2) }
  |  class_instance_creation_expression
     { $1 }
  |  method_invocation
     { $1 }
  |  array_creation_expression
     { $1 }
;

primary_not_name
  : primary_not_name_no_new_array
   { $1 }
  |  array_creation_expression
     { $1 }

;

primary_not_name_no_new_array
  :  literal
     { $1 }
  |  THIS
     { make_exp $startpos (Ast.This) }
  |  left_hand_side_not_name
     { make_exp $startpos (Ast.Lvalue $1) }
  |  L_PAREN expression R_PAREN
     { make_exp $startpos (Ast.InnerExp $2) }
  |  class_instance_creation_expression
     { $1 }
  |  method_invocation
     { $1 }
     
  ;

class_instance_creation_expression
  :  NEW name L_PAREN argument_list R_PAREN
     { make_exp $startpos 
	 (Ast.New(make_typeexp $startpos($2) (Ast.Named $2),$4)) }
  ;

array_creation_expression
  :  NEW element_type dims_expression
     { let dims_exp = List.rev $3 in
       let type_exp = make_tarray_expression $startpos($2) $2 (List.length dims_exp) in 
       make_exp $startpos (Ast.NewArray (type_exp, dims_exp)) }
  |  NEW element_type dims_expression dims
     { let dims_exp = List.rev $3 in
       let type_exp = make_tarray_expression $startpos($2) $2 (List.length dims_exp + $4) in 
       make_exp $startpos (Ast.NewArray(type_exp, dims_exp)) }
  ;

dims_expression
  : dims_expression_single
    { [$1] }
  | dims_expression dims_expression_single
    { $2 :: $1 }
;     

dims_expression_single
  : L_BRACKET expression R_BRACKET
    { $2 }
;

argument_list
  :
     { [] }
  |  argument_list_nonempty
     { List.rev $1 }
  ;

argument_list_nonempty
  :  expression
     { [$1] }
  |  argument_list_nonempty COMMA expression
     { $3 :: $1 }
  ;

method_invocation
  :  primary_not_name DOT IDENTIFIER L_PAREN argument_list R_PAREN
     { make_exp $startpos 
	 (Ast.NonstaticInvoke($1,make_identifier $startpos($3) $3,$5)) }
  |  name DOT IDENTIFIER L_PAREN argument_list R_PAREN
     { make_exp $startpos
	 (Ast.AmbiguousInvoke($1,make_identifier $startpos($3) $3,$5)) }
  |  IDENTIFIER L_PAREN argument_list R_PAREN
     { make_exp $startpos (Ast.SimpleInvoke(make_identifier $startpos $1,$3)) }
  ;

unary_expression
  :  unary_expression_not_plus_minus
     { $1 }
  | pre_increment_expression
     { $1 }
  | pre_decrement_expression
     { $1 }
  | MINUS unary_expression
     { make_exp $startpos (Ast.Unop(Ast.Negate,$2)) }
  ;  


unary_expression_not_plus_minus
  : cast_expression
    { $1 }
  | COMPLEMENT unary_expression
    { make_exp $startpos (Ast.Unop(Ast.Complement,$2)) }
  | postfix_expression
    { $1 }
  ; 

postfix_expression
  : primary
    { $1 }
  | post_increment_expression
    { $1 }
  | post_decrement_expression
    { $1 }

cast_expression
  : L_PAREN expression R_PAREN unary_expression_not_plus_minus
    { make_exp $startpos (Ast.PossiblyCast($2, $4)) } 
  | L_PAREN array_type R_PAREN unary_expression_not_plus_minus
    { make_exp $startpos (Ast.Cast($2, $4)) } 
  | L_PAREN primitive_type R_PAREN unary_expression
    { make_exp $startpos (Ast.Cast($2, $4)) }
  ; 
 
multiplicative_expression
  :  unary_expression
     { $1 }
  |  multiplicative_expression STAR unary_expression
     { make_exp $startpos (Ast.Binop($1,Ast.Times,$3)) }
  |  multiplicative_expression DIV unary_expression
     { make_exp $startpos (Ast.Binop($1,Ast.Divide,$3)) }
  |  multiplicative_expression MOD unary_expression
     { make_exp $startpos (Ast.Binop($1,Ast.Modulo,$3)) }
  ;

additive_expression
  :  multiplicative_expression
     { $1 }
  |  additive_expression PLUS multiplicative_expression
     { make_exp $startpos (Ast.Binop($1,Ast.Plus,$3)) }
  |  additive_expression MINUS multiplicative_expression
     { make_exp $startpos (Ast.Binop($1,Ast.Minus,$3)) }
  ;

relational_expression
  :  additive_expression
     { $1 }
  |  relational_expression LT additive_expression
     { make_exp $startpos (Ast.Binop($1,Ast.Lt,$3)) }
  |  relational_expression GT additive_expression
     { make_exp $startpos (Ast.Binop($1,Ast.Gt,$3)) }
  |  relational_expression LTEQ additive_expression
     { make_exp $startpos (Ast.Binop($1,Ast.Le,$3)) }
  |  relational_expression GTEQ additive_expression
     { make_exp $startpos (Ast.Binop($1,Ast.Ge,$3)) }
  |  relational_expression INSTANCEOF complex_type
      { make_exp $startpos (Ast.Instanceof($1,$3))}
  ;

equality_expression
  :  relational_expression
     { $1 }
  |  equality_expression EQ relational_expression
     { make_exp $startpos (Ast.Binop($1,Ast.Eq,$3)) }
  |  equality_expression NEQ relational_expression
     { make_exp $startpos (Ast.Binop($1,Ast.Ne,$3)) }
  ;

and_expression
  :  equality_expression
     { $1 }
  |  and_expression AND equality_expression
     { make_exp $startpos (Ast.Binop($1,Ast.And,$3)) }
  ;

exclusive_or_expression
  :  and_expression
     { $1 }
  |  exclusive_or_expression XOR and_expression
     { make_exp $startpos (Ast.Binop($1,Ast.Xor,$3)) }
  ;

inclusive_or_expression
  :  exclusive_or_expression
     { $1 }
  |  inclusive_or_expression OR exclusive_or_expression
     { make_exp $startpos (Ast.Binop($1,Ast.Or,$3)) }
  ;

lazy_and_expression
  :  inclusive_or_expression
     { $1 }
  |  lazy_and_expression AND_AND inclusive_or_expression
     { make_exp $startpos (Ast.Binop($1,Ast.LazyAnd,$3)) }
  ;

lazy_or_expression
  :  lazy_and_expression
     { $1 }
  |  lazy_or_expression OR_OR lazy_and_expression
     { make_exp $startpos (Ast.Binop($1,Ast.LazyOr,$3)) }
  ;

expression
  :  lazy_or_expression
     { $1 }
  |  assignment
     { $1 }
  ;

assignment
  :  left_hand_side ASSIGN expression
     { make_exp $startpos (Ast.Assignment($1,$3)) }
  ;

pre_increment_expression
  : PLUS_PLUS left_hand_side
     { make_exp $startpos (Ast.IncDec($2,Ast.PreInc)) }

pre_decrement_expression
  : MINUS_MINUS left_hand_side
     { make_exp $startpos (Ast.IncDec($2,Ast.PreDec)) }

post_increment_expression
  : left_hand_side PLUS_PLUS
     { make_exp $startpos (Ast.IncDec($1,Ast.PostInc)) }

post_decrement_expression
  : left_hand_side MINUS_MINUS
     { make_exp $startpos (Ast.IncDec($1,Ast.PostDec)) }

left_hand_side
  :  name
     { make_lvalue $startpos (Ast.AmbiguousName $1) }
  |  primary_not_name DOT IDENTIFIER
     { make_lvalue $startpos
	 (Ast.NonstaticField($1,make_identifier $startpos($3) $3)) }
  |  array_access
     { $1 }
  ;

left_hand_side_not_name
  :  primary_not_name DOT IDENTIFIER
     { make_lvalue $startpos
	 (Ast.NonstaticField($1,make_identifier $startpos($3) $3)) }
  |  array_access
     { $1 }
  ;

array_access
  :  primary_not_name_no_new_array L_BRACKET expression R_BRACKET
  { make_lvalue $startpos($1) (Ast.Array($1,$3)) }
  | name L_BRACKET expression R_BRACKET
     { make_lvalue $startpos($1) (Ast.Array(make_exp $startpos($1) (Ast.Lvalue (make_lvalue $startpos($1) (Ast.AmbiguousName $1))) ,$3)) }
;
