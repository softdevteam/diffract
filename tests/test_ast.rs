// Copyright (c) 2017 King's College London
// created by the Software Development Team <http://soft-dev.org/>
//
// The Universal Permissive License (UPL), Version 1.0
//
// Subject to the condition set forth below, permission is hereby granted to any
// person obtaining a copy of this software, associated documentation and/or
// data (collectively the "Software"), free of charge and under any and all
// copyright rights in the Software, and any and all patent rights owned or
// freely licensable by each licensor hereunder covering either (i) the
// unmodified Software as contributed to or provided by such licensor, or (ii)
// the Larger Works (as defined below), to deal in both
//
// (a) the Software, and
// (b) any piece of software and/or hardware listed in the lrgrwrks.txt file
// if one is included with the Software (each a “Larger Work” to which the Software
// is contributed by such licensors),
//
// without restriction, including without limitation the rights to copy, create
// derivative works of, display, perform, and distribute the Software and make,
// use, sell, offer for sale, import, export, have made, and have sold the
// Software and the Larger Work(s), and to sublicense the foregoing rights on
// either these or other terms.
//
// This license is subject to the following condition: The above copyright
// notice and either this complete permission notice or at a minimum a reference
// to the UPL must be included in all copies or substantial portions of the
// Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

//! Integration tests for ast module.
//! All file paths are relative to the root of the repository.

extern crate treediff;

use treediff::ast::parse_file;

fn compare_ast_dump_to_lrpar_output(filepath: &str, expected: &str) {
    let arena = parse_file(filepath).unwrap();
    let arena_pretty_printed = format!("{:}", arena);
    assert_eq!(expected, arena_pretty_printed);
}

#[test]
fn test_empty_calc() {
    compare_ast_dump_to_lrpar_output("tests/empty.calc",
                                     "Expr
 Term
  Factor
");
}

#[test]
fn test_one_calc() {
    compare_ast_dump_to_lrpar_output("tests/one.calc",
                                     "Expr
 Term
  Factor
   INT 1
");
}

#[test]
fn test_add_calc() {
    compare_ast_dump_to_lrpar_output("tests/add.calc",
                                     "Expr
 Term
  Factor
   INT 1
 PLUS +
 Expr
  Term
   Factor
    INT 2
");
}

#[test]
fn test_mult_calc() {
    compare_ast_dump_to_lrpar_output("tests/mult.calc",
                                     "Expr
 Term
  Factor
   INT 3
  MULT *
  Term
   Factor
    INT 1
 PLUS +
 Expr
  Term
   Factor
    INT 2
");
}

#[test]
fn test_hello_java() {
    compare_ast_dump_to_lrpar_output("tests/Hello.java",
                                     "goal
 compilation_unit
  package_declaration_opt
  import_declarations_opt
  type_declarations_opt
   type_declarations
    type_declaration
     class_declaration
      modifiers_opt
       modifiers
        modifier
         PUBLIC public
      CLASS class
      IDENTIFIER Hello
      type_parameters_opt
      super_opt
      interfaces_opt
      class_body
       LBRACE {
       class_body_declarations_opt
        class_body_declarations
         class_body_declaration
          class_member_declaration
           method_declaration
            method_header
             modifiers_opt
              modifiers
               modifiers
                modifier
                 PUBLIC public
               modifier
                STATIC static
             VOID void
             method_declarator
              IDENTIFIER main
              LPAREN (
              formal_parameter_list_opt
               formal_parameter_list
                formal_parameter
                 type
                  reference_type
                   array_type
                    name
                     simple_name
                      IDENTIFIER String
                    dims
                     LBRACK [
                     RBRACK ]
                 variable_declarator_id
                  IDENTIFIER args
              RPAREN )
             throws_opt
            method_body
             block
              LBRACE {
              block_statements_opt
               block_statements
                block_statement
                 statement
                  statement_without_trailing_substatement
                   expression_statement
                    statement_expression
                     method_invocation
                      qualified_name
                       name
                        qualified_name
                         name
                          simple_name
                           IDENTIFIER System
                         DOT .
                         IDENTIFIER out
                       DOT .
                       IDENTIFIER println
                      LPAREN (
                      argument_list_opt
                       argument_list
                        expression
                         assignment_expression
                          conditional_expression
                           conditional_or_expression
                            conditional_and_expression
                             inclusive_or_expression
                              exclusive_or_expression
                               and_expression
                                equality_expression
                                 instanceof_expression
                                  relational_expression
                                   shift_expression
                                    additive_expression
                                     multiplicative_expression
                                      unary_expression
                                       unary_expression_not_plus_minus
                                        postfix_expression
                                         primary
                                          primary_no_new_array
                                           literal
                                            STRING_LITERAL \"Hello, world!\"
                      RPAREN )
                    SEMICOLON ;
              RBRACE }
       RBRACE }
");
}
