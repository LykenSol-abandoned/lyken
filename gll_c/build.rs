#![feature(decl_macro)]
#![recursion_limit = "256"]

extern crate lyken_gll;

use lyken_gll::grammar::grammar;
use std::env;
use std::fs::File;
use std::path::PathBuf;

fn main() {
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());

    macro from_yacc($($rule:ident { $({ $($part:tt)+ })|+ })+) {
        grammar!{
            $($rule = { $($($part)WhiteSpace+)|+ };)+

            AUTO = { "auto" NotAlphaNum };
            BREAK = { "break" NotAlphaNum };
            CASE = { "case" NotAlphaNum };
            CHAR = { "char" NotAlphaNum };
            CONST = { "const" NotAlphaNum };
            CONTINUE = { "continue" NotAlphaNum };
            DEFAULT = { "default" NotAlphaNum };
            DO = { "do" NotAlphaNum };
            DOUBLE = { "double" NotAlphaNum };
            ELSE = { "else" NotAlphaNum };
            ENUM = { "enum" NotAlphaNum };
            EXTERN = { "extern" NotAlphaNum };
            FLOAT = { "float" NotAlphaNum };
            FOR = { "for" NotAlphaNum };
            GOTO = { "goto" NotAlphaNum };
            IF = { "if" NotAlphaNum };
            INLINE = { { "inline" | "__inline__" /* HACK(eddyb) */ } NotAlphaNum };
            INT = { "int" NotAlphaNum };
            LONG = { "long" NotAlphaNum };
            REGISTER = { "register" NotAlphaNum };
            RESTRICT = { { "restrict" | "__restrict__" /* HACK(eddyb) */ } NotAlphaNum };
            RETURN = { "return" NotAlphaNum };
            SHORT = { "short" NotAlphaNum };
            SIGNED = { "signed" NotAlphaNum };
            SIZEOF = { "sizeof" NotAlphaNum };
            STATIC = { "static" NotAlphaNum };
            STRUCT = { "struct" NotAlphaNum };
            SWITCH = { "switch" NotAlphaNum };
            TYPEDEF = { "typedef" NotAlphaNum };
            UNION = { "union" NotAlphaNum };
            UNSIGNED = { "unsigned" NotAlphaNum };
            VOID = { "void" NotAlphaNum };
            VOLATILE = { { "volatile" | "__volatile__" /* HACK(eddyb) */ } NotAlphaNum };
            WHILE = { "while" NotAlphaNum };
            ALIGNAS = { "_Alignas" NotAlphaNum };
            ALIGNOF = { "_Alignof" NotAlphaNum };
            ATOMIC = { "_Atomic" NotAlphaNum };
            BOOL = { "_Bool" NotAlphaNum };
            COMPLEX = { "_Complex" NotAlphaNum };
            GENERIC = { "_Generic" NotAlphaNum };
            IMAGINARY = { "_Imaginary" NotAlphaNum };
            NORETURN = { "_Noreturn" NotAlphaNum };
            STATIC_ASSERT = { "_Static_assert" NotAlphaNum };
            THREAD_LOCAL = { "_Thread_local" NotAlphaNum };
            FUNC_NAME = { "__func__" NotAlphaNum };

            ASM = { { "asm" | "__asm__" } NotAlphaNum }; /* HACK(eddyb) */
            ATTRIBUTE = { "__attribute__" NotAlphaNum }; /* HACK(eddyb) */

            TYPEDEF_NAME = { Ident };
            ENUMERATION_CONSTANT = { Ident };
            IDENTIFIER = { Ident };

            I_CONSTANT = { IntLit | CharLit };
            F_CONSTANT = { /* unimplemented */ "1f" };
            STRING_LITERAL = { StrLit };

            ELLIPSIS = { "..." };
            RIGHT_ASSIGN = { ">>=" };
            LEFT_ASSIGN = { "<<=" };
            ADD_ASSIGN = { "+=" };
            SUB_ASSIGN = { "-=" };
            MUL_ASSIGN = { "*=" };
            DIV_ASSIGN = { "/=" };
            MOD_ASSIGN = { "%=" };
            AND_ASSIGN = { "&=" };
            XOR_ASSIGN = { "^=" };
            OR_ASSIGN = { "|=" };
            RIGHT_OP = { ">>" };
            LEFT_OP = { "<<" };
            INC_OP = { "++" };
            DEC_OP = { "--" };
            PTR_OP = { "->" };
            AND_OP = { "&&" };
            OR_OP = { "||" };
            LE_OP = { "<=" };
            GE_OP = { ">=" };
            EQ_OP = { "==" };
            NE_OP = { "!=" };

            Tokens = {
                {} |
                rest:Tokens WhiteSpace last:Token
            };
            Token = {
                comment:Comment |
                ident:Ident |
                int:IntLit |
                char:CharLit |
                str:StrLit |
                punct:Punct
            };

            WhiteSpace = { WhiteSpacePrefix { !" " !"\t" !"\n" } };
            WhiteSpacePrefix = { {} | WhiteSpacePrefix { " " | "\t" | "\n" } };

            Comment = {
                "//" SingleLineCommentBody |
                "/*" MultiLineCommentBody "*/"
            };
            SingleLineCommentBody = { "\n" | !"\n" (..) SingleLineCommentBody };
            MultiLineCommentBody = { {} | !"*/" (..) MultiLineCommentBody };

            Ident = { /*IdentStart*/{ ('a'..='z') | ('A'..='Z') | "_" } IdentBody /*NotAlphaNum*/{ !('a'..='z') !('A'..='Z') !"_" !('0'..='9') } };
            IdentStart = { ('a'..='z') | ('A'..='Z') | "_" };
            NotAlphaNum = { !('a'..='z') !('A'..='Z') !"_" !('0'..='9') };
            IdentBody = { {} | IdentBody { /*IdentStart*/{ ('a'..='z') | ('A'..='Z') | "_" } | ('0'..='9') } };

            IntLit = {
                "0" { "x" | "X" } HexIntDigits IntSuffix /*NotAlphaNum*/{ !('a'..='z') !('A'..='Z') !"_" !('0'..='9') } |
                ('1'..='9') DecIntDigits IntSuffix /*NotAlphaNum*/{ !('a'..='z') !('A'..='Z') !"_" !('0'..='9') } |
                "0" OctIntDigits IntSuffix /*NotAlphaNum*/{ !('a'..='z') !('A'..='Z') !"_" !('0'..='9') }
            };
            IntSuffix = {
                {} |
                IntSuffixUnsigned { {} | IntSuffixLong } |
                IntSuffixLong { {} | IntSuffixUnsigned }
            };
            IntSuffixUnsigned = { "u" | "U" };
            IntSuffixLong = { "l" | "L" | "ll" | "LL" };
            HexIntDigits = { { ('a'..='f') | ('A'..='F') | ('0'..='9') } { { !('a'..='f') !('A'..='F') !('0'..='9') } | HexIntDigits } };
            DecIntDigits = { !('0'..='9') | ('0'..='9') DecIntDigits };
            OctIntDigits = { !('0'..='7') | ('0'..='7') OctIntDigits };

            CharLit = { { {} | "u" | "U" | "L" } "'" CharLitBody "'" };
            CharLitBody = { { { !"'" !"\\" !"\n" } (..) | CharEscape } { {} | CharLitBody } };
            CharEscape = {
                "\\" {
                    "'" | "\"" | "?" | "\\" | "a" | "b" | "f" | "n" | "r" | "t" | "v" |
                    ('0'..='7') { {} | ('0'..='7') { {} | ('0'..='7') } }
                    "x" HexIntDigits
                }
            };

            StrLit = { { {} | "u8" | "u" | "U" | "L" } "\"" StrLitBody "\"" };
            StrLitBody = { { { !"\"" !"\\" !"\n" } (..) | CharEscape } { {} | StrLitBody } };

            Punct = {
                "." | "," | ":" | ";" | "+" | "*" | "-" | { "/" !"/" !"*" } | "=" | "^" | "%" | "#" |
                "{" | "}" | "[" | "]" | "(" | ")" | "&" | "|" | "!" | "<" | ">" | "@" | "~" | "?"
            };
        }
    };

    let mut c = from_yacc! {
            primary_expression
        {
            { IDENTIFIER }
            | { constant }
            | { string }
            | { "(" expression ")" }
            | { generic_selection }
        }

    constant
        {
            { I_CONSTANT        /* includes character_constant */ }
            | { F_CONSTANT }
            | { ENUMERATION_CONSTANT  /* after it has been defined as such */ }
        }

    enumeration_constant        /* before it has been defined as such */
        {
            { IDENTIFIER }
        }

    string
        {
            { STRING_LITERAL }
            | { FUNC_NAME }
        }

    generic_selection
        {
            { GENERIC "(" assignment_expression "," generic_assoc_list ")" }
        }

    generic_assoc_list
        {
            { generic_association }
            | { generic_assoc_list "," generic_association }
        }

    generic_association
        {
            { type_name ":" assignment_expression }
            | { DEFAULT ":" assignment_expression }
        }

    postfix_expression
        {
            { primary_expression }
            | { postfix_expression "[" expression "]" }
            | { postfix_expression "(" ")" }
            | { postfix_expression "(" argument_expression_list ")" }
            | { postfix_expression "." IDENTIFIER }
            | { postfix_expression PTR_OP IDENTIFIER }
            | { postfix_expression INC_OP }
            | { postfix_expression DEC_OP }
            | { "(" type_name ")" "{" initializer_list "}" }
            | { "(" type_name ")" "{" initializer_list "," "}" }
        }

    argument_expression_list
        {
            { assignment_expression }
            | { argument_expression_list "," assignment_expression }
        }

    unary_expression
        {
            { postfix_expression }
            | { INC_OP unary_expression }
            | { DEC_OP unary_expression }
            | { unary_operator cast_expression }
            | { SIZEOF unary_expression }
            | { SIZEOF "(" type_name ")" }
            | { ALIGNOF "(" type_name ")" }
        }

    unary_operator
        {
            { "&" }
            | { "*" }
            | { "+" }
            | { "-" }
            | { "~" }
            | { "!" }
        }

    cast_expression
        {
            { "(" type_name ")" cast_expression /* HACK(eddyb) swapped the order */ }
            | { unary_expression }
        }

    multiplicative_expression
        {
            { cast_expression }
            | { multiplicative_expression "*" cast_expression }
            | { multiplicative_expression "/" cast_expression }
            | { multiplicative_expression "%" cast_expression }
        }

    additive_expression
        {
            { multiplicative_expression }
            | { additive_expression "+" multiplicative_expression }
            | { additive_expression "-" multiplicative_expression }
        }

    shift_expression
        {
            { additive_expression }
            | { shift_expression LEFT_OP additive_expression }
            | { shift_expression RIGHT_OP additive_expression }
        }

    relational_expression
        {
            { shift_expression }
            | { relational_expression "<" shift_expression }
            | { relational_expression ">" shift_expression }
            | { relational_expression LE_OP shift_expression }
            | { relational_expression GE_OP shift_expression }
        }

    equality_expression
        {
            { relational_expression }
            | { equality_expression EQ_OP relational_expression }
            | { equality_expression NE_OP relational_expression }
        }

    and_expression
        {
            { equality_expression }
            | { and_expression "&" equality_expression }
        }

    exclusive_or_expression
        {
            { and_expression }
            | { exclusive_or_expression "^" and_expression }
        }

    inclusive_or_expression
        {
            { exclusive_or_expression }
            | { inclusive_or_expression "|" exclusive_or_expression }
        }

    logical_and_expression
        {
            { inclusive_or_expression }
            | { logical_and_expression AND_OP inclusive_or_expression }
        }

    logical_or_expression
        {
            { logical_and_expression }
            | { logical_or_expression OR_OP logical_and_expression }
        }

    conditional_expression
        {
            { logical_or_expression }
            | { logical_or_expression "?" expression ":" conditional_expression }
        }

    assignment_expression
        {
            { unary_expression assignment_operator assignment_expression /* HACK(eddyb) swapped the order */ }
            | { conditional_expression }
        }

    assignment_operator
        {
            { "=" }
            | { MUL_ASSIGN }
            | { DIV_ASSIGN }
            | { MOD_ASSIGN }
            | { ADD_ASSIGN }
            | { SUB_ASSIGN }
            | { LEFT_ASSIGN }
            | { RIGHT_ASSIGN }
            | { AND_ASSIGN }
            | { XOR_ASSIGN }
            | { OR_ASSIGN }
        }

    expression
        {
            { assignment_expression }
            | { expression "," assignment_expression }
        }

    constant_expression
        {
            { conditional_expression    /* with constraints */ }
        }

    declaration
        {
            { {decl_specs0:declaration_specifiers} ";" }
            | { {decl_specs:declaration_specifiers} {init_decl_list:init_declarator_list} ";" }
            | { {static_assert:static_assert_declaration} }
        }

    declaration_specifiers
        {
            { storage_class_specifier declaration_specifiers }
            | { storage_class_specifier }
            | { type_specifier_no_typedef_names declaration_specifiers /* HACK(eddyb) */ }
            | { type_specifier_no_typedef_names /* HACK(eddyb) */ }
            | { TYPEDEF_NAME declaration_specifiers_after_typedef_name /* HACK(eddyb) */ }
            | { TYPEDEF_NAME /* HACK(eddyb) */ }
            | { type_qualifier declaration_specifiers }
            | { type_qualifier }
            | { function_specifier declaration_specifiers }
            | { function_specifier }
            | { alignment_specifier declaration_specifiers }
            | { alignment_specifier }
        }

    declaration_specifiers_after_typedef_name /* HACK(eddyb) */
        {
            { storage_class_specifier declaration_specifiers_after_typedef_name }
            | { storage_class_specifier }
            | { type_qualifier declaration_specifiers_after_typedef_name }
            | { type_qualifier }
            | { function_specifier declaration_specifiers_after_typedef_name }
            | { function_specifier }
            | { alignment_specifier declaration_specifiers_after_typedef_name }
            | { alignment_specifier }
        }

    init_declarator_list
        {
            { init_declarator }
            | { init_declarator_list "," init_declarator }
        }

    init_declarator
        {
            { declarator "=" initializer }
            | { declarator }
        }

    storage_class_specifier
        {
            { TYPEDEF   /* identifiers must be flagged as TYPEDEF_NAME */ }
            | { EXTERN }
            | { STATIC }
            | { THREAD_LOCAL }
            | { AUTO }
            | { REGISTER }
        }

    type_specifier_no_typedef_names /* HACK(eddyb) */
        {
            { VOID }
            | { CHAR }
            | { SHORT }
            | { INT }
            | { LONG }
            | { FLOAT }
            | { DOUBLE }
            | { SIGNED }
            | { UNSIGNED }
            | { BOOL }
            | { COMPLEX }
            | { IMAGINARY     /* non-mandated extension */ }
            | { atomic_type_specifier }
            | { struct_or_union_specifier }
            | { enum_specifier }
        }

    type_specifier
        {
            { type_specifier_no_typedef_names /* HACK(eddyb) */ }
            | { TYPEDEF_NAME      /* after it has been defined as such */ }
        }

    struct_or_union_specifier
        {
            { struct_or_union "{" struct_declaration_list "}" }
            | { struct_or_union IDENTIFIER "{" struct_declaration_list "}" }
            | { struct_or_union IDENTIFIER }
        }

    struct_or_union
        {
            { STRUCT }
            | { UNION }
        }

    struct_declaration_list
        {
            { struct_declaration }
            | { struct_declaration_list struct_declaration }
        }

    struct_declaration
        {
            { specifier_qualifier_list ";"  /* for anonymous struct/union */ }
            | { specifier_qualifier_list struct_declarator_list ";" }
            | { static_assert_declaration }
        }

    specifier_qualifier_list
        {
            { type_specifier_no_typedef_names specifier_qualifier_list_no_typedef_names /* HACK(eddyb) */ }
            | { type_specifier_no_typedef_names /* HACK(eddyb) */ }
            | { TYPEDEF_NAME specifier_qualifier_list_after_typedef_name /* HACK(eddyb) */ }
            | { TYPEDEF_NAME /* HACK(eddyb) */ }
            | { type_qualifier specifier_qualifier_list }
            | { type_qualifier }
        }

    specifier_qualifier_list_no_typedef_names /* HACK(eddyb) */
        {
            { type_specifier_no_typedef_names specifier_qualifier_list_no_typedef_names }
            | { type_specifier_no_typedef_names }
            | { type_qualifier specifier_qualifier_list_no_typedef_names }
            | { type_qualifier }
        }

    specifier_qualifier_list_after_typedef_name /* HACK(eddyb) */
        {
            { type_qualifier specifier_qualifier_list_after_typedef_name }
            | { type_qualifier }
        }

    struct_declarator_list
        {
            { struct_declarator }
            | { struct_declarator_list "," struct_declarator }
        }

    struct_declarator
        {
            { ":" constant_expression }
            | { declarator ":" constant_expression }
            | { declarator }
        }

    enum_specifier
        {
            { ENUM "{" enumerator_list "}" }
            | { ENUM "{" enumerator_list "," "}" }
            | { ENUM IDENTIFIER "{" enumerator_list "}" }
            | { ENUM IDENTIFIER "{" enumerator_list "," "}" }
            | { ENUM IDENTIFIER }
        }

    enumerator_list
        {
            { enumerator }
            | { enumerator_list "," enumerator }
        }

    enumerator  /* identifiers must be flagged as ENUMERATION_CONSTANT */
        {
            { enumeration_constant "=" constant_expression }
            | { enumeration_constant }
        }

    atomic_type_specifier
        {
            { ATOMIC "(" type_name ")" }
        }

    type_qualifier
        {
            { CONST }
            | { RESTRICT }
            | { VOLATILE }
            | { ATOMIC }
            | { attribute_specifier /* HACK(eddyb) */ }
        }

    function_specifier
        {
            { INLINE }
            | { NORETURN }
        }

    alignment_specifier
        {
            { ALIGNAS "(" type_name ")" }
            | { ALIGNAS "(" constant_expression ")" }
        }

    attribute_specifier /* HACK(eddyb) */
        {
            { ATTRIBUTE "(" "(" attribute_list ")" ")" }
        }

    attribute_list /* HACK(eddyb) */
        {
            { attribute }
            | { attribute_list "," attribute }
        }

    attribute /* HACK(eddyb) */
        {
            { IDENTIFIER "(" attribute_parameter_list ")" }
            | { IDENTIFIER }
        }

    attribute_parameter_list /* HACK(eddyb) */
        {
            { attribute_parameter }
            | { attribute_parameter_list "," attribute_parameter }
        }

    attribute_parameter /* HACK(eddyb) */
        {
            { IDENTIFIER }
            | { constant }
            | { string }
        }

    declarator
        {
            { pointer direct_declarator }
            | { attribute_specifier declarator /* HACK(eddyb) */ }
            | { direct_declarator }
        }

    direct_declarator
        {
            { IDENTIFIER }
            | { "(" declarator ")" }
            | { direct_declarator "[" "]" }
            | { direct_declarator "[" "*" "]" }
            | { direct_declarator "[" STATIC type_qualifier_list assignment_expression "]" }
            | { direct_declarator "[" STATIC assignment_expression "]" }
            | { direct_declarator "[" type_qualifier_list "*" "]" }
            | { direct_declarator "[" type_qualifier_list STATIC assignment_expression "]" }
            | { direct_declarator "[" type_qualifier_list assignment_expression "]" }
            | { direct_declarator "[" type_qualifier_list "]" }
            | { direct_declarator "[" assignment_expression "]" }
            | { direct_declarator "(" parameter_type_list ")" }
            | { direct_declarator "(" ")" }
            | { direct_declarator "(" identifier_list ")" }
            | { direct_declarator attribute_specifier /* HACK(eddyb) */ }
        }

    pointer
        {
            { "*" type_qualifier_list pointer }
            | { "*" type_qualifier_list }
            | { "*" pointer }
            | { "*" }
        }

    type_qualifier_list
        {
            { type_qualifier }
            | { type_qualifier_list type_qualifier }
        }


    parameter_type_list
        {
            { parameter_list "," ELLIPSIS }
            | { parameter_list }
        }

    parameter_list
        {
            { parameter_declaration }
            | { parameter_list "," parameter_declaration }
        }

    parameter_declaration
        {
            { declaration_specifiers declarator }
            | { declaration_specifiers abstract_declarator }
            | { declaration_specifiers }
        }

    identifier_list
        {
            { IDENTIFIER }
            | { identifier_list "," IDENTIFIER }
        }

    type_name
        {
            { specifier_qualifier_list abstract_declarator }
            | { specifier_qualifier_list }
        }

    abstract_declarator
        {
            { pointer direct_abstract_declarator }
            | { pointer }
            | { attribute_specifier abstract_declarator /* HACK(eddyb) */ }
            | { direct_abstract_declarator }
        }

    direct_abstract_declarator
        {
            { "(" abstract_declarator ")" }
            | { "[" "]" }
            | { "[" "*" "]" }
            | { "[" STATIC type_qualifier_list assignment_expression "]" }
            | { "[" STATIC assignment_expression "]" }
            | { "[" type_qualifier_list STATIC assignment_expression "]" }
            | { "[" type_qualifier_list assignment_expression "]" }
            | { "[" type_qualifier_list "]" }
            | { "[" assignment_expression "]" }
            | { direct_abstract_declarator "[" "]" }
            | { direct_abstract_declarator "[" "*" "]" }
            | { direct_abstract_declarator "[" STATIC type_qualifier_list assignment_expression "]" }
            | { direct_abstract_declarator "[" STATIC assignment_expression "]" }
            | { direct_abstract_declarator "[" type_qualifier_list assignment_expression "]" }
            | { direct_abstract_declarator "[" type_qualifier_list STATIC assignment_expression "]" }
            | { direct_abstract_declarator "[" type_qualifier_list "]" }
            | { direct_abstract_declarator "[" assignment_expression "]" }
            | { "(" ")" }
            | { "(" parameter_type_list ")" }
            | { direct_abstract_declarator "(" ")" }
            | { direct_abstract_declarator "(" parameter_type_list ")" }
        }

    initializer
        {
            { "{" initializer_list "}" }
            | { "{" initializer_list "," "}" }
            | { assignment_expression }
        }

    initializer_list
        {
            { designation initializer }
            | { initializer }
            | { initializer_list "," designation initializer }
            | { initializer_list "," initializer }
        }

    designation
        {
            { designator_list "=" }
        }

    designator_list
        {
            { designator }
            | { designator_list designator }
        }

    designator
        {
            { "[" constant_expression "]" }
            | { "." IDENTIFIER }
        }

    static_assert_declaration
        {
            { STATIC_ASSERT "(" constant_expression "," STRING_LITERAL ")" ";" }
        }

    statement
        {
            { labeled_statement }
            | { compound_statement }
            | { expression_statement }
            | { selection_statement }
            | { iteration_statement }
            | { jump_statement }
            | { asm_statement /* HACK(eddyb) */ }
        }

    labeled_statement
        {
            { IDENTIFIER ":" statement }
            | { CASE constant_expression ":" statement }
            | { DEFAULT ":" statement }
        }

    compound_statement
        {
            { "{" "}" }
            | { "{"  block_item_list "}" }
        }

    block_item_list
        {
            { block_item }
            | { block_item_list block_item }
        }

    block_item
        {
            { declaration }
            | { statement }
        }

    expression_statement
        {
            { ";" }
            | { expression ";" }
        }

    selection_statement
        {
            { IF "(" expression ")" statement ELSE statement }
            | { IF "(" expression ")" statement }
            | { SWITCH "(" expression ")" statement }
        }

    iteration_statement
        {
            { WHILE "(" expression ")" statement }
            | { DO statement WHILE "(" expression ")" ";" }
            | { FOR "(" expression_statement expression_statement ")" statement }
            | { FOR "(" expression_statement expression_statement expression ")" statement }
            | { FOR "(" declaration expression_statement ")" statement }
            | { FOR "(" declaration expression_statement expression ")" statement }
        }

    jump_statement
        {
            { GOTO IDENTIFIER ";" }
            | { CONTINUE ";" }
            | { BREAK ";" }
            | { RETURN ";" }
            | { RETURN expression ";" }
        }

    asm_statement /* HACK(eddyb) */
        {
            { ASM "(" asm_parameters ")" ";" }
            | { ASM VOLATILE "(" asm_parameters ")" ";" }
        }

    asm_parameters /* HACK(eddyb) */
        {
            { string ":" ":" ":" expression }
            | { string ":" ":" expression ":" expression }
            | { string ":" expression ":" ":" expression }
            | { string ":" expression ":" expression ":" expression }
            | { string ":" ":" expression }
            | { string ":" expression ":" expression }
            | { string ":" expression }
            | { string }
        }

    translation_unit
        {
            { {} }
            | { {rest:translation_unit} {last:external_declaration} }
        }

    external_declaration
        {
            { {fn_def:function_definition} }
            | { {decl:declaration} }
        }

    function_definition
        {
            //{ declaration_specifiers declarator declaration_list compound_statement }
            //| { declaration_specifiers declarator compound_statement }
            { declaration_specifiers declarator "(" { {} | identifier_list | parameter_type_list } ")" compound_statement }
        }

    declaration_list
        {
            { declaration }
            | { declaration_list declaration }
        }
    };
    c.generate(&mut File::create(&out_dir.join("parse.rs")).unwrap());
}
