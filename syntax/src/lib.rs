#![allow(dead_code)]
#![allow(unused_mut)]
#![allow(unreachable_code)]

extern crate onig;

#[macro_use]
extern crate lazy_static;

use onig::{Regex, Syntax, RegexOptions};
use std::collections::HashMap;

/**
 * Stack value.
 */
enum SV {
    Undefined,
    _0(Token),
    _1(Value),
    _2(Expr),
    _3(Vec<Expr>),
    _4(Vec<Value>)
}

/**
 * Lex rules.
 */
static LEX_RULES: [&'static str; 16] = [
    r##########"^\("##########,
    r##########"^\)"##########,
    r##########"^\s+"##########,
    r##########"^"[^\"]*""##########,
    r##########"^[\-]?\d+"##########,
    r##########"^true|false"##########,
    r##########"^null"##########,
    r##########"^begin"##########,
    r##########"^var"##########,
    r##########"^if"##########,
    r##########"^while"##########,
    r##########"^set"##########,
    r##########"^def"##########,
    r##########"^lambda"##########,
    r##########"^[+\-*/=<>]+"##########,
    r##########"^[\w]+"##########
];

/**
 * EOF value.
 */
static EOF: &'static str = "$";

/**
 * A macro for map literals.
 *
 * hashmap!{ 1 => "one", 2 => "two" };
 */
macro_rules! hashmap(
    { $($key:expr => $value:expr),+ } => {
        {
            let mut m = ::std::collections::HashMap::new();
            $(
                m.insert($key, $value);
            )+
            m
        }
     };
);

/**
 * Unwraps a SV for the result. The result type is known from the grammar.
 */
macro_rules! get_result {
    ($r:expr, $ty:ident) => (match $r { SV::$ty(v) => v, _ => unreachable!() });
}

/**
 * Pops a SV with needed enum value.
 */
macro_rules! pop {
    ($s:expr, $ty:ident) => (get_result!($s.pop().unwrap(), $ty));
}

/**
 * Productions data.
 *
 * 0 - encoded non-terminal, 1 - length of RHS to pop from the stack
 */
static PRODUCTIONS : [[i32; 2]; 22] = [
    [-1, 1],
    [0, 1],
    [0, 1],
    [0, 1],
    [1, 1],
    [1, 1],
    [1, 1],
    [1, 1],
    [2, 2],
    [2, 0],
    [3, 3],
    [4, 2],
    [4, 0],
    [5, 4],
    [5, 4],
    [5, 5],
    [5, 6],
    [5, 5],
    [5, 5],
    [5, 5],
    [5, 6],
    [5, 5]
];

/**
 * Table entry.
 */
enum TE {
    Accept,

    // Shift, and transit to the state.
    Shift(usize),

    // Reduce by a production number.
    Reduce(usize),

    // Simple state transition.
    Transit(usize),
}

lazy_static! {
    /**
     * Lexical rules grouped by lexer state (by start condition).
     */
    static ref LEX_RULES_BY_START_CONDITIONS: HashMap<&'static str, Vec<i32>> = hashmap! { "INITIAL" => vec! [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 ] };

    /**
     * Maps a string name of a token type to its encoded number (the first
     * token number starts after all numbers for non-terminal).
     */
    static ref TOKENS_MAP: HashMap<&'static str, i32> = hashmap! { "IDENTIFIER" => 6, "NUMBER" => 7, "STRING" => 8, "BOOLEAN" => 9, "NULL" => 10, "BEGIN" => 11, "VAR" => 12, "IF" => 13, "WHILE" => 14, "ASSIGN" => 15, "OPERATOR" => 16, "FUNCTION" => 17, "LAMBDA" => 18, "'('" => 19, "')'" => 20, "$" => 21 };

    /**
     * Parsing table.
     *
     * Vector index is the state number, value is a map
     * from an encoded symbol to table entry (TE).
     */
    static ref TABLE: Vec<HashMap<i32, TE>>= vec![
    hashmap! { 0 => TE::Transit(1), 1 => TE::Transit(2), 5 => TE::Transit(4), 6 => TE::Shift(3), 7 => TE::Shift(5), 8 => TE::Shift(6), 9 => TE::Shift(7), 10 => TE::Shift(8), 19 => TE::Shift(9) },
    hashmap! { 21 => TE::Accept },
    hashmap! { 6 => TE::Reduce(1), 7 => TE::Reduce(1), 8 => TE::Reduce(1), 9 => TE::Reduce(1), 10 => TE::Reduce(1), 19 => TE::Reduce(1), 20 => TE::Reduce(1), 21 => TE::Reduce(1) },
    hashmap! { 6 => TE::Reduce(2), 7 => TE::Reduce(2), 8 => TE::Reduce(2), 9 => TE::Reduce(2), 10 => TE::Reduce(2), 19 => TE::Reduce(2), 20 => TE::Reduce(2), 21 => TE::Reduce(2) },
    hashmap! { 6 => TE::Reduce(3), 7 => TE::Reduce(3), 8 => TE::Reduce(3), 9 => TE::Reduce(3), 10 => TE::Reduce(3), 19 => TE::Reduce(3), 20 => TE::Reduce(3), 21 => TE::Reduce(3) },
    hashmap! { 6 => TE::Reduce(4), 7 => TE::Reduce(4), 8 => TE::Reduce(4), 9 => TE::Reduce(4), 10 => TE::Reduce(4), 19 => TE::Reduce(4), 20 => TE::Reduce(4), 21 => TE::Reduce(4) },
    hashmap! { 6 => TE::Reduce(5), 7 => TE::Reduce(5), 8 => TE::Reduce(5), 9 => TE::Reduce(5), 10 => TE::Reduce(5), 19 => TE::Reduce(5), 20 => TE::Reduce(5), 21 => TE::Reduce(5) },
    hashmap! { 6 => TE::Reduce(6), 7 => TE::Reduce(6), 8 => TE::Reduce(6), 9 => TE::Reduce(6), 10 => TE::Reduce(6), 19 => TE::Reduce(6), 20 => TE::Reduce(6), 21 => TE::Reduce(6) },
    hashmap! { 6 => TE::Reduce(7), 7 => TE::Reduce(7), 8 => TE::Reduce(7), 9 => TE::Reduce(7), 10 => TE::Reduce(7), 19 => TE::Reduce(7), 20 => TE::Reduce(7), 21 => TE::Reduce(7) },
    hashmap! { 6 => TE::Shift(11), 11 => TE::Shift(10), 12 => TE::Shift(12), 13 => TE::Shift(13), 14 => TE::Shift(14), 15 => TE::Shift(15), 16 => TE::Shift(16), 17 => TE::Shift(17), 18 => TE::Shift(18) },
    hashmap! { 2 => TE::Transit(19), 6 => TE::Reduce(9), 7 => TE::Reduce(9), 8 => TE::Reduce(9), 9 => TE::Reduce(9), 10 => TE::Reduce(9), 19 => TE::Reduce(9), 20 => TE::Reduce(9) },
    hashmap! { 2 => TE::Transit(22), 6 => TE::Reduce(9), 7 => TE::Reduce(9), 8 => TE::Reduce(9), 9 => TE::Reduce(9), 10 => TE::Reduce(9), 19 => TE::Reduce(9), 20 => TE::Reduce(9) },
    hashmap! { 6 => TE::Shift(24) },
    hashmap! { 0 => TE::Transit(27), 1 => TE::Transit(2), 5 => TE::Transit(4), 6 => TE::Shift(3), 7 => TE::Shift(5), 8 => TE::Shift(6), 9 => TE::Shift(7), 10 => TE::Shift(8), 19 => TE::Shift(9) },
    hashmap! { 0 => TE::Transit(31), 1 => TE::Transit(2), 5 => TE::Transit(4), 6 => TE::Shift(3), 7 => TE::Shift(5), 8 => TE::Shift(6), 9 => TE::Shift(7), 10 => TE::Shift(8), 19 => TE::Shift(9) },
    hashmap! { 6 => TE::Shift(34) },
    hashmap! { 0 => TE::Transit(37), 1 => TE::Transit(2), 5 => TE::Transit(4), 6 => TE::Shift(3), 7 => TE::Shift(5), 8 => TE::Shift(6), 9 => TE::Shift(7), 10 => TE::Shift(8), 19 => TE::Shift(9) },
    hashmap! { 6 => TE::Shift(40) },
    hashmap! { 3 => TE::Transit(48), 19 => TE::Shift(42) },
    hashmap! { 0 => TE::Transit(21), 1 => TE::Transit(2), 5 => TE::Transit(4), 6 => TE::Shift(3), 7 => TE::Shift(5), 8 => TE::Shift(6), 9 => TE::Shift(7), 10 => TE::Shift(8), 19 => TE::Shift(9), 20 => TE::Shift(20) },
    hashmap! { 6 => TE::Reduce(13), 7 => TE::Reduce(13), 8 => TE::Reduce(13), 9 => TE::Reduce(13), 10 => TE::Reduce(13), 19 => TE::Reduce(13), 20 => TE::Reduce(13), 21 => TE::Reduce(13) },
    hashmap! { 6 => TE::Reduce(8), 7 => TE::Reduce(8), 8 => TE::Reduce(8), 9 => TE::Reduce(8), 10 => TE::Reduce(8), 19 => TE::Reduce(8), 20 => TE::Reduce(8) },
    hashmap! { 0 => TE::Transit(21), 1 => TE::Transit(2), 5 => TE::Transit(4), 6 => TE::Shift(3), 7 => TE::Shift(5), 8 => TE::Shift(6), 9 => TE::Shift(7), 10 => TE::Shift(8), 19 => TE::Shift(9), 20 => TE::Shift(23) },
    hashmap! { 6 => TE::Reduce(14), 7 => TE::Reduce(14), 8 => TE::Reduce(14), 9 => TE::Reduce(14), 10 => TE::Reduce(14), 19 => TE::Reduce(14), 20 => TE::Reduce(14), 21 => TE::Reduce(14) },
    hashmap! { 0 => TE::Transit(25), 1 => TE::Transit(2), 5 => TE::Transit(4), 6 => TE::Shift(3), 7 => TE::Shift(5), 8 => TE::Shift(6), 9 => TE::Shift(7), 10 => TE::Shift(8), 19 => TE::Shift(9) },
    hashmap! { 20 => TE::Shift(26) },
    hashmap! { 6 => TE::Reduce(15), 7 => TE::Reduce(15), 8 => TE::Reduce(15), 9 => TE::Reduce(15), 10 => TE::Reduce(15), 19 => TE::Reduce(15), 20 => TE::Reduce(15), 21 => TE::Reduce(15) },
    hashmap! { 0 => TE::Transit(28), 1 => TE::Transit(2), 5 => TE::Transit(4), 6 => TE::Shift(3), 7 => TE::Shift(5), 8 => TE::Shift(6), 9 => TE::Shift(7), 10 => TE::Shift(8), 19 => TE::Shift(9) },
    hashmap! { 0 => TE::Transit(29), 1 => TE::Transit(2), 5 => TE::Transit(4), 6 => TE::Shift(3), 7 => TE::Shift(5), 8 => TE::Shift(6), 9 => TE::Shift(7), 10 => TE::Shift(8), 19 => TE::Shift(9) },
    hashmap! { 20 => TE::Shift(30) },
    hashmap! { 6 => TE::Reduce(16), 7 => TE::Reduce(16), 8 => TE::Reduce(16), 9 => TE::Reduce(16), 10 => TE::Reduce(16), 19 => TE::Reduce(16), 20 => TE::Reduce(16), 21 => TE::Reduce(16) },
    hashmap! { 0 => TE::Transit(32), 1 => TE::Transit(2), 5 => TE::Transit(4), 6 => TE::Shift(3), 7 => TE::Shift(5), 8 => TE::Shift(6), 9 => TE::Shift(7), 10 => TE::Shift(8), 19 => TE::Shift(9) },
    hashmap! { 20 => TE::Shift(33) },
    hashmap! { 6 => TE::Reduce(17), 7 => TE::Reduce(17), 8 => TE::Reduce(17), 9 => TE::Reduce(17), 10 => TE::Reduce(17), 19 => TE::Reduce(17), 20 => TE::Reduce(17), 21 => TE::Reduce(17) },
    hashmap! { 0 => TE::Transit(35), 1 => TE::Transit(2), 5 => TE::Transit(4), 6 => TE::Shift(3), 7 => TE::Shift(5), 8 => TE::Shift(6), 9 => TE::Shift(7), 10 => TE::Shift(8), 19 => TE::Shift(9) },
    hashmap! { 20 => TE::Shift(36) },
    hashmap! { 6 => TE::Reduce(18), 7 => TE::Reduce(18), 8 => TE::Reduce(18), 9 => TE::Reduce(18), 10 => TE::Reduce(18), 19 => TE::Reduce(18), 20 => TE::Reduce(18), 21 => TE::Reduce(18) },
    hashmap! { 0 => TE::Transit(38), 1 => TE::Transit(2), 5 => TE::Transit(4), 6 => TE::Shift(3), 7 => TE::Shift(5), 8 => TE::Shift(6), 9 => TE::Shift(7), 10 => TE::Shift(8), 19 => TE::Shift(9) },
    hashmap! { 20 => TE::Shift(39) },
    hashmap! { 6 => TE::Reduce(19), 7 => TE::Reduce(19), 8 => TE::Reduce(19), 9 => TE::Reduce(19), 10 => TE::Reduce(19), 19 => TE::Reduce(19), 20 => TE::Reduce(19), 21 => TE::Reduce(19) },
    hashmap! { 3 => TE::Transit(41), 19 => TE::Shift(42) },
    hashmap! { 0 => TE::Transit(43), 1 => TE::Transit(2), 5 => TE::Transit(4), 6 => TE::Shift(3), 7 => TE::Shift(5), 8 => TE::Shift(6), 9 => TE::Shift(7), 10 => TE::Shift(8), 19 => TE::Shift(9) },
    hashmap! { 4 => TE::Transit(45), 6 => TE::Reduce(12), 20 => TE::Reduce(12) },
    hashmap! { 20 => TE::Shift(44) },
    hashmap! { 6 => TE::Reduce(20), 7 => TE::Reduce(20), 8 => TE::Reduce(20), 9 => TE::Reduce(20), 10 => TE::Reduce(20), 19 => TE::Reduce(20), 20 => TE::Reduce(20), 21 => TE::Reduce(20) },
    hashmap! { 6 => TE::Shift(47), 20 => TE::Shift(46) },
    hashmap! { 6 => TE::Reduce(10), 7 => TE::Reduce(10), 8 => TE::Reduce(10), 9 => TE::Reduce(10), 10 => TE::Reduce(10), 19 => TE::Reduce(10) },
    hashmap! { 6 => TE::Reduce(11), 20 => TE::Reduce(11) },
    hashmap! { 0 => TE::Transit(49), 1 => TE::Transit(2), 5 => TE::Transit(4), 6 => TE::Shift(3), 7 => TE::Shift(5), 8 => TE::Shift(6), 9 => TE::Shift(7), 10 => TE::Shift(8), 19 => TE::Shift(9) },
    hashmap! { 20 => TE::Shift(50) },
    hashmap! { 6 => TE::Reduce(21), 7 => TE::Reduce(21), 8 => TE::Reduce(21), 9 => TE::Reduce(21), 10 => TE::Reduce(21), 19 => TE::Reduce(21), 20 => TE::Reduce(21), 21 => TE::Reduce(21) }
];
}

// ------------------------------------
// Module include prologue.
//
// Should include at least result type:
//
// type TResult = <...>;
//
// Can also include parsing hooks:
//
//   fn on_parse_begin(parser: &mut Parser, string: &str) {
//     ...
//   }
//
//   fn on_parse_end(parser: &mut Parser, result: &TResult) {
//     ...
//   }
//

use common::{Expr,Value};

type TResult = Expr;

// ---  end of Module include ---------

/**
 * Generic tokenizer used by the parser in the Syntax tool.
 *
 * https://www.npmjs.com/package/syntax-cli
 */

// ------------------------------------------------------------------
// Token.

#[derive(Debug, Clone, Copy)]
struct Token {
    kind: i32,
    value: &'static str,

    start_offset: i32,
    end_offset: i32,
    start_line: i32,
    end_line: i32,
    start_column: i32,
    end_column: i32,
}

fn str_as_static<'t>(s: &'t str) -> &'static str {
    unsafe {
        std::mem::transmute::<&'t str, &'static str>(s)
    }
}

// NOTE: LEX_RULES_BY_START_CONDITIONS, and TOKENS_MAP
// are defined in the lazy_static! block in lr.templates.rs

// ------------------------------------------------------------------
// Tokenizer.

lazy_static! {
    /** 
     * Pre-parse the regex instead of parsing it every time when calling `get_next_token`.
     */
    static ref REGEX_RULES: Vec<Regex> = LEX_RULES.iter().map(|rule| Regex::with_options(rule, RegexOptions::REGEX_OPTION_SINGLELINE, Syntax::default()).unwrap()).collect();
}

struct Tokenizer<'t> {
    /**
     * Tokenizing string.
     */
    string: &'t str,

    /**
     * Cursor for current symbol.
     */
    cursor: i32,

    /**
     * States.
     */
    states: Vec<&'static str>,

    /**
     * Line-based location tracking.
     */
    current_line: i32,
    current_column: i32,
    current_line_begin_offset: i32,

    /**
     * Location data of a matched token.
     */
    token_start_offset: i32,
    token_end_offset: i32,
    token_start_line: i32,
    token_end_line: i32,
    token_start_column: i32,
    token_end_column: i32,

    /**
     * Matched text, and its length.
     */
    yytext: &'static str,
    yyleng: usize,

    /*
     * Buffer for manually generated tokens in lex handlers.
     * We do need this buffer because for regular unmodified tokens yytext just points to slice in "string",
     * so no extra memory allocated here.
     * But for generated tokens we need some place in memory to keep them up while Tokenizer is alive.
     */
    yybuffer: Vec<String>,

    handlers: [fn(&mut Tokenizer<'t>) -> &'static str; 16],
}

impl<'t> Tokenizer<'t> {

    /**
     * Creates a new Tokenizer instance.
     *
     * The same instance can be then reused in parser
     * by calling `init_string`.
     */
    pub fn new() -> Tokenizer<'t> {
        let mut tokenizer = Tokenizer {
            string: "",
            cursor: 0,

            states: Vec::new(),

            current_line: 1,
            current_column: 0,
            current_line_begin_offset: 0,

            token_start_offset: 0,
            token_end_offset: 0,
            token_start_line: 0,
            token_end_line: 0,
            token_start_column: 0,
            token_end_column: 0,

            yytext: "",
            yyleng: 0,

            yybuffer: Vec::new(),

            handlers: [
    Tokenizer::_lex_rule0,
    Tokenizer::_lex_rule1,
    Tokenizer::_lex_rule2,
    Tokenizer::_lex_rule3,
    Tokenizer::_lex_rule4,
    Tokenizer::_lex_rule5,
    Tokenizer::_lex_rule6,
    Tokenizer::_lex_rule7,
    Tokenizer::_lex_rule8,
    Tokenizer::_lex_rule9,
    Tokenizer::_lex_rule10,
    Tokenizer::_lex_rule11,
    Tokenizer::_lex_rule12,
    Tokenizer::_lex_rule13,
    Tokenizer::_lex_rule14,
    Tokenizer::_lex_rule15
],
        };

        tokenizer
    }

    /**
     * Initializes a parsing string.
     */
    pub fn init_string(&mut self, string: &'t str) -> &mut Tokenizer<'t> {
        self.string = string;

        // Initialize states.
        self.states.clear();
        self.states.push("INITIAL");

        self.cursor = 0;
        self.current_line = 1;
        self.current_column = 0;
        self.current_line_begin_offset = 0;

        self.token_start_offset = 0;
        self.token_end_offset = 0;
        self.token_start_line = 0;
        self.token_end_line = 0;
        self.token_start_column = 0;
        self.token_end_column = 0;

        self
    }

    /**
     * Replace yytext with given string
     */
    pub fn set_yytext(&mut self, s: String) {
        self.yytext = self.string_ref(s);
    }

    /**
     * Move ownership of given string to tokenizer and returns reference to it as &str.
     * Use this method for overriding yytext with new strings wich are not part of text being parsed.
     */
    pub fn string_ref(&mut self, s: String) -> &'static str {
        self.yybuffer.push(s);
        str_as_static(self.yybuffer.last().unwrap().as_str())
    }

    /**
     * Returns next token.
     */
    pub fn get_next_token(&mut self) -> Token {
        if !self.has_more_tokens() {
            self.yytext = EOF;
            return self.to_token(EOF)
        }

        let str_slice = &self.string[self.cursor as usize..];

        let lex_rules_for_state = LEX_RULES_BY_START_CONDITIONS
            .get(self.get_current_state())
            .unwrap();

        for i in lex_rules_for_state {
            let i = *i as usize;
            
            if let Some(matched) = self._match(str_slice, &REGEX_RULES[i]) {

                // Manual handling of EOF token (the end of string). Return it
                // as `EOF` symbol.
                if matched.len() == 0 {
                    self.cursor = self.cursor + 1;
                }
                
                // lifetime of parsed string is greater than lifetime of tokens or tokenizer
                // so it's safe to extend lifetime of matched text
                self.yytext = str_as_static(matched);
                self.yyleng = matched.len();

                let token_type = self.handlers[i](self);

                // "" - no token (skip)
                if token_type.len() == 0 {
                    return self.get_next_token();
                }

                return self.to_token(token_type)
            }
        }

        if self.is_eof() {
            self.cursor = self.cursor + 1;
            self.yytext = EOF;
            return self.to_token(EOF);
        }

        self.panic_unexpected_token(
            &str_slice[0..1],
            self.current_line,
            self.current_column
        );

        unreachable!()
    }

    /**
     * Throws default "Unexpected token" exception, showing the actual
     * line from the source, pointing with the ^ marker to the bad token.
     * In addition, shows `line:column` location.
     */
    fn panic_unexpected_token(&self, string: &str, line: i32, column: i32) {
        let line_source = self.string
            .split('\n')
            .collect::<Vec<&str>>()
            [(line - 1) as usize];

        let pad = ::std::iter::repeat(" ")
            .take(column as usize)
            .collect::<String>();

        let line_data = format!("\n\n{}\n{}^\n", line_source, pad);

        panic!(
            "{} Unexpected token: \"{}\" at {}:{}.",
            line_data,
            string,
            line,
            column
        );
    }

    fn capture_location<'s>(&mut self, matched: &'s str) {
        let nl_re = Regex::new(r"\n").unwrap();

        // Absolute offsets.
        self.token_start_offset = self.cursor;

        // Line-based locations, start.
        self.token_start_line = self.current_line;
        self.token_start_column = self.token_start_offset - self.current_line_begin_offset;

        // Extract `\n` in the matched token.
        for cap in nl_re.captures_iter(matched) {
            self.current_line = self.current_line + 1;
            self.current_line_begin_offset = self.token_start_offset +
                cap.pos(0).unwrap().0 as i32 + 1;
        }

        self.token_end_offset = self.cursor + matched.len() as i32;

        // Line-based locations, end.
        self.token_end_line = self.current_line;
        self.token_end_column = self.token_end_offset - self.current_line_begin_offset;
        self.current_column = self.token_end_column;
    }

    fn _match<'s>(&mut self, str_slice: &'s str, re: &Regex) -> Option<&'s str> {
        match re.captures(str_slice) {
            Some(caps) => {
                let matched = caps.at(0).unwrap();
                self.capture_location(matched);
                self.cursor = self.cursor + (matched.len() as i32);
                Some(matched)
            },
            None => None
        }
    }

    fn to_token(&self, token: &str) -> Token {
        Token {
            kind: *TOKENS_MAP.get(token).expect(
                format!("Token {} was reached, but there is no grammar rule for them", token).as_str()
            ),
            value: self.yytext,
            start_offset: self.token_start_offset,
            end_offset: self.token_end_offset,
            start_line: self.token_start_line,
            end_line: self.token_end_line,
            start_column: self.token_start_column,
            end_column: self.token_end_column,
        }
    }

    /**
     * Whether there are still tokens in the stream.
     */
    pub fn has_more_tokens(&self) -> bool {
        self.cursor <= self.string.len() as i32
    }

    /**
     * Whether the cursor is at the EOF.
     */
    pub fn is_eof(&self) -> bool {
        self.cursor == self.string.len() as i32
    }

    /**
     * Returns current tokenizing state.
     */
    pub fn get_current_state(&self) -> &'static str {
        self.states.last().unwrap_or(&"INITIAL")
    }

    /**
     * Enters a new state pushing it on the states stack.
     */
    pub fn push_state(&mut self, state: &'static str) -> &mut Tokenizer<'t> {
        self.states.push(state);
        self
    }

    /**
     * Alias for `push_state`.
     */
    pub fn begin(&mut self, state: &'static str) -> &mut Tokenizer<'t> {
        self.push_state(state);
        self
    }

    /**
     * Exits a current state popping it from the states stack.
     */
    pub fn pop_state(&mut self) -> &'static str {
        self.states.pop().unwrap_or(&"INITIAL")
    }

    /**
     * Lex rule handlers.
     */
    fn _lex_rule0(&mut self) -> &'static str {
return "'('";
}

fn _lex_rule1(&mut self) -> &'static str {
return "')'";
}

fn _lex_rule2(&mut self) -> &'static str {
/* skip whitespace */ return "";
}

fn _lex_rule3(&mut self) -> &'static str {
return "STRING"
}

fn _lex_rule4(&mut self) -> &'static str {
return "NUMBER"
}

fn _lex_rule5(&mut self) -> &'static str {
return "BOOLEAN"
}

fn _lex_rule6(&mut self) -> &'static str {
return "NULL"
}

fn _lex_rule7(&mut self) -> &'static str {
return "BEGIN"
}

fn _lex_rule8(&mut self) -> &'static str {
return "VAR"
}

fn _lex_rule9(&mut self) -> &'static str {
return "IF"
}

fn _lex_rule10(&mut self) -> &'static str {
return "WHILE"
}

fn _lex_rule11(&mut self) -> &'static str {
return "ASSIGN"
}

fn _lex_rule12(&mut self) -> &'static str {
return "FUNCTION"
}

fn _lex_rule13(&mut self) -> &'static str {
return "LAMBDA"
}

fn _lex_rule14(&mut self) -> &'static str {
return "OPERATOR"
}

fn _lex_rule15(&mut self) -> &'static str {
return "IDENTIFIER"
}
}


// ------------------------------------------------------------------
// Parser.

/**
 * Parser.
 */
pub struct Parser<'t> {
    /**
     * Parsing stack: semantic values.
     */
    values_stack: Vec<SV>,

    /**
     * Parsing stack: state numbers.
     */
    states_stack: Vec<usize>,

    /**
     * Tokenizer instance.
     */
    tokenizer: Tokenizer<'t>,

    /**
     * Semantic action handlers.
     */
    handlers: [fn(&mut Parser<'t>) -> SV; 22],
}

impl<'t> Parser<'t> {
    /**
     * Creates a new Parser instance.
     */
    pub fn new() -> Parser<'t> {
        Parser {
            // Stacks.
            values_stack: Vec::new(),
            states_stack: Vec::new(),

            tokenizer: Tokenizer::new(),

            handlers: [
    Parser::_handler0,
    Parser::_handler1,
    Parser::_handler2,
    Parser::_handler3,
    Parser::_handler4,
    Parser::_handler5,
    Parser::_handler6,
    Parser::_handler7,
    Parser::_handler8,
    Parser::_handler9,
    Parser::_handler10,
    Parser::_handler11,
    Parser::_handler12,
    Parser::_handler13,
    Parser::_handler14,
    Parser::_handler15,
    Parser::_handler16,
    Parser::_handler17,
    Parser::_handler18,
    Parser::_handler19,
    Parser::_handler20,
    Parser::_handler21
],
        }
    }

    /**
     * Parses a string.
     */
    pub fn parse(&mut self, string: &'t str) -> TResult {
        

        // Initialize the tokenizer and the string.
        self.tokenizer.init_string(string);

        // Initialize the stacks.
        self.values_stack.clear();

        // Initial 0 state.
        self.states_stack.clear();
        self.states_stack.push(0);

        let mut token = self.tokenizer.get_next_token();
        let mut shifted_token = token;

        loop {
            let state = *self.states_stack.last().unwrap();
            let column = token.kind;

            if !TABLE[state].contains_key(&column) {
                self.unexpected_token(&token);
                break;
            }

            let entry = &TABLE[state][&column];

            match entry {

                // Shift a token, go to state.
                &TE::Shift(next_state) => {
                    // Push token.
                    self.values_stack.push(SV::_0(token));

                    // Push next state number: "s5" -> 5
                    self.states_stack.push(next_state as usize);

                    shifted_token = token;
                    token = self.tokenizer.get_next_token();
                },

                // Reduce by production.
                &TE::Reduce(production_number) => {
                    let production = PRODUCTIONS[production_number];

                    self.tokenizer.yytext = shifted_token.value;
                    self.tokenizer.yyleng = shifted_token.value.len();

                    let mut rhs_length = production[1];
                    while rhs_length > 0 {
                        self.states_stack.pop();
                        rhs_length = rhs_length - 1;
                    }

                    // Call the handler, push result onto the stack.
                    let result_value = self.handlers[production_number](self);

                    let previous_state = *self.states_stack.last().unwrap();
                    let symbol_to_reduce_with = production[0];

                    // Then push LHS onto the stack.
                    self.values_stack.push(result_value);

                    let next_state = match &TABLE[previous_state][&symbol_to_reduce_with] {
                        &TE::Transit(next_state) => next_state,
                        _ => unreachable!(),
                    };

                    self.states_stack.push(next_state);
                },

                // Accept the string.
                &TE::Accept => {
                    // Pop state number.
                    self.states_stack.pop();

                    // Pop the parsed value.
                    let parsed = self.values_stack.pop().unwrap();

                    if self.states_stack.len() != 1 ||
                        self.states_stack.pop().unwrap() != 0 ||
                        self.tokenizer.has_more_tokens() {
                        self.unexpected_token(&token);
                    }

                    let result = get_result!(parsed, _2);
                    
                    return result;
                },

                _ => unreachable!(),
            }
        }

        unreachable!();
    }

    fn unexpected_token(&self, token: &Token) {
        
  if token.value == EOF && !self.tokenizer.has_more_tokens() {
    panic!("Unexpected end of input.");
  }
  self.tokenizer.panic_unexpected_token(token.value, token.start_line, token.start_column);

    }

    fn _handler0(&mut self) -> SV {
// Semantic values prologue.
let mut _1 = self.values_stack.pop().unwrap();

let __ = _1;
__
}

fn _handler1(&mut self) -> SV {
// Semantic values prologue.
let mut _1 = pop!(self.values_stack, _1);

let __ = Expr::Literal(_1);
SV::_2(__)
}

fn _handler2(&mut self) -> SV {
// Semantic values prologue.
self.values_stack.pop();

let __ = Expr::Identifier(self.tokenizer.yytext.to_string());
SV::_2(__)
}

fn _handler3(&mut self) -> SV {
// Semantic values prologue.
let mut _1 = self.values_stack.pop().unwrap();

let __ = _1;
__
}

fn _handler4(&mut self) -> SV {
// Semantic values prologue.
self.values_stack.pop();

let __ = Value::Int(self.tokenizer.yytext.parse::<isize>().unwrap());
SV::_1(__)
}

fn _handler5(&mut self) -> SV {
// Semantic values prologue.
self.values_stack.pop();

let __ = Value::Str(self.tokenizer.yytext.to_string());
SV::_1(__)
}

fn _handler6(&mut self) -> SV {
// Semantic values prologue.
self.values_stack.pop();

let __ = Value::Boolean(self.tokenizer.yytext == "true");
SV::_1(__)
}

fn _handler7(&mut self) -> SV {
// Semantic values prologue.
self.values_stack.pop();

let __ = Value::Null;
SV::_1(__)
}

fn _handler8(&mut self) -> SV {
// Semantic values prologue.
let mut _2 = pop!(self.values_stack, _2);
let mut _1 = pop!(self.values_stack, _3);

_1.push(_2);
    let __ = _1;
SV::_3(__)
}

fn _handler9(&mut self) -> SV {
// Semantic values prologue.


let __ = Vec::new();
SV::_3(__)
}

fn _handler10(&mut self) -> SV {
// Semantic values prologue.
self.values_stack.pop();
let mut _2 = pop!(self.values_stack, _4);
self.values_stack.pop();

let __ = _2;
SV::_4(__)
}

fn _handler11(&mut self) -> SV {
// Semantic values prologue.
let mut _2 = pop!(self.values_stack, _0);
let mut _1 = pop!(self.values_stack, _4);

_1.push(Value::Str(_2.value.to_string()));
    let __ = _1;
SV::_4(__)
}

fn _handler12(&mut self) -> SV {
// Semantic values prologue.


let __ = Vec::new();
SV::_4(__)
}

fn _handler13(&mut self) -> SV {
// Semantic values prologue.
self.values_stack.pop();
let mut _3 = pop!(self.values_stack, _3);
self.values_stack.pop();
self.values_stack.pop();

let __ = Expr::BlockStatement(_3);
SV::_2(__)
}

fn _handler14(&mut self) -> SV {
// Semantic values prologue.
self.values_stack.pop();
let mut _3 = pop!(self.values_stack, _3);
let mut _2 = pop!(self.values_stack, _0);
self.values_stack.pop();

let __ = Expr::CallExpression(_2.value.to_string(), _3);
SV::_2(__)
}

fn _handler15(&mut self) -> SV {
// Semantic values prologue.
self.values_stack.pop();
let mut _4 = pop!(self.values_stack, _2);
let mut _3 = pop!(self.values_stack, _0);
self.values_stack.pop();
self.values_stack.pop();

let __ = Expr::VariableDeclaration(_3.value.to_string(), Box::new(_4));
SV::_2(__)
}

fn _handler16(&mut self) -> SV {
// Semantic values prologue.
self.values_stack.pop();
let mut _5 = pop!(self.values_stack, _2);
let mut _4 = pop!(self.values_stack, _2);
let mut _3 = pop!(self.values_stack, _2);
self.values_stack.pop();
self.values_stack.pop();

let __ = Expr::IfExpression(Box::new(_3), Box::new(_4), Box::new(_5));
SV::_2(__)
}

fn _handler17(&mut self) -> SV {
// Semantic values prologue.
self.values_stack.pop();
let mut _4 = pop!(self.values_stack, _2);
let mut _3 = pop!(self.values_stack, _2);
self.values_stack.pop();
self.values_stack.pop();

let __ = Expr::WhileStatement(Box::new(_3), Box::new(_4));
SV::_2(__)
}

fn _handler18(&mut self) -> SV {
// Semantic values prologue.
self.values_stack.pop();
let mut _4 = pop!(self.values_stack, _2);
let mut _3 = pop!(self.values_stack, _0);
self.values_stack.pop();
self.values_stack.pop();

let __ = Expr::Assignment(_3.value.to_string(), Box::new(_4));
SV::_2(__)
}

fn _handler19(&mut self) -> SV {
// Semantic values prologue.
self.values_stack.pop();
let mut _4 = pop!(self.values_stack, _2);
let mut _3 = pop!(self.values_stack, _2);
let mut _2 = pop!(self.values_stack, _0);
self.values_stack.pop();

let __ = Expr::BinaryExpression(_2.value.to_string(), Box::new(_3), Box::new(_4));
SV::_2(__)
}

fn _handler20(&mut self) -> SV {
// Semantic values prologue.
self.values_stack.pop();
let mut _5 = pop!(self.values_stack, _2);
let mut _4 = pop!(self.values_stack, _4);
let mut _3 = pop!(self.values_stack, _0);
self.values_stack.pop();
self.values_stack.pop();

let __ = Expr::FunctionDeclaration(_3.value.to_string(), _4, Box::new(_5));
SV::_2(__)
}

fn _handler21(&mut self) -> SV {
// Semantic values prologue.
self.values_stack.pop();
let mut _4 = pop!(self.values_stack, _2);
let mut _3 = pop!(self.values_stack, _4);
self.values_stack.pop();
self.values_stack.pop();

let __ = Expr::LambdaExpression(_3, Box::new(_4));
SV::_2(__)
}
}
