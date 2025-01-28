#pragma once

#include "common.hpp"
#include "string.hpp"
#include "error.hpp"

enum TokenType {
    TOK_INTEGER,
    TOK_STRING,
    TOK_IDENTIFIER,

    TOK_FN,
    TOK_LET,
    TOK_IF,
    TOK_ELSE,
    TOK_WHILE,
    TOK_RETURN,
    TOK_BREAK,
    TOK_CONTINUE,
    TOK_USE,
    TOK_AS,

    TOK_PAREN_OPEN,
    TOK_PAREN_CLOSE,
    TOK_BRACKET_OPEN,
    TOK_BRACKET_CLOSE,
    TOK_BRACE_OPEN,
    TOK_BRACE_CLOSE,

    TOK_PERIOD,
    TOK_COLON,
    TOK_SEMICOLON,
    TOK_COMMA,
    TOK_ARROW,

    TOK_PLUS,           // +
    TOK_MINUS,          // -
    TOK_ASTERISK,       // *
    TOK_SLASH,          // /
    TOK_PERCENT,        // %
    TOK_AMPERSAND,      // &
    TOK_DB_AMPERSAND,   // &&
    TOK_PIPE,           // |
    TOK_DB_PIPE,        // ||
    TOK_EQUALS,         // =
    TOK_DB_EQUALS,      // ==
    TOK_EXCLAMATION,    // !
    TOK_TILDE,          // ~
    TOK_CARET,          // ^
    TOK_EXC_EQUALS,     // !=
    TOK_GREATER,        // >
    TOK_DB_GREATER,     // >>
    TOK_GREATER_EQUALS, // >=
    TOK_LESSER,         // <
    TOK_DB_LESSER,      // <<
    TOK_LESSER_EQUALS,  // <=

    TOK_ERROR,
    TOK_EOF,
};

extern bool error_compiling;

struct Token {
    TokenType type;
    u32 line, column;
    char* filename;
    union {
        char* string;
        char* ident;
        u64   integer;
    };

    inline bool error(const std::string& msg) {
        error_compiling = true;
        print_error(line, column, filename, msg);
        return false;
    }
};

const char* token_type_to_string(TokenType type);

std::string token_to_string(const Token& token);

class Lexer {
public:
    inline Lexer(const std::string& code, const std::string& filename)
        : code(code), filename(StringManager::create(filename.data(), filename.size())) {}

    /* Returns the next token */
    Token next_token();

    inline Token peek_token() {
        u32 i = index, l = line, c = column;
        Token t = next_token();
        index  = i;
        line   = l;
        column = c;
        return t;
    }

    inline bool match_token(TokenType type) {
        u32 i = index, l = line, c = column;
        if (next_token().type != type) {
            index  = i;
            line   = l;
            column = c;
            return false;
        }
        return true;
    }

private:
    /* Returns the current character */
    inline char peek() const { return index >= code.size() ? 0 : code[index]; }

    /*
        Returns the current character and goes to the next one while updating
        line and column accordingly.
    */
    char consume();

    /* Checks if the current character matches the argument. If so, consume() is called. */
    inline bool match(char ch) {
        if (peek() != ch) return false;
        consume();
        return true;
    }

    /* Creates a token structure */
    inline Token create_token(TokenType type) {
        return { type, line, column, filename };
    }

    char read_string_char(Token& tok);

    Token handle_string();

    /* Handle token that starts with a symbol. */
    Token handle_symbol();

    /* Handle token that starts with a letter or underscore. Could be an identifier or keyword. */
    Token handle_word();

    /* Handle token that starts with a number. Basically an integer. */
    Token handle_number();

private:
    u32 index = 0, line = 0, column = 0;
    char* filename;
    std::string code;
};