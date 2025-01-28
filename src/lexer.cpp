#include "lexer.hpp"
#include <unordered_map>

bool error_compiling = false;

const char* token_type_to_string(TokenType type) {
    switch (type) {
    case TOK_INTEGER:        return "TOK_INTEGER";
    case TOK_STRING:         return "TOK_STRING";
    case TOK_IDENTIFIER:     return "TOK_IDENTIFIER";
    case TOK_FN:             return "TOK_FN";
    case TOK_LET:            return "TOK_LET";
    case TOK_IF:             return "TOK_IF";
    case TOK_ELSE:           return "TOK_ELSE";
    case TOK_WHILE:          return "TOK_WHILE";
    case TOK_RETURN:         return "TOK_RETURN";
    case TOK_PAREN_OPEN:     return "TOK_PAREN_OPEN";
    case TOK_PAREN_CLOSE:    return "TOK_PAREN_CLOSE";
    case TOK_BRACKET_OPEN:   return "TOK_BRACKET_OPEN";
    case TOK_BRACKET_CLOSE:  return "TOK_BRACKET_CLOSE";
    case TOK_BRACE_OPEN:     return "TOK_BRACE_OPEN";
    case TOK_BRACE_CLOSE:    return "TOK_BRACE_CLOSE";
    case TOK_COLON:          return "TOK_COLON";
    case TOK_SEMICOLON:      return "TOK_SEMICOLON";
    case TOK_COMMA:          return "TOK_COMMA";
    case TOK_PLUS:           return "TOK_PLUS";
    case TOK_MINUS:          return "TOK_MINUS";
    case TOK_ASTERISK:       return "TOK_ASTERISK";
    case TOK_SLASH:          return "TOK_SLASH";
    case TOK_PERCENT:        return "TOK_PERCENT";
    case TOK_AMPERSAND:      return "TOK_AMPERSAND";
    case TOK_DB_AMPERSAND:   return "TOK_DB_AMPERSAND";
    case TOK_PIPE:           return "TOK_PIPE";
    case TOK_DB_PIPE:        return "TOK_DB_PIPE";
    case TOK_EQUALS:         return "TOK_EQUALS";
    case TOK_DB_EQUALS:      return "TOK_DB_EQUALS";
    case TOK_EXCLAMATION:    return "TOK_EXCLAMATION";
    case TOK_TILDE:          return "TOK_TILDE";
    case TOK_CARET:          return "TOK_CARET";
    case TOK_EXC_EQUALS:     return "TOK_EXC_EQUALS";
    case TOK_GREATER:        return "TOK_GREATER";
    case TOK_GREATER_EQUALS: return "TOK_GREATER_EQUALS";
    case TOK_LESSER:         return "TOK_LESSER";
    case TOK_LESSER_EQUALS:  return "TOK_LESSER_EQUALS";
    case TOK_ERROR:          return "TOK_ERROR";
    case TOK_EOF:            return "TOK_EOF";
    default:
        return "<unknown>";
    }
}

std::string token_to_string(const Token& token) {
    std::string ret = token_type_to_string(token.type);
    switch (token.type) {
    case TOK_INTEGER:    ret += '(' + std::to_string(token.integer) + ')'; break;
    case TOK_IDENTIFIER: ret += '(' + std::string(token.ident)      + ')'; break;
    case TOK_STRING:     ret += '(' + std::string(token.string)     + ')'; break;
    }
    return ret;
}

static bool is_letter(char ch) {
    return ( ch >= 'a' && ch <= 'z' ) ||
           ( ch >= 'A' && ch <= 'Z' ) ||
           ch == '_';
}

static bool is_number(char ch) {
    return ch >= '0' && ch <= '9';
}

static bool is_alphanum(char ch) {
    return is_letter(ch) || is_number(ch);
}

static bool is_whitespace(char ch) {
    return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r';
}

char Lexer::consume() {
    if (index >= code.size()) return 0;
    char ret = code[index++];
    column++;
    if (ret == '\n') {
        column = 0;
        line++;
    }
    return ret;
}

static bool same_string(const char* str, const char* span, u32 len) {
    if (strlen(str) != len) return false;
    return memcmp(str, span, len) == 0;
}

char Lexer::read_string_char(Token& tok) {
    char ch = consume();
    if (ch != '\\')
        return ch;

    if (ch == 0) {
        tok.error("unterminated string literal");
        return 0;
    }
    
    switch (consume()) {
    case 'a':  return '\a';
    case 'b':  return '\b';
    case 'e':  return '\e';
    case 'f':  return '\f';
    case 'n':  return '\n';
    case 'r':  return '\r';
    case 't':  return '\t';
    case 'v':  return '\v';
    case '\\': return '\\';
    case '\'': return '\'';
    case '\"': return '\"';
    default:
        tok.error("invalid escape code");
        return 0;
    }
}

Token Lexer::handle_string() {
    Token token = create_token(TOK_STRING);
    char ch = consume();

    if (ch == '\'') {
        token.type = TOK_INTEGER;
        token.integer = read_string_char(token);
        if (token.integer == 0)
            return create_token(TOK_ERROR);
        if (!match('\'')) {
            token.error("expected '");
            return create_token(TOK_ERROR);
        }
        return token;
    }

    std::string str = "";
    while (peek() != '\"') {
        ch = read_string_char(token);
        if (ch == 0) return create_token(TOK_ERROR);
        str += (u8)ch;
    }
    token.string = StringManager::create(str.data(), str.size());
    consume();
    return token;
}

Token Lexer::handle_symbol() {
    Token token = create_token(TOK_ERROR);

    switch (consume()) {
    case '+': token.type = TOK_PLUS; break;
    case '-':
        if (match('>')) token.type = TOK_ARROW;
        else            token.type = TOK_MINUS;
        break;
    case '*': token.type = TOK_ASTERISK; break;
    case '/': token.type = TOK_SLASH; break;
    case '%': token.type = TOK_PERCENT; break;
    case '~': token.type = TOK_TILDE; break;
    case '^': token.type = TOK_CARET; break;
    case '.': token.type = TOK_PERIOD; break;
    case ',': token.type = TOK_COMMA; break;
    case ':': token.type = TOK_COLON; break;
    case ';': token.type = TOK_SEMICOLON; break;
    case '(': token.type = TOK_PAREN_OPEN; break;
    case ')': token.type = TOK_PAREN_CLOSE; break;
    case '[': token.type = TOK_BRACKET_OPEN; break;
    case ']': token.type = TOK_BRACKET_CLOSE; break;
    case '{': token.type = TOK_BRACE_OPEN; break;
    case '}': token.type = TOK_BRACE_CLOSE; break;
    case '&':
        if (match('&')) token.type = TOK_DB_AMPERSAND;
        else            token.type = TOK_AMPERSAND;
        break;
    case '|':
        if (match('|')) token.type = TOK_DB_PIPE;
        else            token.type = TOK_PIPE;
        break;
    case '=':
        if (match('=')) token.type = TOK_DB_EQUALS;
        else            token.type = TOK_EQUALS;
        break;
    case '!':
        if (match('=')) token.type = TOK_EXC_EQUALS;
        else            token.type = TOK_EXCLAMATION;
        break;
    case '>':
        if      (match('=')) token.type = TOK_GREATER_EQUALS;
        else if (match('>')) token.type = TOK_DB_GREATER;
        else                 token.type = TOK_GREATER;
        break;
    case '<':
        if      (match('=')) token.type = TOK_LESSER_EQUALS;
        else if (match('<')) token.type = TOK_DB_LESSER;
        else                 token.type = TOK_LESSER;
        break;
    default:
        token.error("unknown symbol");
    }

    return token;
}

Token Lexer::handle_word() {
    Token token = create_token(TOK_IDENTIFIER);

    u32 start = index;
    while (is_alphanum(peek()))
        consume();
    
    char* w = code.data() + start;
    u32   l = index - start;

    if      (same_string("fn",       w, l)) token.type = TOK_FN;
    else if (same_string("let",      w, l)) token.type = TOK_LET;
    else if (same_string("if",       w, l)) token.type = TOK_IF;
    else if (same_string("else",     w, l)) token.type = TOK_ELSE;
    else if (same_string("while",    w, l)) token.type = TOK_WHILE;
    else if (same_string("return",   w, l)) token.type = TOK_RETURN;
    else if (same_string("break",    w, l)) token.type = TOK_BREAK;
    else if (same_string("continue", w, l)) token.type = TOK_CONTINUE;
    else if (same_string("use",      w, l)) token.type = TOK_USE;
    else if (same_string("as",       w, l)) token.type = TOK_AS;

    if (token.type == TOK_IDENTIFIER)
        token.ident = StringManager::create(w, l);
    return token;
}

static i8 get_char_val(char ch) {
    if (ch >= '0' || ch <= '9') return ch - '0';
    if (ch >= 'a' || ch <= 'f') return ch - 'a' + 10;
    if (ch >= 'A' || ch <= 'F') return ch - 'A' + 10;
    return -1;
}

Token Lexer::handle_number() {
    Token token = create_token(TOK_INTEGER);

    char pfx = 0;
    if (peek() == '0') {
        consume();
        if (is_letter(peek()))
            pfx = consume();
    }

    if (pfx) pfx |= 0x20;

    u32 radix = 10;
    switch (pfx) {
    case 'x': radix = 16; break;
    case 'b': radix = 2;  break;
    case 0: break;
    default:
        token.error("invalid integer");
        token.type = TOK_ERROR;
        return token;
    }

    token.integer = 0;

    char ch;
    while (is_alphanum(ch = peek())) {
        if (ch == '_') continue;
        i8 val = get_char_val(ch);
        if (val == -1 || val >= radix) {
            token.error("invalid integer");
            token.type = TOK_ERROR;
            return token;
        }
        token.integer = token.integer * radix + val;
        consume();
    }

    return token;
}

Token Lexer::next_token() {
    while (is_whitespace(peek()))
        consume();
    
    char ch = peek();
    if (ch == 0)
        return create_token(TOK_EOF);
    if (is_letter(ch))
        return handle_word();
    if (is_number(ch))
        return handle_number();
    if (ch == '\'' || ch == '\"')
        return handle_string();
    return handle_symbol();
}