#pragma once

#include "ast.hpp"
#include <unordered_map>
#include <set>

enum BinOpAssoc {
    ASSOC_LTR,
    ASSOC_RTL
};

struct BinOpDef {
    AstBinOp ast;
    u32 precedence;
    BinOpAssoc assoc;
};

struct BinOpPair {
    Token token;
    BinOpDef* def;
};

struct ExprParser {
    std::vector<BinOpPair> binop_stack;
    std::vector<AstExpr*> stack;

    inline void push_binop(BinOpPair expr) { binop_stack.push_back(expr); }
    inline void push(AstExpr* expr) { stack.push_back(expr); }

    inline BinOpPair pop_binop() { BinOpPair e = binop_stack.back(); binop_stack.pop_back(); return e; }
    inline AstExpr* pop() { AstExpr* e = stack.back(); stack.pop_back(); return e; }

    inline bool empty_binop() const { return binop_stack.size() == 0; }
    inline bool empty() const { return stack.size() == 0; }

    ~ExprParser() {
        for (auto e : stack) delete e;
    }
};

class Parser {
public:
    Parser(Lexer& lexer)
        : lexer(lexer) {}
    
    AstExpr* parse_expression(std::set<TokenType> end_tokens, bool can_be_empty = false, TokenType* end_token = nullptr);

    AstBlock* parse_block(TokenType end_token);

    AstType* parse_type();

    AstGlobal* parse_global();

private:
    AstExpr* parse_subexpr(AstExpr* exp);

    inline Token next() { return lexer.next_token(); }

    inline Token peek() { return lexer.peek_token(); }

    inline bool match(TokenType type) { return lexer.match_token(type); }

    inline bool match_or_error(TokenType type, const std::string& msg) {
        auto t = peek();
        if (t.type == type) {
            next();
            return true;
        }
        t.error(msg);
        return false;
    }

private:
    Lexer& lexer;
};