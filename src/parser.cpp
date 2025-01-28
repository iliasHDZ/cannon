#include "parser.hpp"

static std::unordered_map<TokenType, BinOpDef> binop_defs = {
    { TOK_ASTERISK,       { BINOP_MUL,     5,  ASSOC_LTR } },
    { TOK_SLASH,          { BINOP_DIV,     5,  ASSOC_LTR } },
    { TOK_PERCENT,        { BINOP_MOD,     5,  ASSOC_LTR } },
    { TOK_PLUS,           { BINOP_ADD,     6,  ASSOC_LTR } },
    { TOK_MINUS,          { BINOP_SUB,     6,  ASSOC_LTR } },
    { TOK_DB_LESSER,      { BINOP_BIT_SHL, 7,  ASSOC_LTR } },
    { TOK_DB_GREATER,     { BINOP_BIT_SHR, 7,  ASSOC_LTR } },
    { TOK_LESSER,         { BINOP_LT,      9,  ASSOC_LTR } },
    { TOK_GREATER,        { BINOP_GT,      9,  ASSOC_LTR } },
    { TOK_LESSER_EQUALS,  { BINOP_LTE,     9,  ASSOC_LTR } },
    { TOK_GREATER_EQUALS, { BINOP_GTE,     9,  ASSOC_LTR } },
    { TOK_DB_EQUALS,      { BINOP_EQ,      10, ASSOC_LTR } },
    { TOK_EXC_EQUALS,     { BINOP_NEQ,     10, ASSOC_LTR } },
    { TOK_AMPERSAND,      { BINOP_BIT_AND, 11, ASSOC_LTR } },
    { TOK_CARET,          { BINOP_BIT_XOR, 12, ASSOC_LTR } },
    { TOK_PIPE,           { BINOP_BIT_OR,  13, ASSOC_LTR } },
    { TOK_DB_AMPERSAND,   { BINOP_LOG_AND, 14, ASSOC_LTR } },
    { TOK_DB_PIPE,        { BINOP_LOG_OR,  15, ASSOC_LTR } },
    { TOK_EQUALS,         { BINOP_ASG,     16, ASSOC_RTL } },
};

static std::unordered_map<TokenType, AstUnOp> unop_defs = {
    { TOK_PLUS,        UNOP_POS },
    { TOK_MINUS,       UNOP_NEG },
    { TOK_AMPERSAND,   UNOP_REF },
    { TOK_ASTERISK,    UNOP_DEREF },
    { TOK_EXCLAMATION, UNOP_LOG_NOT },
    { TOK_TILDE,       UNOP_BIT_NOT },
};

static bool should_collapse(BinOpDef* a, BinOpDef* b) {
    if (a->precedence != b->precedence) return a->precedence < b->precedence;
    return a->assoc == ASSOC_LTR;
}

static bool expr_collapse(ExprParser& ep, BinOpDef* bop) {
    while (!ep.empty_binop()) {
        auto back = ep.binop_stack.back();
        if (bop != nullptr && !should_collapse(back.def, bop)) break;
        if (ep.stack.size() < 2) return back.token.error("expression expected");
        ep.pop_binop();
        AstExpr* expr = new AstExpr(EXPR_BINOP, back.token);
        expr->binop = back.def->ast;
        expr->rhs = ep.pop();
        expr->lhs = ep.pop();
        ep.push(expr);
    }
    return true;
}

static AstExpr EMPTY_EXPR;

AstExpr* Parser::parse_expression(std::set<TokenType> end_tokens, bool can_be_empty, TokenType* end_token) {
    ExprParser ep;

    bool prevop = true;

    while (true) {
        Token token = next();
        if (end_tokens.find(token.type) != end_tokens.end()) {
            if (end_token)
                *end_token = token.type;
            break;
        }

        if (token.type == TOK_EOF) {
            token.error("unexpected end-of-file");
            break;
        }

        if (prevop) {
            std::vector<AstExpr*> unops;

            while (unop_defs.count(token.type)) {
                AstExpr* expr = new AstExpr(EXPR_UNOP, token);
                expr->unop = unop_defs[token.type];
                unops.push_back(expr);
                token = next();
            }

            AstExpr* expr = nullptr;
            if (token.type == TOK_INTEGER) {
                expr = new AstExpr(EXPR_INT, token);
                expr->integer = token.integer;
            } else if (token.type == TOK_STRING) {
                expr = new AstExpr(EXPR_STRING, token);
                expr->string = token.string;
            } else if (token.type == TOK_IDENTIFIER) {
                expr = new AstExpr(EXPR_IDENT, token);
                expr->ident = token.ident;
            } else if (token.type == TOK_PAREN_OPEN) {
                expr = parse_expression({ TOK_PAREN_CLOSE });
                if (expr == nullptr) {
                    token.error("expected expression");
                    return nullptr;
                }
            } else {
                token.error("unexpected token");
                return nullptr;
            }

            auto exp = parse_subexpr(expr);
            if (exp == nullptr) return nullptr;
            for (i32 i = unops.size() - 1; i >= 0; i--) {
                auto e = unops[i];
                e->operand = exp;
                exp = e;
            }

            ep.push(exp);
        } else {
            if (binop_defs.count(token.type) == 0) {
                token.error("unexpected token");
                return nullptr;
            }

            if (!expr_collapse(ep, &binop_defs[token.type])) return nullptr;

            ep.push_binop({ token, &binop_defs[token.type] });
        }

        prevop = !prevop;
    }

    if (!expr_collapse(ep, nullptr)) return nullptr;
    if (ep.empty()) {
        if (!can_be_empty) {
            peek().error("expected expression");
            return nullptr;
        }
        return &EMPTY_EXPR;
    }
    return ep.pop();
}

AstBlock* Parser::parse_block(TokenType end_token) {
    std::vector<AstStmt*> stmts;
    bool success = false;

    while (true) {
        Token token = peek();
        if (token.type == end_token) {
            next();
            success = true;
            break;
        }

        if (token.type == TOK_EOF) {
            token.error("unexpected end-of-file");
            break;
        }
        
        if (token.type == TOK_LET) {
            next();
            Token ident = next();
            if (ident.type != TOK_IDENTIFIER) {
                ident.error("expected identifier");
                break;
            }
            AstStmt* stmt = new AstStmt(STMT_LET, token);
            stmt->let_ident = ident.ident;
            stmt->let_type  = nullptr;
            stmt->expr = nullptr;
            if (match(TOK_COLON)) {
                stmt->let_type = parse_type();
                if (stmt->let_type == nullptr) break;
            }

            Token t = peek();
            if (t.type != TOK_EQUALS) {
                if (stmt->let_type == nullptr) {
                    t.error("let statement without expression requires a type specifier");
                    break;
                }
                if (!match_or_error(TOK_SEMICOLON, "expected ';'"))
                    break;
                stmts.push_back(stmt);
                continue;
            }
            next();

            stmt->expr = parse_expression({ TOK_SEMICOLON });
            if (stmt->expr == nullptr) {
                delete stmt;
                break;
            }
            stmts.push_back(stmt);
        } else if (token.type == TOK_IF) {
            next();
            AstExpr* cond = parse_expression({ TOK_BRACE_OPEN });
            if (cond == nullptr) break;
            AstBlock* block = parse_block(TOK_BRACE_CLOSE);
            if (block == nullptr) {
                delete cond;
                break;
            }
            AstStmt* stmt = new AstStmt(STMT_IF, token);
            stmt->expr = cond;
            stmt->if_true = block;
            stmt->if_false = nullptr;
            if (!match(TOK_ELSE)) {
                stmts.push_back(stmt);
                continue;
            }
            if (!match_or_error(TOK_BRACE_OPEN, "expected '{'")) break;
            stmt->if_false = parse_block(TOK_BRACE_CLOSE);
            if (stmt->if_false == nullptr) {
                delete stmt;
                break;
            }
            stmts.push_back(stmt);
        } else if (token.type == TOK_WHILE) {
            next();
            AstExpr* cond = parse_expression({ TOK_BRACE_OPEN });
            if (cond == nullptr) break;
            AstBlock* block = parse_block(TOK_BRACE_CLOSE);
            if (block == nullptr) {
                delete cond;
                break;
            }
            AstStmt* stmt = new AstStmt(STMT_WHILE, token);
            stmt->expr = cond;
            stmt->while_block = block;
            stmts.push_back(stmt);
        } else if (token.type == TOK_RETURN) {
            next();
            AstExpr* exp = parse_expression({ TOK_SEMICOLON }, true);
            if (exp == nullptr) break;
            AstStmt* stmt = new AstStmt(STMT_RETURN, token);
            stmt->expr = exp == &EMPTY_EXPR ? nullptr : exp;
            stmts.push_back(stmt);
        } else if (token.type == TOK_BREAK || token.type == TOK_CONTINUE) {
            next();
            if (!match_or_error(TOK_SEMICOLON, "expected ';'")) break;
            stmts.push_back(new AstStmt(token.type == TOK_BREAK ? STMT_BREAK : STMT_CONTINUE, token));
        } else {
            if (match(TOK_SEMICOLON)) continue;
            AstExpr* exp = parse_expression({ TOK_SEMICOLON }, false);
            if (exp == nullptr) break;
            AstStmt* stmt = new AstStmt(STMT_EXPR, token);
            stmt->expr = exp;
            stmts.push_back(stmt);
        }
    }

    if (!success) {
        for (auto stmt : stmts) delete stmt;
        return nullptr;
    }
    return new AstBlock { stmts };
}

AstType* Parser::parse_type() {
    Token token = next();

    AstType* ret = nullptr;

    if (token.type == TOK_IDENTIFIER) {
        ret = new AstType(ATYPE_IDENT, token);
        ret->ident = token.ident;
    } else if (token.type == TOK_BRACE_OPEN) {
        std::vector<AstStructMember> members;
        while (1) {
            Token ident = next();
            if (ident.type == TOK_BRACE_CLOSE) break;
            if (ident.type != TOK_IDENTIFIER) {
                for (auto& mem : members) delete mem.type;
                ident.error("expected identifier");
                return nullptr;
            }
            if (!match_or_error(TOK_COLON, "expected ':'")) {
                for (auto& mem : members) delete mem.type;
                return nullptr;
            }
            AstType* type = parse_type();
            if (type == nullptr) {
                for (auto& mem : members) delete mem.type;
                return nullptr;
            }
            members.push_back({ ident.ident, type, ident });
            if (!match_or_error(TOK_SEMICOLON, "expected ';'")) {
                for (auto& mem : members) delete mem.type;
                return nullptr;
            }
        }
        ret = AstType::create_struct(members, token);
    } else {
        token.error("expected type");
        return nullptr;
    }

    while (1) {
        Token tok = peek();
        if (tok.type == TOK_ASTERISK) {
            next();
            ret->point_count++;
        } else if (tok.type == TOK_BRACKET_OPEN) {
            next();
            Token num = next();
            if (num.type != TOK_INTEGER) {
                num.error("expected integer");
                return nullptr;
            }
            if (!match_or_error(TOK_BRACKET_CLOSE, "expected ']'"))
                return nullptr;
            auto array = new AstType(ATYPE_ARRAY, tok);
            array->elem = ret;
            array->elem_count = num.integer;
            ret = array;
        } else
            break;
    }

    return ret;
}

AstGlobal* Parser::parse_global() {
    std::vector<AstDecl*> decls;
    bool success = false;

    while (true) {
        Token token = peek();
        if (token.type == TOK_EOF) {
            success = true;
            break;
        }
        
        if (token.type == TOK_LET) {
            next();
            Token ident = next();
            if (ident.type != TOK_IDENTIFIER) {
                ident.error("expected identifier");
                break;
            }
            AstDecl* decl = new AstDecl(DECL_LET, ident.ident, token);
            decl->type = nullptr;
            decl->let_expr = nullptr;
            if (match(TOK_COLON)) {
                decl->type = parse_type();
                if (decl->type == nullptr) break;
            }

            Token t = peek();
            if (t.type != TOK_EQUALS) {
                if (decl->type == nullptr) {
                    t.error("let declaration without expression requires a type specifier");
                    break;
                }
                if (!match_or_error(TOK_SEMICOLON, "expected ';'"))
                    break;
                decls.push_back(decl);
                continue;
            }
            next();

            decl->let_expr = parse_expression({ TOK_SEMICOLON });
            if (decl->let_expr == nullptr) {
                delete decl;
                break;
            }
            decls.push_back(decl);
        } else if (token.type == TOK_USE) {
            next();
            Token ident = next();
            if (ident.type != TOK_IDENTIFIER) {
                ident.error("expected identifier");
                break;
            }
            if (!match_or_error(TOK_EQUALS, "expected '='")) break;
            AstType* type = parse_type();
            if (type == nullptr) break;
            if (!match_or_error(TOK_SEMICOLON, "expected ';'")) {
                delete type;
                break;
            }
            AstDecl* decl = new AstDecl(DECL_USE, ident.ident, token);
            decl->type = type;
            decls.push_back(decl);
        } else if (token.type == TOK_FN) {
            next();
            Token ident = next();
            if (ident.type != TOK_IDENTIFIER) {
                ident.error("expected identifier");
                break;
            }
            if (!match_or_error(TOK_PAREN_OPEN, "expected '('")) break;
            std::vector<AstFuncParam> params;
            bool scs = true;
            if (!match(TOK_PAREN_CLOSE)) {
                while (true) {
                    Token pident = next();
                    if (pident.type != TOK_IDENTIFIER) {
                        pident.error("expected identifier");
                        scs = false;
                        break;
                    }
                    if (!match_or_error(TOK_COLON, "expected ':'")) {
                        scs = false;
                        break;
                    }
                    AstType* type = parse_type();
                    if (type == nullptr) {
                        scs = false;
                        break;
                    }
                    params.push_back({ pident.ident, type, pident });
                    Token nxt = next();
                    if (nxt.type == TOK_PAREN_CLOSE) break;
                    if (nxt.type != TOK_COMMA) {
                        nxt.error("expected ')'");
                        scs = false;
                        break;
                    }
                }
            }
            if (!scs) {
                for (auto p : params) delete p.type;
                break;
            }
            if (!match_or_error(TOK_ARROW, "expected '->'")) {
                for (auto p : params) delete p.type;
                break;
            }
            AstType* return_type = parse_type();
            if (return_type == nullptr) {
                for (auto p : params) delete p.type;
                break;
            }
            AstBlock* block = nullptr;
            if (!match(TOK_SEMICOLON)) {
                if (!match_or_error(TOK_BRACE_OPEN, "expected '{'")) {
                    for (auto p : params) delete p.type;
                    break;
                }
                block = parse_block(TOK_BRACE_CLOSE);
                if (block == nullptr) {
                    for (auto p : params) delete p.type;
                    delete return_type;
                    break;
                }
            }
            decls.push_back(AstDecl::create_func(ident.ident, return_type, block, params, token));
        } else {
            token.error("unexpected token");
            break;
        }
    }

    if (!success) {
        for (auto decl : decls) delete decl;
        return nullptr;
    }
    return new AstGlobal { decls };
}

AstExpr* Parser::parse_subexpr(AstExpr* exp) {
    while (true) {
        Token token = peek();

        if (token.type == TOK_PERIOD) {
            next();
            Token ident = next();
            if (ident.type != TOK_IDENTIFIER) {
                ident.error("expected identifier");
                return nullptr;
            }

            AstExpr* expr = new AstExpr(EXPR_MEMBER, token);
            expr->ident  = ident.ident;
            expr->object = exp;
            exp = expr;
        } else if (token.type == TOK_BRACKET_OPEN) {
            next();
            bool array_index = token.type == TOK_BRACKET_OPEN;
            AstExpr* expr = parse_expression({ TOK_BRACKET_CLOSE });
            if (expr == nullptr) {
                token.error("expected expression");
                return nullptr;
            }

            AstExpr* idx = AstExpr::create_binop(BINOP_IDX, exp, expr, token);
            exp = idx;
        } else if (token.type == TOK_PAREN_OPEN) {
            next();
            std::vector<AstExpr*> params;
            bool success = true;
            if (!match(TOK_PAREN_CLOSE)) {
                TokenType end_token = TOK_COMMA;
                while (end_token == TOK_COMMA) {
                    AstExpr* expr = parse_expression({ TOK_COMMA, TOK_PAREN_CLOSE }, false, &end_token);
                    if (expr == nullptr) {
                        success = false;
                        break;
                    }
                    params.push_back(expr);
                }
            }
            if (!success) {
                for (auto p : params) delete p;
                return nullptr;
            }
            exp = AstExpr::create_call(exp, params, token);
        } else if (token.type == TOK_AS) {
            next();
            if (!match_or_error(TOK_PAREN_OPEN, "expected '('")) return nullptr;
            AstType* tp = parse_type();
            if (tp == nullptr) return nullptr;
            if (!match_or_error(TOK_PAREN_CLOSE, "expected ')'")) {
                delete tp;
                return nullptr;
            }
            auto expr = new AstExpr(EXPR_CAST, token);
            expr->cast_expr = exp;
            expr->cast_type = tp;
            exp = expr;
        } else
            return exp;
    }
}