#pragma once

#include "common.hpp"
#include "lexer.hpp"
#include <vector>

enum AstExprKind {
    EXPR_NULL,
    EXPR_IDENT,
    EXPR_INT,
    EXPR_STRING,
    EXPR_UNOP,
    EXPR_BINOP,
    EXPR_MEMBER,
    EXPR_CALL,
    EXPR_CAST
};

enum AstUnOp {
    UNOP_POS,
    UNOP_NEG,
    UNOP_BIT_NOT,
    UNOP_LOG_NOT,
    UNOP_REF,
    UNOP_DEREF,
};

enum AstBinOp {
    BINOP_ADD,
    BINOP_SUB,
    BINOP_MUL,
    BINOP_DIV,
    BINOP_MOD,

    BINOP_ASG,

    BINOP_BIT_AND,
    BINOP_BIT_OR,
    BINOP_BIT_XOR,
    BINOP_BIT_SHL,
    BINOP_BIT_SHR,

    BINOP_LOG_AND,
    BINOP_LOG_OR,

    BINOP_EQ,
    BINOP_NEQ,
    BINOP_GT,
    BINOP_GTE,
    BINOP_LT,
    BINOP_LTE,

    BINOP_IDX,
    BINOP_CALL,
};

std::string ast_expr_to_string(AstExprKind kind);
std::string ast_unop_to_string(AstUnOp kind);
std::string ast_binop_to_string(AstBinOp kind);

struct AstType;

struct AstExpr {
    AstExprKind kind;
    Token token;
    union {
        struct {
            AstExpr* object;
            char* ident;
        };

        struct {
            AstExpr* func;
            u32 param_count;
            AstExpr** params;
        };

        u64 integer;
        char* string;

        struct {
            AstUnOp  unop;
            AstExpr* operand;
        };

        struct {
            AstBinOp binop;
            AstExpr* lhs;
            AstExpr* rhs;
        };

        struct {
            AstExpr* cast_expr;
            AstType* cast_type;
        };
    };

    inline AstExpr() : kind(EXPR_NULL) {}

    inline AstExpr(AstExprKind kind, const Token& token)
        : kind(kind), token(token) {}

    inline static AstExpr* create_binop(AstBinOp binop, AstExpr* lhs, AstExpr* rhs, const Token& token) {
        auto ret = new AstExpr(EXPR_BINOP, token);
        ret->binop = binop;
        ret->lhs   = lhs;
        ret->rhs   = rhs;
        return ret;
    }

    inline static AstExpr* create_call(AstExpr* func, const std::vector<AstExpr*> params, const Token& token) {
        auto ret = new AstExpr(EXPR_CALL, token);
        ret->func = func;
        ret->param_count = params.size();
        ret->params = new AstExpr*[ret->param_count];
        memcpy(ret->params, params.data(), params.size() * sizeof(AstExpr*));
        return ret;
    }

    inline ~AstExpr() {
        if (kind == EXPR_UNOP) {
            if (operand) delete operand;
        } else if (kind == EXPR_BINOP) {
            if (lhs) delete lhs;
            if (rhs) delete rhs;
        }
    }

    std::string to_string(u32 i = 0);
};

struct AstType;

struct AstStructMember {
    char* name;
    AstType* type;
    Token token;
};

enum AstTypeKind {
    ATYPE_IDENT,
    ATYPE_STRUCT,
    ATYPE_ARRAY
};

struct AstType {
    AstTypeKind kind;
    Token token;
    u32 point_count = 0;
    union {
        char* ident;
        struct {
            u32 member_count;
            AstStructMember* members;
        };
        struct {
            AstType* elem;
            u32 elem_count;
        };
    };

    inline AstType(AstTypeKind kind, const Token& token)
        : kind(kind), token(token) {}

    inline ~AstType() {
        if (kind == ATYPE_STRUCT) {
            for (u32 i = 0; i < member_count; i++)
                delete members[i].type;
            delete[] members;
        } else if (kind == ATYPE_ARRAY)
            delete elem;
    }

    static AstType* create_struct(const std::vector<AstStructMember>& members, const Token& token);

    std::string to_string();
};

enum AstStmtKind {
    STMT_LET,
    STMT_IF,
    STMT_WHILE,
    STMT_RETURN,
    STMT_EXPR,
    STMT_BREAK,
    STMT_CONTINUE,
};

struct AstBlock;

struct AstStmt {
    AstStmtKind kind;
    Token token;
    AstExpr* expr = nullptr;
    union {
        struct {
            char* let_ident;
            AstType* let_type;
        };
        AstBlock* while_block;
        struct {
            AstBlock* if_true;
            AstBlock* if_false;
        };
    };

    inline AstStmt(AstStmtKind kind, const Token& token)
        : kind(kind), token(token) {}

    inline ~AstStmt() {
        if (kind == STMT_LET) {
            if (let_type) delete let_type;
        } else if (kind == STMT_IF) {
            if (if_true) delete if_true;
            if (if_false) delete if_false;
        } else if (kind == STMT_WHILE) {
            if (while_block) delete while_block;
        }
        if (expr) delete expr;
    }

    std::string to_string(u32 i);
};

struct AstBlock {
    std::vector<AstStmt*> stmts;

    std::string to_string(u32 i);

    inline ~AstBlock() {
        for (auto s : stmts) delete s;
    }
};

enum AstDeclKind {
    DECL_LET,
    DECL_USE,
    DECL_FN
};

struct AstFuncParam {
    char* name;
    AstType* type;
    Token token;
};

struct AstDecl {
    AstDeclKind kind;
    Token token;
    char* name;
    union {
        struct {
            AstType* type;
            AstExpr* let_expr;
        };
        struct {
            AstType* return_type;
            AstBlock* body;
            u32 param_count;
            AstFuncParam* params;
        };
    };

    inline AstDecl(AstDeclKind kind, char* name, const Token& token)
        : kind(kind), token(token), name(name) {}

    inline static AstDecl* create_func(char* name, AstType* return_type, AstBlock* block, const std::vector<AstFuncParam>& params, const Token& token) {
        auto ret = new AstDecl(DECL_FN, name, token);
        ret->return_type = return_type;
        ret->body = block;
        ret->param_count = params.size();
        ret->params = new AstFuncParam[params.size()];
        memcpy(ret->params, params.data(), params.size() * sizeof(AstFuncParam));
        return ret;
    }

    std::string to_string(u32 i);

    inline ~AstDecl() {
        switch (kind) {
        case DECL_LET:
            if (type) delete type;
            if (let_expr) delete let_expr;
            break;
        case DECL_USE:
            if (type) delete type;
            break;
        case DECL_FN:
            for (u32 i = 0; i < param_count; i++) delete params[i].type;
            if (return_type) delete return_type;
            break;
        }
    }
};

struct AstGlobal {
    std::vector<AstDecl*> decls;

    std::string to_string();

    inline ~AstGlobal() {
        for (auto d : decls)
            delete d;
    }
};