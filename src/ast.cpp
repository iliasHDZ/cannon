#include "ast.hpp"

std::string ast_expr_to_string(AstExprKind type) {
    switch (type) {
    case EXPR_IDENT:  return "EXPR_IDENT";
    case EXPR_INT:    return "EXPR_INT";
    case EXPR_UNOP:   return "EXPR_UNOP";
    case EXPR_BINOP:  return "EXPR_BINOP";
    case EXPR_MEMBER: return "EXPR_MEMBER";
    case EXPR_CALL:   return "EXPR_CALL";
    default:
        return "<unknown>";
    }
}

std::string ast_unop_to_string(AstUnOp type) {
    switch (type) {
    case UNOP_POS:     return "UNOP_POS";
    case UNOP_NEG:     return "UNOP_NEG";
    case UNOP_REF:     return "UNOP_REF";
    case UNOP_BIT_NOT: return "UNOP_BIT_NOT";
    case UNOP_LOG_NOT: return "UNOP_LOG_NOT";
    case UNOP_DEREF:   return "UNOP_DEREF";
    default:
        return "<unknown>";
    }
}

std::string ast_binop_to_string(AstBinOp type) {
    switch (type) {
    case BINOP_ADD:     return "BINOP_ADD";
    case BINOP_SUB:     return "BINOP_SUB";
    case BINOP_MUL:     return "BINOP_MUL";
    case BINOP_DIV:     return "BINOP_DIV";
    case BINOP_MOD:     return "BINOP_MOD";
    case BINOP_ASG:     return "BINOP_ASG";
    case BINOP_BIT_AND: return "BINOP_BIT_AND";
    case BINOP_BIT_OR:  return "BINOP_BIT_OR";
    case BINOP_BIT_XOR: return "BINOP_BIT_XOR";
    case BINOP_BIT_SHL: return "BINOP_BIT_SHL";
    case BINOP_BIT_SHR: return "BINOP_BIT_SHR";
    case BINOP_LOG_AND: return "BINOP_LOG_AND";
    case BINOP_LOG_OR:  return "BINOP_LOG_OR";
    case BINOP_EQ:      return "BINOP_EQ";
    case BINOP_NEQ:     return "BINOP_NEQ";
    case BINOP_GT:      return "BINOP_GT";
    case BINOP_GTE:     return "BINOP_GTE";
    case BINOP_LT:      return "BINOP_LT";
    case BINOP_LTE:     return "BINOP_LTE";
    case BINOP_IDX:     return "BINOP_IDX";
    case BINOP_CALL:    return "BINOP_CALL";
    default:
        return "<unknown>";
    }
}

static std::string nline(u32 i, std::string str) {
    std::string out;
    out.resize(i + str.size() + 1, ' ');
    memcpy(out.data() + i, str.data(), str.size());
    out[i + str.size()] = '\n';
    return out;
}

std::string AstExpr::to_string(u32 i) {
    std::string es = ast_expr_to_string(kind);
    switch (kind) {
    case EXPR_IDENT:
        return nline(i, es + "(" + std::string(ident) + ")");
    case EXPR_INT:
        return nline(i, es + "(" + std::to_string(integer) + ")");
    case EXPR_UNOP:
        return nline(i, es + "(" + ast_unop_to_string(unop) + ")") + operand->to_string(i + 4);
    case EXPR_BINOP:
        return nline(i, es + "(" + ast_binop_to_string(binop) + ")") + lhs->to_string(i + 4) + rhs->to_string(i + 4);
    case EXPR_MEMBER:
        return nline(i, es + "(" + std::string(ident) + ")") + object->to_string(i + 4);
    case EXPR_CALL:
        es = nline(i, es) + func->to_string(i + 4);
        for (u32 j = 0; j < param_count; j++)
            es += params[j]->to_string(i + 4);
        return es;
    }
    return "\n";
}

AstType* AstType::create_struct(const std::vector<AstStructMember>& members, const Token& token) {
    auto ret = new AstType(ATYPE_STRUCT, token);
    ret->member_count = members.size();
    ret->members = new AstStructMember[members.size()];
    memcpy(ret->members, members.data(), members.size() * sizeof(AstStructMember));
    return ret;
}

std::string AstType::to_string() {
    std::string ret = "";
    if (kind == ATYPE_IDENT)
        ret = ident;
    else if (kind == ATYPE_STRUCT) {
        ret = "{ ";
        for (u32 i = 0; i < member_count; i++) {
            if (i != 0) ret += ", ";
            ret += members[i].name + std::string(": ") + members[i].type->to_string();
        }
        ret += " }";
    } else if (kind == ATYPE_ARRAY) {
        ret = elem->to_string() + '[' + std::to_string(elem_count) + ']';
    }
    for (u32 i = 0; i < point_count; i++)
        ret += '*';
    return ret;
}

std::string AstStmt::to_string(u32 i) {
    std::string str;
    switch (kind) {
    case STMT_LET:
        str = "STMT_LET";
        if (let_type)
            str += '(' + let_type->to_string() + ')';
        str += ' ' + std::string(let_ident);
        return nline(i, str) + (expr ? expr->to_string(i + 4) : "");
    case STMT_IF:
        str = nline(i, "STMT_IF");
        str += expr->to_string(i + 4);
        str += nline(i + 2, "{");
        str += if_true->to_string(i + 4);
        str += nline(i + 2, "}");
        if (if_false) {
            str += nline(i + 2, "else");
            str += nline(i + 2, "{");
            str += if_false->to_string(i + 4);
            str += nline(i + 2, "}");
        }
        return str;
    case STMT_WHILE:
        return nline(i, "STMT_WHILE") + expr->to_string(i + 4) + nline(i + 2, "{") + while_block->to_string(i + 4) + nline(i + 2, "}");
    case STMT_RETURN:
        return nline(i, "STMT_RETURN") + expr->to_string(i + 4);
    case STMT_EXPR:
        return nline(i, "STMT_EXPR") + expr->to_string(i + 4);
    case STMT_CONTINUE:
        return nline(i, "STMT_CONTINUE");
    case STMT_BREAK:
        return nline(i, "STMT_BREAK");
    }
    return "\n";
}

std::string AstBlock::to_string(u32 i) {
    std::string str = "";
    for (auto stmt : stmts)
        str += stmt->to_string(i);
    return str;
}

std::string AstDecl::to_string(u32 i) {
    std::string str;
    switch (kind) {
    case DECL_LET:
        str = "DECL_LET";
        if (type)
            str += '(' + type->to_string() + ')';
        str += ' ' + std::string(name);
        return nline(i, str) + (let_expr ? let_expr->to_string(i + 4) : "");
    case DECL_USE:
        str = "DECL_USE";
        if (type)
            str += '(' + type->to_string() + ')';
        str += ' ' + std::string(name);
        return nline(i, str);
    case DECL_FN:
        str = "DECL_FN " + std::string(name) + '(';
        for (u32 i = 0; i < param_count; i++) {
            if (i != 0) str += ", ";
            str += params[i].name + std::string(": ") + params[i].type->to_string();
        }
        str += ") -> " + type->to_string();
        return nline(i, str) + (body ? body->to_string(i + 1) : "");
    }
    return "\n";
}

std::string AstGlobal::to_string() {
    std::string ret = "";
    for (auto d : decls)
        ret += d->to_string(0);
    return ret;
}