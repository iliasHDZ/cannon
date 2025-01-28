#include "process.hpp"

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Host.h>

static llvm::LLVMContext llvm_ctx;

static std::unordered_map<BaseType*, llvm::Type*> type_cache;

static llvm::Type* type_to_llvm(BaseType* type) {
    if (type_cache.contains(type)) return type_cache[type];

    llvm::Type* ret = nullptr;
    switch (type->get_kind()) {
    case TYPE_ERROR: break;
    case TYPE_VOID:
        ret = llvm::Type::getVoidTy(llvm_ctx);
        break;
    case TYPE_INT:
        ret = llvm::Type::getIntNTy(llvm_ctx, type->get_size() * 8);
        break;
    case TYPE_POINTER:
        ret = llvm::PointerType::get(llvm_ctx, 0);//getUnqual(type_to_llvm(type->get_pointee()));
        break;
    case TYPE_STRUCT:
        {
            std::vector<llvm::Type*> types;
            for (auto& mem : type->get_members())
                types.push_back(type_to_llvm(mem.type));

            llvm::StructType* stc;
            if (type->is_named())
                stc = llvm::StructType::create(llvm_ctx, types, type->to_string());
            else
                stc = llvm::StructType::create(llvm_ctx, types);
            ret = stc;
            break;
        }
    case TYPE_FUNC:
        {
            std::vector<llvm::Type*> types;
            for (auto& param : type->get_params())
                types.push_back(type_to_llvm(param));
            
            ret = llvm::FunctionType::get(type_to_llvm(type->get_return_type()), types, false);
            break;
        }
    case TYPE_ARRAY:
        ret = llvm::ArrayType::get(type_to_llvm(type->get_pointee()), type->get_array_size());
        break;
    }
    if (ret != nullptr)
        type_cache[type] = ret;
    return ret;
}

llvm::Value* CodegenValue::get_value() {
    if (value) return value;
    return proc->get_builder()->CreateLoad(type_to_llvm(type), address, ident ? ("val." + std::string(ident)) : "");
}

CodeIdentifier LocalScope::find_ident(char* name) {
    if (local_exists(name))
        return locals[name];

    if (parent)
        return parent->find_ident(name);

    i32 i = 0;
    for (auto& param : func->get_params()) {
        if (param.name == name) {
            CodeIdentifier id = { param.type };
            id.func_param = i;
            return id;
        }
        i++;
    }

    GlobalObject* obj = func->get_global()->fetch_object(name);
    if (obj == nullptr) return { nullptr };

    CodeIdentifier id = { obj->get_type() };
    id.global = obj;
    return id;
}

CodeProcessor::CodeProcessor() {
    add_binop(
        {
            BINOP_ADD, BINOP_SUB, BINOP_MUL, BINOP_DIV,
            BINOP_MOD, BINOP_BIT_AND, BINOP_BIT_OR, BINOP_BIT_XOR
        }, 
        TYPE_INT,
        TYPE_INT,
        [](AstBinOp op, AstExpr* ast, AnalysisValue& a, AnalysisValue& b) -> AnalysisValue {
            return {
                Type::get_common_int(a.type, b.type),
                false
            };
        },
        [](CodeProcessor* proc, AstBinOp op, CodegenValue& lhs, CodegenValue& rhs, BaseType* out) -> CodegenValue {
            llvm::Value* vlhs = proc->codegen_cast(lhs, lhs.get_type(), out);
            llvm::Value* vrhs = proc->codegen_cast(rhs, rhs.get_type(), out);
            auto builder = proc->get_builder();
            llvm::Value* vout = nullptr;

            switch (op) {
            case BINOP_ADD: vout = builder->CreateAdd(vlhs, vrhs); break;
            case BINOP_SUB: vout = builder->CreateSub(vlhs, vrhs); break;
            case BINOP_MUL: vout = builder->CreateMul(vlhs, vrhs); break;
            case BINOP_DIV:
                if (out->int_signed())
                    vout = builder->CreateSDiv(vlhs, vrhs);
                else
                    vout = builder->CreateUDiv(vlhs, vrhs);
                break;
            case BINOP_MOD:
                if (out->int_signed())
                    vout = builder->CreateSRem(vlhs, vrhs);
                else
                    vout = builder->CreateURem(vlhs, vrhs);
                break;
            case BINOP_BIT_AND: vout = builder->CreateAnd(vlhs, vrhs); break;
            case BINOP_BIT_OR:  vout = builder->CreateOr(vlhs, vrhs); break;
            case BINOP_BIT_XOR: vout = builder->CreateXor(vlhs, vrhs); break;
            }
            return CodegenValue::create(proc, out, vout);
        }
    );

    add_binop({ BINOP_BIT_SHL, BINOP_BIT_SHR }, TYPE_INT, TYPE_INT,
        [](AstBinOp op, AstExpr* ast, AnalysisValue& a, AnalysisValue& b) -> AnalysisValue {
            return { a.type, false };
        },
        [](CodeProcessor* proc, AstBinOp op, CodegenValue& lhs, CodegenValue& rhs, BaseType* out) -> CodegenValue {
            auto builder = proc->get_builder();
            llvm::Value* vout = nullptr;
            switch (op) {
            case BINOP_BIT_SHL: vout = builder->CreateShl(lhs.get_value(), rhs.get_value()); break;
            case BINOP_BIT_SHR: vout = builder->CreateLShr(lhs.get_value(), rhs.get_value()); break;
            }
            return CodegenValue::create(proc, out, vout);
        }
    );

    auto analyse_logic = [](AstBinOp op, AstExpr* ast, AnalysisValue& a, AnalysisValue& b) -> AnalysisValue {
        return { Type::get_int(1, false), false };
    };

    auto codegen_and_or = [](CodeProcessor* proc, AstBinOp op, CodegenValue& lhs, CodegenValue& rhs, BaseType* out) -> CodegenValue {
        auto builder = proc->get_builder();

        auto block_prev = builder->GetInsertBlock();

        auto block_check = llvm::BasicBlock::Create(llvm_ctx, "", proc->function);
        auto block_end   = llvm::BasicBlock::Create(llvm_ctx, "", proc->function);

        auto vlhs = lhs.get_value();
        auto lhs_nz = builder->CreateICmpNE(vlhs, llvm::ConstantInt::get(vlhs->getType(), 0));
        if (op == BINOP_LOG_AND)
            builder->CreateCondBr(lhs_nz, block_check, block_end);
        else
            builder->CreateCondBr(lhs_nz, block_end, block_check);

        builder->SetInsertPoint(block_check);
        auto vrhs = rhs.get_value();
        auto rhs_nz = builder->CreateICmpNE(vrhs, llvm::ConstantInt::get(vrhs->getType(), 0));
        builder->CreateBr(block_end);

        builder->SetInsertPoint(block_end);

        auto phi = builder->CreatePHI(llvm::Type::getInt1Ty(llvm_ctx), 2);
        if (op == BINOP_LOG_AND)
            phi->addIncoming(builder->getInt1(0), block_prev);
        else
            phi->addIncoming(builder->getInt1(1), block_prev);
        phi->addIncoming(rhs_nz, block_check);

        auto val = builder->CreateZExt(phi, type_to_llvm(out));
        return CodegenValue::create(proc, out, val);
    };

    auto codegen_logic = [](CodeProcessor* proc, AstBinOp op, CodegenValue& lhs, CodegenValue& rhs, BaseType* out) -> CodegenValue {
        auto builder = proc->get_builder();
        llvm::Value* cond = nullptr;

        llvm::Value* vlhs = nullptr;
        llvm::Value* vrhs = nullptr;

        if (lhs.get_type()->get_kind() != TYPE_POINTER) {
            auto ct = Type::get_common_int(lhs.get_type(), rhs.get_type());
            vlhs = proc->codegen_cast(lhs, lhs.get_type(), ct);
            vrhs = proc->codegen_cast(rhs, rhs.get_type(), ct);
        } else {
            vlhs = lhs.get_value();
            vrhs = rhs.get_value();
        }

        bool signd = lhs.get_type()->get_kind() != TYPE_POINTER && lhs.get_type()->int_signed() && rhs.get_type()->int_signed();

        switch (op) {
        case BINOP_EQ:  cond = builder->CreateICmpEQ(vlhs, vrhs); break;
        case BINOP_NEQ: cond = builder->CreateICmpNE(vlhs, vrhs); break;
        case BINOP_GT:
            if (signd) cond = builder->CreateICmpSGT(vlhs, vrhs);
            else       cond = builder->CreateICmpUGT(vlhs, vrhs);
            break;
        case BINOP_GTE:
            if (signd) cond = builder->CreateICmpSGE(vlhs, vrhs);
            else       cond = builder->CreateICmpUGE(vlhs, vrhs);
            break;
        case BINOP_LT:
            if (signd) cond = builder->CreateICmpSLT(vlhs, vrhs);
            else       cond = builder->CreateICmpULT(vlhs, vrhs);
            break;
        case BINOP_LTE:
            if (signd) cond = builder->CreateICmpSLE(vlhs, vrhs);
            else       cond = builder->CreateICmpULE(vlhs, vrhs);
            break;
        }

        auto val = builder->CreateZExt(cond, type_to_llvm(out));
        return CodegenValue::create(proc, out, val);
    };

    add_binop({ BINOP_LOG_AND, BINOP_LOG_OR }, TYPE_INT,     TYPE_INT,     analyse_logic, codegen_and_or);
    add_binop({ BINOP_LOG_AND, BINOP_LOG_OR }, TYPE_POINTER, TYPE_POINTER, analyse_logic, codegen_and_or);

    add_binop(
        {
            BINOP_EQ, BINOP_NEQ,
            BINOP_GT, BINOP_GTE, BINOP_LT, BINOP_LTE
        }, TYPE_INT, TYPE_INT, analyse_logic, codegen_logic
    );

    add_binop(
        {
            BINOP_EQ, BINOP_NEQ,
            BINOP_GT, BINOP_GTE, BINOP_LT, BINOP_LTE
        }, TYPE_POINTER, TYPE_POINTER, analyse_logic, codegen_logic
    );

    auto analyse_assignment = [](AstBinOp op, AstExpr* ast, AnalysisValue& a, AnalysisValue& b) -> AnalysisValue {
        if (a.type->get_kind() == TYPE_POINTER || a.type->get_kind() == TYPE_STRUCT) {
            if (a.type != b.type) {
                ast->token.error("type '" + b.type->to_string() + "' cannot be assigned to type '" + a.type->to_string() + "'");
                return { Type::ERROR, true };
            }
        }

        if (!a.in_memory) {
            ast->token.error("cannot assign a constant value");
            return { Type::ERROR, true };
        }

        return { b.type, false };
    };

    auto codegen_assignment = [](CodeProcessor* proc, AstBinOp op, CodegenValue& lhs, CodegenValue& rhs, BaseType* out) -> CodegenValue {
        auto builder = proc->get_builder();
        auto addr    = lhs.get_address();

        auto rhsv = rhs.get_value();
        rhsv = proc->codegen_cast(rhs, rhs.get_type(), lhs.get_type());

        builder->CreateStore(rhsv, addr);
        return CodegenValue::create(proc, rhs.get_type(), rhsv);
    };

    add_binop({ BINOP_ASG }, TYPE_INT,     TYPE_INT,     analyse_assignment, codegen_assignment);
    add_binop({ BINOP_ASG }, TYPE_POINTER, TYPE_POINTER, analyse_assignment, codegen_assignment);
    add_binop({ BINOP_ASG }, TYPE_STRUCT,  TYPE_STRUCT,  analyse_assignment, codegen_assignment);

    auto analyse_addptr = [](AstBinOp op, AstExpr* ast, AnalysisValue& a, AnalysisValue& b) -> AnalysisValue {
        auto ptr = (b.type->get_kind() == TYPE_INT) ? a : b;
        BaseType* out = ptr.type;
        if (out->get_kind() == TYPE_ARRAY)
            out = Type::create_pointer(out->get_pointee());
        return { out, false };
    };

    auto codegen_addptr = [](CodeProcessor* proc, AstBinOp op, CodegenValue& lhs, CodegenValue& rhs, BaseType* out) -> CodegenValue {
        bool l_ptr = rhs.get_type()->get_kind() == TYPE_INT;
        auto ptr = l_ptr ? lhs : rhs;
        auto idx = l_ptr ? rhs : lhs;
        llvm::Value* ptrv;
        if (ptr.get_type()->get_kind() == TYPE_ARRAY)
            ptrv = proc->get_builder()->CreateCast(llvm::Instruction::CastOps::BitCast, ptr.get_address(), llvm::PointerType::get(type_to_llvm(ptr.get_type()->get_pointee()), 0));
        else
            ptrv = ptr.get_value();
        return CodegenValue::create(
            proc, out,
            proc->get_builder()->CreateGEP(type_to_llvm(ptr.get_type()->get_pointee()), ptrv, idx.get_value())
        );
    };

    add_binop({ BINOP_ADD }, TYPE_POINTER, TYPE_INT, analyse_addptr, codegen_addptr);
    add_binop({ BINOP_ADD }, TYPE_ARRAY,   TYPE_INT, analyse_addptr, codegen_addptr);

    auto analyse_index = [](AstBinOp op, AstExpr* ast, AnalysisValue& a, AnalysisValue& b) -> AnalysisValue {
        return { a.type->get_pointee(), true };
    };

    auto codegen_index = [](CodeProcessor* proc, AstBinOp op, CodegenValue& lhs, CodegenValue& rhs, BaseType* out) -> CodegenValue {
        llvm::Value* ptrv;
        if (lhs.get_type()->get_kind() == TYPE_ARRAY)
            ptrv = proc->get_builder()->CreateCast(llvm::Instruction::CastOps::BitCast, lhs.get_address(), llvm::PointerType::get(type_to_llvm(lhs.get_type()->get_pointee()), 0));
        else
            ptrv = lhs.get_value();
        return CodegenValue::create_addressable(
            proc, out,
            proc->get_builder()->CreateGEP(type_to_llvm(lhs.get_type()->get_pointee()), ptrv, rhs.get_value())
        );
    };

    add_binop({ BINOP_IDX }, TYPE_POINTER, TYPE_INT, analyse_index, codegen_index, true);
    add_binop({ BINOP_IDX }, TYPE_ARRAY,   TYPE_INT, analyse_index, codegen_index, true);

    add_unop(
        { UNOP_POS, UNOP_NEG, UNOP_BIT_NOT, UNOP_LOG_NOT }, TYPE_INT,
        [](AstUnOp op, AstExpr* ast, AnalysisValue& a) -> AnalysisValue {
            return { a.type, false };
        },
        [](CodeProcessor* proc, AstUnOp op, CodegenValue& val, BaseType* out) -> CodegenValue {
            if (op == UNOP_POS) return val;
            auto builder = proc->get_builder();
            llvm::Value* ret = nullptr;
            switch (op) {
            case UNOP_NEG: ret = builder->CreateNeg(val.get_value()); break;
            case UNOP_BIT_NOT: ret = builder->CreateNot(val.get_value()); break;
            case UNOP_LOG_NOT:
                ret = builder->CreateICmpEQ(val.get_value(), builder->getInt32(0));
                ret = builder->CreateZExt(ret, type_to_llvm(out));
                break;
            };
            return CodegenValue::create(proc, out, ret);
        }
    );

    auto analyse_reference = [](AstUnOp op, AstExpr* ast, AnalysisValue& a) -> AnalysisValue {
        if (!a.in_memory) {
            ast->token.error("cannot get reference of constant value");
            return { Type::ERROR, true };
        }
        return { Type::create_pointer(a.type), false };
    };

    auto codegen_reference = [](CodeProcessor* proc, AstUnOp op, CodegenValue& val, BaseType* out) -> CodegenValue {
        return CodegenValue::create(proc, out, val.get_address());
    };
    
    add_unop({ UNOP_REF }, TYPE_INT,     analyse_reference, codegen_reference);
    add_unop({ UNOP_REF }, TYPE_POINTER, analyse_reference, codegen_reference);
    add_unop({ UNOP_REF }, TYPE_STRUCT,  analyse_reference, codegen_reference);
    
    add_unop(
        { UNOP_DEREF }, TYPE_POINTER,
        [](AstUnOp op, AstExpr* ast, AnalysisValue& a) -> AnalysisValue {
            return { a.type->get_pointee(), false };
        },
        [](CodeProcessor* proc, AstUnOp op, CodegenValue& val, BaseType* out) -> CodegenValue {
            auto builder = proc->get_builder();
            return CodegenValue::create_addressable(proc, out, val.get_value());
        }
    );

    add_cast(TYPE_INT, TYPE_INT, [](CodeProcessor* proc, llvm::Value* val, BaseType* from, BaseType* to) -> llvm::Value* {
        if (from->int_signed() && to->int_signed() && to->get_size() > from->get_size())
            val = proc->get_builder()->CreateSExt(val, type_to_llvm(to));
        else if (to->get_size() > from->get_size())
            val = proc->get_builder()->CreateZExt(val, type_to_llvm(to));
        else if (to->get_size() < from->get_size())
            val = proc->get_builder()->CreateTrunc(val, type_to_llvm(to));
        return val;
    });
    add_cast(TYPE_ARRAY, TYPE_POINTER, [](CodeProcessor* proc, llvm::Value* val, BaseType* from, BaseType* to) -> llvm::Value* {
        return proc->get_builder()->CreateCast(llvm::Instruction::CastOps::BitCast, val, type_to_llvm(to));
    });
    add_explicit_cast(TYPE_POINTER, TYPE_POINTER, [](CodeProcessor* proc, llvm::Value* val, BaseType* from, BaseType* to) -> llvm::Value* {
        return val;
    });
    add_explicit_cast(TYPE_POINTER, TYPE_INT, [](CodeProcessor* proc, llvm::Value* val, BaseType* from, BaseType* to) -> llvm::Value* {
        return proc->get_builder()->CreateCast(llvm::Instruction::CastOps::PtrToInt, val, type_to_llvm(to));
    });
    add_explicit_cast(TYPE_INT, TYPE_POINTER, [](CodeProcessor* proc, llvm::Value* val, BaseType* from, BaseType* to) -> llvm::Value* {
        return proc->get_builder()->CreateCast(llvm::Instruction::CastOps::IntToPtr, val, type_to_llvm(to));
    });
}

AnalysisValue CodeProcessor::analyse_expr(LocalScope* scope, AstExpr* ast) {
    if (ast->kind == EXPR_INT) {
        return { Type::get_int(4, true), false };
    } else if (ast->kind == EXPR_STRING) {
        return { Type::create_pointer( Type::get_int(1, false) ), false };
    } else if (ast->kind == EXPR_IDENT) {
        CodeIdentifier id = scope->find_ident(ast->ident);
        if (!id.valid()) {
            ast->token.error("identifier '" + std::string(ast->ident) + "' is undefined");
            return { Type::ERROR, true };
        }

        return { id.type, true };
    } else if (ast->kind == EXPR_UNOP) {
        AnalysisValue value = analyse_expr(scope, ast->operand);
        if (value.type == Type::ERROR) return value;

        UnOpProcessor* proc = get_unop_proc(ast->unop, value.type->get_kind());

        if (proc == nullptr) {
            ast->token.error("operator cannot be applied on value of type '" + value.type->to_string() + "'");
            return { Type::ERROR, true };
        }

        return proc->analyse(ast->unop, ast, value);
    } else if (ast->kind == EXPR_BINOP) {
        AnalysisValue lhs = analyse_expr(scope, ast->lhs);
        if (lhs.type == Type::ERROR) return lhs;
        AnalysisValue rhs = analyse_expr(scope, ast->rhs);
        if (rhs.type == Type::ERROR) return rhs;

        BinOpProcessor* proc = get_binop_proc(ast->binop, lhs.type->get_kind(), rhs.type->get_kind());

        if (proc == nullptr) {
            ast->token.error("operator cannot be applied on value of types '" + lhs.type->to_string() + "' and '" + rhs.type->to_string() + "'");
            return { Type::ERROR, true };
        }

        return proc->analyse(ast->binop, ast, lhs, rhs);
    } else if (ast->kind == EXPR_MEMBER) {
        AnalysisValue object = analyse_expr(scope, ast->object);
        if (object.type == Type::ERROR) return object;

        BaseType* struct_type = nullptr;
        if (object.type->get_kind() == TYPE_STRUCT) {
            if (!object.in_memory) {
                ast->token.error("cannot access member of constant struct");
                return { Type::ERROR, true };
            }
            struct_type = object.type;
        } else if (object.type->get_kind() == TYPE_POINTER && object.type->get_pointee()->get_kind() == TYPE_STRUCT)
            struct_type = object.type->get_pointee();
        else {
            ast->token.error("cannot access member of type '" + object.type->to_string() + "'");
            return { Type::ERROR, true };
        }

        auto members = struct_type->get_members();
        BaseType* member_type = nullptr;

        for (auto m : members) {
            if (m.name == ast->ident) {
                member_type = m.type;
                break;
            }
        }

        if (member_type == nullptr) {
            ast->token.error("struct '" + struct_type->to_string() + "' has no member '" + ast->ident + "'");
            return { Type::ERROR, true };
        }

        return { member_type, true };
    } else if (ast->kind == EXPR_CALL) {
        AnalysisValue func = analyse_expr(scope, ast->func);
        if (func.type == Type::ERROR) return func;
        if (func.type->get_kind() != TYPE_FUNC) {
            ast->token.error("cannot call a '" + func.type->to_string() + "'");
            return { Type::ERROR, true };
        }

        if (ast->param_count < func.type->get_params().size()) {
            ast->token.error("too few parameters in call");
            return { Type::ERROR, true };
        }

        if (ast->param_count > func.type->get_params().size()) {
            ast->token.error("too many parameters in call");
            return { Type::ERROR, true };
        }

        for (u32 i = 0; i < ast->param_count; i++) {
            AnalysisValue param = analyse_expr(scope, ast->params[i]);
            if (param.type == Type::ERROR) continue;
            analyse_cast(ast->token, param.type, func.type->get_params()[i]);
        }
        return { func.type->get_return_type(), false };
    } else if (ast->kind == EXPR_CAST) {
        AnalysisValue expr = analyse_expr(scope, ast->cast_expr);
        if (expr.type == Type::ERROR) return expr;

        BaseType* type = scope->func->get_global()->resolve_type(ast->cast_type);
        if (type == nullptr)
            return { Type::ERROR, true };
        
        analyse_cast(ast->token, expr.type, type, false);
        return { type, false };
    }
    
    return { Type::ERROR, true };
}

void CodeProcessor::analyse_stmt(LocalScope* scope, AstStmt* stmt, bool in_loop) {
    if (stmt->kind == STMT_BREAK || stmt->kind == STMT_CONTINUE) {
        if (!in_loop)
            stmt->token.error("cannot use " + std::string(stmt->kind == STMT_BREAK ? "break" : "continue") + " outside loop");
        return;
    }

    if (stmt->kind == STMT_LET) {
        BaseType* type = nullptr;
        if (stmt->expr)
            type = analyse_expr(scope, stmt->expr).type;
        if (stmt->let_type)
            type = scope->func->get_global()->resolve_type(stmt->let_type);
        if (type == nullptr)
            type = Type::ERROR;
        if (scope->local_exists(stmt->let_ident))
            stmt->token.error("redefinition of local '" + std::string(stmt->let_ident) + "'");
        else {
            listed_locals.push_back({ stmt, type });
            CodeIdentifier id { type };
            id.local = stmt;
            scope->locals[stmt->let_ident] = id;
        }
    } else if (stmt->kind == STMT_IF) {
        BaseType* type = analyse_expr(scope, stmt->expr).type;
        if (type->get_kind() != TYPE_INT && type->get_kind() != TYPE_POINTER)
            stmt->token.error("'" + type->to_string() + "' is an invalid condition type");
    
        LocalScope btrue = LocalScope(scope);
        analyse_block(&btrue, stmt->if_true, in_loop);
        if (stmt->if_false) {
            LocalScope bfalse = LocalScope(scope);
            analyse_block(&bfalse, stmt->if_false, in_loop);
        }
    } else if (stmt->kind == STMT_WHILE) {
        BaseType* type = analyse_expr(scope, stmt->expr).type;
        if (type->get_kind() != TYPE_INT && type->get_kind() != TYPE_POINTER)
            stmt->token.error("'" + type->to_string() + "' is an invalid condition type");
        LocalScope scp = LocalScope(scope);
        analyse_block(&scp, stmt->while_block, true);
    } else if (stmt->kind == STMT_EXPR) {
        analyse_expr(scope, stmt->expr);
    } else if (stmt->kind == STMT_RETURN) {
        if (stmt->expr == nullptr) return;
        BaseType* type = analyse_expr(scope, stmt->expr).type;
        analyse_cast(stmt->token, type, scope->func->get_return_type());
    }
}

void CodeProcessor::analyse_block(LocalScope* scope, AstBlock* block, bool in_loop) {
    for (auto stmt : block->stmts)
        analyse_stmt(scope, stmt, in_loop);
}

void CodeProcessor::analyse_cast(Token& token, BaseType* from, BaseType* to, bool implicit) {
    if (from == to) return;

    for (auto proc : cast_processors) {
        if (implicit && !proc.implicit) continue;
        if (proc.from != from->get_kind() || proc.to != to->get_kind()) continue;
        if (proc.filter == nullptr || proc.filter(from, to)) return;
    }

    token.error(std::string("cannot ") + (implicit ? "implicitly" : "explicitly") + " cast '" + from->to_string() + "' to '" + to->to_string() + "'");
}

CodegenValue CodeProcessor::codegen_expr(LocalScope* scope, AstExpr* ast) {
    if (ast->kind == EXPR_INT) {
        return CodegenValue::create(this, Type::get_int(4, true), builder->getInt32(ast->integer));
    } else if (ast->kind == EXPR_STRING) {
        llvm::GlobalVariable* str;
        if (strings.contains(ast->string))
            str = strings[ast->string];
        else {
            str = new llvm::GlobalVariable(
                *current_module,
                llvm::ArrayType::get(llvm::Type::getInt8Ty(llvm_ctx), strlen(ast->string) + 1),
                true, llvm::GlobalValue::LinkageTypes::InternalLinkage,
                llvm::ConstantDataArray::getString(llvm_ctx, ast->string)
            );
            strings[ast->string] = str;
        }
        return CodegenValue::create(this, Type::create_pointer( Type::get_int(1, false) ), str);
    } else if (ast->kind == EXPR_IDENT) {
        CodeIdentifier id = scope->find_ident(ast->ident);

        if (id.global) {
            if (id.global->as_func())
                return CodegenValue::create(this, id.type, functions[id.global->as_func()], ast->ident);
            else
                return CodegenValue::create_addressable(this, id.type, global_vars[id.global->as_var()], ast->ident);
        } else if (id.local) {
            auto val = locals[id.local];
            return CodegenValue::create_addressable(this, id.type, val, ast->ident);
        } else {
            return CodegenValue::create_addressable(this, id.type, params[id.func_param], ast->ident);
        }
    } else if (ast->kind == EXPR_UNOP) {
        AnalysisValue preval = analyse_expr(scope, ast->operand);
        CodegenValue val = codegen_expr(scope, ast->operand);

        UnOpProcessor* proc = get_unop_proc(ast->unop, preval.type->get_kind());

        AnalysisValue postval = proc->analyse(ast->unop, ast, preval);

        return proc->codegen(this, ast->unop, val, postval.type);
    } else if (ast->kind == EXPR_BINOP) {
        AnalysisValue alhs = analyse_expr(scope, ast->lhs);
        AnalysisValue arhs = analyse_expr(scope, ast->rhs);

        CodegenValue lhs = codegen_expr(scope, ast->lhs);
        CodegenValue rhs = codegen_expr(scope, ast->rhs);

        BinOpProcessor* proc = get_binop_proc(ast->binop, alhs.type->get_kind(), arhs.type->get_kind());

        AnalysisValue postval = proc->analyse(ast->binop, ast, alhs, arhs);

        return proc->codegen(this, ast->binop, lhs, rhs, postval.type);
    } else if (ast->kind == EXPR_MEMBER) {
        AnalysisValue aobject = analyse_expr(scope, ast->object);
        CodegenValue  object  = codegen_expr(scope, ast->object);

        llvm::Value* addr_value = nullptr;

        BaseType* struct_type = nullptr;
        if (aobject.type->get_kind() == TYPE_STRUCT) {
            struct_type = aobject.type;
            addr_value = object.get_address();
        } else if (aobject.type->get_kind() == TYPE_POINTER) {
            struct_type = aobject.type->get_pointee();
            addr_value = object.get_value();
        }

        BaseType* member_type = nullptr;
        u32 midx = 0;

        for (auto m : struct_type->get_members()) {
            if (m.name == ast->ident) {
                member_type = m.type;
                break;
            }
            midx++;
        }

        auto val = llvm::GetElementPtrInst::Create(type_to_llvm(struct_type), addr_value, { builder->getInt32(0), builder->getInt32(midx) }, "", builder->GetInsertBlock());

        return CodegenValue::create_addressable(this, member_type, val);
    } else if (ast->kind == EXPR_CALL) {
        AnalysisValue afunc = analyse_expr(scope, ast->func);
        CodegenValue  func  = codegen_expr(scope, ast->func);
        std::vector<llvm::Value*> params;

        for (u32 i = 0; i < ast->param_count; i++) {
            CodegenValue val = codegen_expr(scope, ast->params[i]);
            params.push_back(codegen_cast(val, val.get_type(), afunc.type->get_params()[i]));
        }

        auto val = builder->CreateCall((llvm::FunctionType*)type_to_llvm(afunc.type), func.get_value(), params);
        return CodegenValue::create(this, afunc.type->get_return_type(), val);
    } else if (ast->kind == EXPR_CAST) {
        CodegenValue val = codegen_expr(scope, ast->cast_expr);

        BaseType* type = scope->func->get_global()->resolve_type(ast->cast_type);
        
        return CodegenValue::create(this, type, codegen_cast(val, val.get_type(), type, false));
    }
    
    return CodegenValue::create(this, nullptr, nullptr);
}

llvm::Value* CodeProcessor::codegen_cast(CodegenValue& val, BaseType* from, BaseType* to, bool implicit) {
    CastProcessor* proc = nullptr;
    for (auto& c : cast_processors) {
        if (implicit && !c.implicit) continue;
        if (c.from == from->get_kind() && c.to == to->get_kind()) {
            proc = &c;
            break;
        }
    }

    if (proc == nullptr) return builder->CreateCast(llvm::Instruction::CastOps::BitCast, val.get_value(), type_to_llvm(to));
    llvm::Value* lval;
    if (from->get_kind() == TYPE_ARRAY && to->get_kind() == TYPE_POINTER)
        lval = val.get_address();
    else
        lval = val.get_value();
    return proc->codegen(this, lval, from, to);
}

bool CodeProcessor::codegen_stmt(LocalScope* scope, AstStmt* stmt, CodeLoop* loop) {
    auto bd = builder;

    if (stmt->kind == STMT_BREAK) {
        bd->CreateBr(loop->end_block);
        return false;
    } else if (stmt->kind == STMT_CONTINUE) {
        bd->CreateBr(loop->loop_block);
        return false;
    }

    if (stmt->kind == STMT_LET) {
        BaseType* type = nullptr;
        if (stmt->expr)
            type = analyse_expr(scope, stmt->expr).type;
        if (stmt->let_type)
            type = scope->func->get_global()->resolve_type(stmt->let_type);
        if (stmt->expr) {
            CodegenValue val = codegen_expr(scope, stmt->expr);
            auto lval = codegen_cast(val, val.get_type(), type);
            bd->CreateStore(lval, locals[stmt]);
        }
        CodeIdentifier id { type };
        id.local = stmt;
        scope->locals[stmt->let_ident] = id;
    } else if (stmt->kind == STMT_IF) {
        CodegenValue val = codegen_expr(scope, stmt->expr);
        auto lval = val.get_value();
        llvm::Value* cond = bd->CreateICmpNE(lval, llvm::ConstantInt::get(lval->getType(), 0));

        auto true_block = llvm::BasicBlock::Create(llvm_ctx, "if.then", function);
        auto end_block  = llvm::BasicBlock::Create(llvm_ctx, stmt->if_false ? "if.else" : "if.end", function);
        bd->CreateCondBr(cond, true_block, end_block);

        if (stmt->if_false) {
            bd->SetInsertPoint(end_block);
            LocalScope bfalse = LocalScope(scope);
            if (codegen_block(&bfalse, stmt->if_false, loop)) {
                end_block = llvm::BasicBlock::Create(llvm_ctx, "if.end", function);
                bd->CreateBr(end_block);
            } else {
                end_block = true_block;
            }
        }

        bd->SetInsertPoint(true_block);
        LocalScope btrue = LocalScope(scope);
        if (codegen_block(&btrue, stmt->if_true, loop))
            bd->CreateBr(end_block);
        
        bd->SetInsertPoint(end_block);
    } else if (stmt->kind == STMT_WHILE) {
        auto cond_block = llvm::BasicBlock::Create(llvm_ctx, "while.cond", function);
        auto loop_block = llvm::BasicBlock::Create(llvm_ctx, "while.do", function);
        auto end_block  = llvm::BasicBlock::Create(llvm_ctx, "while.end", function);
        bd->CreateBr(cond_block);

        bd->SetInsertPoint(cond_block);
        CodegenValue val = codegen_expr(scope, stmt->expr);
        auto lval = val.get_value();
        llvm::Value* cond = bd->CreateICmpNE(lval, llvm::ConstantInt::get(lval->getType(), 0));
        bd->CreateCondBr(cond, loop_block, end_block);

        bd->SetInsertPoint(loop_block);
        LocalScope lscope = LocalScope(scope);
        CodeLoop subloop { cond_block, end_block };
        codegen_block(&lscope, stmt->while_block, &subloop);
        bd->CreateBr(cond_block);

        bd->SetInsertPoint(end_block);
    } else if (stmt->kind == STMT_EXPR) {
        codegen_expr(scope, stmt->expr);
    } else if (stmt->kind == STMT_RETURN) {
        if (stmt->expr == nullptr) {
            bd->CreateRetVoid();
            return false;
        }

        CodegenValue val = codegen_expr(scope, stmt->expr);
        auto out = codegen_cast(val, val.get_type(), scope->func->get_return_type());
        bd->CreateRet(out);
        return false;
    }
    return true;
}

bool CodeProcessor::codegen_block(LocalScope* scope, AstBlock* block, CodeLoop* loop) {
    for (auto stmt : block->stmts) {
        if (!codegen_stmt(scope, stmt, loop))
            return false;
    }
    return true;
}

void CodeProcessor::codegen_func(llvm::Function* lfunc, GlobalFunction* gfunc) {
    if (gfunc->get_ast()->body == nullptr) return;

    function = lfunc;
    LocalScope scope(gfunc);
    listed_locals.clear();
    analyse_block(&scope, gfunc->get_ast()->body);

    if (error_compiling)
        return;

    locals.clear();

    auto entry = llvm::BasicBlock::Create(llvm_ctx, "entry", lfunc);
    builder->SetInsertPoint(entry);

    for (auto let : listed_locals)
        locals[let.stmt] = builder->CreateAlloca(type_to_llvm(let.type), nullptr, let.stmt->let_ident);

    params.clear();
    u32 i = 0;
    for (auto& arg : lfunc->args()) {
        auto aparam = builder->CreateAlloca(arg.getType(), nullptr, gfunc->get_params()[i].name);
        params.push_back(aparam);
        i++;
    }

    i = 0;
    for (auto& arg : lfunc->args()) {
        builder->CreateStore(&arg, params[i]);
        i++;
    }
    
    scope = LocalScope(gfunc);
    codegen_block(&scope, gfunc->get_ast()->body, nullptr);
}

llvm::Module* CodeProcessor::codegen_module(GlobalScope* scope) {
    llvm::Module* module = new llvm::Module("module", llvm_ctx);

    current_module = module;

    module->setDataLayout("");
    module->setTargetTriple(llvm::sys::getDefaultTargetTriple());

    builder = new llvm::IRBuilder<>(llvm_ctx);

    for (auto pair : scope->get_objects()) {
        auto obj = pair.second;
        auto type = obj->get_type();
        if (obj->as_func()) {
            llvm::Function* func = llvm::Function::Create(
                (llvm::FunctionType*)type_to_llvm(type),
                llvm::GlobalObject::LinkageTypes::ExternalLinkage,
                obj->get_name(),
                module
            );
            functions[obj->as_func()] = func;
        } else {
            auto var = new llvm::GlobalVariable(
                *module,
                type_to_llvm(type),
                false,
                llvm::GlobalObject::LinkageTypes::ExternalLinkage,
                nullptr,
                obj->get_name()
            );
            global_vars[obj->as_var()] = var;
        }
    }

    for (auto pair : functions)
        codegen_func(pair.second, pair.first);

    delete builder;

    if (error_compiling) {
        delete module;
        return nullptr;
    }

    return module;
}

BinOpProcessor* CodeProcessor::get_binop_proc(AstBinOp op, TypeKind a, TypeKind b) {
    for (auto& p : binop_processors) {
        if (p.ops.find(op) == p.ops.end()) continue;
        bool ord = a == p.type_a && b == p.type_b;
        bool rev = a == p.type_b && b == p.type_a;
        if (p.static_order) {
            if (ord) return &p;
        } else if (ord || rev)
            return &p;
    }
    return nullptr;
}

UnOpProcessor* CodeProcessor::get_unop_proc(AstUnOp op, TypeKind tp) {
    for (auto& p : unop_processors) {
        if (p.type == tp && p.ops.find(op) != p.ops.end()) {
            return &p;
        }
    }
    return nullptr;
}