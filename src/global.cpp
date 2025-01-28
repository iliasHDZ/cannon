#include "global.hpp"
#include "process.hpp"

GlobalFunction* GlobalObject::as_func() { return nullptr; }
GlobalVariable* GlobalObject::as_var() { return nullptr; }

GlobalVariable* GlobalVariable::as_var() { return this; }
BaseType* GlobalVariable::get_type() { return type; }

GlobalFunction* GlobalFunction::as_func() { return this; }

BaseType* GlobalFunction::get_type() {
    std::vector<BaseType*> param_types;
    for (auto& param : params)
        param_types.push_back(param.type);
    return Type::create_func(param_types, return_type);
}

GlobalScope::GlobalScope() {
    char* n_void = StringManager::create("void");
    char* n_u8   = StringManager::create("u8");
    char* n_u16  = StringManager::create("u16");
    char* n_u32  = StringManager::create("u32");
    char* n_u64  = StringManager::create("u64");
    char* n_i8   = StringManager::create("i8");
    char* n_i16  = StringManager::create("i16");
    char* n_i32  = StringManager::create("i32");
    char* n_i64  = StringManager::create("i64");

    types[n_void] = Type::VOID;

    types[n_u8]  = Type::get_int(1, false);
    types[n_u16] = Type::get_int(2, false);
    types[n_u32] = Type::get_int(4, false);
    types[n_u64] = Type::get_int(8, false);
    types[n_i8]  = Type::get_int(1, true);
    types[n_i16] = Type::get_int(2, true);
    types[n_i32] = Type::get_int(4, true);
    types[n_i64] = Type::get_int(8, true);
}

void GlobalScope::resolve_types(AstGlobal* ast) {
    for (auto decl : ast->decls) {
        if (decl->kind != DECL_USE) continue;

        if (types.count(decl->name)) {
            decl->token.error("redeclaration of type '" + std::string(decl->name) + "'");
            return;
        }
    
        types[decl->name] = new NamedType(decl->name);
    }

    for (auto decl : ast->decls) {
        if (decl->kind != DECL_USE) continue;
        auto type = resolve_type(decl->type);
        if (type == nullptr)
            return;
        auto ntype = fetch_type(decl->name);
        if (ntype->is_named())
            ((NamedType*)ntype)->set_type(type);
    }
}

BaseType* GlobalScope::resolve_type(AstType* ast) {
    BaseType* type = Type::ERROR;

    if (ast->kind == ATYPE_IDENT) {
        type = fetch_type(ast->ident);
        if (type == nullptr) {
            ast->token.error("cannot resolve type '" + std::string(ast->ident) + "'");
            type = Type::ERROR;
        }
    } else if (ast->kind == ATYPE_STRUCT) {
        std::vector<StructMember> members;
        bool success = true;
        for (u32 i = 0; i < ast->member_count; i++) {
            BaseType* tp = resolve_type(ast->members[i].type);
            if (tp == nullptr) {
                success = false;
                break;
            }
            members.push_back({ ast->members[i].name, tp });
            for (u32 j = 0; j < i; j++) {
                if (ast->members[i].name == ast->members[j].name) {
                    ast->members[i].token.error("redefinition of member '" + std::string(ast->members[i].name) + "'");
                    success = false;
                    break;
                }
            }
            if (!success) break;
        }
        if (success)
            type = Type::create_struct(members);
    } else if (ast->kind == ATYPE_ARRAY) {
        return Type::create_array(resolve_type(ast->elem), ast->elem_count);
    }

    for (u32 i = 0; i < ast->point_count; i++)
        type = Type::create_pointer(type);
    return type;
}

void GlobalScope::resolve_objects(AstGlobal* ast) {
    for (auto decl : ast->decls) {
        if (decl->kind != DECL_FN && decl->kind != DECL_LET) continue;

        if (objects.count(decl->name)) {
            decl->token.error("redeclaration of object '" + std::string(decl->name) + "'");
            continue;    
        }

        GlobalObject* obj;
        if (decl->kind == DECL_LET)
            obj = resolve_var(decl);
        else if (decl->kind == DECL_FN)
            obj = resolve_func(decl);
        
        obj->global = this;
        objects[decl->name] = obj;
    }
}

void GlobalScope::process_objects() {
    CodeProcessor processor;
    for (auto obj : objects) {
        if (obj.second->as_func() == nullptr) continue;
        auto func = obj.second->as_func();

        LocalScope scope = LocalScope(func);
        processor.analyse_block(&scope, func->get_ast()->body);
    }
}

GlobalVariable* GlobalScope::resolve_var(AstDecl* decl) {
    return new GlobalVariable(decl->name, resolve_type(decl->type));
}

GlobalFunction* GlobalScope::resolve_func(AstDecl* decl) {
    BaseType* retn = resolve_type(decl->return_type);
    std::vector<FunctionParam> params;

    for (u32 i = 0; i < decl->param_count; i++) {
        auto param = decl->params + i;

        for (u32 j = 0; j < i; j++) {
            if (decl->params[j].name == param->name) {
                param->token.error("redefinition of param '" + std::string(param->name) + "'");
                break;
            }
        }

        params.push_back({ param->name, resolve_type(param->type) });
    }

    return new GlobalFunction(decl->name, retn, params, decl);
}