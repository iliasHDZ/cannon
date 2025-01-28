#pragma once

#include "global.hpp"
#include <set>
#include <unordered_map>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

struct AnalysisValue {
    BaseType* type;
    bool in_memory;
};

class CodeProcessor;

class CodegenValue {
public:
    static inline CodegenValue create(CodeProcessor* proc, BaseType* type, llvm::Value* val, char* ident = nullptr) {
        CodegenValue ret;
        ret.ident = ident;
        ret.proc = proc;
        ret.type = type;
        ret.address = nullptr;
        ret.value = val;
        return ret;
    }
    
    static inline CodegenValue create_addressable(CodeProcessor* proc, BaseType* type, llvm::Value* address, char* ident = nullptr) {
        CodegenValue ret;
        ret.ident = ident;
        ret.proc = proc;
        ret.type = type;
        ret.value = nullptr;
        ret.address = address;
        return ret;
    }

    inline BaseType* get_type() { return type; }

    inline llvm::Value* get_address() { return address; }

    llvm::Value* get_value();

private:
    char* ident = nullptr;
    CodeProcessor* proc;
    BaseType* type;
    llvm::Value* address;
    llvm::Value* value;
};

typedef AnalysisValue(*AnalyseBinOp)(AstBinOp op, AstExpr* ast, AnalysisValue& a, AnalysisValue& b);
typedef AnalysisValue(*AnalyseUnOp)(AstUnOp op, AstExpr* ast, AnalysisValue& v);

typedef CodegenValue(*CodegenBinOp)(CodeProcessor* proc, AstBinOp op, CodegenValue& lhs, CodegenValue& rhs, BaseType* out);
typedef CodegenValue(*CodegenUnOp)(CodeProcessor* proc, AstUnOp op, CodegenValue& value, BaseType* out);

struct BinOpProcessor {
    std::set<AstBinOp> ops;
    TypeKind type_a;
    TypeKind type_b;
    AnalyseBinOp analyse;
    CodegenBinOp codegen;
    bool static_order;
};

struct UnOpProcessor {
    std::set<AstUnOp> ops;
    TypeKind type;
    AnalyseUnOp analyse;
    CodegenUnOp codegen;
};

typedef bool(*CastFilter)(BaseType* from, BaseType* to);
typedef llvm::Value*(*CodegenCast)(CodeProcessor* proc, llvm::Value* val, BaseType* from, BaseType* to);

struct CastProcessor {
    TypeKind from;
    TypeKind to;
    bool implicit;
    CastFilter filter;
    CodegenCast codegen;
};

struct CodeIdentifier {
    BaseType* type;
    GlobalObject* global = nullptr;
    AstStmt* local = nullptr;
    i32 func_param = -1;

    inline bool valid() const { return type != nullptr; }
};

struct LocalScope {
    GlobalFunction* func;
    LocalScope* parent;
    std::unordered_map<char*, CodeIdentifier> locals;

    inline LocalScope(GlobalFunction* func)
        : func(func), parent(nullptr) {}
    inline LocalScope(LocalScope* parent)
        : func(parent->func), parent(parent) {}

    inline bool local_exists(char* name) { return locals.count(name) != 0; }
    CodeIdentifier find_ident(char* name);
};

struct CodeLoop {
    llvm::BasicBlock* loop_block;
    llvm::BasicBlock* end_block;
};

class CodeProcessor {
public:
    CodeProcessor();

    AnalysisValue analyse_expr(LocalScope* scope, AstExpr* ast);

    void analyse_stmt(LocalScope* scope, AstStmt* stmt, bool in_loop = false);

    void analyse_block(LocalScope* scope, AstBlock* block, bool in_loop = false);

    void analyse_cast(Token& token, BaseType* from, BaseType* to, bool implicit = true);

    CodegenValue codegen_expr(LocalScope* scope, AstExpr* expr);

    llvm::Value* codegen_cast(CodegenValue& val, BaseType* from, BaseType* to, bool implicit = true);

    bool codegen_stmt(LocalScope* scope, AstStmt* stmt, CodeLoop* loop);

    bool codegen_block(LocalScope* scope, AstBlock* block, CodeLoop* loop);

    void codegen_func(llvm::Function* lfunc, GlobalFunction* gfunc);

    llvm::Module* codegen_module(GlobalScope* scope);

    inline llvm::IRBuilder<>* get_builder() { return builder; }
    
    inline llvm::Function* get_func(GlobalFunction* func) {
        if (functions.count(func) == 0) return nullptr;
        return functions[func];
    }
    
private:
    inline void add_binop(std::set<AstBinOp> ops, TypeKind type_a, TypeKind type_b, AnalyseBinOp analyse, CodegenBinOp codegen = nullptr, bool static_order = false) {
        binop_processors.push_back({ ops, type_a, type_b, analyse, codegen, static_order });
    }
 
    inline void add_unop(std::set<AstUnOp> ops, TypeKind type, AnalyseUnOp analyse, CodegenUnOp codegen = nullptr) {
        unop_processors.push_back({ ops, type, analyse, codegen });
    }

    inline void add_cast(TypeKind from, TypeKind to, CodegenCast cg = nullptr, bool implicit = true, CastFilter filter = nullptr) {
        cast_processors.push_back({ from, to, implicit, filter, cg });
    }

    inline void add_explicit_cast(TypeKind from, TypeKind to, CodegenCast cg = nullptr, CastFilter filter = nullptr) { add_cast(from, to, cg, false, filter); }

    BinOpProcessor* get_binop_proc(AstBinOp op, TypeKind a, TypeKind b);

    UnOpProcessor* get_unop_proc(AstUnOp op, TypeKind tp);

public:
    llvm::Function* function;

private:
    std::vector<BinOpProcessor> binop_processors;
    std::vector<UnOpProcessor> unop_processors;
    std::vector<CastProcessor> cast_processors;
    
    llvm::IRBuilder<>* builder;

    struct ListedLocal {
        AstStmt* stmt;
        BaseType* type;
    };

    llvm::Module* current_module;
    std::vector<ListedLocal> listed_locals;
    std::vector<llvm::Value*> params;
    std::unordered_map<AstStmt*, llvm::Value*> locals;
    std::unordered_map<GlobalVariable*, llvm::GlobalVariable*> global_vars;
    std::unordered_map<GlobalFunction*, llvm::Function*> functions;
    std::unordered_map<char*, llvm::GlobalVariable*> strings;
};