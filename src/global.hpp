#pragma once

#include "common.hpp"
#include "type.hpp"
#include "ast.hpp"

#include <unordered_map>

class GlobalVariable;
class GlobalFunction;
class GlobalScope;

class GlobalObject {
public:
    inline GlobalObject(char* name)
        : name(name) {}

    virtual GlobalVariable* as_var();
    virtual GlobalFunction* as_func();

    virtual BaseType* get_type() = 0;
    
    inline char*        get_name()   { return name; }
    inline GlobalScope* get_global() { return global; }

private:
    char* name;
    GlobalScope* global;

    friend class GlobalScope;
};

class GlobalVariable : public GlobalObject {
public:
    inline GlobalVariable(char* name, BaseType* type)
        : GlobalObject(name), type(type) {}

    GlobalVariable* as_var() override;

    BaseType* get_type() override;

private:
    BaseType* type;
};

struct FunctionParam {
    char* name;
    BaseType* type;
};

class GlobalFunction : public GlobalObject {
public:
    inline GlobalFunction(char* name, BaseType* return_type, const std::vector<FunctionParam>& params, AstDecl* decl)
        : GlobalObject(name), return_type(return_type), params(params), decl(decl) {}
    
    GlobalFunction* as_func() override;

    inline BaseType* get_return_type() { return return_type; }
    inline const std::vector<FunctionParam>& get_params() { return params; }
    inline AstDecl* get_ast() { return decl; }

    BaseType* get_type() override;

private:
    BaseType* return_type;
    std::vector<FunctionParam> params;
    AstDecl* decl;
};

class GlobalScope {
public:
    GlobalScope();

    inline BaseType* fetch_type(char* name) {
        if (types.count(name) == 0) return nullptr;
        return types[name];
    }

    inline GlobalObject* fetch_object(char* name) {
        if (objects.count(name) == 0) return nullptr;
        return objects[name];
    }

    inline std::unordered_map<char*, GlobalObject*>& get_objects() {
        return objects;
    }

    void resolve_types(AstGlobal* ast);

    BaseType* resolve_type(AstType* ast);

    void resolve_objects(AstGlobal* ast);

    void process_objects();

    GlobalVariable* resolve_var(AstDecl* decl);

    GlobalFunction* resolve_func(AstDecl* decl);

private:
    std::unordered_map<char*, BaseType*> types;
    std::unordered_map<char*, GlobalObject*> objects;
};