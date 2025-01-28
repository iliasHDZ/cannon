#pragma once

#include "common.hpp"
#include <vector>
#include <span>

#define MACHINE_SIZE 8

enum TypeKind {
    TYPE_ERROR,
    TYPE_VOID,
    TYPE_INT,
    TYPE_POINTER,
    TYPE_STRUCT,
    TYPE_FUNC,
    TYPE_ARRAY
};

struct BaseType;

struct StructMember {
    char* name;
    BaseType* type;
};

struct BaseType {
public:
    virtual TypeKind get_kind() = 0;

    virtual bool int_signed() = 0;

    virtual BaseType* get_pointee() = 0;

    virtual std::span<StructMember> get_members() = 0;

    virtual std::span<BaseType*> get_params() = 0;

    virtual BaseType* get_return_type() = 0;

    virtual u32 get_alignment() = 0;

    virtual u32 get_size() = 0;

    virtual u32 get_array_size() = 0;

    virtual i32 get_member_offset(char* member) = 0;

    virtual bool is_named();

    virtual std::string to_string() = 0;

    virtual ~BaseType();
};

struct NamedType : public BaseType {
public:
    NamedType(char* name);
    NamedType(char* name, BaseType* type);

    TypeKind get_kind() override;

    bool int_signed() override;

    BaseType* get_pointee() override;

    std::span<StructMember> get_members() override;

    std::span<BaseType*> get_params() override;

    BaseType* get_return_type() override;

    u32 get_alignment() override;

    u32 get_size() override;

    u32 get_array_size() override;

    i32 get_member_offset(char* member) override;

    bool is_named() override;

    std::string to_string() override;

    inline void set_type(BaseType* t) { type = t; }

    inline BaseType* get_type() { return type; }

private:
    char* name;
    BaseType* type;
};

struct Type : public BaseType {
    TypeKind kind;
    union {
        struct {
            u8   int_size;
            bool is_signed;
        };
        BaseType* ptr_type;
        struct {
            u32 member_count;
            StructMember* members;
        };
        struct {
            BaseType* return_type;
            u32 param_count;
            BaseType** param_types;
        };
        struct {
            BaseType* array_elem;
            u32 array_size;
        };
    };

    TypeKind get_kind() override;

    bool int_signed() override;

    BaseType* get_pointee() override;

    std::span<StructMember> get_members() override;

    std::span<BaseType*> get_params() override;

    BaseType* get_return_type() override;

    u32 get_alignment() override;

    u32 get_size() override;

    u32 get_array_size() override;

    i32 get_member_offset(char* member) override;

    std::string to_string() override;

    inline ~Type() override {
        if (kind == TYPE_STRUCT)
            delete[] members;
    }

    static Type* create_struct(const std::vector<StructMember>& members);

    static Type* create_func(const std::vector<BaseType*> params, BaseType* return_type);

    static Type* create_pointer(BaseType* pointee);

    static Type* create_array(BaseType* elem, u32 size);

    static Type* VOID;
    static Type* ERROR;

    static Type* get_int(u8 size, bool sign);

    inline static Type* get_common_int(BaseType* a, BaseType* b) {
        return Type::get_int(
            std::max(a->get_size(), b->get_size()),
            a->int_signed() && b->int_signed()
        );
    }

    inline Type(TypeKind kind) : kind(kind) {}

    inline Type(u8 int_size, bool int_signed)
        : kind(TYPE_INT), int_size(int_size), is_signed(int_signed) {}
};