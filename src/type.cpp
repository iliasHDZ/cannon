#include "type.hpp"
#include <list>

static std::list<BaseType*> types;

BaseType::~BaseType() {}

bool BaseType::is_named() {
    return false;
}

inline u32 next_aligned(u32 v, u32 a) {
    return ( v + (a - 1) ) / a * a;
}

NamedType::NamedType(char* name)
    : name(name), type(nullptr)
{
    types.push_back(this);
}

NamedType::NamedType(char* name, BaseType* type)
    : name(name), type(type)
{
    types.push_back(this);
}

TypeKind NamedType::get_kind() {
    return type ? type->get_kind() : TYPE_ERROR;
}

bool NamedType::int_signed() {
    return type ? type->int_signed() : false;
}

BaseType* NamedType::get_pointee() {
    return type ? type->get_pointee() : nullptr;
}

std::span<StructMember> NamedType::get_members() {
    return type ? type->get_members() : (std::span<StructMember> { (StructMember*)nullptr, 0 });
}

std::span<BaseType*> NamedType::get_params() {
    return type ? type->get_params() : (std::span<BaseType*> { (BaseType**)nullptr, 0 });
}

BaseType* NamedType::get_return_type() {
    return type ? type->get_return_type() : nullptr;
}

u32 NamedType::get_alignment() {
    return type ? type->get_alignment() : 1;
}

u32 NamedType::get_size() {
    return type ? type->get_size() : 0;
}

u32 NamedType::get_array_size() {
    return type ? type->get_array_size() : 0;
}

i32 NamedType::get_member_offset(char* member) {
    return type ? type->get_member_offset(member) : 0;
}

bool NamedType::is_named() {
    return true;
}

std::string NamedType::to_string() {
    return name;
}

TypeKind Type::get_kind() {
    return kind;
}

bool Type::int_signed() {
    if (kind != TYPE_INT) return false;
    return is_signed;
}

BaseType* Type::get_pointee() {
    if (kind == TYPE_ARRAY) return array_elem;
    if (kind == TYPE_POINTER) return ptr_type;
    return nullptr;
}

std::span<StructMember> Type::get_members() {
    if (kind != TYPE_STRUCT) return { (StructMember*)nullptr, 0 };
    return { members, member_count };
}

std::span<BaseType*> Type::get_params() {
    if (kind != TYPE_FUNC) return { (BaseType**)nullptr, 0 };
    return { param_types, param_count };
}

BaseType* Type::get_return_type() {
    if (kind != TYPE_FUNC) return nullptr;
    return return_type;
}

u32 Type::get_alignment() {
    switch (kind) {
    default:
    case TYPE_ERROR:
    case TYPE_VOID:
        return 1;
    case TYPE_INT:
        return int_size;
    case TYPE_POINTER:
        return MACHINE_SIZE;
    case TYPE_STRUCT:
        break;
    }

    u32 align = 1;
    for (u32 i = 0; i < member_count; i++) {
        u32 a = members[i].type->get_alignment();
        if (a > align)
            align = a;
    }
    return align;
}

u32 Type::get_size() {
    switch (kind) {
    default:
    case TYPE_ERROR:
    case TYPE_VOID:
        return 0;
    case TYPE_INT:
        return int_size;
    case TYPE_POINTER:
        return MACHINE_SIZE;
    case TYPE_STRUCT:
        break;
    }

    u32 align = 1;
    u32 size = 0;
    for (u32 i = 0; i < member_count; i++) {
        u32 a = members[i].type->get_alignment();
        size = next_aligned(size, a) + members[i].type->get_size();
        if (a > align)
            align = a;
    }
    size = next_aligned(size, align);
    return size;
}

u32 Type::get_array_size() {
    return (kind == TYPE_ARRAY) ? array_size : 0;
}

i32 Type::get_member_offset(char* member) {
    if (kind != TYPE_STRUCT) return -1;
    u32 offset = 0;
    for (u32 i = 0; i < member_count; i++) {
        u32 a = members[i].type->get_alignment();
        offset = next_aligned(offset, a);
        if (members[i].name == member)
            return offset;
        offset += members[i].type->get_size();
    }
    return -1;
}

std::string Type::to_string() {
    std::string str;
    switch (kind) {
    case TYPE_ERROR:   return "<error>";
    case TYPE_VOID:    return "void";
    case TYPE_INT:     return (int_signed() ? 'i' : 'u') + std::to_string(get_size() * 8);
    case TYPE_POINTER: return ptr_type->to_string() + "*";
    case TYPE_STRUCT:
        str = "{";
        for (u32 i = 0; i < member_count; i++) {
            if (i != 0) str += ", ";
            str += members[i].name + std::string(": ") + members[i].type->to_string();
        }
        return str + "}";
    case TYPE_FUNC:
        str = "(";
        for (u32 i = 0; i < param_count; i++) {
            if (i != 0) str += ", ";
            str += param_types[i]->to_string();
        }
        str += ") -> " + return_type->to_string();
        return str;
    case TYPE_ARRAY:
        return array_elem->to_string() + '[' + std::to_string(array_size) + ']';
    }
    return "<error>";
}

Type* Type::create_struct(const std::vector<StructMember>& members) {
    Type* t = new Type(TYPE_STRUCT);
    t->member_count = members.size();
    t->members = new StructMember[members.size()];
    memcpy(t->members, members.data(), members.size() * sizeof(StructMember));
    types.push_back(t);
    return t;
}

Type* Type::create_func(const std::vector<BaseType*> params, BaseType* return_type) {
    for (auto tp : types) {
        if (tp->is_named()) continue;
        if (tp->get_kind() != TYPE_FUNC) continue;
        if (tp->get_return_type() != return_type) continue;
        if (tp->get_params().size() != params.size()) continue;
        bool same = true;
        for (u32 i = 0; i < params.size(); i++) {
            if (tp->get_params()[i] != params[i]) {
                same = false;
                break;
            }
        }
        if (same)
            return (Type*)tp;
    }

    Type* t = new Type(TYPE_FUNC);
    t->param_count = params.size();
    t->param_types = new BaseType*[t->param_count];
    memcpy(t->param_types, params.data(), params.size() * sizeof(BaseType*));
    t->return_type = return_type;
    return t;
}

Type* Type::create_pointer(BaseType* pointee) {
    for (auto t : types) {
        if (t->is_named()) continue;
        if (t->get_kind() != TYPE_POINTER) continue;
        if (t->get_pointee() == pointee)
            return (Type*)t;
    }

    Type* ret = new Type(TYPE_POINTER);
    ret->ptr_type = pointee;
    types.push_back(ret);
    return ret;
}

Type* Type::create_array(BaseType* elem, u32 size) {
    for (auto t : types) {
        if (t->is_named()) continue;
        if (t->get_kind() != TYPE_ARRAY) continue;
        if (t->get_pointee() == elem && t->get_array_size() == size)
            return (Type*)t;
    }

    Type* ret = new Type(TYPE_ARRAY);
    ret->array_elem = elem;
    ret->array_size = size;
    types.push_back(ret);
    return ret;
}

static Type void_type { TYPE_VOID };
static Type error_type { TYPE_ERROR };

Type* Type::VOID = &void_type;
Type* Type::ERROR = &error_type;

static Type int_types[] = {
    { 1, false },
    { 2, false },
    { 4, false },
    { 8, false },
    { 1, true },
    { 2, true },
    { 4, true },
    { 8, true },
};

Type* Type::get_int(u8 size, bool sign) {
    for (u32 i = 0; i < sizeof(int_types) / sizeof(Type); i++) {
        if (int_types[i].int_size == size && int_types[i].is_signed == sign)
            return &int_types[i];
    }
    return nullptr;
}