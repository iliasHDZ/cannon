#pragma once

#include <stdlib.h>
#include <stdio.h>
#include <iostream>
#include "common.hpp"
#include "parser.hpp"
#include "global.hpp"
#include "process.hpp"
#include <fstream>

int main() {
    FILE* f = fopen("test.cn", "rb");
    if (f == NULL) {
        std::cout << "Could not open source file" << std::endl;
        return -1;
    }

    fseek(f, 0, SEEK_END);
    u32 size = ftell(f);
    std::string code;
    code.resize(size, ' ');
    fseek(f, 0, SEEK_SET);
    fread(code.data(), 1, size, f);
    fclose(f);

    Lexer lexer(code, "test.cn");
    Parser parser(lexer);

    auto global = parser.parse_global();

    if (global == nullptr) return -1;

    GlobalScope gsc;

    gsc.resolve_types(global);

    if (error_compiling) {
        delete global;
        return -1;
    }

    gsc.resolve_objects(global);

    if (error_compiling) {
        delete global;
        return -1;
    }

    CodeProcessor proc;

    llvm::Module* mod = proc.codegen_module(&gsc);

    if (mod == nullptr) {
        delete global;
        return -1;
    }

    std::string buf;
    llvm::raw_string_ostream os(buf);
    mod->print(os, nullptr);
    os.flush();

    std::cout << buf << std::endl;

    std::ofstream fl("code.ll");
    if (fl.bad()) {
        std::cout << "could not write to code.ll" << std::endl;
        delete global;
        return -1;
    }

    fl << buf;
    fl.close();

    delete global;
    return 0;
}