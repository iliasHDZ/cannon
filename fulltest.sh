cmake -S . -B build
cmake --build build
./build/cannon
llc -opaque-pointers -filetype=obj code.ll -o code.o
clang -no-pie -o test code.o
./test