#!/usr/bin/env sh

rm test
cargo run > test.s
gcc -nostdlib test.s -o test
objdump -M intel -d test
