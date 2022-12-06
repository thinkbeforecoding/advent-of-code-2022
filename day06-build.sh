path_to_executable=$(which aarch64-linux-gnu-as)
if [ -x "$path_to_executable" ] ; then
    mkdir -p ./obj
    mkdir -p ./bin
    aarch64-linux-gnu-as -o ./obj/day06.o day06.asm
    aarch64-linux-gnu-ld -o ./bin/day06 ./obj/day06.o
else
    echo "before build run the following command"
    echo "sudo apt install gcc-12-aarch64-linux-gnu"
fi