exe_path=$(which qemu-aarch64)
if [ -x "$exe_path" ]; then
    cat ./input/day06.txt | qemu-aarch64 ./bin/day06
else
    echo "Before running intall"
    echo "sudo apt install qemu-user"
fi
