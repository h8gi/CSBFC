# csbfc

chicken scheme brainfuck compiler

## OPTIMIZE

+と-の連続、<と>の連続をrun-length圧縮。

## Usage

~~~~~{.sh}
$ csbfc
csbfc - chicken scheme brainfuck compiler
Usage: csbfc FILENAME | OPTION ...
    -h -help        display this text and exit
    -d -debug       debug mode
~~~~~
