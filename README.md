# csbfc

chicken scheme brainfuck compiler

## OPTIMIZE

* `+`と`-`の連続、`<`と`>`の連続をrun-length圧縮
* `[-]`などのクリアループを最適化

## Usage

~~~~~{.sh}
csbfc - chicken scheme brainfuck compiler
Usage: csbfc FILENAME | OPTION ...
    -h -help            display this text and exit
    -n -no-optimize     no optimize
    -d -debug           debug mode%
~~~~~


## 参考サイト 

* [http://calmerthanyouare.org/2015/01/07/optimizing-brainfuck.html](http://calmerthanyouare.org/2015/01/07/optimizing-brainfuck.html)
