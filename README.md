# csbfc

chicken scheme brainfuck compiler

## OPTIMIZE

* `+`と`-`の連続、`<`と`>`の連続をrun-length圧縮
* `[-]`などのクリアループを最適化
* `[->>>++>>-<<<<<]`のようなかけ算を目的としたループの最適化(見逃しがありそう)
* offset optimization: ポインタの移動を引数に

## Usage

`chicken-install`すると多分`/usr/local/bin`以下に`csbfc`という実行ファイルができます。

~~~~~{.sh}
csbfc - chicken scheme brainfuck compiler
Usage: csbfc <file> | <option> ...
    -d -debug           debug mode
    -h -help            display this text and exit
    -O -O0 -O1 -O2 -O3  enable certain sets of optimization options
    -o <file>           write output to <file>
~~~~~

## 参考サイト 

* [http://calmerthanyouare.org/2015/01/07/optimizing-brainfuck.html](http://calmerthanyouare.org/2015/01/07/optimizing-brainfuck.html)


