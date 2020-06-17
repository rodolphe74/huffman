#!/bin/sh
file=$1
file_comp=$file.huf
file_uncomp=$file.unhuf

eval="(progn (defparameter pth \"$file\") (defparameter pthOut \"$file_comp\") (defparameter uncompPthOut \"$file_uncomp\")  (createCompressedFile) (uncompressFile)  (exit))"
echo $eval

sbcl --noinform --load huffman.fasl --eval "$eval"

cmp -l $file $file_uncomp
