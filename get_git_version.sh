#!/usr/bin/env sh

if [ "X"${CC} = "X" ] ; then
    gcc show_git.c -o a.out ; ./a.out ; rm a.out
else
   ${CC} show_git.c -o a.out ; ./a.out ; rm a.out
fi
