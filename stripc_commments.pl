#!/usr/bin/perl
$/ = undef;
    $_ = <>;
    s#/\*[^*]*\*+([^/*][^*]*\*+)*/|("(\\.|[^"\\])*"|'(\\.|[^'\\])*'|\n+|.[^/"'\\]*)#$2#g;
    print;
