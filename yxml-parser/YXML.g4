grammar YXML;
yxml: tree* EOF;
tree: open inner* close;
open: x y tag (y key '=' value)* x;
inner: tree | body;
close: x y x;

x: '\u0005';
y: '\u0006';
value: (~('\u0005' | '\u0006'))*;
tag: (~('\u0005' | '\u0006'))+;
body: (~('\u0005' | '\u0006'))+;
key: (~('\u0005' | '\u0006' | '='))+;
