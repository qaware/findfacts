grammar YXML;
yxml: tree+ EOF;
tree: ws? tag ws? inner* ws? close ws?;
tag: x y name (y key '=' value)* x;
inner: tree | body;
close: x y x;

x: '\u0005';
y: '\u0006';
ws: (' ' | '\t' | '\r' | '\n')+;
value: (~('\u0005' | '\u0006'))*;
name: (~('\u0005' | '\u0006'))+;
body: ~('\u0005' | '\u0006' | ' ' | '\t' | '\r' | '\n') (ws* ~('\u0005' | '\u0006' | ' ' | '\t' | '\r' | '\n'))*;
key: (~('\u0005' | '\u0006' | '='))+;
