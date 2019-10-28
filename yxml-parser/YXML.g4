grammar YXML;

file: markup yxml EOF;
yxml: (markup | text)*;
markup: X Y text kv* X yxml X Y X;
text: (TS | EQ)+;
kv: Y TS EQ text?;

TS: ~('\u0005' | '\u0006' | '=')+;
EQ: '=';
X: '\u0005';
Y: '\u0006';