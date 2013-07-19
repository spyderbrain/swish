
% Reading and writing
% -------------------


hello_world :-
    writeln('Hello World!'),
    hello_world.
    


read_and_write :-
    repeat,
    prompt(_, 'Input'),
    read(Something),
    writeln(Something),
    Something = stop.

 

/** Examples

hello_world.
read_and_write.

*/
