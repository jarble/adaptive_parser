:- initialization(main).
:- set_prolog_flag('double_quotes','chars').
:- use_module(library(prolog_stack)).
:- use_module(library(error)).
:- use_module(library(pio)).
:- use_module(library(chr)).
:- chr_constraint recursive_rewrite_/2.
:- chr_constraint rewrite2(+,+).
recursive_rewrite_(X,Y) \ recursive_rewrite_(X,Y) <=> true.
rewrite2(X,Y) \ rewrite2(X,Y) <=> true.

rewrite2(Input,Output) ==>
	rewrite_rule(Input1,Output1),
	once(append([Left, Input1, Right], Input)),
    append([Left, Output1, Right], Output),
    recursive_rewrite_(Output1,Output1).

recursive_rewrite_([],[]).
recursive_rewrite_(A,B),recursive_rewrite_(C,B) <=> recursive_rewrite_(A,B).
recursive_rewrite_(A,B) ==> nonvar(A),var(B),rewrite2(A,B1),recursive_rewrite_(B1,B),writeln(recursive_rewrite_(A,B));A=[A1],(is_string(A1);is_number(A1);bool(A1)),A=B.

user:prolog_exception_hook(Exception, Exception, Frame, _):-
    (   Exception = error(Term)
    ;   Exception = error(Term, _)),
    get_prolog_backtrace(Frame, 20, Trace),
    format(user_error, 'Error: ~p', [Term]), nl(user_error),
    print_prolog_backtrace(user_error, Trace), nl(user_error), fail.

main :-
	Input="G + B is not Q+R and Z is less than A + Q and T is greater than R+G-1 and Q is more than R and Z is less than T+1",
	recursive_rewrite(Input,C),writeln(C).


recursive_rewrite(A) :- recursive_rewrite(A,_).

recursive_rewrite(A,B) :-
	phrase(tokenize(A1),A),writeln('calling recursive rewrite'),recursive_rewrite_(A1,B).

rewrite_rule([A,is,more,than,B],[A>B]).
rewrite_rule([A,is,less,than,B],[A<B]).
rewrite_rule([A,and,B],[(A,B)]).
rewrite_rule([A,or,B],[(A;B)]).
rewrite_rule(['(',A,')'],[(A)]).
rewrite_rule([A,>,B],[A>B]).
rewrite_rule([A,is,greater,than,B],[A>B]).
rewrite_rule([A,<,B],[A<B]).
rewrite_rule([A,=<,B],[A=<B]).
rewrite_rule([A,>=,B],[A>=B]).
rewrite_rule([A,=,B],[A=B]).
rewrite_rule([A,is,equal,to,B],[A=B]).
rewrite_rule([A,is,not,equal,to,B],[A\=B]).
rewrite_rule([A,implies,B],[A->B]).
rewrite_rule([height,of,A],[height(A)]).
rewrite_rule([A,^,B],[(A^B)]).
rewrite_rule([A,+,B],[(A+B)]).
rewrite_rule([A,*,B],[(A*B)]).
rewrite_rule([A,-,B],[(A-B)]).
rewrite_rule([A,/,B],[(A/B)]).
rewrite_rule([greater],[more]).
rewrite_rule([A,is,B],[A=B]).
rewrite_rule([A,is,not,B,than,C],['(','(',A,is,B,than,C,')',=,false,')']).
rewrite_rule([A,is,not,B],[A \= B]).


rewrite_if_possible(A,B) :-
	recursive_rewrite_(A,B),writeln(['rewrite if possible',A,B]);A=B.
	
atoms_or_vars([]).
atoms_or_vars([A|B]) :- (atom(A);var(A));atoms_or_vars(B).

same_var_type(A,B) :- bool(A),bool(B).
same_var_type(A,B) :- is_string(A),is_string(B).
same_var_type(A,B) :- is_number(A),is_number(B).
is_string(A) :- starts_with_upper(A).
is_number(A) :- starts_with_upper(A).
is_number(A) :- number(A).
is_number(height(A)) :- is_number(A).
is_number(factorial(A)) :- is_number(A).
is_number(mod(A,B)) :- is_number(A),is_number(B).
is_number(A+B) :- is_number(A),is_number(B).
is_number(A*B) :- is_number(A),is_number(B).
is_number(A/B) :- is_number(A),is_number(B).
is_number(A-B) :- is_number(A),is_number(B).
is_number(A^B) :- is_number(A),is_number(B).

bool(A) :- starts_with_upper(A).
bool('false').
bool('true').
bool(A<B) :- is_number(A),is_number(B).
bool(A=B) :- same_var_type(A,B).
bool(A\=B) :- same_var_type(A,B).
bool(A>B) :- is_number(A),is_number(B).
bool((A,B)) :- bool(A),bool(B).
bool((A;B)) :- bool(A),bool(B).
bool((A->B)) :- bool(A),bool(B).
bool(\+A) :- bool(A).
starts_with_upper(Atom) :- atom(Atom),atom_chars(Atom,[List1|_]),char_type(List1,upper).



words_or_numbers_(A) --> symbol(A);string_literal(A).
words_or_numbers__([A]) --> words_or_numbers_(A_),{atom_chars(A,A_)}.
words_or_numbers__([A]) --> an_int(A_),{number_chars(A,A_)}.
words_or_numbers([A]) --> words_or_numbers__([A]).
words_or_numbers([A|B]) --> words_or_numbers__([A]),ws,tokens(B).
words_or_numbers([A|B]) --> words_or_numbers__([A]),ws_,words_or_numbers(B).
tokens([A]) --> {token(A)},[A].
tokens([A|B]) --> {token(A)},[A],ws,words_or_numbers(B).
tokens([A|B]) --> {token(A)},[A],ws,tokens(B).
tokenize(A) --> ws,words_or_numbers(A),ws.
tokenize(A) --> ws,tokens(A),ws.
tokenize([]) --> ws.
token(A) --> [A],{token(A)}.
token('>').
token('<').
token('(').
token(')').
token('{').
token('}').
token('[').
token(']').
token('=').
token('^').
token('!=').
token('!').
token('/').
token('*').
token('+').
token('-').
token(',').
token('%').
token('.').

string_literal("\"\"") --> "\"\"".
string_literal(S1) --> "\"",string_inner(S),"\"",{string_to_literal(S,S1)}.
string_literal1("") --> "\'\'".
string_literal1(S) --> "\'",string_inner1(S),"\'","\'\''".
string_inner([A]) --> string_inner_(A).
string_inner([A|B]) --> string_inner_(A),string_inner(B).
string_inner_(A) --> {A="\\\""},A;{dif(A,'"'),dif(A,'\n')},[A].
string_inner1([A]) --> string_inner1_(A).
string_inner1([A|B]) --> string_inner1_(A),string_inner1(B).
string_inner1_(A) --> {A="\\'"},A;{dif(A,'\''),dif(A,'\n')},[A].

ws --> "";ws_.
ws_ --> (" ";"\n";"\r"),ws.

symbol([L|Ls]) --> letter(L), symbol_r(Ls).
symbol_r([L|Ls]) --> csym(L), symbol_r(Ls).
symbol_r([])     --> [].
letter(Let)     --> [Let], { code_type(Let, alpha) }.
csym(Let)     --> [Let], {code_type(Let, csym)}.

a_double([A,B]) -->
        (an_int(A), ".", an_int(B)).
a_double([A,['0']]) -->
        (an_int(A)).
an_int([L|Ls]) --> digit(L), an_int_r(Ls).
an_int_r([L|Ls]) --> digit(L), an_int_r(Ls).
an_int_r([])     --> [].
digit(Let)     --> [Let], { code_type(Let, digit) }.
