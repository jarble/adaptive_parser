:- initialization(main).
:- set_prolog_flag('double_quotes','chars').

main :-
	tokenize("a is more than b",Tokens),
	recursive_rewrite_(Tokens,Tokens1),
	writeln(Tokens1).

recursive_rewrite_(Input,Output) :-
	ground(Input),
	rewrite2(Input,Output1),recursive_rewrite_(Output1,Output);
	Input = Output,Output=[Output1],bool(Output1).

rewrite2(Input,Output) :-
	rewrite_rule(Input1,Output1),
	replace(Input1,Output1,Input,Output).

rewrite_rule(['(',A,')'],[A]).
rewrite_rule([A,is,more,than,B],[A,>,B]) :- is_number(A),is_number(B).
rewrite_rule([A,is,less,than,B],[A,<,B]) :- is_number(A),is_number(B).
rewrite_rule([A,>,B],[A>B]) :- is_number(A),is_number(B).
rewrite_rule([A,<,B],[A<B]) :- is_number(A),is_number(B).
rewrite_rule([A,+,B],[A+B]) :- is_number(A),is_number(B).

rewrite_rule([A,or,B],[(A;B)]) :- bool(A),bool(B).
rewrite_rule([A,and,B],[(A,B)]) :- bool(A),bool(B).
rewrite_rule([A,implies,B],[(A->B)]) :- bool(A),bool(B).
rewrite_rule([A,if,B],[B,implies,A]) :- bool(A),bool(B).

bool(A) :- atom(A).
bool(true).
bool(false).
bool(A>B).
bool(A<B).
bool((A,B)).
bool((A;B)).
bool(forall(_,_)).
bool(_->_).

is_number(A) :- atom(A).
is_number(A) :- number(A).
is_number(A+B).
is_number(A-B).
is_number(A*B).


replace_with_var([A],[A1],List_of_vars) :- member([A,A1],List_of_vars);A=A1.
replace_with_var([A|B],[A1|B1],List_of_vars) :- replace_with_var([A],[A1],List_of_vars),replace_with_var(B,B1,List_of_vars).
list_of_vars([A],[[]]) :- \+starts_with_upper(A).
list_of_vars([A],[[A,_]]) :- starts_with_upper(A).
list_of_vars([Word|Words],[Vars|Vars1]) :- list_of_vars([Word],[Vars]),list_of_vars(Words,Vars1).
starts_with_upper(Atom) :- atom_chars(Atom,[List1|_]),char_type(List1,upper).

replace(ToReplace, ToInsert, List, Result) :-
    once(append([Left, ToReplace, Right], List)),
    append([Left, ToInsert, Right], Result).

words_or_numbers_(A) --> symbol(A);string_literal(A).
words_or_numbers__([A]) --> words_or_numbers_(A_),{atom_chars(A,A_)}.
words_or_numbers__([A]) --> an_int(A_),{number_chars(A,A_)}.
words_or_numbers([A]) --> words_or_numbers__([A]).
words_or_numbers([A|B]) --> words_or_numbers__([A]),ws,tokens(B).
words_or_numbers([A|B]) --> words_or_numbers__([A]),ws_,words_or_numbers(B).
tokens([A]) --> {token(A)},[A].
tokens([A|B]) --> {token(A)},[A],ws,words_or_numbers(B).
tokens([A|B]) --> {token(A)},[A],ws,tokens(B).
tokenize(A,B) :- phrase(tokenize(B),A).
tokenize(A) --> words_or_numbers(A).
tokenize(A) --> tokens(A).
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

string_literal("") --> "\"\"".
string_literal(S) --> "\"",string_inner(S),"\"".
string_literal1("") --> "\'\'".
string_literal1(S) --> "\'",string_inner1(S),"\'","\'\''",{S=""}.
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
