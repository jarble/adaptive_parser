:- initialization(main).
:- set_prolog_flag('double_quotes','chars').
:- use_module(library(prolog_stack)).
:- use_module(library(error)).
:- use_module(library(pio)).
:- use_module(library(chr)).

user:prolog_exception_hook(Exception, Exception, Frame, _):-
    (   Exception = error(Term)
    ;   Exception = error(Term, _)),
    get_prolog_backtrace(Frame, 20, Trace),
    format(user_error, 'Error: ~p', [Term]), nl(user_error),
    print_prolog_backtrace(user_error, Trace), nl(user_error), fail.

main :-
	phrase_from_file(lines(Ls), 'input_file.txt'),
	!,
	writeln('\n'),
	Input='C is not less than 10 percent of E and R implies that Q is not true and Z is the sum of G and R',
	writeln(Input),
	writeln('\nmeans\n'),
	atom_chars(Input,Input1),
	recursive_rewrite(Input1,[Output]),
	has_type(Output,_),
	writeln(Output),
	writeln('\n').

lines([])           --> call(eos), !.
lines([Line|Lines]) --> line(Line),{writeln(Line),recursive_rewrite(Line),!}, lines(Lines).

eos([], []).

line([])     --> ( {char_code('.',Dot)},[Dot];call(eos) ), !.
line([L1|Ls]) --> [L],{char_code(L1,L)},line(Ls).

tokenize(A,B) :- phrase(tokenize(B),A).

adaptive_parse([A]) :- recursive_rewrite(A).
adaptive_parse([A|B]) :- adaptive_parse([A]),adaptive_parse(B).

recursive_rewrite(A) :- recursive_rewrite(A,_).
recursive_rewrite(A,B) :-
	tokenize(A,A1),recursive_rewrite_(A1,B).
recursive_rewrite_([],[]).
recursive_rewrite_(Input,Output) :-
	ground(Input),
	recursive_rewrite_all([Input],_,Output),
	writeln(['input to output',Input,Output]).

recursive_rewrite_all(Inputs,Outputs,Output) :-
	rewrite_all(Inputs,Inputs1),!,length(Inputs1,Len),writeln(Inputs1),writeln(['length of inputs',Len]),recursive_rewrite_all(Inputs1,Outputs,Output);
	Inputs=Outputs,member(Output,Outputs),rewrite_rule(_,Output).
	
rewrite_all(Inputs,Outputs) :-
	setof(Output,rewrite2(Inputs,Output), Outputs).

rewrite2(Inputs,Output) :-
	member(Input,Inputs),
	rewrite_rule(Input1,Output1),
	replace(Input1,Output1,Input,Output),
	rewrite_rule(Input1,Output1),(is_string(Output1);bool(Output1);is_number(Output1)).

is_string(A,A) :- \+(starts_with_lower(A)),has_type(A,string).
is_string(A,B) :- \+(starts_with_lower(A);starts_with_lower(B)),nonvar(A),rewrite_rule(A,B),is_string(B).
bool(A,A) :- has_type(A,bool).
bool(A,B) :- nonvar(A),rewrite_rule(A,B),bool(B).
imperative(A,B) :- bool(A,B).
number(A,A) :- has_type(A,number).
number(A,B) :- nonvar(A),rewrite_rule(A,B),number(B).

atom_to_literal(Atom,Literal) :-
	atom_chars(Atom,Atom_),
	string_to_literal(Atom_,Literal__),
	atom_chars(Literal,Literal__).
literal_to_atom(Literal,Atom) :-
	atom_chars(Literal,Literal__),
	string_to_literal(Atom_,Literal__),
	atom_chars(Atom,Atom_).
string_to_literal(Atom_,Literal__) :-
	append(['"'],Atom_,Literal_),
	append(Literal_,['"'],Literal__).
	
rewrite_if_possible(A,B) :-
	recursive_rewrite_(A,B),writeln(['rewrite if possible',A,B]);A=B.

rewrite_rule1(A1_,B1_) :- string_to_literal(A1,A1_),string_to_literal(B1,B1_),tokenize(A1,A2),tokenize(B1,B2),list_of_vars(A2,List_of_vars),replace_with_var(A2,A3,List_of_vars),replace_with_var(B2,B3,List_of_vars),rewrite_if_possible(B3,B4),assertz(rewrite_rule(A3,B4)).
replace_with_var([A],[A1],List_of_vars) :- member([A,A1],List_of_vars);A=A1.
replace_with_var([A|B],[A1|B1],List_of_vars) :- replace_with_var([A],[A1],List_of_vars),replace_with_var(B,B1,List_of_vars).
list_of_vars([A],[[]]) :- \+starts_with_upper(A).
list_of_vars([A],[[A,_]]) :- starts_with_upper(A).
list_of_vars([Word|Words],[Vars|Vars1]) :- list_of_vars([Word],[Vars]),list_of_vars(Words,Vars1).

starts_with_upper(Atom) :- atom(Atom),atom_chars(Atom,[List1|_]),char_type(List1,upper).
starts_with_lower(Atom) :- atom(Atom),atom_chars(Atom,[List1|_]),char_type(List1,lower).

%rewrite_rule(A,B) :- rewrite_rule1(A,B).

rewrite_rule([[comma([A,B])|C]],[D]) :- BC = [B|C],D=[A|BC].
rewrite_rule([('false'->A)],[false]) :- bool(A,_).
rewrite_rule(['true'->A],[A1]) :- bool(A,A1).
rewrite_rule(['[',A,',',B,']'],[[A,B]]) :- dif(B,comma(_)).
rewrite_rule(['[',A,',',B,','],['[',comma([A,B]),',']) :- dif(B,comma(_)).
rewrite_rule(['[',A,']'],[[A]]) :- dif(A,comma(_)).
rewrite_rule([select,A,from,B,where,C],[list_comprehension(A,B,C1)]) :- bool(C,C1).
rewrite_rule([determinant,of,A],[det(A1)]) :- is_string(A,A1).
rewrite_rule([A,is,a,member,of,B],[B,is,in,A]).
rewrite_rule([A,is,an,element,of,B],[A,is,a,member,of,B]).
rewrite_rule([!,=],[\=]).
rewrite_rule([A,\=,B],[A\=B]) :- has_type(A,C),has_type(B,C).
rewrite_rule(bool,A,B) :- bool(A,B).
rewrite_rule(number,A,B) :- number(A,B).
rewrite_rule(string,A,B) :- is_string(A,B).
rewrite_rule([height,of,A],[height(A)]).
rewrite_rule([A,is,in,B],[member(A,B)]).
rewrite_rule([A**B],[A1^B1]) :- number(A,A1),number(B,B1).
rewrite_rule([A,/,B],[A1/B1]) :- number(A,A1),number(B,B1).
rewrite_rule([A,+,B],[A1+B1]) :- number(A,A1),number(B,B1).
rewrite_rule([A,-,B],[A1-B1]) :- number(A,A1),number(B,B1).
rewrite_rule([A,*,B],[A1*B1]) :- number(A,A1),number(B,B1).
rewrite_rule([A,'%',B],[mod(A1,B1)]) :- number(A,A1),number(B,B1).
rewrite_rule([A,'^',B],[A1^B1]) :- number(A,A1),number(B,B1).
rewrite_rule([A,'=',B],[A=B]) :- has_type(A,C),has_type(B,C).
rewrite_rule([A,<,B],[A1<B1]) :- number(A,A1),number(B,B1).
rewrite_rule([A,>,B],[A1>B1]) :- number(A,A1),number(B,B1).
rewrite_rule([the,A],[A1]) :- rewrite_rule_(A,A1).
rewrite_rule([maximize,A,subject,to,B],[maximize(A1,B1)]) :- number(A,A1),bool(B,B1).
rewrite_rule([A,for,all,B],[forall(A1,B1)]) :- bool(A,A1),bool(B,B1).
rewrite_rule([natural,logarithm,of,A],[ln(A1)]) :- number(A,A1).
rewrite_rule([union,of,A,and,B],[union(A1,B1)]) :- number(A,A1),number(B,B1).
rewrite_rule([intersection,of,A,and,B],[intersection(A1,B1)]) :- number(A,A1),number(B,B1).
rewrite_rule([solve,A,for,B],[solve(A1,B1)]) :- bool(A,A1),number(B,B1).
rewrite_rule([A,is,an,integer],[integer(A1)]) :- number(A,A1).
rewrite_rule([A,is,a,number],[number(A1)]) :- number(A,A1).
rewrite_rule([A,if,and,only,if,B],['(',A1,if,B1,')',and,'(',B1,if,A1,')']) :- bool(A,A1),bool(B,B1).
rewrite_rule([A,iff,B],[A1,if,and,only,if,B1]) :- bool(A,A1),bool(B,B1).
rewrite_rule([greatest,common,denominator,of,A,and,B],[gcd(A1,B1)]) :- number(A,A1),number(B,B1).
rewrite_rule([A,is,a,subset,of,B],[subset(A1,B1)]) :- number(A,A1),number(B,B1).
rewrite_rule([A,is,a,superset,of,B],[subset(B,A)]).
rewrite_rule([sine,of,A],[sin(A1)]) :- number(A,A1).
rewrite_rule([absolute,value,of,A],[abs(A1)]) :- number(A,A1).
rewrite_rule([floor,of,A],[floor(A1)]) :- number(A,A1).
rewrite_rule([ceiling,of,A],[ceiling(A1)]) :- number(A,A1).
rewrite_rule([A,divided,by,B],[A1/B1]) :-  number(A,A1),number(B,B1).
rewrite_rule([dot,product,of,A,and,B],[dot_product(A1,B1)]) :-  number(A,A1),number(B,B1).
rewrite_rule([cross,product,of,A,and,B],[cross_product(A1,B1)]) :-  number(A,A1),number(B,B1).
rewrite_rule([remainder,of,A/B],[remainder(A1,B1)]) :- number(A,A1),number(B,B1).
rewrite_rule([cosine,of,A],[cos(A1)]) :- number(A,A1).
rewrite_rule([tangent,of,A],[tan(A1)]) :- number(A,A1).
rewrite_rule([factorial,of,A],[factorial(A1)]) :- number(A,A1).
rewrite_rule([A,!],[factorial,of,A]).
rewrite_rule([inverse,sine,of,A],[asin(A1)]) :- number(A,A1).
rewrite_rule([inverse,cosine,of,A],[acos(A1)]) :- number(A,A1).
rewrite_rule([inverse,tangent,of,A],[atan(A1)]) :- number(A,A1).
rewrite_rule([derivative,of,A,with,respect,to,B],[derivative(A1,B1)]) :- number(A,A1),number(B,B1).
rewrite_rule([distance,between,A,and,B],[distance(A1,B1)]) :- number(A,A1),number(B,B1).
rewrite_rule([distance,from,A,to,B],[distance,between,A,and,B]).
rewrite_rule([integral,of,A,with,respect,to,B],[integral(A1,B1)]) :- number(A,A1),number(B,B1).
rewrite_rule([A,plus,B],[A1+B1]) :- number(A,A1),number(B,B1).
rewrite_rule([A,times,B],[A1*B1]) :- number(A,A1),number(B,B1).
rewrite_rule([A,minus,B],[A1-B1]) :- number(A,A1),number(B,B1).
rewrite_rule([A,to,the,power,of,B],[A1^B1]) :- number(A,A1),number(B,B1).
rewrite_rule([square,root,of,A],[sqrt(A1)]) :- number(A,A1).
rewrite_rule([cube,root,of,A],[(A1**(1/3))]) :- number(A,A1).
rewrite_rule([A,is,not,B,than,C,and,not,D,than,E],[A,is,not,B,than,C,and,A,is,not,D,than,E]).
rewrite_rule([A,is,not,B,than,C,or,not,D,than,E],[A,is,not,B,than,C,or,A,is,not,D,than,E]).
rewrite_rule([A,is,not,B,than,C],['(','(',A,is,B,than,C,')',is,false,')']).
rewrite_rule([A,is,B,than,C,or,D,than,E],['(',A1,is,B,than,C1,')',or,'(',A1,is,D,than,E1,')']).
rewrite_rule([A,is,B,than,C,and,D,than,E],['(',A1,is,B,than,C1,')',and,'(',A1,is,D,than,E1,')']).
rewrite_rule([do,A,unless,B],[A1,unless,B1]) :- bool(A,A1),bool(B,B1).
rewrite_rule([A,unless,B],[(B1=false)->A1]) :- bool(A,A1),bool(B,B1).
rewrite_rule([sum,of,A,and,B],[A1+B1]) :- number(A,A1),number(B,B1).
rewrite_rule([A,is,implied,by,B],[B1,implies,A1]) :- bool(A,A1),bool(B,B1).
rewrite_rule([substring,of,A,to,B,from,C],[substring,of,A,from,C,to,B]) :- is_string(A,A1),number(B,B1),number(C,C1).
rewrite_rule([A,is,less,than,B],[A1<B1]) :- number(A,A1),number(B,B1).
rewrite_rule([A,is,more,than,B],[A1>B1]) :- number(A,A1),number(B,B1).
rewrite_rule([A,is,a,substring,of,B],[is_substring(A1,B1)]) :- is_string(A,A1),is_string(B,B1).
rewrite_rule([A,or,B],[(A1;B1)]) :- bool(A,A1),bool(B,B1).
rewrite_rule([A,and,B],[(A1,B1)]) :- bool(A,A1),bool(B,B1).
rewrite_rule([either,A,or,B],[(A1;B1)]) :- bool(A,A1),bool(B,B1).
rewrite_rule([A,implies,B],[(A1->B1)]) :- bool(A,A1),bool(B,B1).
rewrite_rule([A,if,B],[B1->A1]) :- bool(A,A1),bool(B,B1).
rewrite_rule([if,A,then,B],[B1,if,A1]) :- bool(A,A1),bool(B,B1).
rewrite_rule([A,is,not,B],[A1 \= B1]) :- rewrite_rule(C,A,A1),rewrite_rule(C,B,B1).
rewrite_rule([A,is,B],[A1 = B1]) :- rewrite_rule(C,A,A1),rewrite_rule(C,B,B1).
rewrite_rule([A,equals,B],[A1 = B1]) :- rewrite_rule(C,A,A1),rewrite_rule(C,B,B1).
rewrite_rule([A,does,not,equal,B],[A1\=B1]) :- rewrite_rule(C,A,A1),rewrite_rule(C,B,B1).
rewrite_rule([A,is,not,equal,to,B],[A1\=B1]) :- rewrite_rule(C,A,A1),rewrite_rule(C,B,B1).
rewrite_rule([A,is,equal,to,B],[A1=B1]) :- rewrite_rule(C,A,A1),rewrite_rule(C,B,B1).
rewrite_rule([A,while,B],[while(B1,A1)]) :- imperative(A,A1),bool(B,B1).
rewrite_rule([substring,of,A,from,B,to,C],[substring(A1,B1,C1)]) :- is_string(A,A1),number(B,B1),number(C,C1).
rewrite_rule([sum,of,A,and,B],[A1+B1]) :- number(A,A1),number(B,B1).
rewrite_rule([A,until,B],[while(B1=false,A1)]) :- bool(A,A1),bool(B,B1).
rewrite_rule([do,A,until,B],[A1,until,B1]) :- bool(A,A1),bool(B,B1).
rewrite_rule([until,A,do,B],[do,A,until,B1]) :- bool(A,A1),bool(B,B1).
rewrite_rule([A,means,B],[means(A1,B1)]) :- is_string(A,A1),is_string(B,B1).
rewrite_rule([A,is,a,permutation,of,B],[permutation(A,B)]) :- is_string(A,A1),is_string(B,B1).
rewrite_rule([print,A],[print(A1)]) :- rewrite_rule_(A,A1).
rewrite_rule_(A,A1) :- rewrite_rule(_,A,A1).
rewrite_rule([save,the,string,A,to,the,file,B],[save_file(A,B)]) :- is_string(A,A1),is_string(B,B1).
rewrite_rule([append,the,string,A,to,the,file,B],[append_to_file(A,B)]) :- is_string(A,A1),is_string(B,B1).
rewrite_rule([append,the,list,A,to,B],[append(A,B)]) :- is_string(A,A1),is_string(B,B1).
rewrite_rule([run,shell,command,A],[shell(A1)]) :- is_string(A,A1).

has_type(A,imperative) :- has_type(A,bool).
has_type(A,number) :- var(A);ground(A),is_number(A).
has_type(A,bool) :- var(A);ground(A),bool(A).
has_type(A,string) :- var(A);ground(A),is_string(A).

bool(forall(_,_)).
bool(print(A)).
bool(while(A,B)).
bool(means(A,B)) :- atom_chars(A,A1),atom_chars(B,B1),rewrite_rule1(A1,B1).
bool(A->B) :- bool(A),bool(B).
bool((A;B)) :- bool(A),bool(B).
bool((A,B)) :- bool(A),bool(B).
bool(A=B) :- has_type(A,C),has_type(B,C).
bool(A \= B) :- has_type(A,C),has_type(B,C).
bool([A,iff,B]) :- bool(A),bool(B).
bool(A>B) :- is_number(A),is_number(B).
bool(A<B) :- is_number(A),is_number(B).
bool(A>=B) :- is_number(A),is_number(B).
bool(A=<B) :- is_number(A),is_number(B).
bool(integer(A)).
bool(number(A)).
bool(member(_,_)).
bool(true).
bool(false).
bool(permutation(A,B)).
bool(A) :- starts_with_upper(A),dif(A,'('),dif(A,')'),dif(A,'not'),dif(A,'than'),dif(A,'more').
bool(save_file(A,B)) :-
	open(B,write,Stream),
    write(Stream,A),
    nl(Stream), 
    close(Stream).
bool(append_to_file(A,B)) :- open(B, append, Handle), write(Handle, A), close(Handle).
bool(shell(A)) :- writeln(shell(A)),shell(A).
is_string(A) :- var(A);starts_with_upper(A);atom(A),(\+(starts_with_lower(A))),literal_to_atom(A,_).
is_string(substring(_,_,_)).
is_string(is_substring(_,_)).
is_string(A) :- (\+starts_with_lower(A)),A \= [comma(_),_].
is_string(A) :- is_list(A).
is_string(det(A)) :- is_string(A).
is_string(list_comprehension(A,B,C)).
is_number(A) :- starts_with_upper(A).
is_number(A) :- number(A).
is_number(A+B) :- is_number(A),is_number(B).
is_number(sqrt(_)).
is_number(height(_)).
is_number(factorial(_)).
is_number(mod(_,_)).
is_number(_*_).
is_number(_/_).
is_number(_-_).
is_number(_^_).
is_number(derivative(_,_)).
is_number(integral(_,_)).
is_number(ln(_)).
is_number(sin(_)).
is_number(cos(_)).
is_number(tan(_)).
is_number(asin(_)).
is_number(acos(_)).
is_number(atan(_)).
is_number(distance(_,_)).
is_number(gcd(_,_)).
is_number(abs(_)).
is_number(floor(_)).
is_number(ceiling(_)).
is_number(cross_product(_,_)).
is_number(dot_product(_,_)).
is_number(subset(_,_)).
is_number(union(_,_)).
is_number(intersection(_,_)).
is_number(solve(_,_)).
is_number(maximize(_,_)).

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
