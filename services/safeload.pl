% sandbox execution of the `main` predicate of a program.

:- use_module(library(sandbox)).

main :-
	current_prolog_flag(argv,[Prog]),
	safe_exec(Prog), !.
main :-
	write('Illegal arguments!'), nl, halt(1).

% consult and execute a Prolog program:
safe_exec(Prog) :-
	catch(consultFile(Prog),CError,true),
	(var(CError) -> true
         ; message_to_string(CError,CMsg),
	   write(CMsg), nl, halt(1)),
	catch(execMain,Error,true),
	(var(Error)
         -> true
          ; message_to_string(Error,Msg),
	    write(Msg), nl, halt(1)).

% consult program but don't allow directives:
consultFile(FileName) :-
	open(FileName,read,Stream),
	repeat,
	read_term(Stream,Term,[]),
	consultTerm(Term),
	Term == end_of_file,
	!,
	close(Stream).

consultTerm(Term) :-
        expand_term(Term,ExpTerm),
	assertClause(ExpTerm).

assertClause(Var) :- var(Var), !, instantiation_error(Var).
assertClause((:- Directive)) :- !, permission_error(execute,directive,Directive).
assertClause(Clause) :-
	%write(Clause), nl,
	assertz(Clause),
	!.

% execute unary main predicate if it is safe:
execMain :-
	safe_goal(user:main(X)),
	write('Solving main(S)...'),
	main(X),
	!,
	write('computed solution:'), nl,
	write('S = '), write(X), nl.
