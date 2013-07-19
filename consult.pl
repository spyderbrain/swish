:- module(consult, []).

% http library modules 
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_session)).
:- use_module(library(memfile)).
:- use_module(library(debug)).

:- use_module(sandbox).

:- style_check(-atom).


:- http_handler(root(prolog/consult), consult, []).


consult(Request) :-
    catch(consult_1(Request), Error, true),
    (   var(Error)
    ->  true
    ;   message_to_string(Error, Msg),
        reply_json(json([success= @false, message=Msg]), [width(0)])
    ).


consult_1(Request) :-
    setup_call_cleanup(new_memory_file(Handle),
        consult_code(Request, Handle),
        free_memory_file(Handle)
    ).


%%    consult_code(+Request, +MemFile)

consult_code(Request, Handle) :-
    setup_call_cleanup(open_memory_file(Handle, write, Output),
        http_read_data(Request, _, [to(stream(Output))]),
        close(Output)),
    setup_call_cleanup(open_memory_file(Handle, read, Stream),
        consult_stream(Stream),
        close(Stream)),
    reply_json(json([success= @true, message=updated]), [width(0)]).


consult_stream(Stream) :-
    http_session_id(SessionId),
    forall(current_predicate(SessionId:PI),
        (   memberchk(PI, [read/1, write/1, writeln/1, nl/0])
        ->  true
        ;   abolish(SessionId:PI)
        )
    ),
    repeat,
        read_term(Stream, Term, [module(SessionId)]),
        (   Term == end_of_file
        ->  !
        ;   expand_term(Term, ExpandedTerm),
            consult_term(ExpandedTerm, SessionId),
            fail
        ).


consult_term(Var, _) :-
    var(Var), !,
    instantiation_error(Var).
consult_term([], _) :- !.
consult_term([H|T], Module) :- !,
    consult_term(H, Module),
    consult_term(T, Module).
consult_term((:- Directive), Module) :- !,
    expand_goal(Directive, Goal),
    (   safe_goal(Module:Goal)
    ->  Module:Goal
    ;   permission_error(execute, goal, Goal)
    ).
consult_term(Clause, Module) :-
    not_qualified(Clause),
    assert(Module:Clause).

not_qualified(Clause) :-
    Clause = _:_, !,
    permission_error(assert, clause, Clause).
not_qualified(_).

