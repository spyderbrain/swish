:- module(prolog_server,
      [ server/1,            % ?Port
        input/1,
        output/1
      ]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_server_files)).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(broadcast)).
:- use_module(library(time)).
:- use_module(library(sandbox)).

%:- debug(pengine).


/** <module> Prolog Web Server

This module is a demonstration of   handling  a conversation with Prolog
through a web-interface where state for the   conversation  is kept in a
non-deterministic Prolog predicate.

The design associates a thread that runs the conversation (and keeps the
state) with each HTTP session. This thread is created by first/1 and can
end due to next/1, stop/1  or  timeout   of  the  session. The latter is
signalled through the library(broadcast).  See   the  directive  calling
listen/2.

@author    Torbjörn Lager
@author    Jan Wielemaker

*/

:- http_handler(root(swish), http_reply_file('www/app.html', []), [prefix]).
:- http_handler(root(debug), http_reply_file('www/debug.html', []), [prefix]).

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(app, Dir)).

user:file_search_path(www, app(www)).
user:file_search_path(app_plugins, app(plugins)).
user:file_search_path(examples, app(examples)).


:- http_handler(root(.), serve_files_in_directory(www), [prefix]).

:- http_handler(root(examples), serve_files_in_directory(examples), [prefix]).

:- http_handler(/, http_redirect(moved, root(swish)), []).


%%    server(?Port) is det.
%
%    Start the web-server on Port.

server(Port) :-
    http_server(http_dispatch,
            [ port(Port),
              workers(16)
            ]),
    format('You can access the server at http://localhost:~w/~n', [Port]).


:- http_set_session_options([timeout(600)]).


%   When a client session begins, an output queue is created
%   from which the client will have to fetch all output
%   generated in the process of solving a goal. Furthermore,
%   a module, named by the session ID and thus "private" to the 
%   client, is created and prepared.

:- listen(http_session(begin(SessionId, _Peer)), begin_session(SessionId)).
    
begin_session(SessionId) :-
    debug(pengine, 'Begin session: ~q', [SessionId]),
    atom_concat(SessionId, '.out', Output),
    atom_concat(SessionId, '.comm', Communication),
    message_queue_create(Output),
    message_queue_create(Communication),
    prepare_module(SessionId).    


%   When a client session ends, the currently running goal (if any)
%   is aborted, the output queue is destroyed, and the client module 
%   is emptied.

:- listen(http_session(end(SessionId, _Peer)), end_session(SessionId)).

end_session(SessionId) :-
    debug(pengine, 'End session: ~q', [SessionId]),
    catch(thread_signal(SessionId, abort), _, true),
    atom_concat(SessionId, '.out', Output),
    atom_concat(SessionId, '.comm', Communication),
    catch(message_queue_destroy(Output), _, true),
    catch(message_queue_destroy(Communication), _, true),
    forall(current_predicate(SessionId:PI),
        (   memberchk(PI, [read/1, write/1, writeln/1, nl/0])
        ->  true
        ;   abolish(SessionId:PI)
        )
    ).


%%  input_queue(-QueueName) is det.
%
%   The input queue is given the same name as the current session ID.

input_queue(Input) :-
    http_session_id(Input).


%%  output_queue(-QueueName) is det.
%
%   The name of the output queue is derived from the session ID too.
%   (Note that the session ID is also the thread id for goals that 
%   are running.)

output_queue(Output) :-
    http_session_id(SessionId),
    atom_concat(SessionId, '.out', Output).
    

%%  communication_queue(-QueueName) is det.
%
%   The name of the communication queue is derived from the session ID too.
%   (Note that the session ID is also the thread id for goals that are running.)

communication_queue(Communication) :-
    http_session_id(SessionId),
    atom_concat(SessionId, '.comm', Communication).

    
%%  prepare_module(+Module) is det.
%
%   Create and prepare a module by (re)defining some
%   I/O predicates in it.
%
%   @tbd Investigate if there are other and better ways to
%   redefine the I/O predicates.

prepare_module(Module) :-
    Module:redefine_system_predicate(read(_)),
    Module:redefine_system_predicate(write(_)),
    Module:redefine_system_predicate(writeln(_)),
    Module:redefine_system_predicate(nl),
    assert(Module:(read(Term) :- input(Term))),
    assert(Module:(write(Term) :- term_to_atom(Term, Atom), output(Atom))),
    assert(Module:(writeln(Term) :- term_to_atom(Term, Atom0), atom_concat(Atom0, '<br />', Atom), output(Atom))),
    assert(Module:(nl :- output('<br />'))).
    


%%%%%%%%%%%     HTTP handlers    %%%%%%%%%%%

%    Declare HTTP locations we serve and how.

:- http_handler(root(prolog/first), first, []).
:- http_handler(root(prolog/next),  next,  []).
:- http_handler(root(prolog/input),  input_read,  []).
:- http_handler(root(prolog/stop),  stop,  []).
:- http_handler(root(prolog/abort), abort, []).
:- http_handler(root(prolog/result), result, []).


%%   first(+Request) is det.
%
%    HTTP handler that starts generating solutions for a query.

first(Request) :-
    http_parameters(Request,
            [ goal(GoalAtom, [])
            ]),
    http_session_id(SessionId),
    catch(text_to_goal(GoalAtom, Goal, Bindings), E, true),
    (   var(E)
    ->  output_queue(Output),
        empty_queue(Output),
        thread_create(solve(SessionId:Goal, Bindings), _, 
            [detached(true), alias(SessionId)]),
        output_result
    ;   message_to_string(E, Msg),
        reply_json(json([event=error, msg=Msg]))
    ).


:- multifile
	sandbox:safe_primitive/1,		% Goal
	sandbox:safe_meta/2.			% Goal, Calls

sandbox:safe_primitive(clpfd:nb_getval(_,_)).
sandbox:safe_primitive(M:write(_)) :- http_session_id(M).
sandbox:safe_primitive(M:writeln(_)) :- http_session_id(M).
sandbox:safe_primitive(M:read(_)) :- http_session_id(M).
sandbox:safe_primitive(M:nl) :- http_session_id(M).
sandbox:safe_primitive(system:sleep(_)). 
sandbox:safe_primitive(system:prompt(_,_)). 
sandbox:safe_primitive(dif:dif(_,_)).
sandbox:safe_primitive(clpfd:Pred) :- predicate_property(clpfd:Pred, exported).
sandbox:safe_primitive(atomic(_)).


%%  text_to_goal(+GoalAtom, -Goal, -Bindings) is det.
%
%   True if Goal is the term represention of GoalAtom and Bindings
%   the bindings as given by atom_to_term/3.
%   
%   Throws a syntax error if GoalAtom doesn't adhere to Prolog syntax,
%   or a permission error if safe_goal/1 says Goal is unsafe.
%
%   @tbd Find a way to distinguish permission errors from errors generated
%   for undefined predicates.

text_to_goal(GoalAtom, Goal, Bindings) :-
    http_session_id(SessionId),
    catch(
            (   %atom_to_term(GoalAtom, Goal, Bindings),
                read_term_from_atom(GoalAtom, Goal, [variable_names(Bindings)]),
                safe_goal(SessionId:Goal)
            ),
        Error,
        throw(Error)
    ).


%%  empty_queue(+Queue) is det.
%
%   Empty the queue. Just to be sure we are not hit by 
%   stuff left from the processing of a previous goal.

empty_queue(Queue) :-
    thread_peek_message(Queue, _), !,
    thread_get_message(Queue, _),
    empty_queue(Queue).
empty_queue(_Queue).


%%  solve(:Goal, +Bindings) is det.
%
%   Generate for Goal and send results to the output queue. 
%   Starts a timer that will abort the current goal on timeout.
%   We run solve/2 in setup_call_cleanup/3 so that the timer
%   and associated data is properly setup and cleaned up
%   afterwards even if the goal is aborted.


solve(Goal, Bindings) :-
    setup_call_cleanup(set_timeout, solve_1(Goal, Bindings), clear_timeout).


%%  set_timeout is det.
%

set_timeout :-
    % Hmm, why is this part needed? Shouldn't
    % setup_call_cleanup/3 take care of it?
    (   http_session_data(alarmId(AlarmId0))
    ->  catch(remove_alarm(AlarmId0), _, true),
        http_session_retractall(alarmId(_))
    ;   true
    ),
    alarm(15, (
            debug(pengine, 'ALARM!!!', []),
            input_queue(Input),
            output_queue(Output),
            thread_send_message(Output, result(error, 'Time limit exceeded', _, _, _)),
            sleep(0.3), % Hmm, can we avoid this?
            catch(thread_signal(Input, abort), _, true)
        ),
        AlarmId
    ),
    http_session_assert(alarmId(AlarmId)).


%%  clear_timeout is det.
%

clear_timeout :-
    http_session_data(alarmId(AlarmId)),
    catch(remove_alarm(AlarmId), _, true).


%%  solve_1(:Goal, +Bindings) is det.
%
%   Generate for Goal and send results to the output queue. 

solve_1(Goal, Bindings) :-
    thread_self(Me),
    thread_statistics(Me, cputime, T0a),
    nb_setval(time, T0a),
    solve_2(Goal, Bindings, Solution),
    nb_getval(time, T0),
    thread_statistics(Me, cputime, T1),
    Time is T1 - T0,
    result_time(Solution, Time),
    nb_setval(time, T1),
    debug(pengine, 'Sending: ~q', [Solution]),
    output_queue(Output),
    thread_send_message(Output, Solution),
    result_more_sol(Solution, More),
    (   More == false
    ->  true
    ;   More == true
    ->  thread_get_message(command(Command)),
        debug(pengine, 'Command: ~q', [Command]),
        Command == stop
    ;   true
    ).

result_more_sol( result(_, _, _, T, _), T).
result_time(     result(_, _, _, _, T), T).


%%   solve_2(:Goal, +Bindings, -Solution) is nondet.
%
%    Solve Goal. This predicate catches errors and detects whether
%    Goal succeeded deterministically.

solve_2(Goal, Bindings, Solution) :-
    call_cleanup(catch(Goal, Error, true), Det=true),
    (   var(Error)
    ->  (    Det == true
        ->   More = false
        ;    More = true
        ),
        Solution = result(true, Goal, Bindings, More, _)
    ;   message_to_string(Error, Msg),
        Solution = result(error, Msg, _, _, _)
    ).
solve_2(_Goal, _, result(false, _, _, _, _)).


%%    next(+Request) is det.
%
%    HTTP handler for the next answer.

next(_Request) :-
    input_queue(Input),
    catch(thread_send_message(Input, command(next)), _, true),
    output_result.


%%   input_read(+Request) is det.
%
%    HTTP handler for handling data client input (read/1).

input_read(Request) :-
    http_parameters(Request,
            [ input(TermAtom, [])
            ]),
    input_queue(Input),
    thread_send_message(Input, input(TermAtom)),
    output_result.


%%   stop(+Request) is det.
%
%    HTTP handler to stop the interaction.

stop(_Request) :-
    input_queue(Input),
    catch(thread_send_message(Input, command(stop)), _, true),
    reply_json(json([event=halted])).   


%%   abort(+Request) is det.
%
%    HTTP handler to abort the interaction.

abort(_Request) :-
    input_queue(Input),
    output_queue(Output),
    thread_send_message(Output, result(error, 'Execution Aborted', _, _, _)),
    sleep(0.3),    
    catch(thread_signal(Input, abort), _, true),
    reply_json(json([ok= @true])).


%%   input(-Term) is det.
%
%    Simulates read/1.

input(Term) :-
    input_queue(Input),
    output_queue(Output),
    prompt(CurrentPrompt, CurrentPrompt),
    thread_send_message(Output, result(prompt, CurrentPrompt, _, _, _)),
    thread_get_message(Input, input(TermAtom)),
    atom_to_term(TermAtom, Term, _).


%%   output(+Term) is det.
%
%    Simulates write/1.

output(Term) :-
    output_queue(Output),
    communication_queue(Communication),
    thread_send_message(Output, result(output, Term, _, _, _)),
    thread_get_message(Communication, ack).
    

%%   result(+Request) is det.
%
%    HTTP handler to collect (more) results.

result(_Request) :-
    output_result.
    

%%   output_result is det.
%
%    Wait for the goal thread to send a result and
%    call the output_result/5 predicate

output_result :-
    debug(pengine, 'Waiting for result ...', []),
    output_queue(Output),
    communication_queue(Communication),
    thread_get_message(Output, Msg),
    debug(pengine, 'Received: ~q', [Msg]),
    (   arg(1, Msg, output)
    ->  thread_send_message(Communication, ack)
    ;   true
    ),
    Msg = result(Success, Message, Bindings, More, Time),
    output_result(Success, Message, Bindings, More, Time).


%%   output_result(+Kind, +Message, +Bindings, +More, +Time) is det.
%
%    Convert result/5 terms into JSON.

output_result(true, Message0, Bindings0, More0, Time0) :-
    term_to_atom(Message0, Message),
    bindings_to_json(Bindings0, Bindings),
    reply_json(json([event=answer, success= @true, msg=Message, bindings=Bindings, more= @More0, time=Time0])).
output_result(false, Message0, _Bindings0, _More0, Time0) :-
    term_to_atom(Message0, Message),
    reply_json(json([event=answer, success= @false, msg=Message, time=Time0])).
output_result(error, Message0, _Bindings0, _More0, _Time0) :-
    reply_json(json([event=error, msg=Message0])).
output_result(halted, Message0, _Bindings0, _More0, _Time0) :-
    reply_json(json([event=halted, msg=Message0])).
output_result(prompt, Message0, _Bindings0, _More0, _Time0) :-
    reply_json(json([event=prompt, msg=Message0])).
output_result(output, Message0, _Bindings0, _More0, _Time0) :-
    reply_json(json([event=output, msg=Message0])).


bindings_to_json(BindingsIn, json(BindingsOut)) :-
    maplist(swap, BindingsIn, BindingsOut).

swap(N=V, N=A) :- term_to_atom(V, A).   




    

    
    