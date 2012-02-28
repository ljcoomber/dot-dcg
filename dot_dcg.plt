:- use_module(dot_dcg).
:- use_module(library(readutil)).

:- begin_tests(dot_dcg).

% Whitespace
test(w_spc_single_chars) :-
    dot_dcg:w_spc([10], []),
    dot_dcg:w_spc([13], []),    
    dot_dcg:w_spc([32], []).

test(w_spc_multi_chars) :-
    dot_dcg:w_spc([10, 10], []),    
    dot_dcg:w_spc([10, 10, 10], []),
    dot_dcg:w_spc([10, 13, 32], []).

test(w_spc_single_fail, fail) :-
    dot_dcg:w_spc([65], []).

test(w_spc_multi_fail, fail) :-
    dot_dcg:w_spc([10, 65, 13], []).

test(w_spc_opt) :-
    dot_dcg:w_spc_opt([10], []),
    dot_dcg:w_spc_opt([10, 13], []),
    dot_dcg:w_spc_opt([65], [65]).

    
% IDs
test(id_symbol_list) :-
    test_codes_match('ABC', dot_dcg:id, 'ABC').

test(id_any_numeral) :-
    dot_dcg:id(8, [56], []).

test(id_symbol_list_fail, fail) :-
    test_codes_match(_, dot_dcg:id, 'AB C').

test(id_quoted_string) :-
    test_codes_match('"Quoted String"', dot_dcg:id, '"Quoted String"').

% More in-depth tests for quoted strings
test(quoted_string_plain) :-
    test_codes_match('"Quoted String"', dot_dcg:quoted_string, '"Quoted String"').

test(quoted_string_fail, fail) :-
    test_codes_match(_, dot_dcg:quoted_string, 'Not a "Quoted String"'),
    test_codes_match(_, dot_dcg:quoted_string, '"No closed quote').

test(quoted_string_with_remainder) :-
    atom_codes('"Quoted String"Remainder', InputCodes),    
    atom_codes('Remainder', RemainderCodes),
    dot_dcg:quoted_string('"Quoted String"', InputCodes, RemainderCodes).

% Attribute lists
test(attr) :-
    test_codes_match(attr(foo), dot_dcg:attr, 'foo'),
    test_codes_match(attr(foo, bar), dot_dcg:attr, 'foo=bar'),
    test_codes_match(attr(foo, bar), dot_dcg:attr, 'foo = bar').

test(a_list) :-
    test_codes_match([attr(foo,bar), attr(foo2, bar2)],
                     dot_dcg:a_list,
                     'foo=bar,foo2=bar2').

test(attr_list) :-
    test_codes_match([attr(foo,bar), attr(foo2, bar2)],
                     dot_dcg:attr_list,
                     '[foo=bar,foo2=bar2]'),
    
    test_codes_match([attr(foo,bar), attr(foo2, bar2),attr(foo3,bar3)],
                     dot_dcg:attr_list,
                     '[foo=bar,foo2=bar2][foo3=bar3]').

% Nodes
test(node_stmt_plain) :-
    test_codes_match(node_stmt(node0), dot_dcg:node_stmt, 'node0').

test(node_stmt_with_attributes) :-
    test_codes_match(node_stmt(node0,[attr(foo,bar),attr(foo2,bar2)]),
                     dot_dcg:node_stmt,
                     'node0 [foo=bar][foo2=bar2]').

% Edges
test(edge_stmt_plain) :-
    test_codes_match(edge(node0,node1), dot_dcg:edge_stmt, 'node0 -> node1').

test(edge_stmt_with_attributes) :-
    test_codes_match(edge(node0,node1,[attr(foo,bar)]), dot_dcg:edge_stmt,
                     'node0 -> node1 [foo=bar]').
    
    format("Result: ~w~n", [X]).
    

%test(graph, [ setup(read_file_to_codes('test/test.dot', Result, [])) ]) :-
%    format('~nResult: ~s~n', [Result]),
%    dot_dcg:graph(X, Result, Y).

:- end_tests(dot_dcg).

% TODO: Better name for Match and Goal and Input
test_codes_match(Expected, Goal, Input) :-
    atom_codes(Input, Codes),
    call(Goal, Expected, Codes, []).