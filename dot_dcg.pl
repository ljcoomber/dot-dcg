:- module(dot_dcg, [graph/3]).

% Subset of the dot language grammar. See www.graphviz.org/doc/info/lang.html
% Comments prefixed "DOT Spec" are taken verbatim from the specification.

% TODO: Allow logical lines to be separated by backslash and  newline
% TODO: Allow double-quoted strings to be concatenated using a '+' operator
% TODO: Support comments
% TODO: Semi-colons are generally optional, but need to handle exlusion (see spec)
% TODO: Keywords should be case-insensitive
% TODO: Unify representation of quote and unquoted IDs
% TODO: Enforcement of quoted keyword IDs

% DOT Spec: graph : [ strict ] (graph | digraph) [ ID ] '{' stmt_list '}'
% TODO: Support strict
% TODO: Support un-directed graph
graph(digraph(Name, StmtList)) --> w_spc_opt, "digraph", w_spc, id(Name), w_spc,
    "{", w_spc, stmt_list(StmtList), w_spc, "}", w_spc_opt.

% DOT Spec: stmt_list :	[ stmt [ ';' ] [ stmt_list ] ]
stmt_list([Stmt|Rest]) --> stmt(Stmt), w_spc_opt, stmt_list(Rest), !.
stmt_list([Stmt]) --> stmt(Stmt).

% DOT Spec: stmt : node_stmt | edge_stmt | attr_stmt | ID '=' ID | subgraph
% TODO: subgraph not implemented
% TODO: attr_stmt
% TODO: ID =' ID
stmt(EdgeStmt) --> edge_stmt(EdgeStmt), w_spc_opt, ";", !.
stmt(EdgeStmt) --> edge_stmt(EdgeStmt), !.
stmt(NodeStmt) --> node_stmt(NodeStmt), w_spc_opt, ";", !.
stmt(NodeStmt) --> node_stmt(NodeStmt).

% DOT Spec: attr_stmt :	(graph | node | edge) attr_list
% TODO

% DOT Spec: attr_list : '[' [ a_list ] ']' [ attr_list ]
attr_list(Merged) --> "[", w_spc_opt, a_list(AList), w_spc_opt, "]",
    w_spc_opt, attr_list(Rest), { merge(AList, Rest, Merged) }, !.
attr_list(AList) --> "[", w_spc_opt, a_list(AList), w_spc_opt, "]".

% DOT Spec: a_list : ID [ '=' ID ] [ ',' ] [ a_list ]
a_list([Attr|Rest]) --> attr(Attr), w_spc_opt, ",", w_spc_opt, a_list(Rest), !.
a_list([Attr]) --> attr(Attr).
attr(attr(Name, Value)) --> id(Name), w_spc_opt, "=", w_spc_opt, id(Value), !.
attr(attr(Name)) --> id(Name).            

% DOT Spec: edge_stmt : (node_id | subgraph) edgeRHS [ attr_list ]
% TODO: Subgraph
edge_stmt(edge(Nodes, AttrList)) --> edge(Nodes), w_spc_opt, attr_list(AttrList), !.
edge_stmt(edge(Nodes)) --> edge(Nodes).
edge([First|Rest]) --> node_id(First), w_spc_opt, edge_rhs(Rest).

% DOT Spec: edgeRHS : edgeop (node_id | subgraph) [ edgeRHS ]
% TODO: Subgraph
% TODO: Edge type
edge_rhs([Node|Rest]) --> edge_op, w_spc_opt, node_id(Node),
    w_spc_opt, edge_rhs(Rest), !.
edge_rhs([Node]) --> edge_op, w_spc_opt, node_id(Node).

% DOT Spec: node_stmt : node_id [ attr_list ]
node_stmt(node_stmt(NodeId, AttrList)) --> node_id(NodeId), w_spc, attr_list(AttrList).
node_stmt(node_stmt(NodeId)) --> node_id(NodeId).


% DOT Spec: node_id : ID [ port ]
% TODO: Port
node_id(NodeId) --> id(NodeId).

% DOT Spec: port: ':' ID [ ':' compass_pt ] | ':' compass_pt
% DOT Spec: subgraph : [ subgraph [ ID ] ] '{' stmt_list '}'
% DOT Spec: compass_pt : (n | ne | e | se | s | sw | w | nw | c | _)
% TODO

% DOT Spec: An ID is one of the following:
% DOT Spec: Any string of alphabetic ([a-zA-Z\200-\377]) characters, underscores
% ('_') or digits ([0-9]), not beginning with a digit;
id(AId) --> symbol_list(Id), { atom_codes(AId, Id) }, !.

% DOT Spec: a numeral [-]?(.[0-9]+ | [0-9]+(.[0-9]*)? );
id(Id) --> numeral(Id), !.

% DOT Spec: any double-quoted string ("...") possibly containing escaped quotes (\");
id(Id) --> quoted_string(Id), !.

% DOT Spec: an HTML string (<...>).
% TODO

% Check for list of 'csym' chars, which is a close approximation to DOT standard
% TODO: Tighten up
symbol_list([S|Rest]) --> symbol(S), symbol_list(Rest).
symbol_list([S]) --> symbol(S).
symbol(S) --> [S], { char_type(S, csym) }.

% This uses ISO Prolog standard for conversion, which is assumed to be adequate
% TODO: Tighten up
numeral(Id, Codes, _Rest) :-
    catch(number_codes(Id, Codes), _Ex, fail).

% Quoted string
quoted_string(AString) --> quoted_string_body(String, false, false),
    { atom_codes(AString, String) }, !.

quoted_string_body([34|String], false, false, [34|Codes], Rest):-
    % First character is a quote
    quoted_string_body(String, true, false, Codes, Rest).

quoted_string_body([92|String], true, false, [92|Codes], Rest):-
    % First character is a backslash (i.e., escape symbol)
    quoted_string_body(String, true, true, Codes, Rest).

quoted_string_body([C|String], true, true, [C|Codes], Rest):-
    % Escaped is true, so just (blindly, for now) append whatever was escaped
    quoted_string_body(String, true, false, Codes, Rest).

quoted_string_body([C|String], true, false, [C|Codes], Rest):-
    % Character not a quote
    C \= 34,
    quoted_string_body(String, true, false, Codes, Rest).

quoted_string_body([34], true, false, [34|Codes], Rest) :-
    % Closing quote - unify Rest with remainder of input
    Rest = Codes, !.

% Misc
% TODO: Un-directed graph (--)
edge_op --> "-", ">".

% Mandatory white space
w_spc --> w_spc_char, w_spc, !.
w_spc --> w_spc_char, !.
w_spc_char --> [10]; [11]; [12]; [13]; [32].

% Optional white space
w_spc_opt --> w_spc, !.
w_spc_opt --> [].

% Utility predicate for merging one list into another
merge([], Ys, Ys).
merge([X|Xs], Ys, [X|Zs]) :- merge(Xs, Ys,  Zs).
