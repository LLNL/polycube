% Add functionality to input a series of coordinates and build a polycube from them (testing treelike)
:- use_module(build_cubes).
:- use_module(library(clpfd)).
:- use_module(vec_oper).

% Copyright 2019 Lawrence Livermore National Security, LLC.
% See the top-level LICENSE file for details.
% 
% SPDX-License-Identifier: MIT

% Test condition: the number of adjacencies is Ncoords-1
% Coords can be arranged such that each coord is adjacent to only one coord after itself in the list

input_coords(Clist,Pcube) :-
	reorder(Clist,Tree),
	adjacencies(Tree,ID),
	build(Tree,ID,[C,E,F]),
	assign_edges(Edges,Faces,Vx,C,E,F),
	Pcube = [Edges,Faces,Vx].

reorder(Clist,Tree) :-
	permutation(Clist,Tree),
	test_adj(Tree).

test_adj([_]).
test_adj([H|T]) :-
	test_adj(T),
	get_adj(H,T,Adj),
	subtract(T,[Adj],Sub),
	\+ get_adj(H,Sub,_).

get_adj(C,List,Adj) :-
	member(Adj,List),
	vec_vec_sum(C,Diff,Adj),
	UV = [[1,0,0],[-1,0,0],[0,1,0],[0,-1,0],[0,0,1],[0,0,-1]],
	member(Diff,UV),!.

adjacencies([_],[]).
adjacencies([Hc|Tc],ID) :-
	adjacencies(Tc,IDint),
	compare_each(Hc,Tc,IDint,ID).

compare_each(_,[],ID,ID).
compare_each(Co,[H|T],In,Out) :-
	compare_each(Co,T,In,Int),
	check_adj(Co,H,Int,Out).

check_adj(C1,C2,List,[i(C1,C2)|List]) :-
	vec_vec_sum(C1,Diff,C2),
	UV = [[1,0,0],[-1,0,0],[0,1,0],[0,-1,0],[0,0,1],[0,0,-1]],
	member(Diff,UV).
check_adj(_,_,List,List).

