% Parallel, list-based get_polycubes revision
:- module(redelmeier_parallel,[get_all_treelike/2,get_all_treelike/4,get_all_pc/3,unique_3D/4]).
:- use_module(library(clpfd)).
:- use_module(cubes_compare).
:- use_module(vec_oper).

% Copyright 2019 Lawrence Livermore National Security, LLC.
% See the top-level LICENSE file for details.
% 
% SPDX-License-Identifier: MIT

% Define get_all_treelike/2
get_all_treelike(N,Clists) :-
	set_cut(N,Cut),
	get_all_treelike(N,Cut,0,Clists).
set_cut(N,Cut) :-
        N #=< 7,
        Cut #= N-1.
set_cut(_,7).

% Define get_all_treelike/4
get_all_treelike(N,Nth,Split,Clists) :-
	Nth #=< N, % Threshold for parallelization less than maximum number of cubes
	Split #>= 0, % Threshold for comparison parallelization greater than or equal to zero
	get_all_pc(N,Nth,[],[],[[0,0,0]],0,[],AllCubes),
	unique_3D(AllCubes,[],Split,Clists).
% Define get_all_pc/8 for get_polycubes that implements Redelmeier's algorithm
get_all_pc(Nmax,Nth,Cubes) :- get_all_pc(Nmax,Nth,[],[],[[0,0,0]],0,[],Cubes).
get_all_pc(Nmax,Nth,Coords,ID,U,N,Acc,All) :-
	Nnew #= N+1, Nnew #< Nth,
	member(C1,U), subtract(U,[C1],Uint),
	add_cube(C1,Coords,Cnew,ID,IDnew),
	add_neighbors(C1,Cnew,Uint,Unew),
	concurrent(2,[get_all_pc(Nmax,Nth,Cnew,IDnew,Unew,Nnew,[],Acc1),
		get_all_pc(Nmax,Nth,Coords,ID,Uint,N,Acc,Acc2)],[]),
	append(Acc1,Acc2,All).
get_all_pc(Nmax,Nth,Coords,ID,U,N,Acc,All) :-
	Nnew #= N+1, Nnew #< Nmax,
	member(C1,U), subtract(U,[C1],Uint),
	add_cube(C1,Coords,Cnew,ID,IDnew),
	add_neighbors(C1,Cnew,Uint,Unew),
	get_all_pc(Nmax,Nth,Cnew,IDnew,Unew,Nnew,Acc,Acc1),
	get_all_pc(Nmax,Nth,Coords,ID,Uint,N,Acc1,All).
get_all_pc(Nmax,Nth,Coords,ID,U,N,Acc,All) :-
	Nmax #= N+1,
	member(C1,U), subtract(U,[C1],Uint),
	add_cube(C1,Coords,Cnew,ID,IDnew),
	get_all_pc(Nmax,Nth,Coords,ID,Uint,N,[[Cnew,IDnew]|Acc],All).
get_all_pc(_,_,_,_,[],_,All,All).
% Define add_cube/5 for get_all_pc
add_cube(C0,Coords,[C0|Coords],ID,IDnew) :-
	adjacent(C0,Coords,Adj),
	add_id(C0,Adj,ID,IDnew).
% Define adjacent/3 for add_cube and filter_neighbors
adjacent(C0,Coords,Adj) :-
	member(Adj,Coords), vec_vec_sum(C0,D,Adj),
	UV = [[1,0,0],[0,1,0],[0,0,1],[-1,0,0],[0,-1,0],[0,0,-1]],
	member(D,UV).
adjacent(_,[],[]).
% Define add_id/4 for add_cube
add_id(_,[],ID,ID).
add_id(C0,Adj,ID,[i(C0,Adj)|ID]).
% Define add_neighbors/4 for get_all_pc
add_neighbors(C1,Coords,U,Unew) :-
	get_neighbors(C1,Nlist),
	filter_neighbors(Nlist,Coords,U,Unew).
% Define get_neighbors/2 for add_neighbors
get_neighbors(C1,Nlist) :-
	UV = [[1,0,0],[0,1,0],[0,0,1],[-1,0,0],[0,-1,0],[0,0,-1]],
	vec_mat_sum(C1,UV,Nlist).
% Define filter_neighbors/4 for add_neighbors
filter_neighbors([N|Tn],C,U,Unew) :-
	member(N,C),
	filter_neighbors(Tn,C,U,Unew).
filter_neighbors([N|Tn],C,U,Unew) :-
	member(N,U), subtract(U,[N],Uint),
	filter_neighbors(Tn,C,Uint,Unew).
filter_neighbors([N|Tn],C,U,Unew) :-
	C = [_|Cprev],
	adjacent(N,Cprev,Adj), Adj \== [],
	filter_neighbors(Tn,C,U,Unew).
filter_neighbors([[X,Y,Z]|Tn],C,U,Unew) :-
	(X #< 0, Z #< 1; Y #< 0, Z #< 1; Z #< 0),
	filter_neighbors(Tn,C,U,Unew).
filter_neighbors([N|Tn],C,U,Unew) :-
	filter_neighbors(Tn,C,[N|U],Unew).
filter_neighbors([],_,U,U).

unique_3D([[C,ID]|T],Acc,N,Unique) :-
	add_if_unique(C,ID,N,Acc,Acc1),
	unique_3D(T,Acc1,N,Unique).
unique_3D([],Unique,_,Unique).

% Define add_if_unique/4 for get_all_pc
add_if_unique(_,ID,0,PCin,PCin) :-
	compare_each(ID,PCin),!.
add_if_unique(_,ID,N,PCin,PCin) :-
	N #\= 0,
	split_goals(N,PCin,PCsplit),
	maplist(build_goals(ID),PCsplit,Goals),
	first_solution(_,Goals,[on_fail(continue)]),!.
add_if_unique(C,ID,_,PCin,[[C,ID]|PCin]).

split_goals(N,PCin,[PCin]) :-
	length(PCin,Npc),
	Npc #=< N.
split_goals(N,In,[H|T]) :-
	length(H,N),
	append(H,Rem,In),
	split_goals(N,Rem,T).

build_goals(ID,List,compare_each(ID,List)).

% Define compare_each/2 for add_if_unique, uses compare_cubes from module cubes_compare
compare_each(ID,[[_,Hid]|Tids]) :-
	compare_cubes(ID,Hid,serial); compare_each(ID,Tids).

