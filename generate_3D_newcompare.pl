% Recursive, non-Redelmeier polycube generation with 3D uniqueness
:- module(generate_3D_newcompare,[get_all_treelike/2]).
:- use_module(library(clpfd)).
:- use_module(vec_oper).
:- use_module(bound_compare).

% Copyright 2019 Lawrence Livermore National Security, LLC.
% See the top-level LICENSE file for details.
% 
% SPDX-License-Identifier: MIT

get_all_treelike(1,Cube) :-
	get_basecube([0,0,0],Cube),!.
get_all_treelike(N,PCs) :-
	Prev is N-1,
	get_all_treelike(Prev,Old),
	maplist(add_unique(),Old,New),
	filter_unique(New,[PCs,_,_]).

get_basecube(C0,[[[C0],[]]]).

add_unique([Coords,ID],New) :-
	get_adj(Coords,Adj),
	get_nbrs(Coords,Adj,Nbrs),
	get_tree_nbrs(Nbrs,Tree),
	get_unique([Coords,ID],Tree,New).

get_adj(Coords,Adj) :-
	UV = [[1,0,0],[0,1,0],[0,0,1],[-1,0,0],[0,-1,0],[0,0,-1]],
	add_UV(Coords,UV,Adj).
add_UV([H|T],UV,[A,B,C,D,E,F|Ts]) :-
	vec_mat_sum(H,UV,[A,B,C,D,E,F]),
	add_UV(T,UV,Ts).
add_UV([],_,[]).

get_nbrs(Coords,[Ha|Ta],Nbrs) :-
	member(Ha,Coords),!,
	get_nbrs(Coords,Ta,Nbrs).
get_nbrs(Coords,[Ha|Ta],[Ha|Nbrs]) :-
	get_nbrs(Coords,Ta,Nbrs).
get_nbrs(_,[],[]).

get_tree_nbrs([Hn|Tn],Tree) :-
	member(Hn,Tn),!, subtract(Tn,[Hn],Sub),
	get_tree_nbrs(Sub,Tree).
get_tree_nbrs([Hn|Tn],[Hn|Tree]) :-
	get_tree_nbrs(Tn,Tree).
get_tree_nbrs([],[]).

get_unique([Coords,ID],[Ht|Tt],New) :-
	get_unique([Coords,ID],Tt,Acc),
	add_cube(Ht,Coords,Cnew,ID,IDnew),
	add_if_unique(Cnew,IDnew,Acc,New).
get_unique(_,[],[[],[],[]]).
% Define add_cube/5 for get_unique/3
add_cube(C0,Coords,[C0|Coords],ID,IDnew) :-
	adjacent(C0,Coords,Adj),
	add_id(C0,Adj,ID,IDnew).
% Define adjacent/3 for add_cube/5
adjacent(C0,Coords,Adj) :-
	member(Adj,Coords), vec_vec_sum(C0,D,Adj),
	UV = [[1,0,0],[0,1,0],[0,0,1],[-1,0,0],[0,-1,0],[0,0,-1]],
	member(D,UV).
adjacent(_,[],[]).
% Define add_id/4 for add_cube/5
add_id(_,[],ID,ID).
add_id(C0,Adj,ID,[i(C0,Adj)|ID]).
% Define add_if_unique/4 for get_unique/3
add_if_unique(Cds,_,PCin,PCin) :-
        compare_each(Cds,PCin),!.
add_if_unique(C,ID,[PCin,Inds,Tbds],[[[C,ID]|PCin],[AllInds|Inds],[Bounds|Tbds]]) :-
	all_inds(C,AllInds,Bounds).
% Define compare_each/2 for add_if_unique/4
compare_each(Cds,[[[Hcds,_]|_],[Hinds|_],[Hbds|_]]) :-
        compare_cubes(Cds,Hcds,Hinds,Hbds).
compare_each(Cds,[[_|Tcds],[_|Tinds],[_|Tbds]]) :-
	compare_each(Cds,[Tcds,Tinds,Tbds]).


filter_unique([H|T],PCout) :-
	filter_unique(T,PCin),
	check_each(H,PCin,PCout).
filter_unique([Last],Last).

check_each([[[C,ID]|Tcids],[Hinds|Tinds],[Hbds|Tbds]],In,Out) :-
	check_inds(C,ID,Hinds,Hbds,In,Acc),
	check_each([Tcids,Tinds,Tbds],Acc,Out).
check_each([[],[],[]],Out,Out).
% Analog to add_if_unique/4 for filter_unique
check_inds(_,_,[H|_],Bds,PCin,PCin) :-
	compare_inds(H,Bds,PCin),!.
check_inds(C,ID,Inds,Bds,[PCin,AllInds,AllBds],[[[C,ID]|PCin],[Inds|AllInds],[Bds|AllBds]]).
% Analog to compare_each/2 for filter_unique
compare_inds(Inds,Bds,[_,[Hinds|_],[Bds|_]]) :-
	member(Inds,Hinds).
compare_inds(Inds,[[_|Tpc],[_|Tinds],[_|Tbds]]) :-
	compare_inds(Inds,[Tpc,Tinds,Tbds]).
