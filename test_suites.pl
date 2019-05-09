% File of test predicates for unfolding code
:- module(test_suites,[test_getallpc/1,test_getalltree/4,test_unfoldall/2]).
:- use_module(library(clpfd)).
:- use_module(vec_oper).
:- use_module(build_cubes).
:- use_module(all_first_uf).
:- use_module(cubes_compare).
:- use_module(ntube_first_uf).
:- use_module(redelmeier_parallel).
:- use_module(unfold_parallel).
:- use_module(characterize_cubes).

% Copyright 2019 Lawrence Livermore National Security, LLC.
% See the top-level LICENSE file for details.
% 
% SPDX-License-Identifier: MIT

test_getallpc(NC) :-
	Tests = [[3,1],[3,2],[3,3],[4,1],[4,2],[4,3],[4,4],[5,1],[5,2],[5,3],[5,4],[5,5],[6,1],[6,2],[6,3],[6,4],[6,5],[6,6],[7,1],[7,2],[7,3],[7,4],[7,5],[7,6],[7,7],[8,1],[8,2],[8,3],[8,4],[8,5],[8,6],[8,7],[9,1],[9,5],[9,6],[9,7],[10,1],[10,5],[10,6],[10,7]],
	maplist(getallpc(),Tests,NC).
	
getallpc([N,Cut],NC) :-
	write([N,Cut]),
	time(get_all_pc(N,Cut,Cubes)),
	length(Cubes,NC).

test_getalltree(N,Split,Ntree,Nunique) :-
	write([N,Split]),
	set_cut(N,Cut),
	get_all_pc(N,Cut,[],[],[[0,0,0]],0,[],Cubes),
	time(unique3D(Cubes,[],Split,Unique)),
	length(Cubes,Ntree), length(Unique,Nunique).
set_cut(N,Cut) :-
	N #=< 7,
	Cut #= N-1.
set_cut(_,7).

test_unfoldall(N,Npc) :-
	unfold_all(N,Clists,_,_),
	length(Clists,Npc).
