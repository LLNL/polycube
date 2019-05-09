% Define module breadth_unfold that specializes in finding the first UF
:- module(breadth_unfold,[edges/3]).
:- use_module(library(clpfd)).
:- use_module(vec_oper).
:- use_module(unfold_module).

% Copyright 2019 Lawrence Livermore National Security, LLC.
% See the top-level LICENSE file for details.
% 
% SPDX-License-Identifier: MIT

% Define edges/3
edges(Edges,Vx,UF) :- 
	first_nbrs(Edges,Vx,V0,N0),
	get_cut_edges(Edges,Vx,[],[V0],N0,[],UF).

first_nbrs(Edges,Vx,V0,N0) :-
	member(v(_,V0,Fcs),Vx),
	same_length(Fcs,N0),
	get_all(Edges,V0,N0).
get_all([e(V0,V,_,_,I)|T],V0,N0) :-
	member([V,I],N0),
	get_all(T,V0,N0).
get_all([e(V,V0,_,_,I)|T],V0,N0) :-
	member([V,I],N0),
	get_all(T,V0,N0).
get_all([_|T],V0,N0) :-
	get_all(T,V0,N0).
get_all([],_,_).

get_cut_edges(Edges,_,Cuts,_,[],_,UF) :-
	unfold(Edges,Cuts,UF).
get_cut_edges(Edges,Vx,Ilist,Tree,Nbrs,Vprev,UF) :-
	non_conflicting(Nbrs,Iset,Vset),
	append(Iset,Ilist,Inew),
	locally_planar(Vprev,Edges,Vx,Inew),
	append(Vset,Tree,Tnew),
	new_nbrs(Edges,Vset,Tnew,Nnew),
	get_cut_edges(Edges,Vx,Inew,Tnew,Nnew,Vset,UF).

non_conflicting(Nbrs,Iset,Vset) :-
	all_vx(Nbrs,Vlist),
	list_to_set(Vlist,Vset),
	pick_ind(Vset,Nbrs,Iset).

all_vx([[V,_]|Tn],[V|Tv]) :-
	all_vx(Tn,Tv).
all_vx([],[]).

pick_ind([H|Tv],Nbrs,[I|Ti]) :-
	member([H,I],Nbrs),
	pick_ind(Tv,Nbrs,Ti).
pick_ind([],_,[]).

locally_planar([H|T],Edges,Vx,Icut) :-
	\+ nonplanar(Edges,Vx,H,Icut),
	locally_planar(T,Edges,Vx,Icut).
locally_planar([],_,_,_).

% Define nonplanar/4 for locally_planar
nonplanar(Edges,Vx,V,Icut) :-
	member(v(_,V,Fcs),Vx),
	length(Fcs,D), D #> 4,
	% Make a list of edges from a cut one all the way around, then split it by all cut edges
	member(I1,Icut), member(e(V1,V2,_,_,I1),Edges), member(V,[V1,V2]),
	wheel(V,I1,Edges,Wheel),
	split_wheel(Wheel,Icut,Split),!, % Green cut, doesn't allow backtracking
	too_large(Split).

% Define wheel/4 for nonplanar
wheel(V,I1,Edges,Wheel) :-
	findall(X,member(e(V,_,_,_,X),Edges),First),
	findall(X,member(e(_,V,_,_,X),Edges),Last),
	append(First,Last,Ilist), subtract(Ilist,[I1],Inds),
	sort_wheel(Inds,Wheel,I1,Edges).
% Define sort_wheel/4 for wheel in nonplanar
sort_wheel([],[Ilast],Ilast,_).
sort_wheel(Ilist,[I1|Twheel],I1,Edges) :-
	member(I2,Ilist),
	adj_edges(I1,I2,Edges),
	subtract(Ilist,[I2],Ipass),
	sort_wheel(Ipass,Twheel,I2,Edges).
% Define adj_edges/3 for sort_wheel
adj_edges(A,B,Edges) :-
	member(e(_,_,Fa,Fb,A),Edges),
	member(e(_,_,Fc,Fd,B),Edges),
	member(F1,[Fa,Fb]), member(F1,[Fc,Fd]).

% Define split_wheel/3 for nonplanar
split_wheel([Last],_,[[Last]]).
split_wheel([H|T],Icut,[[H],[Hl|Tl]|To]) :-
	split_wheel(T,Icut,[[Hl|Tl]|To]),
	member(Hl,Icut),!. % Red cut, ensures proper behavior when backtracking
split_wheel([H|T],Icut,[[H|Term]|To]) :-
	split_wheel(T,Icut,[Term|To]).

% Define too_large/1 for nonplanar
too_large([H|_]) :-
	length(H,N), N #> 4.
too_large([_|T]) :-
	too_large(T).

new_nbrs(Edges,_,Visited,[]) :-
	length(Edges,Ne),
	Ne #= 8*N+4,
	Nv #= 4*N+4,
	is_set(Visited), length(Visited,Nv).
new_nbrs(Edges,Vbord,Visited,Nbrs) :-
	concurrent_maplist(get_nbrs(Edges,Visited),Vbord,Nnest),
	semi_flat(Nnest,Nbrs).

get_nbrs(Edges,Visited,V,Nbrs) :-
	findall([X,I],member(e(V,X,_,_,I),Edges),L1),
	findall([X,I],member(e(X,V,_,_,I),Edges),L2),
	append(L1,L2,Ninit),
	prune_nbrs(Ninit,Visited,Nbrs).

prune_nbrs([[H,_]|T],Vlist,Passed) :-
	member(H,Vlist),!,
	prune_nbrs(T,Vlist,Passed).
prune_nbrs([H|T],Vlist,[H|Tp]) :-
	prune_nbrs(T,Vlist,Tp).
prune_nbrs([],_,[]).

semi_flat([Hi|Ti],Out) :-
	semi_flat(Ti,Int),
	append(Hi,Int,Out).
semi_flat([],[]).

