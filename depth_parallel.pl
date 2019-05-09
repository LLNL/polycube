% Define module depth_parallel
:- module(depth_parallel,[edges/3]).
:- use_module(vec_oper).
:- use_module(library(clpfd)).
:- use_module(unfold_module).

% Copyright 2019 Lawrence Livermore National Security, LLC.
% See the top-level LICENSE file for details.
% 
% SPDX-License-Identifier: MIT

% Define edges/3
edges(Edges,Vx,UF) :- 
	length(Vx,Nvx), Nmax #= Nvx-1,
	get_goals(Edges,Vx,Nmax,UF,Goals),
	first_solution(UF,Goals,[]).

% Define get_goals/5 that fills the get_cut_edges calls for first_solution
get_goals(Edges,Vx,Nmax,UF,Goals) :-
	get_D3(Vx,D3s),
	maplist(first_neighbors(Edges),D3s,Inits),
	maplist(build_goals(Edges,Vx,Nmax,UF),D3s,Inits,Goals).
% Define get_D3 that gets the list of indices of all degree 3 vertices
get_D3([v(_,I,Fcs)|Tvx],[I|Td3]) :- 
	length(Fcs,3),
	get_D3(Tvx,Td3).
get_D3([_|Tvx],Td3) :-
	get_D3(Tvx,Td3).
get_D3([],[]).
% Define build_goals that actually puts the goals together with the vertices and first neighbors
build_goals(Edges,Vx,Nmax,UF,I1,Init,get_cut_edges(Edges,Vx,Nmax,[],[I1],Init,0,_,3,UF)).

% Define first_neighbors/3 that finds the neighbors of the first vertex of the spanning tree
first_neighbors(Edges,V,Lall) :-
	findall(I,member(e(V,_,_,_,I),Edges),L1),
	findall(I,member(e(_,V,_,_,I),Edges),L2),
	append(L1,L2,Lall).

% Define get_cut_edges/9 for edges/7 that searches for a good spanning tree
get_cut_edges(Edges,Vx,Nmax,Ilist,Tree,U,N,CutEdges,Dlast,UF) :-
        Nnew is N+1, Nnew < Nmax,
	Dlast #< 5,
        member(I,U), subtract(U,[I],Uint),
        Inew = [I|Ilist], member(e(Va,Vb,_,_,I),Edges),
        member(V,[Va,Vb]), \+ member(V,Tree),
        Tnew = [V|Tree],
	member(v(_,V,Fcs),Vx), length(Fcs,Dnew),
        tree_neighbors(V,Tree,Edges,Uint,Unew),
        get_cut_edges(Edges,Vx,Nmax,Inew,Tnew,Unew,Nnew,CutEdges,Dnew,UF).
get_cut_edges(Edges,Vx,Nmax,Ilist,Tree,U,N,CutEdges,_,UF) :-
        Nnew is N+1, Nnew < Nmax,
	Tree = [Vlast|_], member(v(_,Vlast,Fcsl),Vx),
        member(I,U), \+ nonplanar(Vlast,Fcsl,[I|Ilist],Edges),
	subtract(U,[I],Uint),
        Inew = [I|Ilist], member(e(Va,Vb,_,_,I),Edges),
        member(V,[Va,Vb]), \+ member(V,Tree),
        Tnew = [V|Tree],
	member(v(_,V,Fcs),Vx), length(Fcs,Dnew),
        tree_neighbors(V,Tree,Edges,Uint,Unew),
        get_cut_edges(Edges,Vx,Nmax,Inew,Tnew,Unew,Nnew,CutEdges,Dnew,UF).
get_cut_edges(Edges,Vx,Nmax,Ilist,Tree,U,N,CutEdges,Dlast,UF) :-
        Nnew is N+1, Nnew == Nmax,
	Dlast #< 5,
        member(I,U),
        Inew = [I|Ilist], member(e(Va,Vb,_,_,I),Edges),
        member(V,[Va,Vb]), \+ member(V,Tree),
	member(v(_,V,Fcs),Vx), length(Fcs,Dnew), Dnew #< 5,
        CutEdges = Inew,
	unfold(Edges,CutEdges,UF).

% Define tree_neighbors/5 for get_all_edges that generates and tests new neighbors of an added vertex
tree_neighbors(V,Tree,Edges,Uint,Unew) :-
	all_neighbors(V,Edges,Nbrs),
	prune_untried(V,Edges,Uint,Umod),
	prune_neighbors(Nbrs,Tree,Umod,Unew).

% Define all_neighbors/3 that finds all edge vertices and indices related to the vertex
all_neighbors(V,Edges,Nbrs) :- findall([X,I],member(e(V,X,_,_,I),Edges),L1),
	findall([X,I],member(e(X,V,_,_,I),Edges),L2),
	append(L1,L2,Nbrs).

% Define prune_untried/4 that goes through the current untried list and removes anything pointed to the new vertex
prune_untried(V,Edges,[Hu|Tu],Unew) :- member(e(V1,V2,_,_,Hu),Edges),
	member(V,[V1,V2]),!,
	prune_untried(V,Edges,Tu,Unew).
prune_untried(V,Edges,[Hu|Tu],[Hu|Unew]) :- 
	prune_untried(V,Edges,Tu,Unew).
prune_untried(_,_,[],[]).

% Define prune_neighbors/4 that checks whether an edge is a new neighbor
prune_neighbors([[X,_]|T],Tree,Uin,Uout) :-
	member(X,Tree),!,
	prune_neighbors(T,Tree,Uin,Uout).
prune_neighbors([[_,I]|T],Tree,Uin,Uout) :-
	member(I,Uin),!,
	prune_neighbors(T,Tree,Uin,Uout).
prune_neighbors([[_,I]|T],Tree,Uin,Uout) :-
	prune_neighbors(T,Tree,[I|Uin],Uout).
prune_neighbors([],_,Uout,Uout).

% Define nonplanar/4 for get_cut_edges
nonplanar(_,Fcs,Icut,_) :-
	length(Fcs,D), D #> 4,
	length(Icut,1).
nonplanar(V,Fcs,Icut,Edges) :-
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


