% Separate unfold functionality from spanning tree generation
:- module(unfold_module,[unfold/3]).
:- use_module(library(clpfd)).
:- use_module(vec_oper).

% Copyright 2019 Lawrence Livermore National Security, LLC.
% See the top-level LICENSE file for details.
% 
% SPDX-License-Identifier: MIT

% Define unfold/3 for edges/7 
unfold(Edges,Cuts,UF) :- 
	length(Cuts,Ncut), Nbord is 2*Ncut, length(Border,Nbord), numlist(1,Nbord,IndB),
	split_edges(Border,Edges,Cuts,IndB),
	get_uncut(Edges,Cuts,Uncut),
	perimeter(Border,Uncut,UF),!,
	area_conserved(Border).

% Define split_edges/4 that gives each cut edge a unique face and assigns membership in Border
split_edges(Border,Edges,[Ih|T],[Bi1,Bi2|Tbind]) :-
	member(e(V1,V2,F1,F2,Ih),Edges),
	member(b(V1,V2,F1,Bi1,_,_),Border),
	member(b(V1,V2,F2,Bi2,_,_),Border),
	split_edges(Border,Edges,T,Tbind).
split_edges(_,_,[],[]).

% Define get_uncut/3 that separates out the uncut edges from the cut ones
get_uncut([e(_,_,_,_,I)|Te],Cuts,Uncut) :-
	member(I,Cuts),!,
	get_uncut(Te,Cuts,Uncut).
get_uncut([H|T],Cuts,[H|Uncut]) :- 
	get_uncut(T,Cuts,Uncut).
get_uncut([],_,[]).

% Define perimeter/3 that finds the actual perimeter
perimeter(Border,Uncut,P) :-
	member(b(_,V1,F0,1,[0,0],[0,1]),Border),
	get_perim(V1,[[0,0],[0,1]],1,F0,Border,Uncut,Path,[1]),
	P = [[0,0],[0,1]|Path].

% Define get_perim/8 that does calculations for perimeter/3
get_perim(_,_,_,_,Border,_,[],Ilist) :- same_length(Border,Ilist).
get_perim(V,[C1,C2],Ilast,F,Border,Uncut,[Cnew|P],Ilist) :- 
	get_adj0(V,F,Ilast,Border,Vnew,Inew),!,
	vec_vec_sum(C1,[Dxo,Dyo],C2), Dxn is -1*Dyo, Dyn is Dxo, C2 = [Xo,Yo],
	Xn is Xo+Dxn, Yn is Yo+Dyn, Cnew = [Xn,Yn], % Turn right
	member(b(_,_,_,Inew,C2,Cnew),Border),
	get_perim(Vnew,[C2,Cnew],Inew,F,Border,Uncut,P,[Inew|Ilist]).
get_perim(V,[C1,C2],_,F,Border,Uncut,[Cnew|P],Ilist) :- 
	get_adj1(V,F,Uncut,Border,Vnew,Inew,Fnew),!,
	vec_vec_sum(C1,Di,C2), vec_vec_sum(C2,Di,Cnew), % Stay straight
	member(b(_,_,_,Inew,C2,Cnew),Border),
	get_perim(Vnew,[C2,Cnew],Inew,Fnew,Border,Uncut,P,[Inew|Ilist]).
get_perim(V,[C1,C2],_,F,Border,Uncut,[Cnew|P],Ilist) :- 
	get_adj2(V,F,Uncut,Border,Vnew,Inew,Fnew),!,
	vec_vec_sum(C1,[Dxo,Dyo],C2), Dxn is Dyo, Dyn is -1*Dxo,
	vec_vec_sum(C2,[Dxn,Dyn],Cnew), % Turn left
	member(b(_,_,_,Inew,C2,Cnew),Border),
	get_perim(Vnew,[C2,Cnew],Inew,Fnew,Border,Uncut,P,[Inew|Ilist]).
get_perim(V,[C1,C2],_,F,Border,Uncut,[Cnew|P],Ilist) :- 
	get_adj3(V,F,Uncut,Border,Vnew,Inew,Fnew),!,
	vec_vec_sum(C1,[Dxo,Dyo],C2), Dxn is -1*Dxo, Dyn is -1*Dyo,
	vec_vec_sum(C2,[Dxn,Dyn],Cnew), % Turn around
	member(b(_,_,_,Inew,C2,Cnew),Border),
	get_perim(Vnew,[C2,Cnew],Inew,Fnew,Border,Uncut,P,[Inew|Ilist]).

% Define get_adj0/6 that finds a new perimeter edge on the same face and vertex 
get_adj0(V,F,Ilast,Border,Vnew,Inew) :-
	member(b(V1,V2,F,Inew,_,_),Border), permutation([V1,V2],[V,Vnew]), Inew \== Ilast.
% Define get_adj1/7 that finds a new perimeter edge on an adjacent face
get_adj1(V,F,Uncut,Border,Vnew,Inew,Fnew) :-
	member(e(V1,V2,F1,F2,_),Uncut), member(V,[V1,V2]), permutation([F1,F2],[F,Fnew]),
	member(b(Va,Vb,Fnew,Inew,_,_),Border), permutation([Va,Vb],[V,Vnew]).
% Define get_adj2/7 that finds a new perimeter edge on a face two steps removed
get_adj2(V,F,Uncut,Border,Vnew,Inew,Fnew) :-
	member(e(V1,V2,F1,F2,_),Uncut), member(V,[V1,V2]), permutation([F1,F2],[F,Fint]),
	member(e(V3,V4,F3,F4,_),Uncut), member(V,[V3,V4]), permutation([F3,F4],[Fint,Fnew]), Fnew \== F,
	member(b(Va,Vb,Fnew,Inew,_,_),Border), permutation([Va,Vb],[V,Vnew]).
% Define get_adj3/7 that finds a new perimeter edge on a face three steps removed
get_adj3(V,F,Uncut,Border,Vnew,Inew,Fnew) :-
	member(e(V1,V2,F1,F2,_),Uncut), member(V,[V1,V2]), permutation([F1,F2],[F,Fint]),
	member(e(V3,V4,F3,F4,_),Uncut), member(V,[V3,V4]), permutation([F3,F4],[Fint,Fmed]), Fmed \== F,
	member(e(V5,V6,F5,F6,_),Uncut), member(V,[V5,V6]), permutation([F5,F6],[Fmed,Fnew]), Fnew \== Fint,
	member(b(Va,Vb,Fnew,Inew,_,_),Border), permutation([Va,Vb],[V,Vnew]).

% Define area_conserved/1 that tests whether an unfolding is valid (no overlap)
area_conserved(Border) :-
	bounds(Border,Low,High),
	numlist(Low,High,Xlist),
	check_cols(Xlist,Border).
% Define bounds/3 for area_conserved/1
bounds(Border,Min,Max) :-
	find_bounds(Border,0,Min,0,Max).
find_bounds([b(_,_,_,_,[Xh,_],_)|T],AccMin,Min,AccMax,Max) :-
	Xh #< AccMin,!,
	find_bounds(T,Xh,Min,AccMax,Max).
find_bounds([b(_,_,_,_,[Xh,_],_)|T],AccMin,Min,AccMax,Max) :-
	Xh #> AccMax,!,
	find_bounds(T,AccMin,Min,Xh,Max).
find_bounds([_|T],AccMin,Min,AccMax,Max) :-
	find_bounds(T,AccMin,Min,AccMax,Max).
find_bounds([],Min,Min,Max,Max).
% Define check_cols/2 that is used in area_conserved/1
check_cols([X0,X1|T],Border) :-
	findall(Y,member(b(_,_,_,_,[X0,Y],[X1,Y]),Border),Inlist), msort(Inlist,In),
	findall(Y,member(b(_,_,_,_,[X1,Y],[X0,Y]),Border),Outlist), msort(Outlist,Out),
	weave(In,Out,Mesh), msort(Mesh,Mesh),
	check_cols([X1|T],Border).
check_cols([_],_).
% Define weave/3 that is used in check_cols/2
weave([Hi|Ti],[Ho|To],[Hi,Ho|Tm]) :- weave(Ti,To,Tm).
weave([],[],[]).

