% Bounding-box comparison of polycube coordinates
:- module(bound_compare,[compare_cubes/4,all_inds/3]).
:- use_module(library(clpfd)).
:- use_module(vec_oper).

% Copyright 2019 Lawrence Livermore National Security, LLC.
% See the top-level LICENSE file for details.
% 
% SPDX-License-Identifier: MIT

compare_cubes(Cref,Ctest,AllInds,Bounds) :-
	var(AllInds),
	same_length(Cref,Ctest),
	bounds(Cref,Bounds,Rref),!,
	bounds(Ctest,Bounds,Rtest),
	Rref = [Xr,_,Yr,_,Zr,_], scal_vec_prod(-1,[Xr,Yr,Zr],Tref),
	Rtest = [Xt,_,Yt,_,Zt,_], scal_vec_prod(-1,[Xt,Yt,Zt],Ttest),
	vec_mat_sum(Tref,Cref,C1),
	vec_mat_sum(Ttest,Ctest,C2),
	check_match(C1,C2,Rref,Rtest,Bounds,AllInds).
compare_cubes(Cref,Ctest,AllInds,Bounds) :-
	same_length(Cref,Ctest),
	bounds(Ctest,Bounds,Rtest),
	Rtest = [Xt,_,Yt,_,Zt,_], scal_vec_prod(-1,[Xt,Yt,Zt],Ttest),
	vec_mat_sum(Ttest,Ctest,C2),
	first_indices(C2,Rtest,Bounds,Inds),!,
	member(Inds,AllInds).

all_inds(Coords,AllInds,Bounds) :-
	bounds(Coords,Bounds,Ranges),
	Ranges = [X,_,Y,_,Z,_], scal_vec_prod(-1,[X,Y,Z],Trans),
	vec_mat_sum(Trans,Coords,Ci),
	all_indices(Ci,Ranges,Bounds,AllInds).

bounds(Cds,Bounds,[Minx,Maxx,Miny,Maxy,Minz,Maxz]) :-
	length(Bounds,3),
	Cds = [[Hx,Hy,Hz]|_],
	get_minmax(Cds,1,Hx,Minx,Hx,Maxx),
	get_minmax(Cds,2,Hy,Miny,Hy,Maxy),
	get_minmax(Cds,3,Hz,Minz,Hz,Maxz),!,
	Bx #= Maxx - Minx, nth1(A,Bounds,Bx),
	By #= Maxy - Miny, nth1(B,Bounds,By),
	Bz #= Maxz - Minz, nth1(C,Bounds,Bz),
	decreasing(Bounds),
	all_distinct([A,B,C]).
decreasing([H1,H2|T]) :-
	H1 #>= H2,
	decreasing([H2|T]).
decreasing([_]).

get_minmax([H|T],Ind,MinIn,Min,MaxIn,Max) :-
	nth1(Ind,H,C),
	C #< MinIn,
	get_minmax(T,Ind,C,Min,MaxIn,Max).
get_minmax([H|T],Ind,MinIn,Min,MaxIn,Max) :-
	nth1(Ind,H,C),
	C #> MaxIn,
	get_minmax(T,Ind,MinIn,Min,C,Max).
get_minmax([_|T],Ind,MinIn,Min,MaxIn,Max) :-
	get_minmax(T,Ind,MinIn,Min,MaxIn,Max).
get_minmax([],_,Min,Min,Max,Max).


check_match(C1,C2,B1,B2,Bounds,AllInds) :-
	var(AllInds),
	all_indices(C1,B1,Bounds,AllInds),
	first_indices(C2,B2,Bounds,Inds),!,
	member(Inds,AllInds).
check_match(_,C2,_,B2,Bounds,AllInds) :-
	first_indices(C2,B2,Bounds,Inds),!,
	member(Inds,AllInds).

all_indices(Cds,[Xl,Xh,Yl,Yh,Zl,Zh],Bounds,Out) :-
	Rx #= Xh - Xl, Ry #= Yh - Yl, Rz #= Zh - Zl,
	findall(Ord,swap(Ord,[Rx,Ry,Rz],Bounds),Ptns),
	Pts = [[0,0,0],[0,0,Rz],[0,Ry,0],[Rx,0,0],[Rx,Ry,0],[Rx,0,Rz],[0,Ry,Rz],[Rx,Ry,Rz]],
	list_to_set(Ptns,Perms),
	list_to_set(Pts,Pset),
	convolve(Pset,Perms,[],[],Pout,Dirs),
	maplist(get_indices(Cds,Bounds),Pout,Dirs,All),
	list_to_set(All,Out).

first_indices(Cds,[Xl,Xh,Yl,Yh,Zl,Zh],Bounds,Out) :-
	Rx #= Xh - Xl, Ry #= Yh - Yl, Rz #= Zh - Zl,
	swap(Ord,[Rx,Ry,Rz],Bounds),
	get_indices(Cds,Bounds,[0,0,0],Ord,Out).

swap([A,B,C],Cin,Cout) :-
	permutation(Cin,Cout),
	nth1(1,Cin,V1), nth1(A,Cout,V1),
	nth1(2,Cin,V2), nth1(B,Cout,V2),
	nth1(3,Cin,V3), nth1(C,Cout,V3),
	all_different([A,B,C]).

convolve([Hpt|Tpts],Perms,PtsIn,DirsIn,Npts,Ndirs) :-
	get_dirs(Hpt,Perms,PtsIn,DirsIn,Pts1,Dirs1),
	convolve(Tpts,Perms,Pts1,Dirs1,Npts,Ndirs).
convolve([],_,Pts,Dirs,Pts,Dirs).

get_dirs(Pt,[Hp|Tps],Pin,Din,Pout,Dout) :-
	get_dirs(Pt,Tps,[Pt|Pin],[Hp|Din],Pout,Dout).
get_dirs(_,[],Pts,Dirs,Pts,Dirs).


get_indices(Cds,Bounds,Corner,Dir,Matches) :-
	build_box(Bounds,Corner,Dir,Box),
	matches(Cds,Box,Hits),
	sort(Hits,Matches).

build_box(Bounds,Pt,Dirs,Box) :-
	get_ranges(Bounds,Pt,Dirs,[R1,R2,R3]),
	get_C3(R1,R2,R3,Dirs,[],Box).

get_ranges([B1,B2,B3],Pts,Ord,[R1,R2,R3]) :-
	swap(Ord,Pts,[P1,P2,P3]),
	numlist(0,B1,Ra), ((P1 #= 0, R1 = Ra); reverse(Ra,R1)),
	numlist(0,B2,Rb), ((P2 #= 0, R2 = Rb); reverse(Rb,R2)),
	numlist(0,B3,Rc), ((P3 #= 0, R3 = Rc); reverse(Rc,R3)).

get_C3(R1,R2,[H|T],Dir,In,Out) :-
	get_C3(R1,R2,T,Dir,In,Int),
	get_C2(R1,R2,H,Dir,Int,Out).
get_C3(_,_,[],_,In,In).
get_C2(R1,[H|T],C3,Dir,In,Out) :-
	get_C2(R1,T,C3,Dir,In,Int),
	get_C1(R1,H,C3,Dir,Int,Out).
get_C2(_,[],_,_,In,In).
get_C1([H|T],C2,C3,Dir,In,[Coord|Int]) :-
	swap(Dir,Coord,[H,C2,C3]), 
	get_C1(T,C2,C3,Dir,In,Int).
get_C1([],_,_,_,In,In).

matches([Hc|Tc],Box,[I|Hits]) :-
	matches(Tc,Box,Hits),!,
	nth1(I,Box,Hc).
matches([_|Tc],Box,Hits) :-
	matches(Tc,Box,Hits).
matches([],_,[]).
