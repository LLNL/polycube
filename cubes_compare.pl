% Compare cubes version 2.0 
:- module(cubes_compare,[compare_cubes/2,compare_cubes/3]).

% Copyright 2019 Lawrence Livermore National Security, LLC.
% See the top-level LICENSE file for details.
% 
% SPDX-License-Identifier: MIT

% Check the dimension of the polycube (1D, 2D, or 3D)
dx(Id,1) :- member(i([Xi,_,_],_),Id), Xi \== 0;
	member(i(_,[Xj,_,_]),Id), Xj \== 0.
dx(_,0).
dy(Id,1) :- member(i([_,Yi,_],_),Id), Yi \== 0;
	member(i(_,[_,Yj,_]),Id), Yj \== 0.
dy(_,0).
dz(Id,1) :- member(i([_,_,Zi],_),Id), Zi \== 0;
	member(i(_,[_,_,Zj]),Id), Zj \== 0.
dz(_,0).
dim(Id,D) :- dx(Id,X), dy(Id,Y), dz(Id,Z), D is X+Y+Z,!.
dimcheck(Id1,Id2) :- dim(Id1,D1), dim(Id2,D2), D1 == D2.

% Put each coordinate pair in an unordered list
flatter([i(P1,P2)|T],Acc,Out) :-
	flatter(T,[P1,P2|Acc],Out).
flatter([],Acc,Acc).

% Check the degree of each vertex (cube) of both polycubes
degrees([H|T],[Dh|Dlist],ID) :- 
	findall(Ind,nth1(Ind,ID,H),Res), length(Res,Dh),
	degrees(T,Dlist,ID).
degrees([],[],_).
v_deg_check(Id1,Id2,Id1s,Id2s,Dset1,Dset2) :- flatter(Id1,[],Id1f), flatter(Id2,[],Id2f),
	list_to_set(Id1f,Id1s), list_to_set(Id2f,Id2s),
	degrees(Id1s,Dset1,Id1f), degrees(Id2s,Dset2,Id2f),
	msort(Dset1,D1s), msort(Dset2,D1s).

% Define leaves/3, which collects all the cube coords of degree 1 into a list
leaves([Hi|Ti],[Di|Td],[Hi|Rem]) :- Di == 1,!,
	leaves(Ti,Td,Rem).
leaves([_|Ti],[_|Td],Rem) :- leaves(Ti,Td,Rem).
leaves([],[],[]).

% Define translate/3 to shift a cube id or a list of leaves in 3D space
translate([i([X1,Y1,Z1],[X2,Y2,Z2])|Ti],[X,Y,Z],[i([X1n,Y1n,Z1n],[X2n,Y2n,Z2n])|To]) :- 
	X1n is X1-X, Y1n is Y1-Y, Z1n is Z1-Z, X2n is X2-X, Y2n is Y2-Y, Z2n is Z2-Z,
	translate(Ti,[X,Y,Z],To).
translate([[Xi,Yi,Zi]|Ti],[X,Y,Z],[[Xo,Yo,Zo]|To]) :- Xo is Xi-X, Yo is Yi-Y, Zo is Zi-Z,
	translate(Ti,[X,Y,Z],To).
translate([],_,[]).

% Define get_rtn/2 that says how to rotate the cube id
get_rtn(Id,R) :- member(i([0,0,0],R),Id);
	member(i(R,[0,0,0]),Id). 

% Define rotate/3 for both ids and leaf lists
rotate(Id,[1,0,0],Id).
rotate([i([X1,Y1,Z1],[X2,Y2,Z2])|Ti],[0,1,0],[i([X1n,Y1n,Z1],[X2n,Y2n,Z2])|To]) :-
	X1n is Y1, Y1n is -1*X1, X2n is Y2, Y2n is -1*X2,
	rotate(Ti,[0,1,0],To).
rotate([i([X1,Y1,Z1],[X2,Y2,Z2])|Ti],[0,-1,0],[i([X1n,Y1n,Z1],[X2n,Y2n,Z2])|To]) :-
	X1n is -1*Y1, Y1n is X1, X2n is -1*Y2, Y2n is X2,
	rotate(Ti,[0,-1,0],To).
rotate([i([X1,Y1,Z1],[X2,Y2,Z2])|Ti],[-1,0,0],[i([X1n,Y1n,Z1],[X2n,Y2n,Z2])|To]) :-
	X1n is -1*X1, Y1n is -1*Y1, X2n is -1*X2, Y2n is -1*Y2,
	rotate(Ti,[-1,0,0],To).
rotate([i([X1,Y1,Z1],[X2,Y2,Z2])|Ti],[0,0,1],[i([X1n,Y1,Z1n],[X2n,Y2,Z2n])|To]) :-
	X1n is Z1, Z1n is -1*X1, X2n is Z2, Z2n is -1*X2,
	rotate(Ti,[0,0,1],To).
rotate([i([X1,Y1,Z1],[X2,Y2,Z2])|Ti],[0,0,-1],[i([X1n,Y1,Z1n],[X2n,Y2,Z2n])|To]) :-
	X1n is -1*Z1, Z1n is X1, X2n is -1*Z2, Z2n is X2,
	rotate(Ti,[0,0,-1],To).
rotate([[X,Y,Z]|Ti],[0,1,0],[[Xn,Yn,Z]|To]) :-
	Xn is Y, Yn is -1*X,
	rotate(Ti,[0,1,0],To).
rotate([[X,Y,Z]|Ti],[0,-1,0],[[Xn,Yn,Z]|To]) :-
	Xn is -1*Y, Yn is X,
	rotate(Ti,[0,-1,0],To).
rotate([[X,Y,Z]|Ti],[-1,0,0],[[Xn,Yn,Z]|To]) :-
	Xn is -1*X, Yn is -1*Y,
	rotate(Ti,[-1,0,0],To).
rotate([[X,Y,Z]|Ti],[0,0,1],[[Xn,Y,Zn]|To]) :-
	Xn is Z, Zn is -1*X,
	rotate(Ti,[0,0,1],To).
rotate([[X,Y,Z]|Ti],[0,0,-1],[[Xn,Y,Zn]|To]) :-
	Xn is -1*Z, Zn is X,
	rotate(Ti,[0,0,-1],To).
rotate([[X,Y,Z]|Ti],[0,1,1],[[X,Yn,Zn]|To]) :-
	Yn is -1*Z, Zn is Y,
	rotate(Ti,[0,1,1],To).
rotate([[X,Y,Z]|Ti],[0,2,2],[[X,Yn,Zn]|To]) :-
	Yn is -1*Y, Zn is -1*Z,
	rotate(Ti,[0,2,2],To).
rotate([[X,Y,Z]|Ti],[0,3,3],[[X,Yn,Zn]|To]) :-
	Yn is Z, Zn is -1*Y,
	rotate(Ti,[0,3,3],To).
rotate([],_,[]).

% Define orient/3 that orients a cube id with a reference vertex at [0,0,0] and link to [1,0,0]
orient(Id,Idset,Lref,Idref,Idset_ref) :- translate(Id,Lref,Idtr), get_rtn(Idtr,Rtn), rotate(Idtr,Rtn,Idref),
	translate(Idset,Lref,Idstr), rotate(Idstr,Rtn,Idset_ref).

% Define check_match/2 that checks each of the 4 rotations of id2 about the x-axis for a match with id1
check_match(Ref,Cset) :- permutation(Ref,Cset).
check_match(Ref,Cset) :- rotate(Cset,[0,1,1],Cset2), permutation(Ref,Cset2).
check_match(Ref,Cset) :- rotate(Cset,[0,2,2],Cset2), permutation(Ref,Cset2).
check_match(Ref,Cset) :- rotate(Cset,[0,3,3],Cset2), permutation(Ref,Cset2).

% Define each_leaf/4 that checks each leaf of id2 in 4 rotations for a match with id1
each_leaf(Idset_ref,Id,[Hlv|_],Ids) :-
	orient(Id,Ids,Hlv,_,Or1ids), check_match(Idset_ref,Or1ids).
each_leaf(Idset_ref,Id,[_|Tlvs],Ids) :-
	each_leaf(Idset_ref,Id,Tlvs,Ids).

% Parallel version of each_leaf
check_leaf(Ref,ID,Leaf,IDset) :-
	orient(ID,IDset,Leaf,_,Ord), check_match(Ref,Ord).
build_goals(Ref,ID,IDset,Leaf,check_leaf(Ref,ID,Leaf,IDset)).

% Main function, succeeds if two IDs are identical up to 3D rotation
compare_cubes(Id1,Id2) :-
	dimcheck(Id1,Id2), v_deg_check(Id1,Id2,Id1s,Id2s,Dset1,Dset2), % Quick fail conditions
	leaves(Id1s,Dset1,Lv1), leaves(Id2s,Dset2,Lv2), % Get leaves for each ID
	Lv1 = [Lref|_], orient(Id1,Id1s,Lref,_,Idset_ref), % Make the reference ID
	each_leaf(Idset_ref,Id2,Lv2,Id2s). % Check each leaf


compare_cubes(Id1,Id2,parallel) :-
	dimcheck(Id1,Id2), v_deg_check(Id1,Id2,Id1s,Id2s,Dset1,Dset2), % Quick fail conditions
	leaves(Id1s,Dset1,Lv1), leaves(Id2s,Dset2,Lv2), % Get leaves for each ID
	Lv1 = [Lref|_], orient(Id1,Id1s,Lref,_,Idset_ref), % Make the reference ID
	maplist(build_goals(Idset_ref,Id2,Id2s),Lv2,Goals),
	first_solution(_,Goals,[on_fail(continue)]). % Check each leaf in parallel
compare_cubes(Id1,Id2,serial) :-
	dimcheck(Id1,Id2), v_deg_check(Id1,Id2,Id1s,Id2s,Dset1,Dset2), % Quick fail conditions
	leaves(Id1s,Dset1,Lv1), leaves(Id2s,Dset2,Lv2), % Get leaves for each ID
	Lv1 = [Lref|_], orient(Id1,Id1s,Lref,_,Idset_ref), % Make the reference ID
	each_leaf(Idset_ref,Id2,Lv2,Id2s). % Check each leaf serially

