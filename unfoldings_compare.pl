% Round 2 of unfolding comparison; switch to perimeter notation
:- module(unfoldings_compare,[compare_unfoldings/2]).
:- use_module(library(clpfd)).

% Copyright 2019 Lawrence Livermore National Security, LLC.
% See the top-level LICENSE file for details.
% 
% SPDX-License-Identifier: MIT

compare_unfoldings(P1,P0) :-
	same_length(P1,P0), % Dummy check for same size entries
	same_dims(P1,P0,Axis), % Quick fail clause
	offset(P0,Dx,Dy),
	orient(P0,Dx,Dy,Pref),
	check_both(P1,Axis,Pref). % Actually checking for sameness

same_dims(P1,P0,Axis) :-
	dims(P1,X1,Y1),
	dims(P0,X0,Y0),!,
	axis(X1,Y1,X0,Y0,Axis).

dims(P,X,Y) :- 
	setof(Xi,Yi^member([Xi,Yi],P),Xset),
	Xset = [Xmin|_], last(Xset,Xmax),
	X #= Xmax-Xmin,
	setof(Yi,Xi^member([Xi,Yi],P),Yset),
	Yset = [Ymin|_], last(Yset,Ymax),
	Y #= Ymax-Ymin.

axis(X1,Y1,X0,Y0,1) :- X1 = X0, Y1 = Y0. % if dims are the same, will try Axis=1 and then Axis=2
axis(X1,Y1,X0,Y0,2) :- X1 = Y0, Y1 = X0.

offset(P,Dx,Dy) :-
	setof(Xi,member([Xi,_],P),Xset),
	Xset = [Xmin|_], Dx #= -1*Xmin,
	setof(Yi,member([_,Yi],P),Yset),
	Yset = [Ymin|_], Dy #= -1*Ymin,!.

orient(P0,Dx,Dy,Pref) :-
	P0 = [_|P], % remove leading [0,0]
	translate(P,Dx,Dy,Pref),!.

translate([[Xi,Yi]|Ti],Dx,Dy,[[Xo,Yo]|To]) :-
	Xo #= Xi+Dx, Yo #= Yi+Dy,
	translate(Ti,Dx,Dy,To).
translate([],_,_,[]).

check_both(P1,1,Pref) :-
	offset(P1,Dx,Dy), orient(P1,Dx,Dy,Ptest),
	append(A,B,Pref),
	append(B,A,Ptest).
check_both(P1,1,Pref) :-
	rotate_180(P1,Pout),
	offset(Pout,Dx,Dy), orient(Pout,Dx,Dy,Ptest),
	append(A,B,Pref),
	append(B,A,Ptest).
check_both(P1,2,Pref) :-
	rotate_90(P1,Pout),
	offset(Pout,Dx,Dy), orient(Pout,Dx,Dy,Ptest),
	append(A,B,Pref),
	append(B,A,Ptest).
check_both(P1,2,Pref) :-
	rotate_270(P1,Pout),
	offset(Pout,Dx,Dy), orient(Pout,Dx,Dy,Ptest),
	append(A,B,Pref),
	append(B,A,Ptest).

rotate_90([[Xi,Yi]|Ti],[[Xo,Yo]|To]) :-
	Xo #= Yi, Yo #= -1*Xi,
	rotate_90(Ti,To).
rotate_90([],[]).

rotate_180([[Xi,Yi]|Ti],[[Xo,Yo]|To]) :-
	Xo #= -1*Xi, Yo #= -1*Yi,
	rotate_180(Ti,To).
rotate_180([],[]).

rotate_270([[Xi,Yi]|Ti],[[Xo,Yo]|To]) :-
	Xo #= -1*Xi, Yo #= Yi,
	rotate_270(Ti,To).
rotate_270([],[]).



