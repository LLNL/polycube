% Calculate the number of spanning trees for a given N
:- module(ntrees,[ntrees/3]).

% Copyright 2019 Lawrence Livermore National Security, LLC.
% See the top-level LICENSE file for details.
% 
% SPDX-License-Identifier: MIT

ntrees(C,E,Ntrees) :-
	write_mat(C,E,Diff),
	submat(Diff,1,1,Sub),
	determinant(Sub,Ntrees).

write_mat(C,E,Diff) :-
	length(C,Vmax),
	add_rows(C,E,1,Vmax,Diff).

add_rows(C,E,Vx,Vmax,[Row|Diff]) :-
	Vx < Vmax,
	get_row(Vx,Vmax,C,E,Row),
	Vnew is Vx+1,
	add_rows(C,E,Vnew,Vmax,Diff).
add_rows(C,E,Vmax,Vmax,[Row]) :-
	get_row(Vmax,Vmax,C,E,Row).

get_row(Vx,Vmax,C,E,Row) :- get_row(Vx,Vmax,C,E,1,Row).
get_row(Vx,Vmax,C,E,Col,[Deg|Row]) :-
	Vx == Col,
	degree(C,Vx,Deg),
	Cnew is Col+1,
	get_row(Vx,Vmax,C,E,Cnew,Row).
get_row(Vx,Vmax,C,E,Col,[-1|Row]) :-
	Col =< Vmax,
	is_edge(C,E,Vx,Col),
	Cnew is Col+1,
	get_row(Vx,Vmax,C,E,Cnew,Row).
get_row(Vx,Vmax,C,E,Col,[0|Row]) :-
	Col =< Vmax,
	Cnew is Col+1,
	get_row(Vx,Vmax,C,E,Cnew,Row).
get_row(_,_,_,_,_,[]).

degree(C,Vx,Deg) :-
	nth1(Vx,C,c(_,Fcs)),
	length(Fcs,Deg).

is_edge(C,E,Vx,Col) :-
	nth1(Vx,C,c(C1,_)), nth1(Col,C,c(C2,_)),
	permutation([C1,C2],[A,B]), member(s(A,B,_,_),E).


submat(In,Rd,Cd,Out) :- submat(In,0,Rd,Cd,Out).
submat([_|Trx],Row,Rd,Cd,Mout) :-
	% Traverse Min, eliminating values where Row == Rd or Col == Cd to get Mout
	Rnew is Row+1,
	Rnew == Rd,
	submat(Trx,Rnew,Rd,Cd,Mout).
submat([Hr|Trx],Row,Rd,Cd,[Hcut|Mout]) :-
	Rnew is Row +1,
	subrow(Hr,0,Cd,Hcut),
	submat(Trx,Rnew,Rd,Cd,Mout).
submat([],_,_,_,[]).

subrow([_|Ti],Col,Cd,To) :-
	Cnew is Col+1,
	Cnew == Cd,
	subrow(Ti,Cnew,Cd,To).
subrow([Hi|Ti],Col,Cd,[Hi|To]) :-
	Cnew is Col+1,
	subrow(Ti,Cnew,Cd,To).
subrow([],_,_,[]).

determinant([[A,B],[C,D]],Det) :- Det is A*D-B*C. % Base case: 2x2 matrix ad-cb
determinant(Mat,Det) :-
	Mat = [Hr|_], same_length(Mat,Hr), % Ensure square matrix input
	det_row(Hr,Mat,1,0,Deti),
	sum_list(Deti,Det).

det_row([H|T],Mat,Rd,Col,Deti) :-
	H == 0,
	Cnew is Col+1,
	det_row(T,Mat,Rd,Cnew,Deti).
det_row([H|T],Mat,Rd,Col,[V|Deti]) :-
	Cnew is Col+1,
	S is 2*(Cnew mod 2)-1, % Sign of output, odd columns positive and evens negative
	submat(Mat,Rd,Cnew,Mout),
	determinant(Mout,Det),
	V is S*H*Det,
	det_row(T,Mat,Rd,Cnew,Deti).
det_row([],_,_,_,[]).
