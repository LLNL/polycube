:- module(vec_oper,[vec_vec_sum/3,vec_mat_sum/3,scal_vec_sum/3,scal_vec_prod/3]).
:- use_module(library(clpfd)).

% Copyright 2019 Lawrence Livermore National Security, LLC.
% See the top-level LICENSE file for details.
% 
% SPDX-License-Identifier: MIT

% Define clpfd addition and multiplication
add(A,B,C) :-
	C #= A+B.
multiply(A,B,C) :-
	C #= A*B.

% Define vector addition/subtraction
vec_vec_sum(Alist,Blist,Clist) :-
	maplist(add(),Alist,Blist,Clist).

% Define vector addition like scalar addition for matrices
vec_mat_sum(Alist,Bmat,Cmat) :-
	maplist(vec_vec_sum(Alist),Bmat,Cmat).

% Define scalar-vector addition
scal_vec_sum(A,Blist,Clist) :-
	maplist(add(A),Blist,Clist). 

% Define scalar-vector multiplication
scal_vec_prod(A,Blist,Clist) :-
	maplist(multiply(A),Blist,Clist).
