% Characterizes families of polycubes by vertex degree
:- module(characterize_cubes,[characterize/2]).
:- use_module(library(clpfd)).

% Copyright 2019 Lawrence Livermore National Security, LLC.
% See the top-level LICENSE file for details.
% 
% SPDX-License-Identifier: MIT

% Get the high degree vertex counts of each polycube
characterize(PCs,Degs) :-
	maplist(char_p(),PCs,Degs).

char_p([C,_,_],[D3,D4,D5,D6,D7]) :-
	degree_each(C,Degs),
	hist_deg(Degs,D3,D4,D5,D6,D7).
	

degree_each([c(_,H)|Tc],[Hd|Td]) :-
	degree_each(Tc,Td),
	length(H,Hd).
degree_each([],[]).	

hist_deg([],0,0,0,0,0).
hist_deg([3|Td],D3,D4,D5,D6,D7) :-
	hist_deg(Td,Din,D4,D5,D6,D7),
	D3 #= Din+1,!.
hist_deg([4|Td],D3,D4,D5,D6,D7) :-
	hist_deg(Td,D3,Din,D5,D6,D7),
	D4 #= Din+1,!.
hist_deg([5|Td],D3,D4,D5,D6,D7) :-
	hist_deg(Td,D3,D4,Din,D6,D7),
	D5 #= Din+1,!.
hist_deg([6|Td],D3,D4,D5,D6,D7) :-
	hist_deg(Td,D3,D4,D5,Din,D7),
	D6 #= Din+1,!.
hist_deg([7|Td],D3,D4,D5,D6,D7) :-
	hist_deg(Td,D3,D4,D5,D6,Din),
	D7 #= Din+1,!.
