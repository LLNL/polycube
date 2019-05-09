% Generate a N-tube and find the first unfolding
:- use_module(library(clpfd)).
:- use_module(unfold_cube).
:- use_module(build_cubes).

% Copyright 2019 Lawrence Livermore National Security, LLC.
% See the top-level LICENSE file for details.
% 
% SPDX-License-Identifier: MIT

map_tube(N) :- unfold_tube(N,_).

unfold_tube(N,UF) :-
	get_tube(N,Raw),
	build_cubes([Raw],[[C,E,F]]),
	time(edges(_,C,E,F,UF)), write(N).

get_tube(1,[[[0,0,0]],[]]).
get_tube(N,[Cout,IDout]) :-
	Nless #= N-1, Nskip #= N-2,
	get_tube(Nless,[Cin,IDin]),
	Cout = [[Nless,0,0]|Cin],
	IDout = [i([Nless,0,0],[Nskip,0,0])|IDin].

