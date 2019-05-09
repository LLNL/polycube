% Unfold all polycubes of a given N concurrently
:- use_module(library(clpfd)).
:- use_module(cubes_compare).
:- use_module(depth_parallel).
:- use_module(vec_oper).
:- use_module(build_cubes).
:- use_module(generate_3D_oldcompare).

% Copyright 2019 Lawrence Livermore National Security, LLC.
% See the top-level LICENSE file for details.
%
% SPDX-License-Identifier: MIT

% Define unfold_all/4, the main function of this script
unfold_all(N,Clists,PClist,UFs) :-
	time(get_all_treelike(N,Clists)), % From generate_3D_*
	build_cubes(Clists,PClist),!, % From build_cubes
	concurrent_maplist(get_UF(),PClist,UFs).

% Define get_UF/2 for maplist purposes
get_UF([Edges,_,Vx],UF) :-
	time(edges(Edges,Vx,UF)).    % From depth_parallel

unfold_serial([Hcid|Tcids],[[Edges,_,Vx]|Tpcs],[Huf|Tufs]) :-
	write(user_error,Hcid),nl,
	edges(Edges,Vx,Huf),
	unfold_serial(Tcids,Tpcs,Tufs).
unfold_serial([],[],[]).
