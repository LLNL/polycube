% List-based get_polycubes revision
:- module(build_cubes,[build_cubes/2,build/3,assign_edges/6]).
:- use_module(library(clpfd)).
:- use_module(library(clpb)).
:- use_module(vec_oper).

% Copyright 2019 Lawrence Livermore National Security, LLC.
% See the top-level LICENSE file for details.
% 
% SPDX-License-Identifier: MIT

% Define build_cubes/2
build_cubes([[Cds,ID]|Tc],[Hpc|Tpc]) :-
	build_cubes(Tc,Tpc),
	build(Cds,ID,[C,E,F]),!,
	assign_edges(Edges,Faces,Vx,C,E,F),
	Hpc = [Edges,Faces,Vx].
build_cubes([],[]).
% Define build/3 for build_cubes
build([C1],[],[C,E,F]) :-
	Mat = [[1,0,0],[0,1,0],[0,0,1],[1,1,0],[1,0,1],[0,1,1],[1,1,1]],
	vec_mat_sum(C1,Mat,[C2,C3,C4,C5,C6,C7,C8]),
	F1 = [C1,C3,C7,C4], F2 = [C2,C5,C8,C6], F3 = [C1,C4,C6,C2], F4 = [C3,C7,C8,C5], F5 = [C1,C2,C5,C3], F6 = [C4,C6,C8,C7],
	C =[c(C1,[F1,F3,F5]),c(C2,[F2,F3,F5]),c(C3,[F1,F4,F5]),c(C4,[F1,F3,F6]),c(C5,[F2,F4,F5]),c(C6,[F2,F3,F6]),c(C7,[F1,F4,F6]),c(C8,[F2,F4,F6])],
	E = [s(C1,C2,F3,F5),s(C1,C3,F1,F5),s(C1,C4,F1,F3),s(C2,C5,F2,F5),s(C2,C6,F2,F3),s(C3,C5,F4,F5),s(C3,C7,F1,F4),s(C4,C6,F3,F6),s(C4,C7,F1,F6),s(C5,C8,F2,F4),s(C6,C8,F2,F6),s(C7,C8,F4,F6)],
	F = [fc(F1),fc(F2),fc(F3),fc(F4),fc(F5),fc(F6)].
build([C1|Tc],[i(C1,C0)|Tid],[C,E,F]) :-
	build(Tc,Tid,[Cin,Ein,Fin]),
	vec_vec_sum(C1,D,C0), vec_vec_sum(C1,[1,1,1],Opp),
	get_cube(C1,Cube),
	merge_cube(C1,Opp,D,Cube,[Cin,Ein,Fin],[C,E,F]).
% Define get_cube/2 for build
get_cube(C1,[C,E,F]) :-
	Mat = [[1,0,0],[0,1,0],[0,0,1],[1,1,0],[1,0,1],[0,1,1],[1,1,1]],
	vec_mat_sum(C1,Mat,[C2,C3,C4,C5,C6,C7,C8]),
	F1 = [C1,C3,C7,C4], F2 = [C2,C5,C8,C6], F3 = [C1,C4,C6,C2], F4 = [C3,C7,C8,C5], F5 = [C1,C2,C5,C3], F6 = [C4,C6,C8,C7],
	C = [c(C1,[F1,F3,F5]),c(C2,[F2,F3,F5]),c(C3,[F1,F4,F5]),c(C4,[F1,F3,F6]),c(C5,[F2,F4,F5]),c(C6,[F2,F3,F6]),c(C7,[F1,F4,F6]),c(C8,[F2,F4,F6])],
	E = [s(C1,C2,F3,F5),s(C1,C3,F1,F5),s(C1,C4,F1,F3),s(C2,C5,F2,F5),s(C2,C6,F2,F3),s(C3,C5,F4,F5),s(C3,C7,F1,F4),s(C4,C6,F3,F6),s(C4,C7,F1,F6),s(C5,C8,F2,F4),s(C6,C8,F2,F6),s(C7,C8,F4,F6)],
	F = [fc(F1),fc(F2),fc(F3),fc(F4),fc(F5),fc(F6)].
% Define merge_cube/6 for build
merge_cube([_,Y,Z],[Xpl,Ypl,Zpl],[1,0,0],Cube,In,Out) :-
	C1 = [Xpl,Y,Z], C2 = [Xpl,Ypl,Z], C3 = [Xpl,Ypl,Zpl], C4 = [Xpl,Y,Zpl],
	merge(C1,C2,C3,C4,Cube,In,Out).
merge_cube([X,Y,Z],[_,Ypl,Zpl],[-1,0,0],Cube,In,Out) :-
	C1 = [X,Y,Z], C2 = [X,Ypl,Z], C3 = [X,Ypl,Zpl], C4 = [X,Y,Zpl],
	merge(C1,C2,C3,C4,Cube,In,Out).
merge_cube([X,_,Z],[Xpl,Ypl,Zpl],[0,1,0],Cube,In,Out) :-
	C1 = [X,Ypl,Z], C2 = [X,Ypl,Zpl], C3 = [Xpl,Ypl,Zpl], C4 = [Xpl,Ypl,Z],
	merge(C1,C2,C3,C4,Cube,In,Out).
merge_cube([X,Y,Z],[Xpl,_,Zpl],[0,-1,0],Cube,In,Out) :-
	C1 = [X,Y,Z], C2 = [X,Y,Zpl], C3 = [Xpl,Y,Zpl], C4 = [Xpl,Y,Z],
	merge(C1,C2,C3,C4,Cube,In,Out).
merge_cube([X,Y,_],[Xpl,Ypl,Zpl],[0,0,1],Cube,In,Out) :-
	C1 = [X,Y,Zpl], C2 = [Xpl,Y,Zpl], C3 = [Xpl,Ypl,Zpl], C4 = [X,Ypl,Zpl],
	merge(C1,C2,C3,C4,Cube,In,Out).
merge_cube([X,Y,Z],[Xpl,Ypl,_],[0,0,-1],Cube,In,Out) :-
	C1 = [X,Y,Z], C2 = [Xpl,Y,Z], C3 = [Xpl,Ypl,Z], C4 = [X,Ypl,Z],
	merge(C1,C2,C3,C4,Cube,In,Out).
% Define merge/7 for merge_cube
merge(C1,C2,C3,C4,[C,E,F],[Ci,Ei,Fi],[Co,Eo,Fo]) :-
	Fadj = [C1,C2,C3,C4],
	member(c(C1,Fcs1i),Ci), member(Fadj,Fcs1i), member(c(C1,Fcs1c),C),
	append(Fcs1i,Fcs1c,Fcs1t), subtract(Fcs1t,[Fadj],Fcs1), C1o = c(C1,Fcs1),
	member(c(C2,Fcs2i),Ci), member(Fadj,Fcs2i), member(c(C2,Fcs2c),C),
	append(Fcs2i,Fcs2c,Fcs2t), subtract(Fcs2t,[Fadj],Fcs2), C2o = c(C2,Fcs2),
	member(c(C3,Fcs3i),Ci), member(Fadj,Fcs3i), member(c(C3,Fcs3c),C),
	append(Fcs3i,Fcs3c,Fcs3t), subtract(Fcs3t,[Fadj],Fcs3), C3o = c(C3,Fcs3),
	member(c(C4,Fcs4i),Ci), member(Fadj,Fcs4i), member(c(C4,Fcs4c),C),
	append(Fcs4i,Fcs4c,Fcs4t), subtract(Fcs4t,[Fadj],Fcs4), C4o = c(C4,Fcs4),
	member(s(C1,C2,F1a,F2a),Ei), member(s(C1,C2,F3a,F4a),E),
	permutation([F1a,F2a],[Fadj,Fk1a]), permutation([F3a,F4a],[Fadj,Fk2a]), E1o = s(C1,C2,Fk1a,Fk2a),
	member(s(C2,C3,F1b,F2b),Ei), member(s(C2,C3,F3b,F4b),E),
	permutation([F1b,F2b],[Fadj,Fk1b]), permutation([F3b,F4b],[Fadj,Fk2b]), E2o = s(C2,C3,Fk1b,Fk2b),
	member(s(C4,C3,F1c,F2c),Ei), member(s(C4,C3,F3c,F4c),E),
	permutation([F1c,F2c],[Fadj,Fk1c]), permutation([F3c,F4c],[Fadj,Fk2c]), E3o = s(C4,C3,Fk1c,Fk2c),
	member(s(C1,C4,F1d,F2d),Ei), member(s(C1,C4,F3d,F4d),E),
	permutation([F1d,F2d],[Fadj,Fk1d]), permutation([F3d,F4d],[Fadj,Fk2d]), E4o = s(C1,C4,Fk1d,Fk2d),
	subtract(Fi,[fc(Fadj)],Fint),
	subtract(Ci,[c(C1,Fcs1i),c(C2,Fcs2i),c(C3,Fcs3i),c(C4,Fcs4i)],Cint),
	subtract(Ei,[s(C1,C2,F1a,F2a),s(C2,C3,F1b,F2b),s(C4,C3,F1c,F2c),s(C1,C4,F1d,F2d)],Eint),
	subtract(F,[fc(Fadj)],Fn),
	subtract(C,[c(C1,Fcs1c),c(C2,Fcs2c),c(C3,Fcs3c),c(C4,Fcs4c)],Cn),
	subtract(E,[s(C1,C2,F3a,F4a),s(C2,C3,F3b,F4b),s(C4,C3,F3c,F4c),s(C1,C4,F3d,F4d)],En),
	append(Cn,Cint,Cmed), Co = [C1o,C2o,C3o,C4o|Cmed],
	append(En,Eint,Emed), Eo = [E1o,E2o,E3o,E4o|Emed],
	append(Fn,Fint,Fo).


% Make table for vertices with 3D coords, indices
vertices(Vx,All_coords) :-
	% Each vertex in the list Vx of vertices has 3D coords and an index
	length(All_coords,Nvert),
	length(Vx,Nvert),
	numlist(1,Nvert,Ind),
	assign_vert(All_coords,Ind,Vx).

assign_vert([c(C,Fcs)|T],[Hind|Tind],Vx) :- 
	member(v(C,Hind,Fcs),Vx),
	assign_vert(T,Tind,Vx).
assign_vert([],[],_).

% Make table for faces
faces(Faces,Vx,All_faces,All_coords) :-
	vertices(Vx,All_coords),
	same_length(Faces,All_faces),
	assign_faces(All_faces,Faces,Vx).

assign_faces([fc([A,B,C,D])|T],Faces,Vx) :- 
	member(v(A,I,_),Vx), member(v(B,J,_),Vx), member(v(C,K,_),Vx), member(v(D,L,_),Vx),
	member(f([I,J,K,L],[A,B,C,D]),Faces),
	assign_faces(T,Faces,Vx).
assign_faces([],_,_).

% Define assign_edges/5 that calls faces/4 from unfold_cube and assigns edge properties
assign_edges(Edges,Faces,Vx,C_all,E_all,F_all) :-
	faces(Faces,Vx,F_all,C_all),
	length(E_all,Ne), numlist(1,Ne,Nind),
	length(Edges,Ne),!,
	each_edge(E_all,Edges,Faces,Nind,Vx).

% Define each_edge/5 that asserts the endpoints, faces, and index of each edge
each_edge([s(X,Y,Fc1,Fc2)|T],Edges,Faces,[I|Ti],Vx) :-
	each_edge(T,Edges,Faces,Ti,Vx),
	member(v(X,V1,V1Fcs),Vx),
	member(Fc1,V1Fcs),
	member(v(Y,V2,V2Fcs),Vx),
	member(Fc1,V2Fcs),
	member(f(F1,Fc1),Faces),member(f(F2,Fc2),Faces),
	member(e(V1,V2,F1,F2,I),Edges).
each_edge([],_,_,[],_).
