% Define module depth_unfold that adds one vertex at a time to the spanning tree
:- module(depth_unfold,[edges/3]).
:- use_module(vec_oper).
:- use_module(unfold_module).

% Copyright 2019 Lawrence Livermore National Security, LLC.
% See the top-level LICENSE file for details.
% 
% SPDX-License-Identifier: MIT

% Define degree/3 that gives the degree of the given vertex
degree(Vx,V,D) :-
	member(v(_,V,Fcs),Vx), length(Fcs,D),!.

% Define edges/3
edges(Edges,Vx,UF) :- 
	member(v([0,0,0],I1,_),Vx),
	first_neighbors(Edges,I1,Init), length(Init,Deg0),
	length(Clist,Nvx),Nmax #= Nvx-1,
	get_cut_edges(Edges,Vx,Nmax,[],[I1],Init,0,_,Deg0,UF).

% Define assign_edges/5 that calls faces/4 from unfold_cube and assigns edge properties
assign_edges(Edges,Faces,Vx,C_all,E_all,F_all) :-
	faces(Faces,Vx,F_all,C_all),
	length(E_all,Ne), numlist(1,Ne,Nind),
	length(Edges,Ne),
	each_edge(E_all,Edges,Faces,Nind,Vx).

% Define each_edge/5 that asserts the endpoints, faces, and index of each edge
each_edge([s(X,Y,Fc1,Fc2)|T],Edges,Faces,[I|Ti],Vx) :-
	member(v(X,V1,V1Fcs),Vx),
	member(Fc1,V1Fcs), member(Fc2,V1Fcs),
	member(v(Y,V2,V2Fcs),Vx),
	member(Fc1,V2Fcs), member(Fc2,V2Fcs),
	member(f(F1,Fc1),Faces),member(f(F2,Fc2),Faces),
	member(e(V1,V2,F1,F2,I),Edges),
	each_edge(T,Edges,Faces,Ti,Vx).
each_edge([],_,_,[],_).

% Define first_neighbors/3 that finds the neighbors of the first vertex of the spanning tree
first_neighbors(Edges,V,Lall) :-
	findall(I,member(e(V,_,_,_,I),Edges),L1),
	findall(I,member(e(_,V,_,_,I),Edges),L2),
	append(L1,L2,Lall).

% Define get_cut_edges/9 for edges/7 that searches for a good spanning tree
get_cut_edges(Edges,Vx,Nmax,Ilist,Tree,U,N,CutEdges,Dlast,UF) :-
        Nnew is N+1, Nnew < Nmax,
	Dlast #< 5,
        member(I,U), subtract(U,[I],Uint),
        Inew = [I|Ilist], member(e(Va,Vb,_,_,I),Edges),
        member(V,[Va,Vb]), \+ member(V,Tree),
        Tnew = [V|Tree],
	member(v(_,V,Fcs),Vx), length(Fcs,Dnew),
        tree_neighbors(V,Tree,Edges,Uint,Unew),
        get_cut_edges(Edges,Vx,Nmax,Inew,Tnew,Unew,Nnew,CutEdges,Dnew,UF).
get_cut_edges(Edges,Vx,Nmax,Ilist,Tree,U,N,CutEdges,_,UF) :-
        Nnew is N+1, Nnew < Nmax,
	Tree = [Vlast|_], member(v(_,Vlast,Fcsl),Vx),
        member(I,U), \+ nonplanar(Vlast,Fcsl,[I|Ilist],Edges),
	subtract(U,[I],Uint),
        Inew = [I|Ilist], member(e(Va,Vb,_,_,I),Edges),
        member(V,[Va,Vb]), \+ member(V,Tree),
        Tnew = [V|Tree],
	member(v(_,V,Fcs),Vx), length(Fcs,Dnew),
        tree_neighbors(V,Tree,Edges,Uint,Unew),
        get_cut_edges(Edges,Vx,Nmax,Inew,Tnew,Unew,Nnew,CutEdges,Dnew,UF).
get_cut_edges(Edges,Vx,Nmax,Ilist,Tree,U,N,CutEdges,Dlast,UF) :-
        Nnew is N+1, Nnew == Nmax,
	Dlast #< 5,
        member(I,U),
        Inew = [I|Ilist], member(e(Va,Vb,_,_,I),Edges),
        member(V,[Va,Vb]), \+ member(V,Tree),
	member(v(_,V,Fcs),Vx), length(Fcs,Dnew), Dnew #< 5,
        CutEdges = Inew,
	unfold(Edges,CutEdges,UF).

% Define tree_neighbors/5 for get_all_edges that generates and tests new neighbors of an added vertex
tree_neighbors(V,Tree,Edges,Uint,Unew) :-
	all_neighbors(V,Edges,Nbrs),
	prune_untried(V,Edges,Uint,Umod),
	prune_neighbors(Nbrs,Tree,Umod,Unew).

% Define all_neighbors/3 that finds all edge vertices and indices related to the vertex
all_neighbors(V,Edges,Nbrs) :- findall([X,I],member(e(V,X,_,_,I),Edges),L1),
	findall([X,I],member(e(X,V,_,_,I),Edges),L2),
	append(L1,L2,Nbrs).

% Define prune_untried/4 that goes through the current untried list and removes anything pointed to the new vertex
prune_untried(V,Edges,[Hu|Tu],Unew) :- member(e(V1,V2,_,_,Hu),Edges),
	member(V,[V1,V2]),!,
	prune_untried(V,Edges,Tu,Unew).
prune_untried(V,Edges,[Hu|Tu],[Hu|Unew]) :- 
	prune_untried(V,Edges,Tu,Unew).
prune_untried(_,_,[],[]).

% Define prune_neighbors/4 that checks whether an edge is a new neighbor
prune_neighbors([[X,_]|T],Tree,Uin,Uout) :-
	member(X,Tree),!,
	prune_neighbors(T,Tree,Uin,Uout).
prune_neighbors([[_,I]|T],Tree,Uin,Uout) :-
	member(I,Uin),!,
	prune_neighbors(T,Tree,Uin,Uout).
prune_neighbors([[_,I]|T],Tree,Uin,Uout) :-
	prune_neighbors(T,Tree,[I|Uin],Uout).
prune_neighbors([],_,Uout,Uout).

% Define nonplanar/4 for get_cut_edges
nonplanar(_,Fcs,Icut,_) :-
	length(Fcs,D), D #> 4,
	length(Icut,1).
nonplanar(V,Fcs,Icut,Edges) :-
	length(Fcs,D), D #> 4,
	% Make a list of edges from a cut one all the way around, then split it by all cut edges
	member(I1,Icut), member(e(V1,V2,_,_,I1),Edges), member(V,[V1,V2]),
	wheel(V,I1,Edges,Wheel),
	split_wheel(Wheel,Icut,Split),!, % Green cut, doesn't allow backtracking
	too_large(Split).

% Define wheel/4 for nonplanar
wheel(V,I1,Edges,Wheel) :-
	findall(X,member(e(V,_,_,_,X),Edges),First),
	findall(X,member(e(_,V,_,_,X),Edges),Last),
	append(First,Last,Ilist), subtract(Ilist,[I1],Inds),
	sort_wheel(Inds,Wheel,I1,Edges).
% Define sort_wheel/4 for wheel in nonplanar
sort_wheel([],[Ilast],Ilast,_).
sort_wheel(Ilist,[I1|Twheel],I1,Edges) :-
	member(I2,Ilist),
	adj_edges(I1,I2,Edges),
	subtract(Ilist,[I2],Ipass),
	sort_wheel(Ipass,Twheel,I2,Edges).
% Define adj_edges/3 for sort_wheel
adj_edges(A,B,Edges) :-
	member(e(_,_,Fa,Fb,A),Edges),
	member(e(_,_,Fc,Fd,B),Edges),
	member(F1,[Fa,Fb]), member(F1,[Fc,Fd]).

% Define split_wheel/3 for nonplanar
split_wheel([Last],_,[[Last]]).
split_wheel([H|T],Icut,[[H],[Hl|Tl]|To]) :-
	split_wheel(T,Icut,[[Hl|Tl]|To]),
	member(Hl,Icut),!. % Red cut, ensures proper behavior when backtracking
split_wheel([H|T],Icut,[[H|Term]|To]) :-
	split_wheel(T,Icut,[Term|To]).

% Define too_large/1 for nonplanar
too_large([H|_]) :-
	length(H,N), N #> 4.
too_large([_|T]) :-
	too_large(T).


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

