make_matrix1(N,M):- make_matrix1(0,N,M),!.
make_matrix1(N,N,[]).
make_matrix1(I,N,[Head|Tail]):- 
                 length(Head,N),
				 I1 is I+1,
				 make_matrix1(I1,N,Tail).
grid_build(N,M):-
            make_matrix1(N,M).
			
			




                          
grid_gen(N,M):-
		 grid_build(N,M),
		 trans(M,M1),
		 acceptable_permutation(M,M1),
         num_gen(1,N,R),
		 salsa(M,N,R).
		 
salsa([],_,_).	 
salsa([H|T],N,R):-
		gen_Permutation(N,R,H),
		salsa(T,N,R).
		
	
	
	
	
gen_list([],_).
gen_list([H|T],Set):-
			member(H,Set),
			gen_list(T,Set).
gen_Permutation(N,Set,L):-length(L,N),gen_list(L,Set).








num_gen(S,E,R):-D is E+1,helper(S,D,[],R),!.
helper(S,S,A,A).
helper(S,E,A,R):-
	append(A,[S],NA),
	S1 is S+1,
	helper(S1,E,NA,R).
	
	
	
	
	
search(0,[],_).
search(X,[H|_],G):-
	X\=0,
	X1 is X-1,
	member(X,H),
	search(X1,G,G).
search(X,[_|T],G):-
	search(X,T,G).
	
check_num_grid(G):-
	max_matrix(G,R),
	length(G,R1),
	\+ R>R1,
	check_num_grid(G,R,G),!.
	
check_num_grid(G,R,G):-
	search(R,G,G).

max_matrix(L, M) :-
    maplist(max_list, L, Ms),
    max_list(Ms, M).





compare_l([],[]).
compare_l([H|T],[H|T1]):-
            compare_l(T,T1).
compare_m([],[]).
compare_m([H|T],[H1|T1]):-
		\+compare_l(H,H1),
		compare_m(T,T1).
acceptable_distribution(G):-
                 trans(G,GT1),
				 compare_m(G,GT1).
				 





row_col_match(G):-
         trans(G,G1),
		 helpchacha(G,G1).
helpchacha([],_).		 
helpchacha([H|T],G1):-
		member(H,G1),
		helpchacha(T,G1).





acceptable_permutation(L,R):-
                         permutation(L,R1),
						 hopa(L,R1,R).
hopa([],[],_).
hopa([H|T],[H1|T1],[H1|T1]):-
                   H\==H1,
				   hopa(T,T1,T1).








	
trans([], []).
trans([F|Fs], Ts) :-
    trans(F, [F|Fs], Ts).

trans([], _, []).
trans([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        trans(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

%OR	
trans1([], []).		   
trans1([[H|T] |Tail], [[H|NT] |NTail]) :- 
	            firstCol(Tail, NT, Rest), 
				trans1(Rest, NRest), 
				firstCol(NTail, T, NRest).
firstCol([], [], []).
firstCol([[H|T] |Tail], [H|Col], [T|Rows]) :- 
                firstCol(Tail, Col, Rows).

	

	

distinct_rows(M):-
               distinct_rows_helper(M).
			   
distinct_rows_helper([_]).			   
distinct_rows_helper([H1,H2|T]):- 
                            \+compare_l(H1,H2),
                            distinct_rows_helper([H1|T]),distinct_rows_helper([H2|T]).


distinct_columns(M):-
                 trans(M,M1),
				 distinct_rows(M1).
				 
							
helsinki(N,G):-
			   grid_gen(N,G),
			   
			   check_num_grid(G),
			   acceptable_distribution(G),
			   distinct_rows(G),
			   distinct_columns(G),
			   row_col_match(G).