:- ['tools.pro'].
:-use_module(library(clpfd)).

:- multifile clpfd:dispatch_global/4.

 :- attribute ftAttr/2 , lstAttr/1.
/* tools */


        clpfd:dispatch_global(and(P,LF), state(_), state(_), Actions) :-
            and_solver(P,  LF,Actions).  


/*

 A in 0..1, B in 0..1, C in 0..1,put_atts(FT1 ,ftAttr(_,A)),
           put_atts(FT2 ,ftAttr(_,B)),
           put_atts(FT3 ,ftAttr(_,C)),
and(FT1,[FT2,FT3]), B = 1.
*/

        and(P,LF):-
              get_atts(P,ftAttr(_,AttrP)), 
              dom_suspensions(LF,Susp),Susp2 = [val(AttrP),min(AttrP)|Susp],
              fd_global(and(P,LF), state(_), Susp2).

        
         and_solver(P,LF,Actions) :-
                get_atts(P,ftAttr(_,StP)),
                filter(LF,_,N,M),

             ( M > 0                   -> Actions = [exit |Ps], ex_eq([P |LF],0,Ps)
             ; StP == 0                 -> Actions = [exit |Ps], ex_eq(LF,0,Ps)
             ; fd_min(StP,K), K > 0     -> Actions = [exit |Ps],ex_neq(LF,0,Ps)
             ; N > 0                   -> Actions = [exit |Ps], ex_neq([P |LF],0,Ps)
             ; Actions = []
             ).
          

        clpfd:dispatch_global(or(P,LF), state(_), state(_), Actions) :-
            or_solver(P,  LF,Actions).     
/*
A in 0..1, B in 0..1, C in 0..1,put_atts(FT1 ,ftAttr(_,A)),
           put_atts(FT2 ,ftAttr(_,B)),
           put_atts(FT3 ,ftAttr(_,C)),
or(FT1,[FT2,FT3]). 
 */
        or(P,LF):-       
              get_atts(P,ftAttr(_,AttrP)), 
              dom_suspensions(LF,Susp),Susp2 = [val(AttrP),min(AttrP)|Susp],
              fd_global(or(P,LF), state(_), Susp2).

        
               or_solver(P,  LF0,Actions):-
                 get_atts(P,ftAttr(_,StP)),
               filter(LF0,LF,N,M),
      
               length(LF0,L),/* N : nbre de 0 dans la liste */
       
             ( StP == 0                                              -> Actions = [exit |Ps],ex_eq(LF0,0,Ps)
             ; fd_min(StP,K), K > 0, N > 1                           -> Actions = [exit]
             ; fd_min(StP,K), K > 0,L2 is  L - 1 ,M == L2, N==0      ->Actions = [exit |Ps],or_eq(LF,Ps) /* il faut au minimum un zero dans la liste */
             ; N > 0                                                -> Actions = [exit |Ps1],ex_neq([P], 0, Ps1)
             ; M == L                                               -> Actions = [exit |Ps1],ex_eq([P], 0, Ps1)
             ; Actions = []
             ).
          

        clpfd:dispatch_global(xor(P,LF), state(_), state(_), Actions) :-
            xor_solver(P,  LF,Actions).           
/*
A in 0..1, B in 0..1, C in 0..1, D in -1..0,put_atts(FT1 ,ftAttr(_,A)),
           put_atts(FT2 ,ftAttr(_,B)),
           put_atts(FT3 ,ftAttr(_,C)),  put_atts(FT4 ,ftAttr(_,D)),
xor(FT1,[FT2,FT3,FT4]). 
 */
printlst22([A|R]):-
         get_atts(A,ftAttr(_,L)),
         write(A), write('   '), write(L), write('\n'),printlst22(R).

printlst22([]).

        xor(P,LF):-
              get_atts(P,ftAttr(_,AttrP)), 
              dom_suspensions(LF,Susp),Susp2 = [val(AttrP),min(AttrP)|Susp],
              fd_global(xor(P,LF), state(_), Susp2).

         xor_solver(P, LF0,Actions) :-
               get_atts(P,ftAttr(_,StP)),
               filter(LF0,LF,N,M) ,
  
                 length(LF0,L),/* N : nbre de 1 dans la liste , M nbre de O*/
           
             ( StP == 0                                  -> Actions = [exit |Ps], ex_eq(LF0,0,Ps)
             ; N > 1                                    -> Actions = [fail]
             ; N == 1                                   -> Actions = [exit |Ps3],ex_neq([P], 0, Ps2), ex_eq(LF, 0, Ps),append(Ps,Ps2,Ps3) /* tous les éléments de la liste sont égaux à -1 */
             ; L2 is L-1, M == L2, fd_min(StP,K), K > 0  -> Actions = [exit |Ps3],ex_neq([P], 0, Ps2), ex_neq(LF, 0, Ps),append(Ps,Ps2,Ps3) 
             ; M == L                                   -> Actions = [exit |Ps1], fdset_singleton(Set, 0),Ps1 = [P in_set Set]
             ; Actions = []
             ).


        clpfd:dispatch_global(mand(P,LF), state(_), state(_), Actions) :-
            mand_solver(P,  LF,Actions).


       mand(P,LF):-
              get_atts(P,ftAttr(_,AttrP)), 
              dom_suspensions(LF,Susp),Susp2 = [val(AttrP),min(AttrP)|Susp],
              fd_global(mand(P,LF), state(_), Susp2).

        mand_solver(P, LF,Actions) :-
                  get_atts(P,ftAttr(_,StP)),
                filter(LF,_,N,M),

             ( M > 0                   -> Actions = [exit |Ps], ex_eq([P |LF],0,Ps)
             ; StP == 0                 -> Actions = [exit |Ps], ex_eq(LF,0,Ps)
             ; fd_min(StP,K), K > 0     -> Actions = [exit |Ps],ex_neq(LF,0,Ps)
             ; N > 0                   -> Actions = [exit |Ps], ex_neq([P |LF],0,Ps)
             ; Actions = []
             ).

/*
A in 0..1, B in 0..1put_atts(FT1 ,ftAttr(_,A)),
           put_atts(FT2 ,ftAttr(_,B)),mutex(FT1,FT2).
 */

   



        mutex(A,B):-       
                get_atts(A,ftAttr(_,StA)),
                get_atts(B,ftAttr(_,StB)),              
                fd_global(mutex(A,B), state(_), [dom(StA),dom(StB)]).
        
        mutex_solver(A,B, Actions) :-
                get_atts(A,ftAttr(_,StA)),
                get_atts(B,ftAttr(_,StB)),
                (
                StA == 1 -> Actions = [exit,StB = 0] ;
                StB == 1 -> Actions = [exit,StA = 0] ;
                StA == 0 -> Actions = [exit] ;
                StB == 0 -> Actions = [exit] ;
                Actions = []
                ).

        clpfd:dispatch_global(opt(P,LF), state(_), state(_), Actions) :-
             opt_solver(P,  LF,Actions).

       opt(P,LF):-
              get_atts(P,ftAttr(_,AttrP)), 
              dom_suspensions(LF,Susp),Susp2 = [val(AttrP),min(AttrP)|Susp],
              fd_global(opt(P,LF), state(_), Susp2).

        opt_solver(P, LF,Actions) :- 
               get_atts(P,ftAttr(_,StP)),
              filter(LF,_,N,_),        
             ( N > 0                 ->  Actions = [exit|Ps], ex_neq([P],0,Ps)
             ; StP == 0               ->  Actions = [exit|Ps], ex_eq(LF,0,Ps)
             ; fd_min(StP,K), K > 0   ->  Actions = [exit]
             ; Actions = []
             ).


        clpfd:dispatch_global(card(Min,Max,P,LF),state(_),state(_),Actions):-
             card_solver(Min,Max,P,LF,Actions).
       

        card(Min,Max,P,LF):-
              get_atts(P,ftAttr(_,AttrP)), 
              dom_suspensions(LF,Susp),Susp2 = [val(AttrP),min(AttrP)|Susp],
              fd_global(and(P,LF), state(_), Susp2).

        card_solver(Min,Max,P, LF,Actions) :-
                  get_atts(P,ftAttr(_,StP)),
                filter(LF,LF2,N,_),
                (
                 StP == 0                           -> Actions = [exit|Ps], ex_eq(LF,0,Ps);
                StP == 1, N > Max                  -> Actions = [fail];
                 StP == 1, N < Min, D is Min - N,length(LF2,A), D > A 
                                                  -> Actions = [fail];
                 StP == 1, N == Max                 -> Actions = [exit|Ps], ex_eq(LF2,0,Ps);
                 StP == 1, N < Min, D is Min - N,length(LF2,D) 
                                                  -> Actions = [exit|Ps], ex_eq(LF2,1,Ps);
                 Actions = []
                 ).


        clpfd:dispatch_global(require(A,B), state(_), state(_), Actions) :-
            require_solver(A, B,Actions).
        

         require(A,B):-
                get_atts(A,ftAttr(_,StA)),
                get_atts(B,ftAttr(_,StB)),              
                fd_global(require(A,B), state(_), [dom(StA),dom(StB)]).

         require_solver(A, B,Actions) :-
                get_atts(A,ftAttr(_,StA)),
                get_atts(B,ftAttr(_,StB)),
               (
                StB == 0 -> Actions =  [exit, StA = 0];   
                StB == 1 -> Actions =  [exit];     
                StA == 0 -> Actions =  [exit];   
                StA == 1 -> Actions =  [exit, StB = 1];       
                Actions = []
              ).



        clpfd:dispatch_global(mutex(A,B), state(_), state(_), Actions) :-
            mutex_solver(A,B,Actions).
                        
 
                        
        clpfd:dispatch_global(lineConstraint(Li,Ctr,FT),state(_),state(_),Actions) :-
               lineConstraint_solver(Li,Ctr,FT,Actions).

        clpfd:dispatch_global(monitoring(K,_),state(I0),state(I),Actions) :-
                monitoring_solver(K,I0,I,Actions).

        clpfd:dispatch_global(pw(I,L1,L2,V1,V2), state, state, Actions) :-
                pw_solver(I,L1,L2,V1,V2,Actions).


        clpfd:dispatch_global(monitoringPaire(Val1,Val2,Index,L1,L2),state(_),state(_),Actions) :-
                monitoringPaire_solver(Val1,Val2,Index,L1,L2,Actions).

      

        copy([],[]).
        copy([A|R],[A|R2]):- 
                copy(R,R2).

     dom_suspensions([], []).
     dom_suspensions([X|Xs], [min(AttrX),val(AttrX)|Susp]) :-
              get_atts(X,ftAttr(_,AttrX)), 
             dom_suspensions(Xs, Susp).


			
				
/*
Permet de visualiser le nombre de paires inscrites dans la matrice
*/
        monitoring(K,I):-
                dom_suspensionsDomains(I,Susp,[]),
                fd_global(monitoring(K,I), state(I), Susp).

        monitoring_solver(K,I0,I,Actions):-
		
	                monitoring_filter(I0,I2,N),
              length(I0,Li),
            	   copy(I2,I),
                   write('I  : '),writeln(I),
                 open('/home/hervieu/monitoringIFF.txt', append, Stream),write(Stream,Li),write(Stream,','),write(Stream,K),nl(Stream),close(Stream) ,      
                 (  N == 0 -> Actions = [exit]
                  ;Actions = []).


        monitoring_filter([A|R],I2,N):- 
               number(A), 
               ! ,
               monitoring_filter(R,I2,N).

        monitoring_filter([A|R],[A|I2],N2) :- 
               monitoring_filter(R,I2,N),
               N2 is N +1 .
        
        monitoring_filter([],[],0).


        lineConstraint(Li,Ctr,FT):-
                dom_suspensionsDomains(Li,Susp,[]),
                fd_global(lineConstraint(Li,Ctr,FT), state(Li), Susp).
        

        cpDomain([],[],[]).     

        cpDomain([Li0|Li0R],[Li|LiR],[Li in_set Set|Ps0]) :-
                fd_set(Li0,Set), 
                cpDomain(Li0R,LiR,Ps0).

 

        ligne_Filter([A|B],I) :- 
                number(A),
                !,
                ligne_Filter(B,I).
        
        ligne_Filter([_|B],I) :- 
                ligne_Filter(B,I1),
                I is I1 +1.
        
        ligne_Filter([],0).



     dom_suspensionsDomains([], Tail,Tail).

     dom_suspensionsDomains([X|Xs], [dom(X)|Susp],Tail) :-
             dom_suspensionsDomains(Xs, Susp,Tail).

     dom_suspensionsValues([], Tail,Tail).
     dom_suspensionsValues([X|Xs], [val(X)|Susp],Tail) :-
             dom_suspensionsValues(Xs, Susp,Tail).






   








   





/*
Impose la présence d'une valeur supérieur à 0
*/
       or_eq([_|LFr],Ps) :-
                 or_eq(LFr,Ps).
               
       or_eq([ALF|LFr],[LF in_set Set|Ps]) :-
               get_atts(ALF,ftAttr(_,LF)),
                fdset_singleton(Set0, 0),
                fdset_complement(Set0, Set),
                or_eq2(LFr,Ps).
                                  
       or_eq2(_,[]).        

        filter([],[],0,0).
/*
Compte le nolmbre d'occurence de -1 et de 0, rend la liste des variables non instanciées
       N : nbre de valeur supérieure à -1
       M : nbre de -1
*/
        filter([LF|LFs],LF0,N,M) :- 
                 get_atts(LF,ftAttr(_,J)),
                 fd_min(J,K), K > 0 ,!,
                 filter(LFs,LF0,N1,M), N is N1 +1 .
               
     
        filter([LF|LFs],LF0,N,M) :-  
                get_atts(LF,ftAttr(_,J)),         
                J == 0,!,
               filter(LFs,LF0,N,M1), M is M1 +1 .

        filter([LF|LFs],LF0,N,M):-
                LF0 = [LF|LF02],
                filter(LFs,LF02,N,M).



                

                       


% exactly.pl
     

     % rules [1,2]: filter the X's, decrementing N
     ex_filter([], [], N, N, _).
     ex_filter([X|Xs], Ys, L, N, I) :- X==I, !,
             M is L-1,
             ex_filter(Xs, Ys, M, N, I).
     ex_filter([X|Xs], Ys0, L, N, I) :-
             fd_set(X, Set),
             fdset_member(I, Set), !,
             Ys0 = [X|Ys],
             ex_filter(Xs, Ys, L, N, I).
     ex_filter([_|Xs], Ys, L, N, I) :-
             ex_filter(Xs, Ys, L, N, I).
     
     % rule [3]: all must be neq I
     ex_neq(Xs, I, Ps) :-
             fdset_singleton(Set0, I),
             fdset_complement(Set0, Set),
             eq_all(Xs, Set, Ps).
     
     % rule [4]: all must be eq I
     ex_eq(Xs, I, Ps) :-
             fdset_singleton(Set, I),
             eq_all(Xs, Set, Ps).
     
     eq_all([], _, []).
     eq_all([X|Xs], Set, [StX in_set Set|Ps]) :-
             get_atts(X,ftAttr(_,StX)),      
             eq_all(Xs, Set, Ps).

      contrainte(A,B,L1,L2,R)  :-
             dom_suspensions2(L1,L2, Susp),
             fd_global(contrainte(A,B,L1,L2,R) , state(L1,L2), Susp).
     
     dom_suspensions2([], [], []).
     dom_suspensions2([XA|XAs],[XB|XBs], [dom(XA),dom(XB)|Susp]) :-
             dom_suspensions2(XAs,XBs, Susp).
     
 
% pairwise(I, (L1,L2), (V1,V2)) is true iff L1[I]=V1 AND L2[I]=V2
% Prerequisites : L1,L2 are both lists of FD_var with the same size, I,V1,V2 are FD_var
%                 --- All the FD_vars MUST have a bounded domains ---
% Warning : all the FD_var are supposed to have numeric bounds (inf,sup not managed)
% Maintain bound-consistency on V1,V2 and arc-consistency on I  (To Be Verified)

% Designed for sicstus 4.1.1
% Date : June 2010
% Authors : Arnaud Gotlieb -- INRIA Rennes Bretagne Atlantique
%           Aymerick Hervieu -- INRIA Rennes Bretagne Atlantique
% usage:

% ?- domain([X1,X2,X3,V1,V2],-1,0), pairwise(I, ([X1,X2,X3],[-1,0,-1]), (V1,V2)), I = 3.
%     V1 = X3, V2 = -1.
% ?- pairwise(I, ([X1,X2,X3],[0,-1,-1]), (V1,V2)), V1 = 0, X1 #< 0.
%     I in 2..3, %%  could deduce also V2 = -1 but at the cost of an awakening - suspended ctr 
% ?- pairwise(I, ([X1,X2,X3], [Y1,Y2,Y3]), (V1,V2)), X1=0, X2=0,X3=0.
%     V1 = 0.
% ?- pairwise(I, ([0,-1,-1], [Y1,Y2,Y3]), (0,-1)).
%     I = 1, Y1 = -1.
% ?- pairwise(I, ([0,-1,-1], [Y1,Y2,Y3]), (0,-1)), Y1=0.
%    no
% ?- domain([X1,X2,X3,V1,V2],0,2), pairwise(I, ([X1,X2,X3],[0,0,0]), (V1,V2)), X1 #< 2, X2 #< 2, V1=2.
%    X1 in 0..1,
%    X2 in 0..1,
%    X3 = 2,
%    V2 = 0


:- ensure_loaded(library(clpfd)).
:- ensure_loaded(library(lists)).
:- multifile clpfd:dispatch_global/4.

pairwise(I, (L1,L2), (V1,V2)) :-
        lists:is_list(L1), lists:is_list(L2),
        minmax_suspensions(L1, Susp1, 0, Len1),
        minmax_suspensions(L2, Susp2, 0, Len2),
        Len1 == Len2,
        I in 1 .. Len1,
        !,
        lists:append(Susp1, Susp2, Susp3),
        fd_global(pw(I,L1,L2,V1,V2), state, [dom(I),minmax(V1),minmax(V2)|Susp3]).
%pairwise(_, _, _) :-
%        write('Pairwise constraint problem').

/*
clpfd:dispatch_global(pw(I,L1,L2,V1,V2), state, state, Actions) :-
        pw_solver(I,L1,L2,V1,V2,Actions).
*/
pw_solver(I, L1, L2, V1, V2, Actions) :-
        number(I),
        !,
        lists:nth1(I,L1,X1),
        lists:nth1(I,L2,X2),
        Actions = [exit, call(V1=X1), call(V2=X2)].
pw_solver(I, L1, L2, V1, V2, Actions) :-
%        nl,nl,
        fd_set(I, SetI), fd_set(V1, SetV1), fd_set(V2, SetV2),
        fdset_to_list(SetI, ListI),
        pw_rec(ListI, SetI, L1, L2, SetV1, SetV2, NSetI, [], L1Union, [], L2Union),
        fdset_intersection(SetV1, L1Union, NSetV1),
%        write(L1Union),nl,
        fdset_intersection(SetV2, L2Union, NSetV2),
%        write(L2Union),nl,
        build_actions(NSetI, NSetV1, NSetV2, I,L1, L2, V1,V2,Actions).

pw_rec([], SetI, L1, L2, SetV1, SetV2, SetI, L1U, L1U, L2U, L2U):- !.
pw_rec([Val|S], SetI, L1, L2, SetV1, SetV2, NSetI, L1U, NL1U, L2U, NL2U) :-
        lists:nth1(Val, L1, X1),
        lists:nth1(Val, L2, X2),
        fd_set(X1, SetX1),
        fd_set(X2, SetX2),
        test_disjoint(SetX1, SetV1, Val, SetI, SetI1, L1U, L1Ua),
        test_disjoint(SetX2, SetV2, Val, SetI1, SetI2, L2U, L2Ua),
        pw_rec(S, SetI2, L1, L2, SetV1, SetV2, NSetI, L1Ua, NL1U, L2Ua, NL2U).

test_disjoint(SetX, SetV, Val, SetI, NSetI, LU, LU) :-
        fdset_disjoint(SetX, SetV),
        !,
        fdset_del_element(SetI, Val, NSetI).
test_disjoint(SetX, SetV, Val, SetI, SetI, LU, NLU) :-
        fdset_union(LU, SetX, NLU).

build_actions(NSetI, NSetV1, NSetV2, I,L1,L2,V1,V2,Actions):-
        ( empty_fdset(NSetI) ; empty_fdset(NSetV1) ; empty_fdset(NSetV2)), 
        !,
        Actions = [fail].
build_actions(NSetI, NSetV1, NSetV2, I,L1, L2, V1,V2,Actions):-
        fdset_singleton(NSetI, Ival),
        !,
        lists:nth1(Ival, L1, X1),
        lists:nth1(Ival, L2, X2),
        Actions = [exit, call(I=Ival), call(V1=X1), call(V2=X2)].


%build_actions(NSetI, NSetV1, NSetV2, I,L1, L2, V1,V2,Actions):-
%        ( fdset_singleton(NSetV1, V1val) -> (L1 = [V1=V1val],X=true) ; L1 = [] ),
%        ( fdset_singleton(NSetV2, V2val) -> (L2 = [V2=V2val],X=true) ; L2 = [] ),
%        ( X == true -> lists:append(L1,L2,Actions) ; fail ),
%        !.
build_actions(NSetI, NSetV1, NSetV2, I,L1, L2, V1,V2,Actions):-
        Actions = [I in_set NSetI, V1 in_set NSetV1, V2 in_set NSetV2].
        
%=====================================UTILS============================================
minmax_suspensions([], [], Len, Len).
minmax_suspensions([X|L], [minmax(X)|SL], Len, Len1) :-
        LenI is Len+1,
        minmax_suspensions(L,SL, LenI,Len1).



