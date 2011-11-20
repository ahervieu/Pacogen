:- ['tools.pro','globalConstraints.pro'].


:-use_module(library(samsort)).


 :- attribute ftAttr/2 , lstAttr/1.



/*
sort(L1,L2)
predicat de tri 
*/



borneRec([A|R]):-    
        get_atts(A,ftAttr(_,I)),
        I in 0..1,
        borneRec(R).

borneRec([]).


my_sort(L1,L2):-samsort(crit_dpthOp,L1,L2).



crit_dpthOp(A,B):-
        
         get_atts(A,lstAttr([OpA,Dpth_opA,DpthA])),
         get_atts(B,lstAttr([OpB,Dpth_opB,DpthB])),
         Dpth_opA > Dpth_opB.

crit_dpthFt(A,B):-
        
         get_atts(A,lstAttr([OpA,Dpth_opA,DpthA])),
         get_atts(B,lstAttr([OpB,Dpth_opB,DpthB])),
         DpthA > DpthB.

crit_op(A,B):-
 
         get_atts(A,lstAttr([OpA,Dpth_opA,DpthA])),
         get_atts(B,lstAttr([OpB,Dpth_opB,DpthB])),
        operatorComp(OpA,OpB).

operatorComp(xor,and).
operatorComp(xor,opt).
operatorComp(xor,or).
operatorComp(xor,null).

operatorComp(and,opt).
operatorComp(and,or).
operatorComp(and,null).

operatorComp(or,opt).
operatorComp(or,null).
        
operatorComp(opt,null).

/*
pairwiseGenerator(LstFeature,Matrix,PWCTRLST,RANKLST).

A in -1..0, B in -1.. 0 , C in -1..0, LstFeature = [A,B,C],CA = [A1,A2,A3], CB = [B1,B2,B3], CC =[C1,C2,C3] , Matrix = [CA,CB,CC], 
pairwiseGenerator(LstFeature,Matrix,PWCTRLST,RANKLST), 
[pairwise(-1,-1,CA,CB,_A),pairwise(-1,0,CA,CB,_B),pairwise(0,-1,CA,CB,_C),pairwise(0,0,CA,CB,_D),
pairwise(-1,-1,CA,CC,_E),pairwise(-1,0,CA,CC,_F),pairwise(0,-1,CA,CC,_G),pairwise(0,0,CA,CC,_H),
pairwise(-1,-1,CB,CC,_I),pairwise(-1,0,CB,CC,_J),pairwise(0,-1,CB,CC,_K),pairwise(0,0,CB,CC,_L)]
RANKLST = [[_A,_B,_C,_D],[_E,_F,_G,_H],[_I_,_J,_K,_L]].

A in -1..0, B in -1.. 0 , C in -1..0,mutex(A,B),mutex(B,C), LstFeature = [A,B,C],CA = [A1,A2,A3], CB = [B1,B2,B3], CC =[C1,C2,C3] , Matrix = [CA,CB,CC], 
pairwiseGenerator(LstFeature,Matrix,PWCTRLST,RANKLST)
*/

pairwiseGenerator([A|R],[CA|CR],PWCTRLST,RANKLST3) :-
     R\= [],   pairwiseGenerator2(A,R,CA,CR,PWCTRLST1,RANKLST1),
        pairwiseGenerator(R,CR,PWCTRLST2,RANKLST2),
        append(PWCTRLST1,PWCTRLST2,PWCTRLST) ,
        append(RANKLST1,RANKLST2,RANKLST3) .

pairwiseGenerator([_|[]],_,[],[]).

        
        /*
        
        
| ?- A in -1..0, B in -1.. 0 , C in -1..0, D in -1..0,  LstFeature = [A,B,C],CA = [A1,A2,A3], CB = [B1,B2,B3], CC =[C1,C2,C3], CD = [D1,D2,D3] , Matrix = [CA,CB,CC,DD], 
pairwiseGenerator(LstFeature,Matrix,PWCTRLST,RANKLST).
CA = [A1,A2,A3],
CB = [B1,B2,B3],
CC = [C1,C2,C3],
Matrix = [[A1,A2,A3],[B1,B2,B3],[C1,C2,C3]],
RANKLST = [[_A,_B,_C,_D],[_E,_F,_G,_H],[_I,_J,_K,_L]],
PWCTRLST = [pairwise(-1,-1,[A1,A2,A3],[B1,B2,B3],_A),pairwise(-1,0,[A1,A2,A3],[B1,B2,B3],_B),pairwise(0,-1,[A1,A2,A3],[B1,B2,B3],_C),pairwise(0,0,[A1,A2,A3],[B1,B2,B3],_D),pairwise(-1,-1,[A1,A2,A3],[C1,C2,C3],_E),pairwise(-1,0,[A1,A2,A3],[C1,C2,C3],_F),pairwise(0,-1,[A1,A2,A3],[C1,C2,C3],_G),pairwise(0,0,[A1,A2|...],[C1,C2|...],_H),pairwise(-1,-1,[B1|...],[C1|...],_I),pairwise(...)|...],
LstFeature = [A,B,C],
A in-1..0,
B in-1..0,
C in-1..0 ? ;
no
% source_info
            */

pairwiseGenerator2(A,[B|R],CA,[CB|CR],PWCTRLST,RANKLST) :-
        pairwiseConstraintsGenerator(A,B,CA,CB,PWCTRLST1,RANKLST1),
        pairwiseGenerator2(A,R,CA,CR,PWCTRLST2,RANKLST2),
        append(PWCTRLST1,PWCTRLST2,PWCTRLST) ,
        RANKLST = [RANKLST1|RANKLST2].

pairwiseGenerator2(_,[],_,[],[],[]).


/*

Starting point : Takes Two finite domains variables and generate corresponding pw constraints
pairwsiseConstraintsGenerator(+A,+B,+CA,+CB,-PWCTRLST,-RANKLST)
      A : finite domain variable (in -1 .. 0)
      B : finite domain variable (in -1 .. 0)
      CA : A matrix Column
      CB : B matrix Column
      PWCTRLST : list of pairwiseconstraints
      RANKLST : list of matrix ranks      
      
      
A in -1..0, B in -1..0, CA = [A1,A2,A3], CB = [B1,B2,B3],pairwiseConstraintsGenerator(A,B,CA,CB,PWCTRLST,RANKLST),
PWCTRLST = [pairwise(-1,-1,CA,CB,I1),pairwise(-1,0,CA,CB,I2),pairwise(0,-1,CA,CB,I3),pairwise(0,0,CA,CB,I4)],
RANKLST = [I1,I2,I3,I4]. 
Order doesn't matter                 
*/

pairwiseConstraintsGenerator(A,B,CA,CB,PWCTRLST2,RANKLST) :-
                domainToList(A,LA),
                avalueGenerator(A,LA,B,CA,CB,PWCTRLST,RANKLST),
                             pairvalidator(A,B,PWCTRLST,PWCTRLST2).
        


/*
Test
                pairvalidator(A,B,PWCTRLST,PWCTRLST2).
   
   | ?- A in -1..0, B in -1..0, CA = [A1,A2,A3], CB = [B1,B2,B3],pairwiseConstraintsGenerator(A,B,CA,CB,PWCTRLST,RANKLST).
CA = [A1,A2,A3],
CB = [B1,B2,B3],
RANKLST = [_A,_B,_C,_D],
PWCTRLST = [pairwise(-1,-1,[A1,A2,A3],[B1,B2,B3],_A),pairwise(-1,0,[A1,A2,A3],[B1,B2,B3],_B),pairwise(0,-1,[A1,A2,A3],[B1,B2,B3],_C),pairwise(0,0,[A1,A2,A3],[B1,B2,B3],_D)],
A in-1..0,
B in-1..0 ? ;
no
% source_info
            
Constraint handling :
                    % source_info
| ?- A in -1..0, B in -1..0,mutex(A,B), CA = [A1,A2,A3], CB = [B1,B2,B3],pairwiseConstraintsGenerator(A,B,CA,CB,PWCTRLST,RANKLST).
CA = [A1,A2,A3],
CB = [B1,B2,B3],
RANKLST = [_A,_B,_C],
PWCTRLST = [pairwise(-1,-1,[_D,_E,_F],[_G,_H,_I],_A),pairwise(-1,0,[_D,_E,_F],[_G,_H,_I],_B),pairwise(0,-1,[_J,_K,_L],[_M,_N,_O],_C)],
A in-1..0,
B in-1..0 ? ;
no
% source_info
            OK
*/

/*
pairvalidator(FTA,FTB,LstPWCtr)
retire les paire interdites (au cas ou)

*/
pairvalidator(FTA,FTB,[A|R],[R1|R2]):-pairvalidator2(FTA,FTB,A,R1),pairvalidator(FTA,FTB,R,R2).

pairvalidator(FTA,FTB,[],[]).

pairvalidator2(FTA,FTB,pairwise(I,(CA,CB),(VA,VB)),[]):- \+(\+(\+(callRec([FTA#=VA, FTB#=VB])))),write('invalidPair').
pairvalidator2(FTA,FTB,Ctr,Ctr):-Ctr=pairwise(I,(CA,CB),(VA,VB)), \+(\+(callRec([FTA#=VA, FTB#=VB]))).
/*
domainToList(+A,-L).
A in -1..0, domainToList(A,L),L =[-1,0].
No backtrack        
*/

domainToList(A,L) :-
          fd_dom(A,RA),range_to_fdset(RA,SA),fdset_to_list(SA,L).

/*
| ?- A in -1..0, domainToList(A,L),L =[-1,0].
L = [-1,0],
A in-1..0 ? 
*/



/*
avalueGenerator(LA,B,CA,CB,PWCTRLST,RANKLST).
LA : int List
      B : finite domain variable (in -1 .. 0)
      CA : A matrix Column
      CB : B matrix Column
      PWCTRLST : list of pairwiseconstraints
      RANKLST : list of matrix ranks    
      
      A = [-1,0], B in -1..0, CA = [A1,A2,A3], CB = [B1,B2,B3],avalueGenerator(A,B,CA,CB,PWCTRLST,RANKLST),
PWCTRLST = [pairwise(-1,-1,CA,CB,I1),pairwise(-1,0,CA,CB,I2),pairwise(0,-1,CA,CB,I3),pairwise(0,0,CA,CB,I4)],
RANKLST = [I1,I2,I3,I4]. 
*/

avalueGenerator(_,[],_,_,_,[],[]).
avalueGenerator(A,[VA|R],B,CA,CB,PWCTRLST,RANKLST) :-
        \+(\+(callRec([A = VA,    pairListGenerator(VA,B,PairList),assert(p1(PairList))]))),
        p1(PairList),pairsGeneretor(PairList,CA,CB,PWCTRLST1,RANKLST1),retract(p1(_)),
        avalueGenerator(A,R,B,CA,CB,PWCTRLST2,RANKLST2),
        append(PWCTRLST1,PWCTRLST2,PWCTRLST),
        append(RANKLST1,RANKLST2,RANKLST).

/* Test : 

| ?-  A in -1..0 ,LA = [-1,0], B in -1..0, CA = [A1,A2,A3], CB = [B1,B2,B3],avalueGenerator(A,LA,B,CA,CB,PWCTRLST,RANKLST).
CA = [A1,A2,A3],
CB = [B1,B2,B3],
LA = [-1,0],
RANKLST = [_A,_B,_C,_D],
PWCTRLST = [pairwise(-1,-1,[_E,_F,_G],[_H,_I,_J],_A),pairwise(-1,0,[_E,_F,_G],[_H,_I,_J],_B),pairwise(0,-1,[_K,_L,_M],[_N,_O,_P],_C),pairwise(0,0,[_K,_L,_M],[_N,_O,_P],_D)],
A in-1..0,
B in-1..0 ? ;
no
% source_info
 */

/*
pairListGenerator(VA,B,PairList) 
Va : int
 B : FiniteDomaine Variable
 PairList : list of pairs between VA and B
VA = 1, B in -1..0, pairListGenerator(VA,B,PairList) ,PairList = [[1,-1],[1,0]].
*/
pairListGenerator(VA,B,PairList) :-
        domainToList(B,LB),
        pairListUnitGenerator(VA,LB,PairList).

/*
| ?- VA = 1, B in -1..0, pairListGenerator(VA,B,PairList).
VA = 1,
PairList = [[1,-1],[1,0]],
B in-1..0 ? ;
no
% source_info
*/

/*
pairListUnitGenerator(VA,LB,PairList) 
Va : int
 LB : int List
 PairList : list of pairs between VA and B
VA = 1, LB = [-1,0], pairListUnitGenerator(VA,LB,PairList) ,PairList = [[-1,-1],[-1,0]].
*/

 pairListUnitGenerator(_,[],[]).
 pairListUnitGenerator(VA,[B|R],[[VA,B]|Res]):-
                      pairListUnitGenerator(VA,R,Res).                              

/*
  
| ?- VA = 1, LB = [-1,0], pairListUnitGenerator(VA,LB,PairList).
LB = [-1,0],
VA = 1,
PairList = [[1,-1],[1,0]] ? ;
no
% source_info
| ?- */


/*
 pairsGeneretor(PairList,CA,CB,PWCTRLST,RANKLST).
        PairList : [[-1,-1],[1,1]...]
      CA : A matrix Column
      CB : B matrix Column
      PWCTRLST : list of pairwiseconstraints
      RANKLST : list of matrix ranks              

PairList = [[1,1],[0,0]], CA = [A1,A2,A3], CB = [B1,B2,B3],pairsGeneretor(PairList,CA,CB,PWCTRLST,RANKLST),
PWCTRLST = [pairwise(0,0,CA,CB,I1),pairwise(1,1,CA,CB,I2)],
RANKLST = [I1,I2,I3,I4]. 
Order doesn't matter        
No backtrack         

*/

 pairsGeneretor([],_,_,[],[]).
 pairsGeneretor([[VA,VB]|R],CA,CB,PWCTRLST3,RANKLST3) :-
        PWCTRLST = [pairwise(I,(CA,CB),(VA,VB))],
        RANKLST = [I],
        pairsGeneretor(R,CA,CB,PWCTRLST2,RANKLST2),
        append(PWCTRLST,PWCTRLST2,PWCTRLST3),
        append(RANKLST,RANKLST2,RANKLST3).

/*
| ?- PairList = [[1,1],[0,0]], CA = [A1,A2,A3], CB = [B1,B2,B3],pairsGeneretor(PairList,CA,CB,PWCTRLST,RANKLST).
CA = [A1,A2,A3],
CB = [B1,B2,B3],
RANKLST = [_A,_B],
PairList = [[1,1],[0,0]],
PWCTRLST = [pairwise(1,1,[A1,A2,A3],[B1,B2,B3],_A),pairwise(0,0,[A1,A2,A3],[B1,B2,B3],_B)] ? ;
no
% source_info
| ?- 
*/



contrainteFD(M,LF) :- nbligne(M,I), contrainteFD2(M,LF,I).

contrainteFD1(M,LF,I) :-I > 0, ligne(M,I,L), validLine(LF,L),I2 is I -1, contrainteFD1(M,LF,I2).
contrainteFD1(_,_,0).



contrainteFD2(M,LF,I) :-I > 0, ligne(M,I,L), validLine(LF,L),I2 is I -1, contrainteFD1(M,LF,I2).
contrainteFD2(_,_,0).

/*

LF = [A,B,C],domain(LF,-1,0),mutex(A,B),Values = [0,0,0],  validLine(LF,Values)
*/

validLine(LF,Valeur) :-lineConstraint(Valeur,_,LF).
  

contrainteFDV2(M,LF,CTR,Res):-nbligne(M,I),contrainteFD2V2(M,LF,I,CTR,Res).
        
        
        
contrainteFD2V2(M,LF,I,CTR,R2) :-I > 0, ligne(M,I,L), genCtrRec(LF,CTR,L,Res),I2 is I -1, contrainteFD2V2(M,LF,I2,CTR,R),append(Res,R,R2).
contrainteFD2V2(M,LF,0,CTR,[]).


genCtrRec(LF,[P|Pred],Line, [MappedPred|Res]):-
        genCtr(LF,P,Line, MappedPred),
        genCtrRec(LF,Pred,Line, Res).
        
genCtrRec(_,[],_, []).


genCtr(LF,Pred,Line, MappedPred):-
        Pred =.. Li ,
        Li = [and,Father,Lst],
        mapping(LF,Line,Father,Father2),
        mappingRec(LF,Line,Lst,Lst2),
        MappedPred =.. [and,Father2,Lst2]
        .

genCtr(LF,Pred,Line, MappedPred):-
        Pred =.. Li ,
        Li = [opt,Father,Lst],
        mapping(LF,Line,Father,Father2),
        mappingRec(LF,Line,Lst,Lst2),
        MappedPred =.. [opt,Father2,Lst2]
        .

genCtr(LF,Pred,Line, MappedPred):-
        Pred =.. Li ,
        Li = [or,Father,Lst],
        mapping(LF,Line,Father,Father2),
        mappingRec(LF,Line,Lst,Lst2),
        MappedPred =.. [or,Father2,Lst2]
        .

genCtr(LF,Pred,Line, MappedPred):-
        Pred =.. Li ,
        Li = [xor,Father,Lst],
        mapping(LF,Line,Father,Father2),
        mappingRec(LF,Line,Lst,Lst2),
        MappedPred =.. [xor,Father2,Lst2]
        .

genCtr(LF,Pred,Line, MappedPred):-
        Pred =.. Li ,
        Li = [require,Father,Son],
        mapping(LF,Line,Father,Father2),
        mapping(LF,Line,Son,Son2),
        MappedPred =.. [require,Father2,Son2]
        .
genCtr(LF,Pred,Line, MappedPred):-
       Pred =.. Li ,
        Li = [mutex,Father,Son],
        mapping(LF,Line,Father,Father2),
        mapping(LF,Line,Son,Son2),
        MappedPred =.. [mutex,Father2,Son2]
        .


mappingRec(LF,Line,[A|B],[Res|R]) :-
        mapping(LF,Line,A,Res),mappingRec(LF,Line,B,R).
mappingRec(_,_,[],[]).
        

mapping([A|R1],[B|R2],Ref,B):- Ref == A.
mapping([A|R1],[B|R2],Ref,Res):-mapping(R1,R2,Ref,Res).     




/* variable(M,V) : obtention de la liste des variable de M */
/* variable(M,V) : obtention de la liste des variable de M */

nvariable2(_,[],0).
nvariable2([A|R],L,I):-I \=0, I2 is I -1, nvariable2(R,L2,I2),append(A,L2,L).

nvariable(M,L,I):-mytranspose(M,M2),nvariable2(M2,L,I).


submatrix(M,M2,I):-mytranspose(M,MT),submatrix2(MT,M2,I).

submatrix2([A|R],[A|R2],I):- I\=0, I2 is I -1, submatrix2(R,R2,I2).
submatrix2(_,[],0).


