:-use_module(library(samsort)).
:-use_module(library(clpfd)).

 :- attribute ftAttr/2 , lstAttr/1.
  verify_attributes(Var, Other, Goals).
openfile(PairInformations,Attributes,Features,CtrList,MatrixSize,ModelName,ParamSort,Labelling) :- open('model.txt', read, Stream),read(Stream,[PairInformations,Attributes,Features,CtrList,MatrixSize,ModelName,ParamSort,Labelling]),close(Stream).

 user:runtime_entry(start) :-openfile(PairInformations,Attributes,Features,CtrList,MatrixSize,ModelName,ParamSort,Labelling),pwsolver(PairInformations,Attributes,Features,CtrList,MatrixSize,ModelName,ParamSort,Labelling).
test2 :-openfile(PairInformations,Attributes,Features,CtrList,MatrixSize,ModelName,ParamSort,Labelling),pwsolver(PairInformations,Attributes,Features,CtrList,MatrixSize,ModelName,ParamSort,Labelling).
test3 :-openfile(PairInformations,Attributes,Features,CtrList,MatrixSize,ModelName,ParamSort,Labelling),consistance(PairInformations,Attributes,Features,CtrList,MatrixSize,ModelName,ParamSort,Labelling).

/*
/*

 A in 0..1, B in 0..1, C in 0..1,put_atts(FT1 ,ftAttr(_,A)),
           put_atts(FT2 ,ftAttr(_,B)),
           put_atts(FT3 ,ftAttr(_,C)),
and(FT1,[FT2,FT3]), B = 1.
*/
/*
ppwsolver([a(wireless,wireless,[or,1,2]),a(wireless,accu_cell,[null,0,2]),a(wireless,display,[null,0,2]),a(wireless,infrared,[or,1,3]),a(wireless,bluetooth,[or,1,3]),a(wireless,li_ion,[null,0,3]),a(wireless,ni_mh,[null,0,3]),a(wireless,ni_ca,[null,0,3]),a(wireless,color,[null,0,3]),a(wireless,monochrome,[null,0,3]),a(wireless,cellphone,[null,0,1]),a(accu_cell,wireless,[null,0,2]),a(accu_cell,accu_cell,[xor,1,2]),a(accu_cell,display,[null,0,2]),a(accu_cell,infrared,[null,0,3]),a(accu_cell,bluetooth,[null,0,3]),a(accu_cell,li_ion,[xor,1,3]),a(accu_cell,ni_mh,[xor,1,3]),a(accu_cell,ni_ca,[xor,1,3]),a(accu_cell,color,[null,0,3]),a(accu_cell,monochrome,[null,0,3]),a(accu_cell,cellphone,[null,0,1]),a(display,wireless,[null,0,2]),a(display,accu_cell,[null,0,2]),a(display,display,[xor,1,2]),a(display,infrared,[null,0,3]),a(display,bluetooth,[null,0,3]),a(display,li_ion,[null,0,3]),a(display,ni_mh,[null,0,3]),a(display,ni_ca,[null,0,3]),a(display,color,[xor,1,3]),a(display,monochrome,[xor,1,3]),a(display,cellphone,[null,0,1]),a(infrared,wireless,[or,1,3]),a(infrared,accu_cell,[null,0,3]),a(infrared,display,[null,0,3]),a(infrared,infrared,[null,2,4]),a(infrared,bluetooth,[or,1,4]),a(infrared,li_ion,[null,0,4]),a(infrared,ni_mh,[null,0,4]),a(infrared,ni_ca,[null,0,4]),a(infrared,color,[null,0,4]),a(infrared,monochrome,[null,0,4]),a(infrared,cellphone,[null,0,2]),a(bluetooth,wireless,[or,1,3]),a(bluetooth,accu_cell,[null,0,3]),a(bluetooth,display,[null,0,3]),a(bluetooth,infrared,[or,1,4]),a(bluetooth,bluetooth,[null,2,4]),a(bluetooth,li_ion,[null,0,4]),a(bluetooth,ni_mh,[null,0,4]),a(bluetooth,ni_ca,[null,0,4]),a(bluetooth,color,[null,0,4]),a(bluetooth,monochrome,[null,0,4]),a(bluetooth,cellphone,[null,0,2]),a(li_ion,wireless,[null,0,3]),a(li_ion,accu_cell,[xor,1,3]),a(li_ion,display,[null,0,3]),a(li_ion,infrared,[null,0,4]),a(li_ion,bluetooth,[null,0,4]),a(li_ion,li_ion,[null,2,4]),a(li_ion,ni_mh,[xor,1,4]),a(li_ion,ni_ca,[xor,1,4]),a(li_ion,color,[and,0,4]),a(li_ion,monochrome,[and,0,4]),a(li_ion,cellphone,[null,0,2]),a(ni_mh,wireless,[null,0,3]),a(ni_mh,accu_cell,[xor,1,3]),a(ni_mh,display,[null,0,3]),a(ni_mh,infrared,[null,0,4]),a(ni_mh,bluetooth,[null,0,4]),a(ni_mh,li_ion,[xor,1,4]),a(ni_mh,ni_mh,[null,2,4]),a(ni_mh,ni_ca,[xor,1,4]),a(ni_mh,color,[and,0,4]),a(ni_mh,monochrome,[and,0,4]),a(ni_mh,cellphone,[null,0,2]),a(ni_ca,wireless,[null,0,3]),a(ni_ca,accu_cell,[xor,1,3]),a(ni_ca,display,[null,0,3]),a(ni_ca,infrared,[null,0,4]),a(ni_ca,bluetooth,[null,0,4]),a(ni_ca,li_ion,[xor,1,4]),a(ni_ca,ni_mh,[xor,1,4]),a(ni_ca,ni_ca,[null,2,4]),a(ni_ca,color,[and,0,4]),a(ni_ca,monochrome,[and,0,4]),a(ni_ca,cellphone,[null,0,2]),a(color,wireless,[null,0,3]),a(color,accu_cell,[null,0,3]),a(color,display,[xor,1,3]),a(color,infrared,[null,0,4]),a(color,bluetooth,[null,0,4]),a(color,li_ion,[and,0,4]),a(color,ni_mh,[and,0,4]),a(color,ni_ca,[and,0,4]),a(color,color,[null,2,4]),a(color,monochrome,[xor,1,4]),a(color,cellphone,[null,0,2]),a(monochrome,wireless,[null,0,3]),a(monochrome,accu_cell,[null,0,3]),a(monochrome,display,[xor,1,3]),a(monochrome,infrared,[null,0,4]),a(monochrome,bluetooth,[null,0,4]),a(monochrome,li_ion,[and,0,4]),a(monochrome,ni_mh,[and,0,4]),a(monochrome,ni_ca,[and,0,4]),a(monochrome,color,[xor,1,4]),a(monochrome,monochrome,[null,2,4]),a(monochrome,cellphone,[null,0,2]),a(cellphone,wireless,[null,0,1]),a(cellphone,accu_cell,[null,0,1]),a(cellphone,display,[null,0,1]),a(cellphone,infrared,[null,0,2]),a(cellphone,bluetooth,[null,0,2]),a(cellphone,li_ion,[null,0,2]),a(cellphone,ni_mh,[null,0,2]),a(cellphone,ni_ca,[null,0,2]),a(cellphone,color,[null,0,2]),a(cellphone,monochrome,[null,0,2]),a(cellphone,cellphone,[null,0,0])],[
           put_atts(WIRELESS ,ftAttr(wireless,_)),
           put_atts(ACCU_CELL ,ftAttr(accu_cell,_)),
           put_atts(DISPLAY ,ftAttr(display,_)),
           put_atts(INFRARED ,ftAttr(infrared,_)),
           put_atts(BLUETOOTH ,ftAttr(bluetooth,_)),
           put_atts(LI_ION ,ftAttr(li_ion,_)),
           put_atts(NI_MH ,ftAttr(ni_mh,_)),
           put_atts(NI_CA ,ftAttr(ni_ca,_)),
           put_atts(COLOR ,ftAttr(color,_)),
        put_atts(MONOCHROME ,ftAttr(monochrome,_)),
        put_atts(CELLPHONE ,ftAttr(cellphone,1))],[WIRELESS,ACCU_CELL,DISPLAY,INFRARED,BLUETOOTH,LI_ION,NI_MH,NI_CA,COLOR,MONOCHROME,CELLPHONE],[opt(CELLPHONE,[WIRELESS]),and(CELLPHONE,[ACCU_CELL,DISPLAY]), or(WIRELESS,[INFRARED,BLUETOOTH]),xor(ACCU_CELL,[LI_ION,NI_MH,NI_CA]),xor(DISPLAY,[COLOR,MONOCHROME]),  require(BLUETOOTH,LI_ION)],10,'test',crit_dpthOp,[ff]).
**/

test:-pwsolver([a(wireless,wireless,[or,1,2]),a(wireless,accu_cell,[null,0,2]),a(wireless,display,[null,0,2]),a(wireless,infrared,[or,1,3]),a(wireless,bluetooth,[or,1,3]),a(wireless,li_ion,[null,0,3]),a(wireless,ni_mh,[null,0,3]),a(wireless,ni_ca,[null,0,3]),a(wireless,color,[null,0,3]),a(wireless,monochrome,[null,0,3]),a(wireless,cellphone,[null,0,1]),a(accu_cell,wireless,[null,0,2]),a(accu_cell,accu_cell,[xor,1,2]),a(accu_cell,display,[null,0,2]),a(accu_cell,infrared,[null,0,3]),a(accu_cell,bluetooth,[null,0,3]),a(accu_cell,li_ion,[xor,1,3]),a(accu_cell,ni_mh,[xor,1,3]),a(accu_cell,ni_ca,[xor,1,3]),a(accu_cell,color,[null,0,3]),a(accu_cell,monochrome,[null,0,3]),a(accu_cell,cellphone,[null,0,1]),a(display,wireless,[null,0,2]),a(display,accu_cell,[null,0,2]),a(display,display,[xor,1,2]),a(display,infrared,[null,0,3]),a(display,bluetooth,[null,0,3]),a(display,li_ion,[null,0,3]),a(display,ni_mh,[null,0,3]),a(display,ni_ca,[null,0,3]),a(display,color,[xor,1,3]),a(display,monochrome,[xor,1,3]),a(display,cellphone,[null,0,1]),a(infrared,wireless,[or,1,3]),a(infrared,accu_cell,[null,0,3]),a(infrared,display,[null,0,3]),a(infrared,infrared,[null,2,4]),a(infrared,bluetooth,[or,1,4]),a(infrared,li_ion,[null,0,4]),a(infrared,ni_mh,[null,0,4]),a(infrared,ni_ca,[null,0,4]),a(infrared,color,[null,0,4]),a(infrared,monochrome,[null,0,4]),a(infrared,cellphone,[null,0,2]),a(bluetooth,wireless,[or,1,3]),a(bluetooth,accu_cell,[null,0,3]),a(bluetooth,display,[null,0,3]),a(bluetooth,infrared,[or,1,4]),a(bluetooth,bluetooth,[null,2,4]),a(bluetooth,li_ion,[null,0,4]),a(bluetooth,ni_mh,[null,0,4]),a(bluetooth,ni_ca,[null,0,4]),a(bluetooth,color,[null,0,4]),a(bluetooth,monochrome,[null,0,4]),a(bluetooth,cellphone,[null,0,2]),a(li_ion,wireless,[null,0,3]),a(li_ion,accu_cell,[xor,1,3]),a(li_ion,display,[null,0,3]),a(li_ion,infrared,[null,0,4]),a(li_ion,bluetooth,[null,0,4]),a(li_ion,li_ion,[null,2,4]),a(li_ion,ni_mh,[xor,1,4]),a(li_ion,ni_ca,[xor,1,4]),a(li_ion,color,[and,0,4]),a(li_ion,monochrome,[and,0,4]),a(li_ion,cellphone,[null,0,2]),a(ni_mh,wireless,[null,0,3]),a(ni_mh,accu_cell,[xor,1,3]),a(ni_mh,display,[null,0,3]),a(ni_mh,infrared,[null,0,4]),a(ni_mh,bluetooth,[null,0,4]),a(ni_mh,li_ion,[xor,1,4]),a(ni_mh,ni_mh,[null,2,4]),a(ni_mh,ni_ca,[xor,1,4]),a(ni_mh,color,[and,0,4]),a(ni_mh,monochrome,[and,0,4]),a(ni_mh,cellphone,[null,0,2]),a(ni_ca,wireless,[null,0,3]),a(ni_ca,accu_cell,[xor,1,3]),a(ni_ca,display,[null,0,3]),a(ni_ca,infrared,[null,0,4]),a(ni_ca,bluetooth,[null,0,4]),a(ni_ca,li_ion,[xor,1,4]),a(ni_ca,ni_mh,[xor,1,4]),a(ni_ca,ni_ca,[null,2,4]),a(ni_ca,color,[and,0,4]),a(ni_ca,monochrome,[and,0,4]),a(ni_ca,cellphone,[null,0,2]),a(color,wireless,[null,0,3]),a(color,accu_cell,[null,0,3]),a(color,display,[xor,1,3]),a(color,infrared,[null,0,4]),a(color,bluetooth,[null,0,4]),a(color,li_ion,[and,0,4]),a(color,ni_mh,[and,0,4]),a(color,ni_ca,[and,0,4]),a(color,color,[null,2,4]),a(color,monochrome,[xor,1,4]),a(color,cellphone,[null,0,2]),a(monochrome,wireless,[null,0,3]),a(monochrome,accu_cell,[null,0,3]),a(monochrome,display,[xor,1,3]),a(monochrome,infrared,[null,0,4]),a(monochrome,bluetooth,[null,0,4]),a(monochrome,li_ion,[and,0,4]),a(monochrome,ni_mh,[and,0,4]),a(monochrome,ni_ca,[and,0,4]),a(monochrome,color,[xor,1,4]),a(monochrome,monochrome,[null,2,4]),a(monochrome,cellphone,[null,0,2]),a(cellphone,wireless,[null,0,1]),a(cellphone,accu_cell,[null,0,1]),a(cellphone,display,[null,0,1]),a(cellphone,infrared,[null,0,2]),a(cellphone,bluetooth,[null,0,2]),a(cellphone,li_ion,[null,0,2]),a(cellphone,ni_mh,[null,0,2]),a(cellphone,ni_ca,[null,0,2]),a(cellphone,color,[null,0,2]),a(cellphone,monochrome,[null,0,2]),a(cellphone,cellphone,[null,0,0])],[
           put_atts(WIRELESS ,ftAttr(wireless,_)),
           put_atts(ACCU_CELL ,ftAttr(accu_cell,_)),
           put_atts(DISPLAY ,ftAttr(display,_)),
           put_atts(INFRARED ,ftAttr(infrared,_)),
           put_atts(BLUETOOTH ,ftAttr(bluetooth,_)),
           put_atts(LI_ION ,ftAttr(li_ion,_)),
           put_atts(NI_MH ,ftAttr(ni_mh,_)),
           put_atts(NI_CA ,ftAttr(ni_ca,_)),
           put_atts(COLOR ,ftAttr(color,_)),
        put_atts(MONOCHROME ,ftAttr(monochrome,_)),
        put_atts(CELLPHONE ,ftAttr(cellphone,1))],[WIRELESS,ACCU_CELL,DISPLAY,INFRARED,BLUETOOTH,LI_ION,NI_MH,NI_CA,COLOR,MONOCHROME,CELLPHONE],[opt(CELLPHONE,[WIRELESS]),and(CELLPHONE,[ACCU_CELL,DISPLAY]), or(WIRELESS,[INFRARED,BLUETOOTH]),xor(ACCU_CELL,[LI_ION,NI_MH,NI_CA]),xor(DISPLAY,[COLOR,MONOCHROME]),  require(BLUETOOTH,LI_ION)],10,'test',crit_dpthOp,[ff]).

pwsolver(PairInformations,Attributes,Features,CtrList,MatrixSize,ModelName,ParamSort,Labelling):-
        statistics(runtime, [T0|_]),
       assertRec(PairInformations),
        callRec(Attributes),
        borneRec(Features),
        callRec(CtrList),
        length(Features,SizeMat),
        matrice(M,SizeMat,30),
        limitMatrix(M,Features),
        pairwiseGenerator(Features,M,PWCTRLST,RANKLST),
        callRec(PWCTRLST),   
        contrainteFDV2(M,Features,CtrList,Res),
        callRec(Res),
        alldiffrec(RANKLST),
        flatten(RANKLST,Ilist),  
        my_sort(Ilist,LLL,ParamSort), 
        domain(Ilist,1,30),
         maximum(Kn,Ilist),
        length(Ilist,Nbpaire),
      
        labeling([ff],LLL),
            maximum(Kn,Ilist),
            writeln(Kn),
        statistics(runtime, [T1|_]),
        T is T1 - T0,
        writeStat(ModelName,SizeMat,T,Kn,Nbpaire, 'Stats.txt').


consistance(PairInformations,Attributes,Features,CtrList,MatrixSize,ModelName,ParamSort,Labelling):-
        statistics(runtime, [T0|_]),
       assertRec(PairInformations),
        callRec(Attributes),
        borneRec(Features),
        callRec(CtrList),
        length(Features,SizeMat),
        matrice(M,SizeMat,30),
        limitMatrix(M,Features),
getRecR(Features,L),
        labeling([ff],L),
       write(L),
           
        statistics(runtime, [T1|_]),
        T is T1 - T0,
        writeStat(ModelName,SizeMat,T,Kn,Nbpaire, 'Stats.txt').



     
mymaximum([A|R],Res):-mymax([A|R],0,Res).





mymax([A|R],Ref,Res):- A > Ref, mymax(R,A,Res).

mymax([A|R],Ref,Res):- A =< Ref, mymax(R,Ref,Res).
mymax([],Ref,Ref).

save:-save_program('/home/aymeric/pacogen.sav').





/*
required files

*/
/*
Printing tools :

*/

writeln(A) :- write(A), write('\n').
writeStat(Etiquette,Nbft,Temps,NbConfig,ValeurdeK, File):-open(File, append, Stream),write(Stream,Etiquette),write(Stream,';'),write(Stream,Nbft),write(Stream,';'),write(Stream,Temps),write(Stream,';'),write(Stream,NbConfig),write(Stream,';'),write(Stream,ValeurdeK),write(Stream,';'),nl(Stream),close(Stream).


writeStat2(Etiquette,Nbft,Temps,NbConfig,ValeurdeK, File):-open(File, append, Stream),write(Stream,Nbft),write(Stream,';'),write(Stream,Temps),write(Stream,';'),write(Stream,NbConfig),write(Stream,';'),write(Stream,ValeurdeK),write(Stream,';'),nl(Stream),close(Stream).
writeMat(File,M) :- mytranspose(M,M2), open(File, append, Stream),writeMatrice(M2,Stream),nl(Stream),close(Stream).

writeMatrice([A|B],S) :- writeligne(A,S), write(S,'\n'), writeMatrice(B,S).
writeMatrice([],_).


writeligne([A|B],S):- write(S,A), write(S,','), writeligne(B,S).
writeligne([],_).

writeMatrixScr(M1):-mytranspose(M1,M2),writeMatrixScr0(M2).
writeMatrixScr0([A|B]) :- writelineScr(A), write('\n'), writeMatrixScr0(B).
writeMatrixScr0([]).

writelineScr([A|B]):- write(A), write(','), writelineScr(B).
writelineScr([]).


writeMatVal(File,M) :- open(File, write, Stream),write(Stream,M),write(Stream,'.'),nl(Stream),close(Stream).


writedom([A|R]) :- fd_set(A,J),writeln(J),writedom(R).
writedom([]). 
/*
Constraints tools
*/
alldiffrec([_]).
alldiffrec([A|R]) :- R \=[] ,
         all_different(A),alldiffrec(R). 

/*

Ouverture d'un fichier résultat et chargement de la matrice en memoire.
attention il faut la transposer pour revenir à la strucuture de donnée de pariwise
*/


readM(File,M) :- open(File, read, Stream),readFile(M,Stream),close(Stream).

readFile(M,Stream):- read(Stream,M).
        
/* -*- Tableau -*- */

/*
Transpose(M1,M2)
*/

mytranspose(M1,M2):- nbligne(M1,_), creeMat(M1,M2,1).
creeMat(M1,[],K) :- nbligne(M1,I), K is I +1.
creeMat(M1,R,I):-nbligne(M1,K), I =< K, ligne(M1,I,Li), I2 is I +1, R = [Li|M12], creeMat(M1,M12,I2).

/* matrice(M,X,Y) : Matrice M, X : nombre de colonnes, et y nombre de lignes */
/* Crée une liste de colonne */
matrice([],0,_).
matrice([L|M],X,Y) :- X >0 , X1 is X -1 ,length(L,Y), matrice(M, X1, Y) .  



/* elementdeL(L,I,E) :  L : liste, I : rang de l'élément, E : élement */

elementdeL([A|_],1,A).
elementdeL([_|B],I,C):- I >1 , I1 is I-1 , elementdeL(B,I1 ,C).

/* Obtenir Colonne*/
/* Colonne(M,I,C) */
/* M : matrice définie précedement, I : l'indice de colonne, C : la colonne résultante */ 

colonne(M,I,C):- elementdeL(M,I,C).


/*
Nbligne
*/


nbligne([A|_],L):- length(A,L).
/* Obtenir Ligne */
/* ligne(M,I,L) : M : matrice définie précédement, I : indice de ligne, L : la ligne resultante */

ligne([],_,[]).
ligne([A|R],I,[B|T]) :- elementdeL(A,I,B) , ligne(R,I,T).

/* variable(M,V) : obtention de la liste des variable de M */

variable([],[]).
variable([A|R],L):- variable(R,L2),append(A,L2,L). 

/* -*- Fin de la définition de la structure de donnée -*- */

/* Outils */

/*
permet de tester si une variable appartient à une lsite, retire la variable de la liste
*/
appartient(X,[A|N],[A|N2]):- appartient(X,N,N2).
appartient(X,[A|N],N):- A == X.
/*
spit(L1,L2,L3,L4) : 
L1 : liste d'éléments
L2 : liste contenant certain élément de L1,
L3 : liste des éléments de L1 présent dans L2
L4 : liste des éléments de L2 sans les éléments de L1                                           
*/

assertRec([A|T]):-
                 assert(A),assertRec(T).
assertRec([]).
value([],[]).
value([A|R],[B|R2]) :-
        A = B, value(R,R2).

callRec([A|T]) :-
        call(A),
        callRec(T).
callRec([]).



split([],LR,[],LR).
split([A|N],Lx,L1,L) :- (appartient(A,Lx,Ln) -> L1 = [A|L11],split(N,Ln,L11,L));split(N,Lx,L1,L).

/*
RANKLST = [[_A,_B,_C,_D],[_E,_F,_G,_H],[_I_,_J,_K,_L]],flatten(RANKLST,Lst),Lst =
[_A,_B,_C,_D,_E,_F,_G,_H,_I_,_J,_K,_L]
*/
flatten([L|T],Lst):- flatten(T,Lst2), append(L,Lst2,Lst).
flatten([],[]).


limitMatrix([A|R],[FA|FR]):-get_atts(FA,ftAttr(_,I)), fd_min(I,MinA),fd_max(I,MaxA), domain(A,MinA,MaxA), limitMatrix(R,FR).
limitMatrix([],[]).     

/* Initialisation du problème */
limit([],_,_).
limit([A|R],Min,Max) :- fdset_interval(A,Min,Max),limit(R,Min,Max).
/*
Compte le nolmbre d'occurence de -1 et de 0, rend la liste des variables non instanciées
       N : nbre de 0
   M : nbre de -1
*/


        occur_cpt([LF|LFs],LF0,N,M) :- 
                LF == 0,!,
                 occur_cpt(LFs,LF0,N1,M), N is N1 +1 .
               
     
        occur_cpt([LF|LFs],LF0,N,M) :-  
                LF == -1,!,
               occur_cpt(LFs,LF0,N,M1), M is M1 +1 .

        occur_cpt([LF|LFs],LF0,N,M):-
                LF0 = [LF|LF02],
                occur_cpt(LFs,LF02,N,M).

       occur_cpt([],[],0,0).



domainRec([A|R],V1,V2):-domain(A,V1,V2),domainRec(R,V1,V2).
domainRec([],_,_).
   

/*
Global Ctr
*/

:- multifile clpfd:dispatch_global/4.


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



/*
Pw Gen
*/


/*
sort(L1,L2)
predicat de tri 
*/

getRecR([A|R],[I|R2]):-    
        get_atts(A,ftAttr(_,I)),!,
        I in 0..1,
        getRecR(R,R2).



getRecR([],[]).


borneRec([A|R]):-    
        get_atts(A,ftAttr(_,I)),!,
        I in 0..1,
        borneRec(R).



borneRec([]).

my_sort(L1,L2,Param):-samsort(Param,L1,L2).



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

printlst([A|R]):-
         get_atts(A,lstAttr(L)),
         write(A), write('   '), write(L), write('\n'),printlst(R).

printlst([]).
printlst2([A|R]):-
         write(A),  write('\n'),printlst2(R).

printlst2([]).
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
,a(A,B,Liste),writeln(Liste),
                puttattReq(RANKLST,Liste),printlst(RANKLST),     pairvalidator(A,B,PWCTRLST,PWCTRLST2)      ,pairvalidator(StA,StB,PWCTRLST,PWCTRLST2)
*/

pairwiseConstraintsGenerator(A,B,CA,CB,PWCTRLST,RANKLST) :-
                get_atts(A,ftAttr(NameA,StA)),
                domainToList(StA,LA),
                get_atts(B,ftAttr(NameB,_)),a(NameA,NameB,Attr) ,
                avalueGenerator(A,LA,B,CA,CB,PWCTRLST,RANKLST,Attr).
        
puttattReq([I|Ir],Attr):-
        write(' I0 : '),
        writeln(I),
       put_atts(I,lstAttr(Attr)),
       write(' I1 : ' ),
       writeln(I),
       puttattReq(Ir,Attr).

puttattReq([],_).
        
        


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

pairvalidator2(FTA,FTB,pairwise(I,(CA,CB),(VA,VB)),[]):- \+(\+(\+(callRec([ get_atts(FTA,ftAttr(NameA,StA)), get_atts(FTB,ftAttr(NameB,StB)),StA#=VA, StB#=VB])))),write('invalidPair').
pairvalidator2(FTA,FTB,Ctr,Ctr):-Ctr=pairwise(I,(CA,CB),(VA,VB)), \+(\+(callRec([get_atts(FTA,ftAttr(NameA,StA)), get_atts(FTB,ftAttr(NameB,StB)),StA#=VA, StB#=VB]))).
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

avalueGenerator(_,[],_,_,_,[],[],_).
avalueGenerator(A,[VA|R],B,CA,CB,PWCTRLST,RANKLST,Attr) :-
           get_atts(A,ftAttr(NameA,StA)),
        \+(\+(callRec([StA = VA,    pairListGenerator(VA,B,PairList),assert(p1(PairList))]))),
        p1(PairList),pairsGeneretor(PairList,CA,CB,PWCTRLST1,RANKLST1,Attr),retract(p1(_)),
        avalueGenerator(A,R,B,CA,CB,PWCTRLST2,RANKLST2,Attr),
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
         get_atts(B,ftAttr(_,StB)),
        domainToList(StB,LB),
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

 pairsGeneretor([],_,_,[],[],_).
 pairsGeneretor([[VA,VB]|R],CA,CB,PWCTRLST3,RANKLST3,Attr) :-
          
       put_atts(I,lstAttr(Attr)),
        PWCTRLST = [pairwise(I,(CA,CB),(VA,VB))],
        RANKLST = [I],
        pairsGeneretor(R,CA,CB,PWCTRLST2,RANKLST2,Attr),
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


putAttsRec([A|R],[B|Rb]):-
                put_atts(A,ftAttr(_,B)),putAttsRec(R,Rb).
putAttsRec([],[]).

genCtrRec(LF,[P|Pred],Line, [MappedPred|Res]):-
        genCtr(LF,P,Line, MappedPred),
        genCtrRec(LF,Pred,Line, Res).
        
genCtrRec(_,[],_, []).


genCtr(LF,Pred,Line, MappedPred):-
        Pred =.. Li ,
        Li = [and,Father,Lst],
        mapping(LF,Line,Father,Father2),
        mappingRec(LF,Line,Lst,Lst2),
        MappedPred =.. [and,A,L],
        put_atts(A,ftAttr(_,Father2)),
        putAttsRec(L,Lst2)         
        .

genCtr(LF,Pred,Line, MappedPred):-
        Pred =.. Li ,
        Li = [opt,Father,Lst],  
        mapping(LF,Line,Father,Father2),
        mappingRec(LF,Line,Lst,Lst2),
        MappedPred =.. [opt,A,L],
        put_atts(A,ftAttr(_,Father2)),
        putAttsRec(L,Lst2)        
        .

genCtr(LF,Pred,Line, MappedPred):-
        Pred =.. Li ,
        Li = [or,Father,Lst],
        mapping(LF,Line,Father,Father2),
        mappingRec(LF,Line,Lst,Lst2),
        MappedPred =.. [or,A,L],
        put_atts(A,ftAttr(_,Father2)),
        putAttsRec(L,Lst2)           
        .

genCtr(LF,Pred,Line, MappedPred):-
        Pred =.. Li ,
        Li = [xor,Father,Lst],
        mapping(LF,Line,Father,Father2),
        mappingRec(LF,Line,Lst,Lst2),
       
        MappedPred =.. [xor,A,L],
        put_atts(A,ftAttr(_,Father2)),
        putAttsRec(L,Lst2)            
        .

genCtr(LF,Pred,Line, MappedPred):-
        Pred =.. Li ,
        Li = [require,Father,Son],
        mapping(LF,Line,Father,Father2),
        mapping(LF,Line,Son,Son2),
        MappedPred =.. [require,A,B],
        put_atts(A ,ftAttr(_,Father2)),
        put_atts(B ,ftAttr(_,Son2)) 
        .


genCtr(LF,Pred,Line, MappedPred):-
        Pred =.. Li ,
        Li = [mutex,Father,Son],
        mapping(LF,Line,Father,Father2),
        mapping(LF,Line,Son,Son2),
        MappedPred =.. [mutex,A,B],
        put_atts(A,ftAttr(_,Father2)), 
        put_atts(B ,ftAttr(_,Son2)) 
        .

/*
genCtr(LF,Pred,Line, MappedPred):-
        Pred =.. Li ,
        Li = [mutex,Lst],
        mappingRec(LF,Line,Lst,Lst2),
        MappedPred =.. [mutex,L],             
        putAttsRec(Lst2,L)        
        .

*/
mappingRec(LF,Line,[A|B],[Res|R]) :-
        mapping(LF,Line,A,Res),mappingRec(LF,Line,B,R).
mappingRec(_,_,[],[]).
        
/*
mapping(LF,Line,Father,Father2),
*/
mapping([A|_],[B|_],Ref,B):- A == Ref.
mapping([A|R1],[_|R2],Ref,Res):- A \== Ref, mapping(R1,R2,Ref,Res).     




/* variable(M,V) : obtention de la liste des variable de M */
/* variable(M,V) : obtention de la liste des variable de M */

nvariable2(_,[],0).
nvariable2([A|R],L,I):-I \=0, I2 is I -1, nvariable2(R,L2,I2),append(A,L2,L).

nvariable(M,L,I):-mytranspose(M,M2),nvariable2(M2,L,I).


submatrix(M,M2,I):-mytranspose(M,MT),submatrix2(MT,M2,I).

submatrix2([A|R],[A|R2],I):- I\=0, I2 is I -1, submatrix2(R,R2,I2).
submatrix2(_,[],0).



/*selector(ENV-K,[V|Vars], V, Vars) :-*/
        
        
