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

*/