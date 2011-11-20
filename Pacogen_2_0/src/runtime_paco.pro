:-['tools.pro','globalConstraints.pro','pairwisegenerator.pro' ].

problemMinimization(FeaturesList, Constraints, _,TimeOut, OutputFileMatrix, Size) :-
        domain(FeaturesList,-1,0),
        writeln('Loading Model...'),
        callRec(Constraints),
        length(FeaturesList,SizeMat),
        matrice(M,SizeMat,Size),
        limitMatrix(M,FeaturesList),
        writeln('Modeling problem...'),
        pairwiseGenerator(FeaturesList,M,PWCTRLST,RANKLST),
        callRec(PWCTRLST),
        contrainteFDV2(M,FeaturesList,Constraints,Res),
        callRec(Res),
        alldiffrec(RANKLST),
        flatten(RANKLST,Ilist),
        domain(Ilist,1,Size),
        length(Ilist,J),
        writeln(J),
        maximum(Kn,Ilist),
        writeln('Solving...'),
        (number(TimeOut) ->   labeling([ff,minimize(Kn),time_out(TimeOut,_)],Ilist); 
        labeling([ff],Ilist)),
        write(' Number of configurations : '), writeln(Kn),
       nvariable(M,L,Kn),
       labeling([ff],L),
       submatrix(M,M2,Kn),
       mytranspose(M2,M3),
       writeMat(OutputFileMatrix,M3),
       halt.



openfile(FeaturesList,Constraints,TimeOut,Size) :- open('model.txt', read, Stream),read(Stream,[FeaturesList,Constraints,TimeOut,Size]),close(Stream).


 
 
 
 user:runtime_entry(start) :-openfile(FeaturesList,Constraints,TimeOut,Size),problemMinimization(FeaturesList, Constraints, _,TimeOut, 'matrix.txt', Size).

 