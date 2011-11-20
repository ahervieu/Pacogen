:-['tools.pro','globalConstraints.pro','pairwisegenerator.pro'].

buildattsSimple([A|R],[put_atts(A ,ftAttr(_,_))|R2]):-buildattsSimple(R,R2).
buildattsSimple([],[]).

a(_,_,_).
        
     verify_attributes(Var, Other, Goals).

test :- length(LstFt,13),writeln(LstFt),buildattsSimple(LstFt,Attr),
        callRec(Attr),
        customborneRec(LstFt,0,2),
        length(LstFt,SizeMat),
        matrice(M,SizeMat,20),
        limitMatrix(M,LstFt),
        pairwiseGenerator(LstFt,M,PWCTRLST,RANKLST),
        writeln(PWCTRLST),
        callRec(PWCTRLST),        
         alldiffrec(RANKLST),
        flatten(RANKLST,Ilist),  
        domain(Ilist,1,20),
        maximum(K,Ilist),      
        length(Ilist,Nbpaire),   
        writeln(Nbpaire),
        labeling([],Ilist),
        writeln('end'),
        writeln(K).