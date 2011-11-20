
:-['tools.pro','globalConstraints.pro','pairwisegenerator.pro' ].
:-use_module(library(timeout)).
cell_phone2:-FT=[BASIS,MSG,MMS,SMS,VOICE,COM,WLAN,BLUETOOTH,UMTS,EXTRAS,MP3,CAMERA,ME3,ME8],PFT =[SMS,MMS,VOICE,WLAN,BLUETOOTH,UMTS,MP3,ME3,ME8],
            CTR= [and(1,[BASIS,EXTRAS]),opt(1,[COM]),and(BASIS,[MSG,VOICE]),and(MSG,[SMS]),opt(MSG,[MMS]),opt(COM,[WLAN,BLUETOOTH,UMTS]),
                       or(EXTRAS,[MP3,CAMERA]),xor(CAMERA,[ME3,ME8]),require(MMS,CAMERA),mutex(BLUETOOTH,MP3)],simpleRun(FT,PrimitivesFeaturesList, CTR, Name,TimeOut, OutputFileMatrix, OutputFileStats,10) .

writelst([A|R]):- writeln(A),writelst(R).
writelst([]).


simpleRun(FeaturesList,PrimitivesFeaturesList, Constraints, Name,TimeOut, OutputFileMatrix, OutputFileStats,Size) :- 
        domain(FeaturesList,0,1),      
        callRec(Constraints),
       length(FeaturesList,SizeMat),
        matrice(M,SizeMat,Size),
        writeln(M),writeln('E'),    
        domainRec(M,0,1),
        contrainteFDV2(M,FeaturesList,Constraints,Res),
     callRec(Res),  
        pairwiseGenerator(FeaturesList,M,PWCTRLST,RANKLST),
     callRec(PWCTRLST),
      alldiffrec(RANKLST),
        flatten(RANKLST,Ilist),     length(Ilist,L), writeln(L),
                                    
    labeling([ff],Ilist),  
        maximum(Kn, Ilist), 
        monitoring(Kn,Ilist),
        writeMat('test0.txt',M),fd_statistics,
        halt.
    
cell_phone2cfg:-FT=[BASIS,MSG,MMS,SMS,VOICE,COM,WLAN,BLUETOOTH,UMTS,EXTRAS,MP3,CAMERA,ME3,ME8],PFT =[SMS,MMS,VOICE,WLAN,BLUETOOTH,UMTS,MP3,ME3,ME8],
            CTR= [and(1,[BASIS,EXTRAS]),opt(1,[COM]),and(BASIS,[MSG,VOICE]),and(MSG,[SMS]),opt(MSG,[MMS]),opt(COM,[WLAN,BLUETOOTH,UMTS]),
                       or(EXTRAS,[MP3,CAMERA]),xor(CAMERA,[ME3,ME8]),require(MMS,CAMERA),mutex(BLUETOOTH,MP3)],nbconfig2(FT,PrimitivesFeaturesList, CTR,'cellphonecfg') .




nbconfig2(FeaturesList,PrimitivesFeaturesList, Constraints,Etiquette) :-
        assert((t(0))),
             open('nbConfig.txt', append, Stream),write(Stream,Etiquette),write(Stream,';'),close(Stream),
        allconfig(FeaturesList,PrimitivesFeaturesList, Constraints),
%        length(S,L).
        retract((t(L))),
                           open('nbConfig.txt', append, Stream2),write(Stream2,L),nl(Stream2),close(Stream2),
        write(' L : '),writeln(L), halt.
        
lab(FeaturesList) :-      
        labeling([],FeaturesList),
        retract((t(N))),
        N1 is N+1,
        asserta((t(N1))).

  allconfig(FeaturesList,PrimitivesFeaturesList, Constraints) :-
         domain(FeaturesList,-1,0),
          callRec(Constraints),
      findall(_,lab(FeaturesList),_).