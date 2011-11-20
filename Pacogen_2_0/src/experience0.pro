
:-['tools.pro','globalConstraints.pro','pairwisegenerator.pro'].
:-use_module(library(atts)).

 :- attribute ftAttr/2 , lstAttr/1.


     verify_attributes(Var, Other, Goals).


a(wireless,wireless,[or,1,2]).
a(wireless,accu_cell,[null,0,2]).
a(wireless,display,[null,0,2]).
a(wireless,infrared,[or,1,3]).
a(wireless,bluetooth,[or,1,3]).
a(wireless,li_ion,[null,0,3]).
a(wireless,ni_mh,[null,0,3]).
a(wireless,ni_ca,[null,0,3]).
a(wireless,color,[null,0,3]).
a(wireless,monochrome,[null,0,3]).
a(wireless,cellphone,[null,0,1]).
a(accu_cell,wireless,[null,0,2]).
a(accu_cell,accu_cell,[xor,1,2]).
a(accu_cell,display,[null,0,2]).
a(accu_cell,infrared,[null,0,3]).
a(accu_cell,bluetooth,[null,0,3]).
a(accu_cell,li_ion,[xor,1,3]).
a(accu_cell,ni_mh,[xor,1,3]).
a(accu_cell,ni_ca,[xor,1,3]).
a(accu_cell,color,[null,0,3]).
a(accu_cell,monochrome,[null,0,3]).
a(accu_cell,cellphone,[null,0,1]).
a(display,wireless,[null,0,2]).
a(display,accu_cell,[null,0,2]).
a(display,display,[xor,1,2]).
a(display,infrared,[null,0,3]).
a(display,bluetooth,[null,0,3]).
a(display,li_ion,[null,0,3]).
a(display,ni_mh,[null,0,3]).
a(display,ni_ca,[null,0,3]).
a(display,color,[xor,1,3]).
a(display,monochrome,[xor,1,3]).
a(display,cellphone,[null,0,1]).
a(infrared,wireless,[or,1,3]).
a(infrared,accu_cell,[null,0,3]).
a(infrared,display,[null,0,3]).
a(infrared,infrared,[null,2,4]).
a(infrared,bluetooth,[or,1,4]).
a(infrared,li_ion,[null,0,4]).
a(infrared,ni_mh,[null,0,4]).
a(infrared,ni_ca,[null,0,4]).
a(infrared,color,[null,0,4]).
a(infrared,monochrome,[null,0,4]).
a(infrared,cellphone,[null,0,2]).
a(bluetooth,wireless,[or,1,3]).
a(bluetooth,accu_cell,[null,0,3]).
a(bluetooth,display,[null,0,3]).
a(bluetooth,infrared,[or,1,4]).
a(bluetooth,bluetooth,[null,2,4]).
a(bluetooth,li_ion,[null,0,4]).
a(bluetooth,ni_mh,[null,0,4]).
a(bluetooth,ni_ca,[null,0,4]).
a(bluetooth,color,[null,0,4]).
a(bluetooth,monochrome,[null,0,4]).
a(bluetooth,cellphone,[null,0,2]).
a(li_ion,wireless,[null,0,3]).
a(li_ion,accu_cell,[xor,1,3]).
a(li_ion,display,[null,0,3]).
a(li_ion,infrared,[null,0,4]).
a(li_ion,bluetooth,[null,0,4]).
a(li_ion,li_ion,[null,2,4]).
a(li_ion,ni_mh,[xor,1,4]).
a(li_ion,ni_ca,[xor,1,4]).
a(li_ion,color,[and,0,4]).
a(li_ion,monochrome,[and,0,4]).
a(li_ion,cellphone,[null,0,2]).
a(ni_mh,wireless,[null,0,3]).
a(ni_mh,accu_cell,[xor,1,3]).
a(ni_mh,display,[null,0,3]).
a(ni_mh,infrared,[null,0,4]).
a(ni_mh,bluetooth,[null,0,4]).
a(ni_mh,li_ion,[xor,1,4]).
a(ni_mh,ni_mh,[null,2,4]).
a(ni_mh,ni_ca,[xor,1,4]).
a(ni_mh,color,[and,0,4]).
a(ni_mh,monochrome,[and,0,4]).
a(ni_mh,cellphone,[null,0,2]).
a(ni_ca,wireless,[null,0,3]).
a(ni_ca,accu_cell,[xor,1,3]).
a(ni_ca,display,[null,0,3]).
a(ni_ca,infrared,[null,0,4]).
a(ni_ca,bluetooth,[null,0,4]).
a(ni_ca,li_ion,[xor,1,4]).
a(ni_ca,ni_mh,[xor,1,4]).
a(ni_ca,ni_ca,[null,2,4]).
a(ni_ca,color,[and,0,4]).
a(ni_ca,monochrome,[and,0,4]).
a(ni_ca,cellphone,[null,0,2]).
a(color,wireless,[null,0,3]).
a(color,accu_cell,[null,0,3]).
a(color,display,[xor,1,3]).
a(color,infrared,[null,0,4]).
a(color,bluetooth,[null,0,4]).
a(color,li_ion,[and,0,4]).
a(color,ni_mh,[and,0,4]).
a(color,ni_ca,[and,0,4]).
a(color,color,[null,2,4]).
a(color,monochrome,[xor,1,4]).
a(color,cellphone,[null,0,2]).
a(monochrome,wireless,[null,0,3]).
a(monochrome,accu_cell,[null,0,3]).
a(monochrome,display,[xor,1,3]).
a(monochrome,infrared,[null,0,4]).
a(monochrome,bluetooth,[null,0,4]).
a(monochrome,li_ion,[and,0,4]).
a(monochrome,ni_mh,[and,0,4]).
a(monochrome,ni_ca,[and,0,4]).
a(monochrome,color,[xor,1,4]).
a(monochrome,monochrome,[null,2,4]).
a(monochrome,cellphone,[null,0,2]).
a(cellphone,wireless,[null,0,1]).
a(cellphone,accu_cell,[null,0,1]).
a(cellphone,display,[null,0,1]).
a(cellphone,infrared,[null,0,2]).
a(cellphone,bluetooth,[null,0,2]).
a(cellphone,li_ion,[null,0,2]).
a(cellphone,ni_mh,[null,0,2]).
a(cellphone,ni_ca,[null,0,2]).
a(cellphone,color,[null,0,2]).
a(cellphone,monochrome,[null,0,2]).
a(cellphone,cellphone,[null,0,0]).


test2 :-LstFt =[WIRELESS,ACCU_CELL,DISPLAY,INFRARED,BLUETOOTH,LI_ION,NI_MH,NI_CA,COLOR,MONOCHROME,CELLPHONE], 
        Ctr =  [opt(CELLPHONE,[WIRELESS]),and(CELLPHONE,[ACCU_CELL,DISPLAY]), or(WIRELESS,[INFRARED,BLUETOOTH]),xor(ACCU_CELL,[LI_ION,NI_MH,NI_CA]),xor(DISPLAY,[COLOR,MONOCHROME]),  require(BLUETOOTH,LI_ION)],
        Att = [
           put_atts(WIRELESS ,ftAttr(wireless,A)),
           put_atts(ACCU_CELL ,ftAttr(accu_cell,B)),
           put_atts(DISPLAY ,ftAttr(display,C)),
           put_atts(INFRARED ,ftAttr(infrared,D)),
      
           put_atts(BLUETOOTH ,ftAttr(bluetooth,E)),
           put_atts(LI_ION ,ftAttr(li_ion,F)),
           put_atts(NI_MH ,ftAttr(ni_mh,G)),
           put_atts(NI_CA ,ftAttr(ni_ca,H)),
           put_atts(COLOR ,ftAttr(color,I)),
        put_atts(MONOCHROME ,ftAttr(monochrome,J)),
        put_atts(CELLPHONE ,ftAttr(cellphone,1))],
         callRec(Att),
        borneRec(LstFt),
        callRec(Ctr),
        
             writeln('-----'),
        writeln(A),
        writeln(B),
        writeln(C),
        writeln(D),
        writeln(E),
        writeln(F),
        writeln(G),
        writeln(H),
        writeln(I),
        writeln(J)
        .
        
        
        
test :- LstFt =[WIRELESS,ACCU_CELL,DISPLAY,INFRARED,BLUETOOTH,LI_ION,NI_MH,NI_CA,COLOR,MONOCHROME,CELLPHONE], 
        Ctr =  [opt(CELLPHONE,[WIRELESS]),and(CELLPHONE,[ACCU_CELL,DISPLAY]), or(WIRELESS,[INFRARED,BLUETOOTH]),xor(ACCU_CELL,[LI_ION,NI_MH,NI_CA]),xor(DISPLAY,[COLOR,MONOCHROME]),  require(BLUETOOTH,LI_ION),mutex(COLOR,NI_CA)],
        Att = [
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
        put_atts(CELLPHONE ,ftAttr(cellphone,1))],
        callRec(Att),
        borneRec(LstFt),
        callRec(Ctr),
        length(LstFt,SizeMat),
        matrice(M,SizeMat,15),
        limitMatrix(M,LstFt),
 
        pairwiseGenerator(LstFt,M,PWCTRLST,RANKLST),
        callRec(PWCTRLST),   
        
         
        contrainteFDV2(M,LstFt,Ctr,Res),
        
      
     
        callRec(Res),
           
        alldiffrec(RANKLST),
        
        flatten(RANKLST,Ilist),  
 writeln(Ilist),

        my_sort(Ilist,LLL), writeln(LLL),
        
        domain(Ilist,1,15),
        maximum(K,Ilist),
      
        length(Ilist,Nbpaire),
     
        labeling([ff],Ilist),
        writeln('end'),
        writeln(K).
         
                    
        
        
testlight :- LstFt =[ACCU_CELL,LI_ION,NI_MH,NI_CA], 
        Ctr =  [xor(ACCU_CELL,[LI_ION,NI_MH,NI_CA])],
        Att = [
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
        put_atts(CELLPHONE ,ftAttr(cellphone,1))],
        callRec(Att),
        borneRec(LstFt),
        callRec(Ctr),
        length(LstFt,SizeMat),
        matrice(M,SizeMat,5),
        limitMatrix(M,LstFt),
        pairwiseGenerator(LstFt,M,PWCTRLST,RANKLST),
        callRec(PWCTRLST),   
 
        contrainteFDV2(M,LstFt,Ctr,Res),
       callRec(Res),           
        alldiffrec(RANKLST),
        flatten(RANKLST,Ilist),  
    /*   my_sort(Ilist,LLL), writeln(LLL),
      */  
        domain(Ilist,1,5),
        maximum(K,Ilist),
            writeMatrixScr(M),
        labeling([ff],Ilist),
        writeMatrixScr(M),
        writeln('end'),
        writeln(K).
         
                


      