%1
ime('Mira').
ime('Bruno').
ime('Igor').
ime('Teo').


hrana('Sendvic').
hrana('Pita').
hrana('Hamburger').
hrana('Pica').

hobi('Krstozbor').
hobi('Pishuvanje').
hobi('Chitanje').
hobi('Fotografija').

maica('Bela').
maica('Zolta').
maica('Sina').
maica('Crvena').


jade(ime('Teo'), hrana('Sendvic')).
jade(ime('Mira'), hrana('Pita')).
jade(ime('Igor'), hrana(_)).
jade(ime('Bruno'), hrana(_)).


hobi_jadenje(ime(_), hobi('Pishuvanje'), hrana('Hamburger')) :-  !.
hobi_jadenje(ime(I), hobi(X), hrana(Y)) :- ima_hobi(ime(I), hobi(X)), jade(ime(I),hrana(Y)).
                  
ima_hobi(ime('Mira'), hobi('Krstozbor')).      
ima_hobi(ime('Teo'), hobi(_)).
ima_hobi(ime('Bruno'),hobi(_)).
ima_hobi(ime('Igor'),hobi('Chitanje')).


nosi(ime('Bruno'), maica('Zolta')).
nosi(ime('Teo'), maica(_)).
nosi(ime('Igor'), maica(_)).
nosi(ime('Mira'), maica('Bela')).


devojka_nosi(maica('Bela')).


uslov1(ime(_), hrana(_), hobi(_), maica(X4),
     ime(_), hrana(_), hobi(_), maica('Sina'),
       ime(_), hrana(_), hobi(_), maica(_)) :-
    devojka_nosi(maica(X4)).
uslov1(ime(_), hrana(_), hobi(_), maica(_),
       ime(_), hrana(_), hobi(_), maica(X4),
     ime(_), hrana(_), hobi(_), maica('Sina')) :-
    devojka_nosi(maica(X4)).
uslov1(ime(_), hrana(_), hobi(_), maica('Sina'),
     ime(_), hrana(_), hobi(_), maica(Y4),
       ime(_), hrana(_), hobi(_), maica(Z4)) :- devojka_nosi(maica(M)), M\==Y4, M\==Z4.
uslov1(ime(_), hrana(_), hobi(_), maica(X4),
     ime(_), hrana(_), hobi(_), maica(Y4),
       ime(_), hrana(_), hobi(_), maica(M)) :- devojka_nosi(maica(M)), X4\=='Sina', Y4\=='Sina'.
uslov1(ime(_), hrana(_), hobi(_), maica(X4),
     ime(_), hrana(_), hobi(_), maica(Y4),
       ime(_), hrana(_), hobi(_), maica(Z4)) :- X4\=='Sina', Y4\=='Sina', Z4\=='Sina', devojka_nosi(maica(M)), M\==X4, M\==Y4, M\==Z4.

uslov2(ime('Teo'), hrana(_), hobi(_), maica(_),
     ime(_), hrana('Pita'), hobi(_), maica(_),
       ime(_), hrana(_), hobi(_), maica(_)).
uslov2(ime(X1), hrana(_), hobi(_), maica(_),
     ime(_), hrana(Y2), hobi(_), maica(_),
       ime(_), hrana(_), hobi(_), maica(_)) :- X1 \== 'Teo', Y2\=='Pita'.

uslov3(ime(_), hrana('Pica'), hobi(_), maica(_),
       ime(_), hrana(_), hobi(_), maica(_),
       ime(X1), hrana(_), hobi(_), maica(_)) :- X1 \== 'Bruno'.
uslov3(ime('Bruno'), hrana(_), hobi(_), maica(_),
     ime(_), hrana('Pica'), hobi(_), maica(_),
       ime(_), hrana(_), hobi(_), maica(_)).
uslov3(ime(_), hrana(_), hobi(_), maica(_),
       ime('Bruno'), hrana(_), hobi(_), maica(_),
     ime(_), hrana('Pica'), hobi(_), maica(_)).
uslov3(ime(_), hrana(X1), hobi(_), maica(_),
       ime(_), hrana(_), hobi(_), maica(_),
       ime('Bruno'), hrana(_), hobi(_), maica(_)) :- X1 \== 'Pica'.
uslov3(ime('Bruno'), hrana(_), hobi(_), maica(_),
       ime(_), hrana(_), hobi(_), maica(_),
       ime(_), hrana(X2), hobi(_), maica(_)) :- X2 \== 'Pica'.
uslov3(ime(_), hrana('Pica'), hobi(_), maica(_),
     ime('Bruno'), hrana(_), hobi(_), maica(_),
       ime(_), hrana(_), hobi(_), maica(_)).
uslov3(ime(_), hrana(_), hobi(_), maica(_),
       ime(_), hrana('Pica'), hobi(_), maica(_),
     ime('Bruno'), hrana(_), hobi(_), maica(_)).
uslov3(ime(X1), hrana(_), hobi(_), maica(_),
       ime(_), hrana(_), hobi(_), maica(_),
       ime(_), hrana('Pica'), hobi(_), maica(_)):- X1 \== 'Bruno'.
uslov3(ime(I1), hrana(H1), hobi(_), maica(_),
     ime(I2), hrana(H2), hobi(_), maica(_),
       ime(I3), hrana(H3), hobi(_), maica(_)) :- I1 \== 'Bruno', I2 \== 'Bruno',I3 \== 'Bruno',H1\=='Pica',H2\=='Pica',H3\=='Pica'.

uslov4(ime(_), hrana('Pica'), hobi(_), maica(_),
       ime(_), hrana(_), hobi(_), maica(_),
       ime(_), hrana(_), hobi(_), maica(M)) :- M\=='Bela' .
uslov4(ime(_), hrana(_), hobi(_), maica('Bela'),
     ime(_), hrana('Pica'), hobi(_), maica(_),
       ime(_), hrana(_), hobi(_), maica(_)).
uslov4(ime(_), hrana(_), hobi(_), maica(_),
       ime(_), hrana(_), hobi(_), maica('Bela'),
     ime(_), hrana('Pica'), hobi(_), maica(_)).
uslov4(ime(_), hrana(H), hobi(_), maica(_),
       ime(_), hrana(_), hobi(_), maica(_),
       ime(_), hrana(_), hobi(_), maica('Bela')) :- H\=='Pica'.
uslov4(ime(_), hrana(_), hobi(_), maica(_),
     ime(_), hrana(_), hobi(_), maica(M),
          ime(_), hrana('Pica'), hobi(_), maica(_)) :- M\=='Bela'.
uslov4(ime(_), hrana(_), hobi(_), maica(_),
       ime(_), hrana('Pica'), hobi(_), maica(_),
     ime(_), hrana(_), hobi(_), maica('Bela')).
uslov4(ime(_), hrana('Pica'), hobi(_), maica(_),
     ime(_), hrana(_), hobi(_), maica('Bela'),
       ime(_), hrana(_), hobi(_), maica(_)).
uslov4(ime(_), hrana(_), hobi(_), maica('Bela'),
       ime(_), hrana(_), hobi(_), maica(_),
       ime(_), hrana(H), hobi(_), maica(_)):- H\=='Pica'.
uslov4(ime(_), hrana(X2), hobi(_), maica(X4),
     ime(_), hrana(Y2), hobi(_), maica(Y4),
       ime(_), hrana(Z2), hobi(_), maica(Z4)) :- Z2\=='Pica', Y2 \== 'Pica', X2 \== 'Pica', 
    											X4 \== 'Bela', Y4 \== 'Bela', Z4 \=='Bela'.

sedi(ime(X1), hrana(X2), hobi(X3), maica(X4),
     ime(Y1), hrana(Y2), hobi(Y3), maica(Y4),
     ime(Z1), hrana(Z2), hobi(Z3), maica(Z4))  :-
    Y1\=='Teo', Z1\=='Teo',X1\==Y1, X2\==Y2,X3\==Y3,X4\==Y4,
    Z1\==Y1, Z2\==Y2,Z3\==Y3,Z4\==Y4,
    X1\==Z1, X2\==Z2,X3\==Z3,X4\==Z4,
    uslov1(ime(X1), hrana(X2), hobi(X3), maica(X4),
     ime(Y1), hrana(Y2), hobi(Y3), maica(Y4),
     ime(Z1), hrana(Z2), hobi(Z3), maica(Z4)),
    uslov2(ime(X1), hrana(X2), hobi(X3), maica(X4),
     ime(Y1), hrana(Y2), hobi(Y3), maica(Y4),
     ime(Z1), hrana(Z2), hobi(Z3), maica(Z4)),
    uslov3(ime(X1), hrana(X2), hobi(X3), maica(X4),
     ime(Y1), hrana(Y2), hobi(Y3), maica(Y4),
     ime(Z1), hrana(Z2), hobi(Z3), maica(Z4)),
    uslov4(ime(X1), hrana(X2), hobi(X3), maica(X4),
     ime(Y1), hrana(Y2), hobi(Y3), maica(Y4),
     ime(Z1), hrana(Z2), hobi(Z3), maica(Z4)).

%2

pripagjaNa(Element,[H|_]) :- Element = H.
pripagjaNa(Element, [_|T]) :- pripagjaNa(Element, T).

nePripagjaNaIdentichno(_,_,_,[]). 
nePripagjaNaIdentichno(Element, Skip, Skip, [_|T]) :- NI is Skip + 1,nePripagjaNaIdentichno(Element,NI,Skip, T), !.
nePripagjaNaIdentichno(Element, _, _, [H]) :- Element \== H, !.
nePripagjaNaIdentichno(Element, I, Skip, [H|T]) :- Element \== H, NI is I + 1, nePripagjaNaIdentichno(Element, NI, Skip,T).

elementNaIndeks([H|_], 0, H) :- !.
elementNaIndeks([_|T], I, O) :- NI is I-1, elementNaIndeks(T, NI, O).


jade_constraints(ime(I), hrana(H), hobi(_), maica(_)) :- ime(I), 
    jade(ime(I),hrana(H)).

hobi_constraints(ime(I), hrana(H), hobi(Ho), maica(M), Preth) :-
    pripagjaNa([ime(I),hrana(H),hobi(Ho),maica(M)], Preth),
    ima_hobi(ime(I),hobi(Ho)),
	hobi_jadenje(ime(I),hobi(Ho), hrana(H)).



nosi_constraints(ime(I), hrana(H), hobi(Ho), maica(M), Preth) :-
    pripagjaNa([ime(I),hrana(H),hobi(Ho),maica(M)], Preth),
    nosi(ime(I), maica(M)).

sedenje_constraints([ime(X1), hrana(X2), hobi(X3), maica(X4)],
                    [ime(Y1), hrana(Y2), hobi(Y3), maica(Y4)],
                    [ime(Z1), hrana(Z2), hobi(Z3), maica(Z4)],
                    [ime(T1), hrana(T2), hobi(T3), maica(T4)]
                    ,Preth) :-
    pripagjaNa([ime(X1), hrana(X2), hobi(X3), maica(X4)],Preth), 
    pripagjaNa([ime(Y1), hrana(Y2), hobi(Y3), maica(Y4)],Preth), 
    pripagjaNa([ime(Z1), hrana(Z2), hobi(Z3), maica(Z4)],Preth),
    pripagjaNa([ime(T1), hrana(T2), hobi(T3), maica(T4)],Preth),
    X1\==Y1, X1\==Z1, X1\==T1, Y1\==Z1, Y1\==T1, Z1\==T1, X1=='Teo',
    sedi(ime(X1), hrana(X2), hobi(X3), maica(X4), ime(Y1), hrana(Y2), hobi(Y3), maica(Y4), ime(Z1), hrana(Z2), hobi(Z3), maica(Z4)),
    sedi(ime(Y1), hrana(Y2), hobi(Y3), maica(Y4), ime(Z1), hrana(Z2), hobi(Z3), maica(Z4), ime(T1), hrana(T2), hobi(T3), maica(T4)).

popolni_iminja(_, 4).
popolni_iminja(Iminja, I) :- elementNaIndeks(Iminja, I, Element),
    Element = ime(I1), ime(I1), nePripagjaNaIdentichno(ime(I1),0,I,Iminja), NI is I + 1, popolni_iminja(Iminja, NI), !.

popolni_hrani(_, 4).
popolni_hrani(Hrani, I) :- elementNaIndeks(Hrani, I, Element),
    Element = hrana(I1), hrana(I1), nePripagjaNaIdentichno(hrana(I1),0,I,Hrani), NI is I + 1, popolni_hrani(Hrani, NI), !.

popolni_hobija(_, 4).
popolni_hobija(Hobija, I) :- elementNaIndeks(Hobija, I, Element),
    Element = hobi(I1), hobi(I1), nePripagjaNaIdentichno(hobi(I1),0,I,Hobija), NI is I + 1, popolni_hobija(Hobija, NI), !.

popolni_maici(_, 4).
popolni_maici(Maici, I) :- elementNaIndeks(Maici, I, Element),
    Element = maica(I1), maica(I1), nePripagjaNaIdentichno(maica(I1),0,I,Maici), NI is I + 1, popolni_maici(Maici, NI), !.


reshenie(L) :- findall([ime(I),hrana(H),hobi(Ho),maica(M)],jade_constraints(ime(I), hrana(H), hobi(Ho), maica(M)), List1),
findall([ime(I1), hrana(H1), hobi(Ho1), maica(M1)],hobi_constraints(ime(I1), hrana(H1), hobi(Ho1), maica(M1), List1),List2),
findall([ime(I2), hrana(H2), hobi(Ho2), maica(M2)],nosi_constraints(ime(I2), hrana(H2), hobi(Ho2), maica(M2), List2), List3),
findall([[ime(X1), ime(Y1), ime(Z1), ime(T1)],
          [hrana(X2), hrana(Y2), hrana(Z2), hrana(T2)], 
        [hobi(X3), hobi(Y3), hobi(Z3), hobi(T3)],
          [maica(X4), maica(Y4), maica(Z4), maica(T4)]],
        sedenje_constraints([ime(X1), hrana(X2), hobi(X3), maica(X4)],
                            [ime(Y1), hrana(Y2), hobi(Y3), maica(Y4)], 
                            [ime(Z1), hrana(Z2), hobi(Z3), maica(Z4)],
                            [ime(T1), hrana(T2), hobi(T3), maica(T4)],List3),[Reshenie|_]),
   		Reshenie = [Iminja|[Hrani|[Hobija|[Maici|[]]]]],
                     popolni_iminja(Iminja, 0), popolni_hrani(Hrani, 0), popolni_hobija(Hobija,0), popolni_maici(Maici,0),
                     findall([I,Hr,Ho,M],
                             (
                             	pripagjaNa(Index, [0,1,2,3]),
                                elementNaIndeks(Iminja, Index, I),
                                elementNaIndeks(Hrani, Index, Hr),
                                elementNaIndeks(Hobija, Index, Ho),
                                elementNaIndeks(Maici, Index, M)
                             )
                             ,L).

