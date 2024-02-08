/* Zadacha 1 */

dolzina_parna([], W, W).
dolzina_parna(List, W, T) :- X is W+1, [_|Y] = List, 
    dolzina_neparna(Y, X, T).
dolzina_neparna(List, W, T) :- X is W+1, [_|Y] = List, 
    dolzina_parna(Y, X, T).

reverse_list(_, X, X, 0).
reverse_list(List, Result, O, N) :- X is N-1, [H|T] = List, 
    reverse_list(T, [H|Result], O,X). 


ednakvi_listi([],[]).
ednakvi_listi(A,B) :- [HA|TA] = A, [HB|TB] = B, HA = HB, 
    ednakvi_listi(TA,TB).


neparen_palindrom(Lista) :- A = Lista
    					, dolzina_neparna(Lista, 0, Dolzina)
    					, reverse_list(Lista, _, A1,Dolzina)
    					, ednakvi_listi(A,A1), !.



/* Zadacha 2 */
/* Koristam metodi od Zadacha br. 1 dopolnitelno */

dolzina([], W, W).
dolzina(List, W, T) :- X is W+1, [_|Y] = List, dolzina(Y, X, T).

zemiJaReversedPodnizata(_, 0, X, X).
zemiJaReversedPodnizata(Niza, N, Podniza, O) :- X is N-1, [H|T] = Niza, 
    zemiJaReversedPodnizata(T, X, [H|Podniza], O).

zemiJaPodnizata(Niza,N,W) :- zemiJaReversedPodnizata(Niza,N,[],X),reverse_list(X,[],W,N).

najdiPodnizi(_, -1, _, X, X).
najdiPodnizi(Niza, N, N, Res, O) :-zemiJaPodnizata(Niza, N, Podniza)
    							, najdiPodnizi(_, -1, N, [Podniza|Res], O).
najdiPodnizi(Niza, DolzinaNiza, N, Res, O) :- NewDolzina is DolzinaNiza-1
    							, [_|T] = Niza
    							, zemiJaPodnizata(Niza, N, Podniza)
    							, najdiPodnizi(T, NewDolzina, N, [Podniza|Res], O).

izbroj(_,[],X,X).
izbroj(Sto,Kade,Kolku,Izlez) :- [H|T] = Kade,
    							ednakvi_listi(Sto,H),
    							NewKolku is Kolku + 1,
    							izbroj(Sto,T,NewKolku,Izlez).
izbroj(Sto,Kade,Kolku,Izlez) :- [H|T] = Kade,
    							not(ednakvi_listi(Sto,H)),
    							izbroj(Sto,T,Kolku,Izlez).  

pripagjaNa(Element,[H|_]) :- Element = H.
pripagjaNa(Element, [_|T]) :- pripagjaNa(Element, T).

zemiPosledenElement([X],X).
zemiPosledenElement([_|T],O) :- zemiPosledenElement(T,O).

najdiGiSoNajmnogu(Lista, Kolku, Y) :- pripagjaNa([X,Y], Lista), X = Kolku. 


naj_podniza(L1,N,L2) :- dolzina(L1,0,D), 
  najdiPodnizi(L1,D,N,[],Podnizi),
  setof([Pojavuvanja, Proveri],
        (
          pripagjaNa(Proveri,Podnizi),
          izbroj(Proveri,Podnizi,0,Pojavuvanja)
        ),KrajniPojavuvanja), 
  zemiPosledenElement(KrajniPojavuvanja, [Kolku, _]), !,
  najdiGiSoNajmnogu(KrajniPojavuvanja, Kolku, L2), !.
% brishenjeto na posledniot cut operator kje gi dade site, 
% na ovoj nachin se dobiva samo prvata najdena 

/* Zadacha 3 */

proveri([Z|[X|[]]]) :- !, Z<X. 
proveri([H|T]) :- T = [_|_], proveriTretUslovPomalo(T, H), !.
proveriTretUslovPomalo([], _).
proveriTretUslovPomalo([H|T], Preth) :- Preth < H, proveriTretUslovPogolemo(T,H).
proveriTretUslovPogolemo([],_).
proveriTretUslovPogolemo([H|T], Preth) :- Preth > H, proveriTretUslovPomalo(T,H).

    
/* Zadacha 4 */

% podeli na eden element niza i druga niza
podeli(_, _, [], X, _, X) :- !.
podeli(Curr, Index, [ElDodade|O], TmpLista, Out1, Out2) :- Index =\= Curr, W is (Curr + 1)
										, podeli(W, Index, O, [ElDodade|TmpLista], Out1, Out2).
podeli(Curr, Index, [ElDodade|O], TmpLista, [ElDodade], Out2) :- Index == Curr, W is (Curr + 1)
											, podeli(W, Index, O, TmpLista, [ElDodade], Out2).

konkatenacija([],L, L).
konkatenacija([X|O],L,[X|NL]) :- konkatenacija(O,L,NL).

dodadi_go_prvoto_na_sekoe(P,[PrvaNiza|Ostanati],Res,Out) :- konkatenacija(P,PrvaNiza,Res1),
    										New = [Res1|Res],
    										dodadi_go_prvoto_na_sekoe(P,Ostanati,New,Out).
dodadi_go_prvoto_na_sekoe(_,[],X,X) :- !.


											
resh([X],[Y,Z], _, _, [[X,Y,Z],[X,Z,Y]], [[X,Y,Z],[X,Z,Y]]).
resh([],[X,Y],_,_,_,[[X,Y],[Y,X]]).
resh([],[X],_,_,_,[[X]]).
resh([],[],_,_,_,[]).
resh(_, _, X, X, Z, Z).
resh(Prv, Ost, Index, DolzinaOst, Site, Out) :- podeli(0, Index, Ost, [], P, V),
    														NewDolzina is DolzinaOst-1,
    														resh(P, V, 0, NewDolzina, _, IzlezSledno),
    														dodadi_go_prvoto_na_sekoe(Prv, IzlezSledno, [], Dodadeni),
    														NewIndex is Index+1,
   															konkatenacija(Site, Dodadeni, Zaedno),
    														resh(Prv, Ost, NewIndex, DolzinaOst, Zaedno, Out).

elementOd(X,[X|_]) :- ! .
elementOd(X,[_|L]) :- elementOd(X,L).


izbrishi_duplikati([],X,X) :- !.
izbrishi_duplikati([T|O],NewList,Out) :- elementOd(T,NewList), izbrishi_duplikati(O, NewList, Out).
izbrishi_duplikati([T|O],NewList,Out) :- izbrishi_duplikati(O, [T|NewList], Out).
    
permutacii(Lista,L) :- dolzina(Lista,0,Dolzina),resh([],Lista,0,Dolzina,[],L1),izbrishi_duplikati(L1,[],L),!.


/* Zadacha 5 */

% SOBIRANJE

modulAndIntDivision(X,N,C,OC,OM) :- W is X-N, W>=0, NC is C+1, modulAndIntDivision(W,N,NC,OC,OM).
modulAndIntDivision(X,N,C,C,X) :- W is X-N, W<0, !.

sobiranje_root([X],[Y], W, [O]) :-  Zbir is X+Y, modulAndIntDivision(Zbir,2,0,W,O), !. 
sobiranje_root([H1|T1],[H2|T2],Pam,Out) :-sobiranje_root(T1,T2,Pam1,Out1),
    									Zbir is (H1+H2+Pam1), 
    									modulAndIntDivision(Zbir,2,0,Pam,Mod),
    									Out = [Mod|Out1].
    									
polniXNuli([_], P ,[0|P]).
polniXNuli([_|T],P,O1) :- polniXNuli(T,P,O2), O1 = [0|O2].

napraviEdnakvaGolemina([],[],P1,P2,P1,P2).
napraviEdnakvaGolemina([],X,P1,P2,O1,O2) :- polniXNuli(X,P1,O), O1 = O, O2 = P2 .
napraviEdnakvaGolemina(X,[],P1,P2,O1,O2) :- polniXNuli(X,P2,O), O1 = P1, O2 = O.
napraviEdnakvaGolemina([_|T1],[_|T2],P1,P2,O1,O2) :- napraviEdnakvaGolemina(T1,T2,P1,P2,O1,O2).


dodadiPamtenje(0,X,X).
dodadiPamtenje(1,Out,[1|Out]).


sobiranje(L11,L22,L) :- napraviEdnakvaGolemina(L11,L22,L11,L22,L1,L2),
    					sobiranje_root(L1,L2,Pam,Out), 
    					dodadiPamtenje(Pam,Out,L), 
    					!.


%ODZEMANJE


/*odrediOdzemanjeCifra(W,1) :- W < 0.
odrediOdzemanjeCifra(W,W) :- W >=0.
*/
odrediSlednoPamtenje(0,1,-1,-1,0).
odrediSlednoPamtenje(0,1,0,-1,1).
odrediSlednoPamtenje(1,1,0,0,0).
odrediSlednoPamtenje(1,1,-1,-1,1).
odrediSlednoPamtenje(1,0,-1,0,0).
odrediSlednoPamtenje(1,0,0,0,1).
odrediSlednoPamtenje(0,0,-1,-1,1).
odrediSlednoPamtenje(0,0,0,0,0).

odzemanje_root([X],[Y], O, [Z]) :- odrediSlednoPamtenje(X,Y,0,O,Z).
odzemanje_root([H1|T1],[H2|T2],Pam,Out) :- odzemanje_root(T1,T2,Pam1,Out1),
    									odrediSlednoPamtenje(H1,H2,Pam1,Pam,C),
    										Out = [C|Out1].
    										
    										
    									
    									
proceniPamtenje(Pam,Out,Out) :- Pam = 0, !.
proceniPamtenje(Pam,_,[0]) :- Pam < 0, !.

izbrishiNuli([1|X],[1|X]).
izbrishiNuli([0|[]],[0]).
izbrishiNuli([0|O],T):-izbrishiNuli(O,T).
odzemanje(L1,L2,L) :- napraviEdnakvaGolemina(L1,L2,L1,L2,L11,L22),
    				  odzemanje_root(L11,L22,Pam,Out),
        				izbrishiNuli(Out,Izlez),
    				  proceniPamtenje(Pam,Izlez,L),
    				  !.
    
% MNOZHENJE

dodadiNulaNaKraj([],[0]).
dodadiNulaNaKraj([H|T],O) :- dodadiNulaNaKraj(T,O1), O = [H|O1].

izberiDodadenaNizaMnozhenje(1,MovingL1,MovingL1).
izberiDodadenaNizaMnozhenje(0,_,[0]).

mnozhenje_root(L, [1],L,[L]).
mnozhenje_root(L, [0],L,[[0]]).
mnozhenje_root(L1,[H2|T2],MovingL1,Nizi) :- mnozhenje_root(L1,T2,ML1,Res), 
    								dodadiNulaNaKraj(ML1,MovingL1),
    								izberiDodadenaNizaMnozhenje(H2,MovingL1,Dodadi),
    								Nizi = [Dodadi|Res].


soberi_gi_nizite([H|[]],MS,O) :- sobiranje(H,MS,O).
soberi_gi_nizite([H|T],MS,O) :- sobiranje(H,MS,O1), soberi_gi_nizite(T,O1,O).

mnozenje(L1,L2,L) :- mnozhenje_root(L1,L2,_,Nizi), soberi_gi_nizite(Nizi,[0],O), izbrishiNuli(O,L), !.

%DELENJE


pogolemoEdnakvoOdBinarniNizi([H1|_],[H2|_]) :- H1>H2, !.
pogolemoEdnakvoOdBinarniNizi([H1|[]],[H2|[]]) :- H1=H2, !.
pogolemoEdnakvoOdBinarniNizi([H1|T1],[H2|T2]) :- H1=H2, pogolemoEdnakvoOdBinarniNizi(T1,T2).

/*pomaloOdBinarniNizi([H1|_],[H2|_]) :- H1<H2, !.
pomaloOdBinarniNizi([H1|T1],[H2|T2]) :- H1=H2, pomaloOdBinarniNizi(T1,T2).
*/

delenje_root(L11,L22,C,O) :- napraviEdnakvaGolemina(L11,L22,L11,L22,L1,L2), 
    pogolemoEdnakvoOdBinarniNizi(L1,L2), 
    odzemanje(L1,L2,X), 
    sobiranje(C, [1], NewC), 
    napraviEdnakvaGolemina(X,L2,X,L2,NX,NL2), 
    delenje_root(NX,NL2,NewC,O).
delenje_root(_,_,C,C).
    
delenje(L1,L2,L) :- delenje_root(L1,L2,[0],OL), izbrishiNuli(OL,L), !.

/* Zadacha 6 */

rezultatMnozhenjeListi([],[],[]).
rezultatMnozhenjeListi([H1|T1],[H2|T2],R) :- Res is H1*H2, rezultatMnozhenjeListi(T1,T2,R1), R = [Res|R1].

mnozhi(_,[],[]).
mnozhi(Lista,[PrvaLista|OstanatiListi],O) :- rezultatMnozhenjeListi(Lista,PrvaLista,Rezultat),
    										mnozhi(Lista,OstanatiListi,O1), 
    										O = [Rezultat|O1].  


zbir([],0).
zbir([H1|T1], Rez) :- zbir(T1, R1), Rez is R1 + H1. 

zameniPodlistiSoZbir([],[]).
zameniPodlistiSoZbir([H1|T1],Izlez) :- zameniPodlistiSoZbir(T1,I1), zbir(H1, Res), Izlez = [Res|I1].

presmetaj_root([],_,[]).
presmetaj_root([H1|T1],Lista,R) :- mnozhi(H1,Lista,Podlisti),
                                  zameniPodlistiSoZbir(Podlisti, Izlez),
                                  presmetaj_root(T1,Lista,R1),
    								R = [Izlez|R1].

presmetaj(Lista,R) :- presmetaj_root(Lista,Lista,R), !.


/* Zadacha 7 */

swap_helper([],_,_,_,_,_,_,_,[]). 
swap_helper([H|T],I1,I2,CI,ElI1,ElI2,PrvaLista,VtoraLista,TretaLista) :- CI < I1, NI1 is CI + 1, 
    															swap_helper(T,I1,I2,NI1,ElI1,ElI2,PL1,VtoraLista,TretaLista),
    															PrvaLista = [H|PL1].

swap_helper([H|T],I1,I2,CI,ElI1,ElI2,PrvaLista,VtoraLista,TretaLista) :- CI == I1, NI1 is CI + 1, 
    															swap_helper(T,I1,I2,NI1,ElI1,ElI2,_,VtoraLista,TretaLista),
    															PrvaLista = [], ElI1 = H.

swap_helper([H|T],I1,I2,CI,ElI1,ElI2,PrvaLista,VtoraLista,TretaLista) :- CI == I2, NI1 is CI + 1, 
    															swap_helper(T,I1,I2,NI1,ElI1,ElI2,PrvaLista,_,TretaLista),
    															VtoraLista = [], ElI2 = H.

swap_helper([H|T],I1,I2,CI,ElI1,ElI2,PrvaLista,VtoraLista,TretaLista) :- CI > I1, CI < I2, NI1 is CI + 1, 
    															swap_helper(T,I1,I2,NI1,ElI1,ElI2,PrvaLista,VL1,TretaLista),
    															VtoraLista = [H|VL1].



swap_helper([H|T],I1,I2,CI,ElI1,ElI2,PrvaLista,VtoraLista,TretaLista) :- CI > I2, NI1 is CI + 1, 
    															swap_helper(T,I1,I2,NI1,ElI1,ElI2,PrvaLista,VtoraLista,TL1),
    															TretaLista = [H|TL1].


dodadiKraj([],El,[El]).
dodadiKraj([H|T],El,O) :- dodadiKraj(T,El,O1), O = [H|O1].

spoiListi([],L2,L2).
spoiListi([H|T],L2,NL) :- spoiListi(T,L2,NL1), NL = [H|NL1].

swap(Lista,I1,I2,O) :- swap_helper(Lista,I1,I2,0,ElI1,ElI2,PL,VL,TL), dodadiKraj(PL,ElI2,R1), dodadiKraj(VL,ElI1,R2), 
    							spoiListi(R1,R2,MR), spoiListi(MR,TL,O), !.


dobiListaOdDolzhini([],[]).
dobiListaOdDolzhini([H|T], [Dolzhina|O1]) :- dolzina(H, 0 ,Dolzhina), dobiListaOdDolzhini(T, O1).




sporediListi([H1|_],[H2|_],Ako1,_,Ako1):- H1>H2,!.
sporediListi([H1|_],[H2|_],_,Ako2,Ako2):- H1<H2,!.
sporediListi([],[],_,_,1).
sporediListi([H1|T1],[H2|T2],Ako1,Ako2,O):- H1=H2,sporediListi(T1,T2,Ako1,Ako2,O).

transformirajLista(Param,Lista,_,_,OD):- Param = 2, OD = Lista.
transformirajLista(Param,Lista,I1,I2,OD):- Param = 1, swap(Lista,I1,I2,OD). 

newCurrent(0,X,X,X). 
newCurrent(1,_,X,X).
newCurrent(2,X,_,X).

sortirajSporedDolzhina_helper(D,V,_,_,X,_,_,_,X,D,V).

% D = data- listi V =values- dolzini
sortirajSporedDolzhina_helper(D, V,[HV|TV],[HD|TD],Dolzhina,CEl,_,CI1,CI2,O1,O2) :- CEl < HV, 
    																swap(D,CI1,CI2,OD), 
    																swap(V,CI1,CI2,OV),
    																NCI2 is CI2 + 1,
    																sortirajSporedDolzhina_helper(OD,OV,TV,TD,Dolzhina,HV,HD,CI1,NCI2,O1,O2).

sortirajSporedDolzhina_helper(D,V,[HV|TV],[_|TD],Dolzhina,CEl,CL,CI1,CI2,O1,O2) :- CEl > HV,
    																NCI2 is CI2 + 1,
    																sortirajSporedDolzhina_helper(D,V,TV,TD,Dolzhina,CEl,CL,CI1,NCI2,O1,O2).

sortirajSporedDolzhina_helper(D,V,[HV|TV],[HD|TD],Dolzhina,CEl,CL,CI1,CI2,O1,O2) :- CEl = HV,
    																sporediListi(HD,CL,1,2,Res),
    																transformirajLista(Res,D,CI1,CI2,OD),
    																transformirajLista(Res,V,CI1,CI2,OV),
    																newCurrent(Res,HD,CL,OL),
    																NCI2 is CI2 + 1,
    																sortirajSporedDolzhina_helper(OD,OV,TV,TD,Dolzhina,CEl,OL,CI1,NCI2,O1,O2).


skokniNLista([],_,[]).
skokniNLista([_|T],N,O) :- N > 0, NN is N - 1, skokniNLista(T,NN,O).
skokniNLista([H|T],N,O) :- N =< 0, skokniNLista(T,N,O1), O = [H|O1].
             
sortiraj(L,_,_,_,X,X,L).
sortiraj(Listi, Dolzhini, [P|Ost], [PV|OstV], Dolzhina, CI, O) :- CI =\= Dolzhina,
    												NCI is CI + 1,
         											sortirajSporedDolzhina_helper(Listi,Dolzhini,OstV,Ost,Dolzhina,PV,P,CI,NCI,OListi,ODolzhini),
    												skokniNLista(ODolzhini,NCI,NewOst),
    												skokniNLista(OListi,NCI,NewOstL),
    												sortiraj(OListi, ODolzhini, NewOstL,NewOst, Dolzhina, NCI, O).
reverse([],X,X).
reverse([H|T],Res,O):- reverse(T,[H|Res],O). 

transform(VL,L) :- dobiListaOdDolzhini(VL,DL), dolzina(VL,0,Dolzina),sortiraj(VL, DL, VL, DL, Dolzina, 0, L1), izbrishi_duplikati(L1,[],L2), reverse(L2,[],L),!.
    
    
    
/* Zadacha 8 */

daliENajden(A,A,0).
daliENajden(X,Y,1) :- X\==Y.

dolzinaNested([],X,X):-!.
dolzinaNested([[]|T],N,O):- dolzinaNested(T,N,O),!.
dolzinaNested(Lista,N,O) :- Lista = [[_|_]|_], 
    Lista = [H|T], 
    dolzinaNested(H,0,O1),
    W is N + O1,
    dolzinaNested(T,W,O),!.
dolzinaNested([H|T],N,O) :- H\==[_|_], H\==[], W is N+1, dolzinaNested(T, W, O),!.


najdiPrvaPoslePozicija(_,[],_,_,_,X,X).

najdiPrvaPoslePozicija(Element,Lista,MP,Pozicija,Najden,Res,O) :-
    Najden = 1, [H|T] = Lista, dodadiKraj(Res,H,Res1),
    najdiPrvaPoslePozicija(Element,T,MP,Pozicija,Najden,Res1,O).

najdiPrvaPoslePozicija(Element,Lista,MP,Pozicija,Najden,Res,O):-
    Najden = 0, [[_|_]|_] = Lista, [A|T] = Lista,
    najdiPrvaPoslePozicija(Element,A,MP,Pozicija,Najden,[],O1),
    daliENajden(A,O1,Najden1),
    dodadiKraj(Res,O1,Res2),
    dolzinaNested(O1,0,D),
    NMP is MP + D,
    najdiPrvaPoslePozicija(Element,T,NMP,Pozicija,Najden1,Res2,O).

najdiPrvaPoslePozicija(Element,Lista,MP,Pozicija,Najden,Res,O):-
    Najden = 0, [[]|T] = Lista,
    dodadiKraj(Res,[],Res2),
    najdiPrvaPoslePozicija(Element,T,MP,Pozicija,Najden,Res2,O).

najdiPrvaPoslePozicija(Element,Lista,MomentalnaPozicija,Pozicija,Najden,Res,O):-
    Najden = 0, [H|A] = Lista, MomentalnaPozicija>Pozicija, H = Element,
    NewPozicija is MomentalnaPozicija + 1,
    najdiPrvaPoslePozicija(Element,A,NewPozicija,Pozicija,1,Res,O).

najdiPrvaPoslePozicija(Element,Lista,MomentalnaPozicija,Pozicija,Najden,Res,O):-
    Najden = 0, [H|A] = Lista, MomentalnaPozicija>Pozicija, H \== Element,
    dodadiKraj(Res,H,Res1), NewPozicija is MomentalnaPozicija + 1,
    najdiPrvaPoslePozicija(Element,A,NewPozicija,Pozicija,Najden,Res1,O).

najdiPrvaPoslePozicija(Element,Lista,MomentalnaPozicija,Pozicija,Najden,Res,O):-
    [H|A] = Lista, MomentalnaPozicija<Pozicija, dodadiKraj(Res,H,Res1),
    NewPozicija is MomentalnaPozicija + 1,
    najdiPrvaPoslePozicija(Element,A,NewPozicija,Pozicija,Najden,Res1,O).

najdiPrvaPoslePozicija(Element,Lista,MomentalnaPozicija,Pozicija,Najden,Res,O):-
    [H|A] = Lista, MomentalnaPozicija=Pozicija, Element = H, dodadiKraj(Res,H,Res1),
    NewPozicija is MomentalnaPozicija + 1,
    najdiPrvaPoslePozicija(Element,A,NewPozicija,Pozicija,Najden,Res1,O).
                               
iteriraj(X,Pozicija,X):- D is Pozicija+1, dolzinaNested(X,0,D1), D1=<D.
iteriraj(Lista, Pozicija, O) :- 
    najdiPrvaPoslePozicija(_,Lista,0,Pozicija,0,[],Output),
    NewPozicija is Pozicija + 1,
    iteriraj(Output,NewPozicija, O).

brisi_sekoe_vtoro(Lista,O):- iteriraj(Lista,0,O), !.