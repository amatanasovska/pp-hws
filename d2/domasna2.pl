%%%% Zadacha 1

% Fakti
% lice(Sifra,Ime,Prezime,Pol,Data_raganje,Mesto_raganje,Mesto_ziveenje),
% familija(Sifra_tatko,Sifra_majka,Lista_sifri_deca) 
lice(1,petko,petkovski,m,datum(1,3,1950),kratovo,skopje).
lice(2,marija,petkovska,z,datum(30,5,1954),kumanovo,skopje).
lice(3,ljubica,petkovska,z,datum(29,11,1965),skopje,skopje).
lice(4,vasil,vasilev,m,datum(8,4,1954),bitola,bitola).
lice(5,elena,vasileva,z,datum(19,6,1958),resen,bitola).
lice(6,krste,krstev,m,datum(9,8,1948),veles,veles).
lice(7,biljana,krsteva,z,datum(13,8,1949),veles,veles).
lice(8,igor,krstev,m,datum(26,10,1971),veles,skopje).
lice(9,kristina,krsteva,z,datum(30,5,1974),kumanovo,skopje).
lice(10,julija,petrova,z,datum(30,5,1978),skopje,skopje).
lice(11,bosko,petkovski,m,datum(13,11,1981),skopje,skopje).
lice(12,gjorgji,vasilev,m,datum(15,7,1978),bitola,bitola).
lice(13,katerina,petkovska,z,datum(11,12,1979),bitola,skopje).
lice(14,petar,vasilev,m,datum(21,2,1982),skopje,skopje).
lice(15,andrej,krstev,m,datum(3,8,1998),skopje,skopje).
lice(16,martina,petkovska,z,datum(5,12,2005),skopje,skopje).
familija(1,2,[9,10]).
familija(1,3,[11]).
familija(4,5,[12,13,14]).
familija(6,7,[8]).
familija(8,9,[15]).
familija(11,13,[16]).

%% Baranje A

pripagjaNa(Element,[H|_]) :- Element = H.
pripagjaNa(Element, [_|T]) :- pripagjaNa(Element, T).

ispolnet_uslov(D) :- familija(X,Y,P), 
      pripagjaNa(D, P), 
      lice(X,_,_,_,_,GRR1,_), 
      lice(Y,_,_,_,_,GRR2,_),
      lice(D,_,_,_,_,GRD,_),
      GRD \== GRR1,
      GRD \== GRR2.

dolzhina([],0).
dolzhina([_|T], O) :- dolzhina(T, O1), O is O1 + 1.

rodeni_razlicen_grad(Kolku) :-findall(D, ispolnet_uslov(D),List),
    						dolzhina(List, Kolku). 

%% Baranje B
konkatenacija([],X,X).
konkatenacija([H|T],L2,L) :- konkatenacija(T,L2,O), L = [H|O]. 

prethodenMesec(X,W) :- X \== 1, W is X-1.
prethodenMesec(1,12).

sledenMesec(X,W) :- X \== 12, W is X+1.
sledenMesec(12,1).

uslovMesecPred7Dena(ML1,MP,DP,NDL1,_) :-
    ML1 == MP,
    DP>=NDL1.

uslovMesecPred7Dena(ML1,MP,DP,_,DL) :- 
    sledenMesec(ML1,W),
    W == MP,
    DP=<DL.

uslovMesecPosle7Dena(ML1,MP,DP,NDL1,_) :-
    ML1 == MP,
    DP=<NDL1.

uslovMesecPosle7Dena(ML1,MP,DP,_,DL) :- 
    prethodenMesec(ML1,W),
    W == MP,
    DP>=DL.

pred7Dena(DL,ML,DP,MP) :- NDL is DL-7,
    NDL>0,
    DP>=NDL,
    DP=<DL,
    MP == ML, !.
pred7Dena(DL,ML,DP,MP) :- NDL is DL-7,
    NDL=<0,
    Falat is (-1) * NDL,
    NDL1 is 30 - Falat,
    prethodenMesec(ML,ML1),
    uslovMesecPred7Dena(ML1,MP,DP,NDL1,DL), !.
	

posle7Dena(DL,ML,DP,MP) :- NDL is DL+7,
    NDL=<30,
    DP=<NDL,
    DP>=DL,
    MP == ML, !.

posle7Dena(DL,ML,DP,MP) :- NDL is DL+7,
    NDL>30,
    NDL1 is NDL - 30,
	sledenMesec(ML,ML1),
	uslovMesecPosle7Dena(ML1,MP,DP,NDL1,DL), !.

proveriDatum(DL,ML,DP,MP) :- pred7Dena(DL,ML,DP,MP), !.
proveriDatum(DL,ML,DP,MP) :- posle7Dena(DL,ML,DP,MP), !. 


proveriUslovi(Predok,Pol,Godina,PrethOut,Out) :- 
    lice(Predok,_,_,PolPredok,datum(DP,MP,_),_,_),
    PolPredok == Pol,
    Godina = datum(DL,ML,_),
    proveriDatum(DL,ML,DP,MP),
    Out = [Predok|PrethOut].
proveriUslovi(_,_,_,X,X). 

najdi_predci(H,Pol,Datum,Output) :- familija(X,Y,P), pripagjaNa(H,P),
    						najdi_predci(X,Pol,Datum,O1), 
                            najdi_predci(Y,Pol,Datum,O2),
    						konkatenacija(O1,O2,O),
    						proveriUslovi(X,Pol,Datum,O,OR1),
    						proveriUslovi(Y,Pol,Datum,OR1,Output), !.
najdi_predci(_,_,_,[]). 

predci(Sifra,L) :- lice(Sifra,_,_,Pol,Datum,_,_), najdi_predci(Sifra,Pol,Datum,L).


%%%% Zadacha 2

% telefon(Broj,Ime,Prezime,Lista_na_pojdovni_povici),
%povik(Povikan_broj,Traenje).
% SMS(Broj_koj_ja_ispraka_porakata,Lista_broevi_koi_ja_dobivaat_porakata)

telefon(111111,petko,petkovski,[povik(222222,250),povik(101010,125)]).
telefon(222222,marija,petkovska,[povik(111111,350),povik(151515,113),povik(171717,122)]).
telefon(333333,ljubica,petkovska,[povik(555555,150),povik(101010,105)]).
telefon(444444,vasil,vasilev,[povik(171717,750)]).
telefon(555555,elena,vasileva,[povik(333333,250),povik(101010,225)]).
telefon(666666,krste,krstev,[povik(888888,75),povik(111111,65),povik(141414,50),povik(
161616,111)]).
telefon(777777,biljana,krsteva,[povik(141414,235)]).
telefon(888888,igor,krstev,[povik(121212,160),povik(101010,225)]).
telefon(999999,kristina,krsteva,[povik(666666,110),povik(111111,112),povik(222222,55)]
).
telefon(101010,julija,petrova,[]).
telefon(121212,bosko,petkovski,[povik(444444,235)]).
telefon(131313,gjorgji,vasilev,[povik(141414,125),povik(777777,165)]).
telefon(141414,katerina,petkovska,[povik(777777,315),povik(131313,112)]).
telefon(151515,petar,vasilev,[]).
telefon(161616,andrej,krstev,[povik(666666,350),povik(111111,175),povik(222222,65),povik(101010,215)]).
telefon(171717,martina,petkovska,[povik(222222,150)]).
sms(111111,[222222,999999,101010]).
sms(444444,[333333,121212,161616]).
sms(111111,[777777]).
sms(666666,[888888]).
sms(444444,[555555,121212,131313,141414]).
sms(666666,[777777,888888]).
sms(888888,[999999,151515]).
sms(171717,[131313,161616]).

%% Baranje A

zemiPosledenElement([X],X).
zemiPosledenElement([_|T],O) :- zemiPosledenElement(T,O).



%% Ovoj predikat se koristi za zbirno toj shto ima zbirno pochnato + primeno najmnogu povici 
%% Se naogjaat site sho imaat maksimum 
najdiLista(Lista) :- findall([D, S],
        (
            telefon(S,_,_,PoviciKorisnik),
            findall(X, pripagjaNa(povik(X,_), PoviciKorisnik), Broevi1),
            findall(S1, (
                                    telefon(S1, _, _, Povici),
                                    S1 \== S, 
                                    pripagjaNa(povik(S,_), Povici)
                                ), 
                    Broevi2),
            konkatenacija(Broevi1,Broevi2,Broevi),
            setof(B, pripagjaNa(B,Broevi), FinalBroevi),
            dolzhina(FinalBroevi, D)
        ),
        ListaShifraKolku),
	setof(El, pripagjaNa(El,ListaShifraKolku), Lista).

najdiGiSoNajmnogu(Lista, Kolku, Y) :- pripagjaNa([X,Y], Lista), X = Kolku, !. 
% odzemanjeto na cut operatorot kje ovozmozhi da se vratat site dokolku gi ima poishe

najbroj(X,Y) :- najdiLista(Lista),
    zemiPosledenElement(Lista,[Kolku,_]),
    najdiGiSoNajmnogu(Lista, Kolku, Broj),
    telefon(Broj,X,Y,_).

%% Baranje B

suma([],0).
suma([H|T],O) :- suma(T,O1), O is H+O1. 

smsOdDo(O, D, O1) :- sms(O,Primachi), pripagjaNa(D,Primachi), 
    O1 = 100.

minutiOdSMS(O, D, O1) :- findall(Izlez,smsOdDo(O, D, Izlez),Lista),
    suma(Lista,O1).
    
najdiSuma(X,Pojdovni,Broj,Minuti) :- telefon(Broj,_,_,Dojdovni),
    findall(MP,pripagjaNa(povik(Broj,MP),Pojdovni),LP),
    findall(MD,pripagjaNa(povik(X,MD),Dojdovni),LD),
    minutiOdSMS(X,Broj,MS1),
    minutiOdSMS(Broj,X,MS2),
    suma(LP,M1),
    suma(LD,M2),
    Minuti is M1 + M2 + MS1 + MS2.

postaviSledenBroj(MaxMin,Broj,NewMax,_,MaxMin,Broj) :- 
    MaxMin>=NewMax.
postaviSledenBroj(MaxMin,_,NewMax,NewBroj,NewMax,NewBroj) :- 
    MaxMin<NewMax.

maksimumMinuti([],M,X,X,M).
maksimumMinuti([H|T],Max,Br,BrO,M) :-
    H = [Broj, Minuti], 
    postaviSledenBroj(Max,Br,Minuti,Broj,NewMax,NewBroj),
    maksimumMinuti(T,NewMax,NewBroj,BrO,M).

omilen(X,Y) :- telefon(X,_,_,Pojdovni),
    findall([Broj,Minuti],najdiSuma(X,Pojdovni,Broj,Minuti),Site),
    maksimumMinuti(Site,0,_,_,Max),
    pripagjaNa([Y,Max],Site). % , !.
% --> dodavanje na cut operatorot kje ovozmozhi da se vrati samo prviot najden

%%%% Zadacha 3
klient(1,petko,petkov,[usluga(a,b,50,datum(12,12,2015),23),usluga(c,a,50,datum(7,12,2015)
,34),usluga(c,f,40,datum(7,11,2015),23)]).
klient(2,vasil,vasilev,[usluga(a,e,50,datum(25,12,2015),12),usluga(c,g,40,datum(17,11,2015),56),usluga(g,d,50,datum(17,12,2015),45),
                         usluga(e,a,40,datum(24,12,2015),34)]).
klient(3,krste,krstev,[usluga(c,b,60,datum(31,12,2015),56),usluga(e,f,60,datum(31,12,2015),34)]).
klient(4,petar,petrov,[usluga(a,f,50,datum(25,12,2015),23),usluga(f,d,50,datum(25,12,2015
),34)]).
klient(5,ivan,ivanov,[usluga(d,g,50,datum(7,12,2015),56),usluga(g,e,40,datum(25,12,2015),
34)]).
klient(6,jovan,jovanov,[usluga(c,f,50,datum(5,12,2015),12),usluga(f,d,50,datum(27,12,2015),45)]).
klient(7,ana,aneva,[usluga(e,d,50,datum(11,12,2015),12),usluga(d,g,50,datum(11,12,2015),12)]).
klient(8,lidija,lideva,[usluga(e,g,50,datum(29,12,2015),45),usluga(f,b,50,datum(29,12,2015),34)]).

rastojanie(a,b,4).
rastojanie(a,c,7).
rastojanie(b,c,5).
rastojanie(b,d,3).
rastojanie(c,d,4).
rastojanie(b,e,6).
rastojanie(c,e,2).
rastojanie(b,f,8).
rastojanie(e,f,5).
rastojanie(f,g,3).
%% Baranje A

pochetna_ili_krajna(Lok, All) :- pripagjaNa(usluga(Lok,_,_,_,_),All).
pochetna_ili_krajna(Lok, All) :- pripagjaNa(usluga(Lok1,Lok,_,_,_),All).%, Lok1 \== Lok.
%delov mozhe da se dodade dokolku ako edna lokacija e i pochetna i krajna da se broi kako ednash
%kako momentalno shto e navedena kje se broi 2 pati edna usluga

izbroj_lokacija(Lok,O) :- 
    findall(usluga(A,B,C,D,E),
            (
            	klient(_,_,_,Uslugi),
                pripagjaNa(usluga(A,B,C,D,E),Uslugi)
            ),
            List),
    findall(Lok,pochetna_ili_krajna(Lok,List),Site),
    dolzhina(Site,O).

%% Baranje B

nePripagjaNa(_,[]).
nePripagjaNa(Element,[H|[]]) :- Element \== H.
nePripagjaNa(Element, [H|T]) :- Element \== H, nePripagjaNa(Element, T).

iskalkuliraj_rastojanie(A,A,_,0).
iskalkuliraj_rastojanie(Pochetna,Krajna,Poseteni,O) :- 
    rastojanie(Pochetna,X,E),
    nePripagjaNa(X,Poseteni),
	iskalkuliraj_rastojanie(X,Krajna,[X|Poseteni],O1),
    O is E + O1.

iskalkuliraj_rastojanie(Pochetna,Krajna,Poseteni,O) :- 
    rastojanie(X,Pochetna,E),
    nePripagjaNa(X,Poseteni),
	iskalkuliraj_rastojanie(X,Krajna,[X|Poseteni],O1),
    O is E + O1.

uslugaRastojanie(Uslugi,O) :-
    pripagjaNa(usluga(F,L,_,_,_),Uslugi),
    setof(Rastojanie,iskalkuliraj_rastojanie(F,L,[],Rastojanie),[O|_]).
    

kilometri_klient(X,Y,O) :-
    klient(_,X,Y,Uslugi),
	findall(O,uslugaRastojanie(Uslugi,O),List),
    suma(List,O).

najmnogu_kilometri(X,Y) :-
    setof([O,I,P],kilometri_klient(I,P,O),List), 
    zemiPosledenElement(List,[Km,_,_]),
    pripagjaNa([Km,X,Y], List). % , !. 
% se dodava cut operator dokolku treba samo prviot

%% Baranje V


uslugaCena(Uslugi,O) :-
    pripagjaNa(usluga(F,L,C,_,_),Uslugi),
    setof(Rastojanie,iskalkuliraj_rastojanie(F,L,[],Rastojanie),[NajmaloR|_]),
    O is C*NajmaloR.


vkupnaCenaTaksi(X,O) :-
    findall(usluga(A,B,C,datum(D,12,2015),X),(
                              klient(_,_,_,U),
                              pripagjaNa(usluga(A,B,C,datum(D,12,2015),X),U)
                              ),Uslugi),
	findall(Cena,uslugaCena(Uslugi,Cena),List),
    suma(List,O).



cenaZaTaksist(Taksisti,Taksist,Cena) :- pripagjaNa(Taksist,Taksisti),
    vkupnaCenaTaksi(Taksist,Cena).

najmnogu_zarabotil(X) :- setof(El, 
                               (
                               		findall(Taksist,(
                                                    klient(_,_,_,U), 
                                                    pripagjaNa(usluga(_,_,_,datum(_,12,2015),Taksist),U)
                                                    ),List),
                                   	pripagjaNa(El,List)
                               ),Elementi), 
    setof([C,T],cenaZaTaksist(Elementi,T,C),Lista),
    zemiPosledenElement(Lista,[MxSuma,_]),
    pripagjaNa([MxSuma,X], Lista). %, !.
% se dodava cut operator dokolku treba samo prviot
    
    
  
    