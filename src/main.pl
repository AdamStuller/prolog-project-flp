:- dynamic runner/7.
:- op(600,xfy, : ).
:- op(601,xfy,  - ).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hlavny cyklus programu
% main()

main:-	        %uz pri spusteni moze nacitat databazu
	repeat,
	menu,
    get(C),
    nl,
	execute(C),
	C == 57,
	writeln('Koniec prace.').

%%%
% Menu rozsirite podla zadania
% menu
menu:-
	nl,
	writeln('1 - citanie zo suboru'),
	writeln('2 - zapis do suboru'),
    writeln('3 - vypis vsetkych pretekarov'),
    writeln('4 - zoradit podla'),
    writeln('5 - vyhladat podla'),
    writeln('6 - pridat pretekara'),
    writeln('7 - vymazat pretekara'),
	writeln('9 - koniec prace systemu'),
	writeln('------------------------'),
	nl.

%%%
% vykonanie vybranej moznosti
% vykonaj(+Code)

execute(49):-read_db('input_db.txt'),!.
execute(50):-
    writeln('Zadajte nazov suboru: '),
    read(S),
    write_db(S),!. 
execute(51):-show,!.
execute(52):-nl,sort_runners,!.
execute(53):-nl,search_runners,!.
execute(54):-nl,insert,!.
execute(55):-nl,remove,!.
execute(57):-!.
execute(_):-writeln('Pouzivaj len urcene znaky!').

read_db(S):-
	retractall(runner(_,_,_,_, _, _, _)),
	see(S),
	repeat,
	read(Term),
	(
	    Term = end_of_file,
	    !,
	    seen
	    ;
	    assertz(Term),
	    fail
	).

write_db(S):-
	tell(S),
	runner(Firstname,Lastname,Team, Country, Date, Result, Number),
	writeq(runner(Firstname,Lastname,Team, Country, Date,Result, Number)),
	write('.'),
	nl,
	fail.
write_db(_):-told.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sluzi na vypis
% vypis()

show:-
	runner(Firstname,Lastname,Team, Country, Date, Result, Number),
    format('~w: ~w ~w, ~w, ~w, start date: ~w, result: ~w\n', [Number, Firstname, Lastname, Team, Country, Date, Result]),
    nl,
	fail.
show.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Vlozit pretekara insert/0
insert:-
    write('Zadajte meno pretekara: '),
    read(Firstname),
    write('Zadajte priezvysko pretekara: '),
    read(Lastname),
    write('Zadajte tim pretekara: '),
    read(Team),
    write('Zadajte krajinu pretekara: '),
    read(Country),
    write('Zadajte datum pretekov v tvare DD-MM-YY: '),
    read(Date),
    write('Zadajte vysledok pretekara v tvare HH:MM: '),
    read(Result),
    get_id(Number),
    format('~w: ~w ~w, ~w, ~w, start date: ~w, result: ~w\n', [Number, Firstname, Lastname, Team, Country, Date, Result]),
    assertz(runner(Firstname, Lastname, Team, Country, Date, Result, Number)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Odstranit pretekara remove/0
remove:-
    write('Zadajte  poradove cislo pretekara na vymazanie: '),
    read(Number),
    write(Number),
    runner(Firstname, Lastname, Team, Country, Date, Result, Number),
    format('~w: ~w ~w bude vymazany.\n', [Number, Firstname, Lastname]),
    retract(runner(Firstname, Lastname, Team, Country, Date, Result, Number)).
    


% Vrati najmensie nasledujuce id z pretekarov. V databaze uz nieco musi byt. Nakolko treba mat nejaky jedinecny atribut aby sme vedeli rozlisovat
% medzi pretekarmi, pred vkladanim pretekarov !!!!treba mat aspon jedneho pretekara v databaze!!. 
get_id(Id):-
    findall(Number, runner(_,_,_, _, _, _, Number), List),
    max_list(List, Max),
    Id is Max + 1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Vyhladanie pretekara podla atributu

% search_runners/0
search_runners:-
    writeln('Zadaj atribut : '),
    read(Attr),
    writeln('Zadaj hodnotu: '),
    read(Value),
    search_by(Attr, Value).

% search_by(+Attr, +Value)
search_by(Attr, Value):-
    Attr == 'Number',
    runner(F, L, T, C, D, R, Value),
    format('~w: ~w ~w, ~w, ~w, start date: ~w, result: ~w\n', [Value, F, L, T, C, D, R]), fail.

search_by(Attr, Value):-
    Attr == 'Firstname',
    runner(Value, L, T, C, D, R, N),
    format('~w: ~w ~w, ~w, ~w, start date: ~w, result: ~w\n', [N, Value, L, T, C, D, R]), fail.

search_by(Attr, Value):-
    Attr == 'Lastname',
    runner(F, Value, T, C, D, R, N),
    format('~w: ~w ~w, ~w, ~w, start date: ~w, result: ~w\n', [N, F, Value, T, C, D, R]), fail.

search_by(Attr, Value):-
    Attr == 'Team',
    runner(F, L, Value, C, D, R, N),
    format('~w: ~w ~w, ~w, ~w, start date: ~w, result: ~w\n', [N, F, L, Value, C, D, R]), fail.

search_by(Attr, Value):-
    Attr == 'Coutry',
    runner(F, L, T, Value, D, R, N),
    format('~w: ~w ~w, ~w, ~w, start date: ~w, result: ~w\n', [N, F, L, T, Value, D, R]), fail.

search_by(Attr, Value):-
    Attr == 'Date',
    runner(F, L, T, C, Value, R, N),
    format('~w: ~w ~w, ~w, ~w, start date: ~w, result: ~w\n', [N, F, L, T,  C, Value, R]), fail.

search_by(Attr, Value):-
    Attr == 'Result',
    runner(F, L, T, C, D, Value, N),
    format('~w: ~w ~w, ~w, ~w, start date: ~w, result: ~w\n', [N, F, L, T,  C, D, Value]), fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Triedenie pretekarov podla atributu

% sort_runners/0
sort_runners:-
    writeln('Zadajte atribut: '),
    read(Attr),
    sort_by(Attr).


% sort_by(+Attr)
sort_by(Attr):-
    Attr == 'Number',
    findall(Number , runner(_,_,_, _, _, _, Number), List),
    sort(0, @=<, List, Sorted),
    vypis(Sorted).

sort_by(Attr):-
    Attr == 'Firstname',
    findall(Firstname-Number , runner(Firstname,_,_, _, _, _, Number), List),
    keysort(List, Pairs),
    pairs_values(Pairs, Sorted),
    vypis(Sorted).

sort_by(Attr):-
    Attr == 'Lastname',
    findall(Lastname-Number , runner(_,Lastname,_, _, _, _, Number), List),
    keysort(List, Pairs),
    pairs_values(Pairs, Sorted),
    vypis(Sorted).

sort_by(Attr):-
    Attr == 'Team',
    findall(Team-Number , runner(_,_,Team, _, _, _, Number), List),
    keysort(List, Pairs),
    pairs_values(Pairs, Sorted),
    vypis(Sorted).

sort_by(Attr):-
    Attr == 'Country',
    findall(Country-Number , runner(_,_,_, Country, _, _, Number), List),
    keysort(List, Pairs),
    pairs_values(Pairs, Sorted),
    vypis(Sorted).

sort_by(Attr):-
    Attr == 'Date',
    findall((Y-M-D)-Number, runner(_,_,_, _, D-M-Y, _, Number), List), 
    keysort(List, Pairs),
    pairs_values(Pairs, Sorted),
    vypis(Sorted).

sort_by(Attr):-
    Attr == 'Result',
    findall(Result-Number, runner(_,_,_, _, _, Result, Number), List), 
    keysort(List, Pairs),
    pairs_values(Pairs, Sorted),
    vypis(Sorted).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Vypis po sortovani
vypis([H|T]):-
    runner(Firstname, Lastname, Team, Country, Date, Result, H),!,
    format('~w: ~w ~w, ~w, ~w, start date: ~w, result: ~w\n', [H, Firstname, Lastname, Team, Country, Date, Result]),
    vypis(T).

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    