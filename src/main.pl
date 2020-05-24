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
	writeln('2 << meno suboru >> - zapis do suboru'),
    writeln('3 - vypis vsetkych pretekarov'),
    writeln('4 - zoradit podla startovacieho cisla '),
    writeln('5 - zoradit podla krajiny '),
    writeln('6 - zoradit podla mena '),
    writeln('7 - zoradit podla datumu '),
    writeln('8 - zoradit podla casu '),
	writeln('9 - koniec prace systemu'),
	writeln('------------------------'),
	nl.







%%%
% vykonanie vybranej moznosti
% vykonaj(+Code)

execute(49):-read_db('input_db.pl'),!.
execute(50):-
    nl,
    read_atom(S),
    write_db(S),!. 
execute(51):-show,!.
execute(52):-sort_start_number,!.
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

write_db(''):-
    !,
    writeln('Zadajte aj nazov suboru').
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
    format('Runner: ~s ~s, Start number: ~w, ~w, ~w. Race date: ~w, Result: ~w, ', [Firstname,Lastname, Number, Team, Country, Date, Result]),
    nl,
	fail.
show.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Vlozit pretekara



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pomocne predikaty - priklady
% Predikat, ktory nacita string aj ked tam je velke zaciatocne pismeno
% read_string(?String) argument je ale vhodnejsie pouzivat ako vystupny

read_string(String):-
	current_input(Input),
	read_line_to_codes(Input,Codes),
	string_codes(String,Codes).

%%%
% Predikat sa vykonava opakovane kym pouzivatel nezada korektne cislo
% read_num(?Number) argument je velmi vhodne pouzivat len ako vystupny

read_num(Num) :-
	read_string(Str),
	number_string(Num, Str), !.

read_num(Num) :-
	write('\tMusite zadat cislo: '),
	read_num(Num).


%%%
% Konverzia retazca na atom
% read_atom(?Atom)

read_atom(A):-
	read_string(Str),
	atom_string(A,Str).

%%%
% Najde vsetky riesenia pre dany ciel
% findall(+Template, :Goal, -Bag)
% vrati zoznam Mien a Priezvisk pre vsetkych zakaznikov v databaze
% findall(M^P , zakaznik(M,P,A,O), List).
% findall(M-P-A>O,zakaznik(M,P,A,O),List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Insert
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
% Remove
remove:-
    write('Zadajte  poradove cislo pretekara na vymazanie: '),
    read(Number),
    write(Number),
    runner(Firstname, Lastname, Team, Country, Date, Result, Number),
    format('~w: ~w ~w bude vymazany.\n', [Number, Firstname, Lastname]),
    retract(runner(Firstname, Lastname, Team, Country, Date, Result, Number)).
    


% Get Id
get_id(Id):-
    findall(Number, runner(_,_,_, _, _, _, Number), List),
    max_list(List, Max),
    Id is Max + 1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Search by

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
% Sort by start number
sort_by(Attr):-
    Attr == 'Number',
    findall(Number , runner(_,_,_, _, _, _, Number), List),
    sort(0, @=<, List, Sorted),
    vypis(Sorted).

% Sort by First name
sort_by(Attr):-
    Attr == 'Firstname',
    findall(Firstname-Number , runner(Firstname,_,_, _, _, _, Number), List),
    keysort(List, Pairs),
    pairs_values(Pairs, Sorted),
    vypis(Sorted).

%Sort by Lastname
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

    