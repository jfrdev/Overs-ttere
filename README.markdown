# README
This file contains what we need to get done, and what it is expected for 
us to have done every week. Furthermore our current process is also tracked.


## List of pending issues

* Implement blocks
* Test lexer and compiler together
* Write the parts of the report on lexer and parser


## Implemented

* The lexer should be done (test it before removing this parenthesis)
* The parser is almost done (check precedence and test it)


## Expected Milestones

### Uge 47
Lexeren kan genereres og oversættes (husk at erklære de nye tokens i parseren).
Rapportafsnit om lexer skrives.

### Uge 48
Parseren kan genereres og oversættes.
Rapportafsnit om lexer og parser skrives.

### Uge 49
Typecheckeren er implementeret.
Rapportafsnit om typechecker skrives

### Uge 50
Oversætteren er implementeret,
rapportafsnit om denne skrives.

### Uge 51
Afsluttende afprøvning og rapportskrivning,
rapporten aﬂeveres om torsdagen.

### Bemærk!
...at typechecker og kodegenerering er væsentligt større opgaver end lexer
og parser. Lexeren kan udvides og genereres på en times tid og parseren på 2–3
timer. Typecheckeren vil nok kræve 5–8 timers fuldtidsarbejde og kodegeneratoren
10–20 timer. Her er ikke medregnet tid til at læse op på stoffet i bogen – tiderne
forudsætter, at man har nogenlunde styr på de relevante dele af pensum.

Efter hvert af de ovenstående skridt bør man genoversætte hele oversætteren
og prøvekøre den for testprogrammerne. De endnu ikke udvidede moduler kan
ved oversættelse rapportere om ikke-udtømmende pattern-matching, og ved køretid
kan de rejse undtagelsen “Match”. Man kan i C100.sml udkommentere kald til de
senere faser for at afprøve sprogudvidelserne for de moduler (faser), der allerede
er implementerede.

Jeres instruktor vil gerne løbende læse og komme med feedback til afsnit af
rapporten. I skal dog regne med, at der kan gå noget tid, inden I får svar (så bed
ikke om feedback lige før aﬂeveringsfristen), og I skal ikke forvente, at et afsnit
bliver læst igennem ﬂere gange.
 
