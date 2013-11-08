:- use_module(library(atom_feed)).
:- use_module(library(tap)).

'xkcd feed' :-
    new_feed(url('http://xkcd.com/atom.xml'), Feed),
    title(Feed, 'xkcd.com').
