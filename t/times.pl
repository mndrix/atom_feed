:- use_module(library(atom_feed)).
:- use_module(library(tap)).

xkcd :-
    new_feed(file('samples/xkcd.xml'), Feed),
    entry(Feed, Entry),
    id(Entry, 'http://xkcd.com/1284/'),
    updated(Entry, Time),
    Time == 1383091200.0,
    \+ published(Entry, _),  % contains no publication time
    !.  % cut search through other entries


github :-
    new_feed(file('samples/github.xml'), Feed),
    entry(Feed, Entry),
    title(Entry, 'Introducing Octokit.net'),
    published(Entry, Time),
    Time == 1383171382.0,
    !.  % cut search through other entries

blogger :-
    new_feed(file('samples/blogger.xml'), Feed),
    entry(Feed, Entry),
    id(Entry, 'tag:blogger.com,1999:blog-5589634522109419319.post-9120846642147351916'),
    published(Entry, Time),
    Time == 1383228000.0,
    !.  % cut search through other links

/*
RSS doesn't specify a timestamp format so they
vary dramatically.  Exclude these tests until
we support the relevant variations.


cato :-
    new_feed(file('samples/cato.xml'), Feed),
    entry(Feed, Entry),
    id(Entry, '50177'),
    published(Entry, Time),
    Time == 99,
    !.

popehat :-
    new_feed(file('samples/popehat.xml'), Feed),
    entry(Feed, Entry),
    id(Entry, ''),
    published(Entry, Time),
    Time == 99,
    !.

*/
