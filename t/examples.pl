:- use_module(library(atom_feed)).
:- use_module(library(tap)).

'xkcd: improved keyboard' :-
    new_feed(file('samples/xkcd.xml'), Feed),
    entry(Feed, Entry),
    id(Entry, 'http://xkcd.com/1284/'),
    title(Entry, Title),
    Title == 'Improved Keyboard',
    !.  % cut search through other entries


'github: introducing octokit' :-
    new_feed(file('samples/github.xml'), Feed),
    entry(Feed, Entry),
    title(Entry, 'Introducing Octokit.net'),
    link(Entry, Link),
    rel(Link, alternate),
    href(Link, Url),
    Url == 'https://github.com/blog/1676-introducing-octokit-net',
    !.  % cut search through other entries

'blogger: pubsubhubbub' :-
    new_feed(file('samples/blogger.xml'), Feed),
    link(Feed, Link),
    rel(Link, hub),
    href(Link, Hub),
    Hub == 'http://pubsubhubbub.appspot.com/',
    !.  % cut search through other links

'cato: surveillance' :-
    new_feed(file('samples/cato.xml'), Feed),
    link(Feed, Link),
    href(Link, Href),
    Href == 'http://www.cato.org/',
    entry(Feed, Entry),
    id(Entry, '50177'),
    title(Entry, Title),
    Title == 'In Maryland, A Left-Right Alliance on Surveillance',
    !.

'popehat: books' :-
    new_feed(file('samples/popehat.xml'), Feed),
    entry(Feed, Entry),
    link(Entry, Link),
    href(Link, 'http://feedproxy.google.com/~r/Popehat/~3/La1PTh-AtVk/'),
    title(Entry, Title),
    Title == 'Clark\'s Favorite Books Part 1: Science Fiction',
    !.
