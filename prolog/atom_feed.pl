:- module(atom_feed, [ new_feed/2
                     , author/2
                     , content/2
                     , entry/2
                     , href/2
                     , id/2
                     , link/2
                     , name/2
                     , rel/2
                     , summary/2
                     , title/2
                     , type/2
                     ]
         ).

:- use_module(library(charsio), [open_chars_stream/2]).
:- use_module(library(delay)).
:- use_module(library(http/http_open), [http_open/3]).
:- use_module(library(sgml), [load_sgml/3]).
:- use_module(library(xpath)).


:- multifile delay:mode/1.
delay:mode(system:atomic_list_concat(ground,ground,_)).
delay:mode(system:atomic_list_concat(_,ground,ground)).


%% new_feed(?Source, -Feed) is det.
%
%  Read a Feed from Source.  Source is one of
%
%    * stream(Stream) - an input stream
%    * file(File) - a file name to read from
%    * atom(Atom) - Atom XML in an atom
%    * codes(Codes) - Atom XML in a list of codes
%    * url(Url) - a URL to fetch
%
% This is the first step in working with an Atom feed.
new_feed(stream(Stream), feed(Tree)) :-
    parse_xml(Stream, Tree).
new_feed(file(File), feed(Tree)) :-
    parse_xml(File, Tree).
new_feed(atom(Atom), Feed) :-
    atom_codes(Atom, Codes),
    new_feed(codes(Codes), Feed).
new_feed(codes(Codes), Feed) :-
    open_chars_stream(Codes, Stream),
    new_feed(stream(Stream), Feed).
new_feed(url(Url), Feed) :-
    setup_call_cleanup( http_open(Url, Stream, [timeout(10)])
                      , new_feed(stream(Stream), Feed)
                      , close(Stream)
                      ).


% convenience for new_feed/2 when parsing XML
parse_xml(Source, Tree) :-
    load_sgml( Source
             , Parts
             , [ dialect(xmlns)
               , call(urlns, url_ns)
               %, call(xmlns, on_xmlns)  % to be notified of xmlns declarations
               ]
             ),
    Tree = element(atom:feed,_Attrs,_Children),
    memberchk(Tree, Parts).


% map URLs to namespace prefixes (for convenience)
url_ns('http://www.w3.org/2005/Atom', atom, _).


% convenience for matching XPath expressions w/o needing matched element
xpath(Dom, Spec) :-
    xpath(Dom, Spec, _).


%% id(+Item, -Id:atom) is det.
%
%  True if Id is the 'id' of Item.  Item can be a feed or an entry.
id(feed(Dom), IdText) :-
    once(xpath(Dom, /(atom:feed)/(atom:id), Id)),
    once(xpath(Id, /'*'(text), IdText)).
id(entry(Dom), IdText) :-
    once(xpath(Dom, /(atom:entry)/(atom:id), Id)),
    once(xpath(Id, /'*'(text), IdText)).


%% author(+Item, -Author) is nondet.
%
%  True if Author is the 'author' of Item. Item can be a feed or an
%  entry. An author is a compound item. See name/2 for a predicate that
%  works with this item.
author(feed(Dom), author(Author)) :-
    xpath(Dom, /(atom:feed)/(atom:author), Author).
author(entry(Dom), author(Author)) :-
    xpath(Dom, /(atom:entry)/(atom:author), Author).


%% content(+Entry, -Content:atom) is semidet.
%
%  True if Content is the 'content' of Entry. There's currently no way
%  to access the content's `type` or `src` attributes.
content(entry(Entry), ContentText) :-
    once(xpath(Entry, /(atom:entry)/(atom:content), Content)),
    once(xpath(Content, /'*'(text), ContentText)).


%% summary(+Entry, -Summary:atom) is semidet.
%
%  True if Summary is a summary of Entry. Fails if an entry has no
%  summary or the summary violates standards described in the Atom
%  spec (exact copy the entry's title, etc).
summary(entry(Entry), SummaryText) :-
    % summary on entry is optional. See RFC4287 4.2.13
    once(xpath(Entry, /(atom:entry)/(atom:summary), Summary)),
    once(xpath(Summary, /'*'(text), SummaryText)),

    % must have text different from 'content' and 'title'
    dif(Summary, Title),
    title(entry(Entry), Title),
    ( content(entry(Entry), Content) -> dif(Summary, Content) ; true ).


%% title(+Item, -Title:atom) is det.
%
%  True if Title is the title of Item. Item can be a feed, entry or
%  link.
title(feed(Dom), TitleText) :-
    once(xpath(Dom, /(atom:feed)/(atom:title), Title)),
    once(xpath(Title, /'*'(text), TitleText)).
title(entry(Dom), TitleText) :-
    xpath(Dom, /(atom:entry)/(atom:title), Title),
    once(xpath(Title, /'*'(text), TitleText)).
title(link(Dom), TitleText) :-
    Dom = element(_,Attrs,_),
    memberchk(title=TitleText, Attrs).


%% entry(+Feed, -Entry) is nondet.
%
%  True if Entry is an entry of Feed. Iterates of the feed's entries on
%  backtracking.
entry(feed(Dom), entry(Entry)) :-
    xpath(Dom, /(atom:feed)/(atom:entry), Entry).


%% link(+Item, -Link) is nondet.
%
%  True if Link is a 'link' of Item. Item can be a feed or an entry.
%  Link is a compound item. One typically uses predicates like rel/2 and
%  type/2 to refine which link is desired.
%
%  For example, to find the URL to an HTML alternative:
%
%      link(Feed, Link),
%      rel(Link, alternate),
%      type(Link, text/html),
%      href(Link, Url).
link(feed(Dom), link(Link)) :-
    xpath(Dom, /(atom:feed)/(atom:link), Link).
link(entry(Dom), link(Link)) :-
    xpath(Dom, /(atom:entry)/(atom:link), Link).


%% name(+Author, -Name:atom) is det.
%
%  True if Name is the 'name' of Author. According to the Atom spec,
%  this should be a human readable name.
name(author(Author), NameText) :-
    % name on author is mandatory.  See RFC4287 3.2.1
    once(xpath(Author, /(atom:author)/(atom:name), Name)),
    xpath(Name, /'*'(text), NameText).


%% rel(+Link, -Rel) is det.
%
%  True if Rel is the relationship between Link and its parent. If the
%  Atom XML doesn't explicitly specify a 'rel', `alternate` is used
%  instead.  That's why this predicate's mode is `det`.
rel(link(Link), Rel) :-
    % rel on link is optional, defaults to "alternate".
    % See RFC4287 4.2.7.2
    Link = element(_,Attrs,_),
    ( memberchk(rel=Rel0,Attrs) ->
        true
    ; true ->
        Rel0 = alternate
    ),
    Rel = Rel0.


%% href(+Link, -Href:atom) is det.
%
%  True if Href is the 'href' attribute of Link.
href(link(Link), Href) :-
    % href on link is mandatory. See RFC4287 4.2.7.1
    once(xpath(Link, /'*'(@href=Href))).


%% type(+Link, -MediaType) is semidet.
%
%  True if Link has type attributed MediaType. MediaType has the form
%  `Type/Subtype` which allows one to do things like
%
%      type(Link, text/_)  % link has a textual type
%
%  The Atom spec says that 'type' is optional. If it's missing, this
%  predicate fails.
%
%  Subtype may contain punctuation so remember to quote:
%  `application/'atom+xml'`.
type(link(element(_,Attrs,_)), Type/Subtype) :-
    delay(atomic_list_concat([Type, Subtype], '/', RawType)),

    % type on link is optional, with no default.
    % See RFC4287 4.2.7.3
    memberchk(type=RawType, Attrs).
