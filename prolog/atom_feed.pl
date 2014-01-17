:- module(atom_feed, [ new_feed/2
                     , author/2
                     , content/2
                     , description/2
                     , email/2
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
%    * atom(Atom) - Atom or RSS XML in an atom
%    * codes(Codes) - Atom or RSS XML in a list of codes
%    * url(Url) - a URL to fetch
%
% This is the first step in working with an Atom or RSS feed. If an RSS
% feed has multiple channels, only the first channel is considered.
new_feed(stream(Stream), Feed) :-
    parse_xml(Stream, Flavor, Tree),
    wrap_feed(Flavor, Tree, Feed).
new_feed(file(File), Feed) :-
    parse_xml(File, Flavor, Tree),
    wrap_feed(Flavor, Tree, Feed).
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
parse_xml(Source, Flavor, Tree) :-
    load_sgml( Source
             , Parts
             , [ dialect(xmlns)
               , call(urlns, url_ns)
               %, call(xmlns, on_xmlns)  % to be notified of xmlns declarations
               ]
             ),
    member(Root-Flavor, [(atom:feed)-atom, rss-rss]),
    Tree = element(Root,_Attrs,_Children),
    memberchk(Tree, Parts),
    !.  % accept the first solution



wrap_feed(atom, Tree, atom_feed(Tree)).
wrap_feed(rss, RssTree, rss_feed(ChannelTree)) :-
    once(xpath(RssTree, /rss/channel, ChannelTree)).


% map URLs to namespace prefixes (for convenience)
url_ns('http://www.w3.org/2005/Atom', atom, _).


% convenience for matching XPath expressions w/o needing matched element
xpath(Dom, Spec) :-
    xpath(Dom, Spec, _).


%% id(+Item, -Id:atom) is det.
%
%  True if Id is the 'id' of Item. Item can be a feed or an entry. RSS
%  feeds don't have IDs.
id(atom_feed(Dom), IdText) :-
    once(xpath(Dom, /(atom:feed)/(atom:id), Id)),
    once(xpath(Id, /'*'(text), IdText)).
id(atom_entry(Dom), IdText) :-
    once(xpath(Dom, /(atom:entry)/(atom:id), Id)),
    once(xpath(Id, /'*'(text), IdText)).
id(rss_entry(Dom), IdText) :-
    once(xpath(Dom, /item/guid(text), IdText)).


%% author(+Item, -Author) is nondet.
%
%  True if Author is the 'author' of Item. Item can be a feed or an
%  entry. An author is a compound item. See name/2 for a predicate that
%  works with this item.  RSS feeds don't have an author.
author(atom_feed(Dom), atom_author(Author)) :-
    xpath(Dom, /(atom:feed)/(atom:author), Author).
author(atom_entry(Dom), atom_author(Author)) :-
    xpath(Dom, /(atom:entry)/(atom:author), Author).
author(rss_entry(Dom), rss_author(Author)) :-
    xpath(Dom, /item/author, Author).


%% content(+Entry, -Content:atom) is semidet.
%
%  True if Content is the 'content' of Entry. There's currently no way
%  to access the content's `type` or `src` attributes.
content(atom_entry(Entry), ContentText) :-
    once(xpath(Entry, /(atom:entry)/(atom:content), Content)),
    once(xpath(Content, /'*'(text), ContentText)).
content(rss_entry(E), ContentText) :-
    Entry = rss_entry(E),
    \+ link(Entry, _),  % no link means description is the content
    description(Entry, ContentText).


%% description(+Entry, -Description:atom) is semidet.
%
%  True if Entry has a Description. This predicate is peculiar to RSS
%  entries. The RSS spec confounds summary and content into a single
%  'description' field.
description(rss_entry(Entry), Description) :-
    once(xpath(Entry, /item/description(text), Description)).


%% summary(+Entry, -Summary:atom) is semidet.
%
%  True if Summary is a summary of Entry. Fails if an entry has no
%  summary or the summary violates standards described in the Atom
%  spec (exact copy the entry's title, etc).
%
%  It's not possible to reliably extract summaries from RSS entries.
%  That's because summary and content is confounded into a single
%  'description' element.
summary(atom_entry(Entry), SummaryText) :-
    % summary on entry is optional. See RFC4287 4.2.13
    once(xpath(Entry, /(atom:entry)/(atom:summary), Summary)),
    once(xpath(Summary, /'*'(text), SummaryText)),

    % must have text different from 'content' and 'title'
    dif(SummaryText, Title),
    title(atom_entry(Entry), Title),
    ( content(atom_entry(Entry), Content) -> dif(SummaryText, Content) ; true ).


%% title(+Item, -Title:atom) is det.
%
%  True if Title is the title of Item. Item can be a feed, entry or
%  link.
title(atom_feed(Dom), TitleText) :-
    once(xpath(Dom, /(atom:feed)/(atom:title), Title)),
    once(xpath(Title, /'*'(text), TitleText)).
title(rss_feed(Dom), TitleText) :-
    once(xpath(Dom, /channel/title(text), TitleText)).
title(atom_entry(Dom), TitleText) :-
    xpath(Dom, /(atom:entry)/(atom:title), Title),
    once(xpath(Title, /'*'(text), TitleText)).
title(rss_entry(Dom), TitleText) :-
    once(xpath(Dom, /item/title(text), TitleText)).
title(atom_link(Dom), TitleText) :-
    Dom = element(_,Attrs,_),
    memberchk(title=TitleText, Attrs).


%% entry(+Feed, -Entry) is nondet.
%
%  True if Entry is an entry of Feed. Iterates of the feed's entries on
%  backtracking.
entry(atom_feed(Dom), atom_entry(Entry)) :-
    xpath(Dom, /(atom:feed)/(atom:entry), Entry).
entry(rss_feed(Dom), rss_entry(Entry)) :-
    xpath(Dom, /channel/item, Entry).


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
link(atom_feed(Dom), atom_link(Link)) :-
    xpath(Dom, /(atom:feed)/(atom:link), Link).
link(rss_feed(Dom), rss_link(Link)) :-
    once(xpath(Dom, /channel/link, Link)).  % spec says "the" suggesting 1
link(atom_entry(Dom), atom_link(Link)) :-
    xpath(Dom, /(atom:entry)/(atom:link), Link).
link(rss_entry(Dom), rss_link(Link)) :-
    xpath(Dom, /item/link, Link).


%% email(+Author, -Email:atom) is semidet.
%
%  True if Name is email of Author.
email(atom_author(Author), EmailText) :-
    once(xpath(Author, /(atom:author)/(atom:email), Email)),
    xpath(Email, /'*'(text), EmailText).
email(rss_author(Author), Email) :-
    once(xpath(Author, /author(text), Email)).


%% name(+Author, -Name:atom) is semidet.
%
%  True if Name is the 'name' of Author. According to the Atom spec,
%  this should be a human readable name. RSS authors have no name, only
%  an email.  See email/2.
name(atom_author(Author), NameText) :-
    % name on author is mandatory.  See RFC4287 3.2.1
    once(xpath(Author, /(atom:author)/(atom:name), Name)),
    xpath(Name, /'*'(text), NameText).


%% rel(+Link, -Rel) is det.
%
%  True if Rel is the relationship between Link and its parent. If the
%  Atom XML doesn't explicitly specify a 'rel', `alternate` is used
%  instead.  That's why this predicate's mode is `det`.
%
%  Because RSS links can't specify a 'rel', we use `alternate` for them
%  too.
rel(atom_link(Link), Rel) :-
    % rel on link is optional, defaults to "alternate".
    % See RFC4287 4.2.7.2
    Link = element(_,Attrs,_),
    ( memberchk(rel=Rel0,Attrs) ->
        true
    ; true ->
        Rel0 = alternate
    ),
    Rel = Rel0.
rel(rss_link(_), alternate).


%% href(+Link, -Href:atom) is det.
%
%  True if Href is the 'href' attribute of Link.
href(atom_link(Link), Href) :-
    % href on link is mandatory. See RFC4287 4.2.7.1
    once(xpath(Link, /'*'(@href=Href))).
href(rss_link(Link), Href) :-
    Link = element(link, _, [Href]).


%% type(+Link, -MediaType) is semidet.
%
%  True if Link has type attributed MediaType. MediaType has the form
%  `Type/Subtype` which allows one to do things like
%
%      type(Link, text/_)  % link has a textual type
%
%  The Atom spec says that 'type' is optional. If it's missing, this
%  predicate fails. RSS spec says the link points to "the HTML website",
%  so MediaType is always `text/html`.
%
%  Subtype may contain punctuation so remember to quote:
%  `application/'atom+xml'`.
type(atom_link(element(_,Attrs,_)), Type/Subtype) :-
    delay(atomic_list_concat([Type, Subtype], '/', RawType)),

    % type on link is optional, with no default.
    % See RFC4287 4.2.7.3
    memberchk(type=RawType, Attrs).
type(rss_link(_), text/html).
