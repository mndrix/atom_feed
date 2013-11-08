# Synopsis

    :- use_module(library(atom_feed)).
    ?- new_feed(file('xkcd.xml'), Feed),
       entry(Feed, Entry),
       link(Entry, Link),
       rel(Link, alternate),
       href(Link, Url).
    Url = 'http://xkcd.com/1286/' ;
    Url = 'http://xkcd.com/1285/' ;
    Url = 'http://xkcd.com/1284/' ;
    ...

# Description

Parse and query [Atom Syndication Feeds](http://www.ietf.org/rfc/rfc4287.txt).  This pack doesn't support every aspect of the Atom spec, but it does support the most widely used aspects.  It's intended as a relatively low level library on which more complex Atom feed processing can be implemented.

# Changes in this Version

  * Support new_feed/2 directly from a URL

# Installation

Using SWI-Prolog 6.3 or later:

    ?- pack_install(atom_feed).

This module uses [semantic versioning](http://semver.org/).

Source code available and pull requests accepted at
http://github.com/mndrix/atom_feed

@author Michael Hendricks <michael@ndrix.org>
@license BSD
