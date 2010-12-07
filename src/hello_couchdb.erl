%% Example CocuhDB plugin module--really just an Erlang module.

-module(hello_couchdb).
-author('Jason Smith <jhs@couchone.com>').

-export([hello/0]).


hello()
    -> "Hello, CouchDB!"
    .

% vim: sw=4 sts=4 et
