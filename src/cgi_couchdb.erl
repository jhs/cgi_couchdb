%% Module to handle _cgi requests in design documents.

-module(cgi_couchdb).
-author('Jason Smith <jhs@couchone.com>').

-export([handle_cgi_req/3]).

-include("couch_db.hrl").

handle_cgi_req(Req, Db, DDoc)
    -> ?LOG_INFO("CGI REQUEST: ~p\n", [Req])
    , ?LOG_INFO("Db ~p\n", [Db])
    , ?LOG_INFO("DDoc ~p\n", [DDoc])
    , couch_httpd:send_response(Req, 200, [{"Content-Type", "text/plain"}], <<"Hello, CGI!\r\n">>)
    .

% vim: sw=4 sts=4 et
