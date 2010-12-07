%% Module to handle _cgi requests in design documents.

-module(cgi_couchdb).
-author('Jason Smith <jhs@couchone.com>').

-export([handle_cgi_req/3]).

-include("couch_db.hrl").

handle_cgi_req(Req, Db, DDoc)
    -> ?LOG_INFO("CGI REQUEST: ~p\n", [Req])
    %, ?LOG_INFO("Db ~p\n", [Db])
    , ?LOG_INFO("DDoc ~p\n", [DDoc])
    , Att = lists:nth(1, DDoc#doc.atts)
    , {ok, Resp} = couch_httpd:start_chunked_response(Req, 200, [{"Content-Type", "text/plain"}])
    , {ok, Resp} = couch_doc:att_foldl_decode(Att, fun folder/2, {ok, Resp})
    , {ok, Resp} = couch_httpd:last_chunk(Resp)
    .

folder(Data, {ok, Resp}=State)
    -> ?LOG_INFO("Streamer with ~p bytes", [iolist_size(Data)])
    , couch_httpd:send_chunk(Resp, Data)
    , State
    .

% vim: sw=4 sts=4 et
