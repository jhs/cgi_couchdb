%% Module to handle _cgi requests in design documents.

-module(cgi_couchdb).
-author('Jason Smith <jhs@couchone.com>').

-export([handle_cgi_req/3]).

-include("couch_db.hrl").

handle_cgi_req(Req, Db, DDoc)
    -> ?LOG_INFO("CGI request", [])
    , {ok, Subprocess} = cgi_subprocess(Req, Db, DDoc)
    , {ok, Resp} = couch_httpd:start_chunked_response(Req, 200, [{"Content-Type", "text/plain"}])
    , {ok, Resp} = stream_from_subprocess(Resp, {ok, Subprocess})
    , {ok, Resp} = couch_httpd:last_chunk(Resp)
    .

cgi_subprocess(Req, Db, DDoc)
    -> ?LOG_INFO("Subprocess for request: ~p\n", [Req])
    , process_flag(trap_exit, true)
    , ?LOG_INFO("Db ~p\n", [Db])
    , ?LOG_INFO("DDoc ~p\n", [DDoc])
    , Att = lists:nth(1, DDoc#doc.atts)
    , ?LOG_INFO("Attachment length: ~p", [Att#att.att_len])
    , Closer = filename:join(code:priv_dir(?MODULE), "coffee_for_closer.pl")
    , Env = []
    , Args = ["first", "second"]
    , Port = open_port({spawn_executable, Closer}, [binary, stream, {args, Args}, {env, Env}]) % TODO: catch
    , case Port
        of Port when is_port(Port)
            -> ?LOG_INFO("Sending code to subprocess: ~p", [self()])
            , port_command(Port, [integer_to_list(Att#att.att_len), <<"\n">>])
            , {ok, Port} = couch_doc:att_foldl_decode(Att, fun stream_to_subprocess/2, {ok, Port})
        ; Error
            -> exit({open_port_failed, Error, [{request, theRequest}]})
        end
    .

stream_to_subprocess(Data, {ok, Subprocess}=State)
    -> ?LOG_INFO("Sending ~p bytes to subprocess", [iolist_size(Data)])
    , port_command(Subprocess, Data)
    , State
    .

stream_from_subprocess(Resp, {ok, Subprocess}=State)
    -> ?LOG_INFO("Streaming response from subprocess: ~p", [self()])
    , receive
        {Subprocess, {data, Data}} when is_port(Subprocess)
            -> ?LOG_INFO("Sending ~p bytes from subprocess", [iolist_size(Data)])
            , couch_httpd:send_chunk(Resp, Data)
            , stream_from_subprocess(Resp, State)
        ; {'EXIT', Subprocess, Reason} when is_port(Subprocess)
            % Done. Return the chunks in order.
            -> ?LOG_INFO("Subprocess exited: ~p", [Reason])
            , {ok, Resp}
        ; {'EXIT', Pid, Reason} when is_pid(Pid)
            -> ?LOG_ERROR("Linked process died", [])
            , exit({linked_process_died, Pid, Reason})
        ; Else
            -> ?LOG_ERROR("Woa: ~p", [Else])
            , exit({unknown_process_error, Else})
        after 3000
            -> ?LOG_ERROR("Subprocess timed out", [])
            , exit(timeout)
    end
    .

% vim: sw=4 sts=4 et
