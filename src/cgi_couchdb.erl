%% Module to handle _cgi requests in design documents.

-module(cgi_couchdb).
-author('Jason Smith <jhs@couchone.com>').

-export([handle_cgi_req/3]).

-include("couch_db.hrl").

handle_cgi_req(Req, Db, DDoc)
    -> ?LOG_DEBUG("CGI request: ~p", [Req#httpd.path_parts])
    , [_Db, <<"_design">>, _DDName, _CgiTrigger, CgiKey | PathInfoParts] = Req#httpd.path_parts % TODO: queries for /_cgi and /_cgi/ are throwing badmatch
    , {DDocBody} = DDoc#doc.body
    , case couch_util:get_value(<<"cgi">>, DDocBody)
        of undefined
            -> error({not_found, "Design document has no CGI definitions"})
        ; {CgiDefinitions}
            -> ?LOG_DEBUG("CGI definition: ~p", [CgiDefinitions])
            , case couch_util:get_value(CgiKey, CgiDefinitions)
                of undefined
                    -> error({not_found, io_lib:format("No such CGI definition: ~s", [CgiKey])})
                ; {CgiDefinition}
                    -> ProgramName = case couch_util:get_value(<<"exec">>, CgiDefinition)
                        of undefined
                            -> error({not_found, io_lib:format("No \"exec\" field in CGI definition: ~s", [CgiKey])})
                        ; Unwelcome when not is_binary(Unwelcome)
                            -> error({not_found, "CGI \"exec\" property must be a string"})
                        ; Name -> Name
                        end
                    , Environment = case couch_util:get_value(<<"env">>, CgiDefinition)
                        of undefined -> []
                        ; null -> []
                        ; {Env} -> [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Env]
                        ; _ -> error({not_found, "CGI \"env\" property must be undefined, null, or an object"})
                        end

                    , SourceCode = case couch_util:get_value(<<"code">>, CgiDefinition)
                        of undefined -> error({not_found, "CGI \"code\" property must be defined"})
                        ; Inlined when is_binary(Inlined)
                            -> {inline, Inlined}
                        ; {CodeObj}
                            -> case couch_util:get_value(<<"attachment">>, CodeObj)
                                of AttachmentName when is_binary(AttachmentName)
                                    -> {attachment, AttachmentName, DDoc}
                                ; _ -> error({not_found, "CGI \"code\" object is missing \"attachment\" definition"})
                                end
                        end

                    % Ready! Run it!
                    , run_cgi(Req, Db, DDoc, ProgramName, Environment, SourceCode)
                end
        end
    .

run_cgi(Req, Db, DDoc, ProgramName, Environment, SourceCode)
    -> ?LOG_DEBUG("Running CGI ~p with ~p against ~p", [ProgramName, Environment, SourceCode])
    , {ok, Subprocess} = cgi_subprocess(ProgramName, Environment, SourceCode)
    , {ok, Resp} = couch_httpd:start_chunked_response(Req, 200, [{"Content-Type", "text/plain"}])
    , {ok, Resp} = stream_from_subprocess(Resp, {ok, Subprocess})
    , {ok, Resp} = couch_httpd:last_chunk(Resp)
    .

cgi_subprocess(ProgramName, Environment, SourceCode)
    -> process_flag(trap_exit, true)
    , Closer = filename:join(code:priv_dir(?MODULE), "coffee_for_closer.pl")
    , Args = ["first", "second"]
    , Port = open_port({spawn_executable, Closer}, [binary, stream, {args, Args}, {env, Environment}]) % TODO: catch
    , case Port
        of Port when is_port(Port)
            -> ?LOG_INFO("Sending code to subprocess: ~p", [self()])
            , ok = case SourceCode
                of {inline, Data}
                    -> Size = size(Data)
                    , ?LOG_DEBUG("Sending ~p bytes of inlined code to subprocess", [Size])
                    , port_command(Port, [integer_to_list(Size), "\n"])
                    , port_command(Port, Data)
                    , ok
                ; {attachment, Filename, DDoc}
                    -> error({not_found, "Don't know how to do attachments yet"})
                end
            %, port_command(Port, [integer_to_list(Att#att.att_len), <<"\n">>])
            %, {ok, Port} = couch_doc:att_foldl_decode(Att, fun stream_to_subprocess/2, {ok, Port})

            % All sent; return the subprocess port.
            , {ok, Port}
        ; Error
            -> exit({open_port_failed, Error, [{request, theRequest}]})
        end

    %, Att = lists:nth(1, DDoc#doc.atts)
    %, ?LOG_INFO("Attachment length: ~p", [Att#att.att_len])
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
