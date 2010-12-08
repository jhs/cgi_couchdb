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

run_cgi(Req, Db, DDoc, ProgramName, DDocEnv, SourceCode)
    -> ?LOG_DEBUG("Running CGI ~p with ~p against:\n~p", [ProgramName, DDocEnv, SourceCode])
    , {ok, CgiEnv} = env_for_request(Req)
    , Environment = lists:keymerge(1, lists:keysort(1, DDocEnv), lists:keysort(1, CgiEnv)) % Prefer the ddoc environment over the auto-generated one.
    , {ok, Subprocess} = cgi_subprocess(ProgramName, Environment, SourceCode)
    , {ok, Resp} = stream_from_subprocess(Req, Subprocess)
    , {ok, Resp} = couch_httpd:last_chunk(Resp)
    .

cgi_subprocess(ProgramName, Environment, SourceCode)
    -> process_flag(trap_exit, true)
    , Closer = filename:join(code:priv_dir(?MODULE), "coffee_for_closer.pl")
    , Args = [binary_to_list(ProgramName)]
    , Port = open_port({spawn_executable, Closer}, [binary, stream, {args, Args}, {env, Environment}]) % TODO: catch
    , case Port
        of Port when is_port(Port)
            -> ?LOG_DEBUG("Sending code to subprocess: ~p", [self()])
            , ok = case SourceCode
                of {inline, Data}
                    -> Size = size(Data)
                    , ?LOG_DEBUG("Sending ~p bytes of inlined code to subprocess", [Size])
                    , port_command(Port, [integer_to_list(Size), "\n"])
                    , port_command(Port, Data)
                    , ok
                ; {attachment, Filename, DDoc}
                    -> ?LOG_DEBUG("Attachments ~p: ~p", [Filename, DDoc#doc.atts])
                    , case lists:keyfind(Filename, 2, DDoc#doc.atts)
                        of false
                            -> error({not_found, io_lib:format("No such attachment for CGI: ~s", [Filename])})
                        ; Att when is_record(Att, att)
                            -> ?LOG_DEBUG("Processing attachment ~p with size ~p", [Att#att.name, Att#att.att_len])
                            , port_command(Port, [integer_to_list(Att#att.att_len), "\n"])
                            , {ok, Port} = couch_doc:att_foldl_decode(Att, fun stream_to_subprocess/2, {ok, Port})
                            , ok
                        end
                end

            % All sent; return the subprocess port.
            , {ok, Port}
        ; Error
            -> exit({open_port_failed, Error, [{request, theRequest}]})
        end
    .

stream_to_subprocess(Data, {ok, Subprocess}=State)
    -> ?LOG_DEBUG("Sending ~p bytes to subprocess", [iolist_size(Data)])
    , port_command(Subprocess, Data)
    , State
    .

stream_from_subprocess(Req, Subprocess)
    -> stream_from_subprocess(headers, Req, Subprocess)
    .

stream_from_subprocess(headers, Req, Subprocess)
    -> ?LOG_DEBUG("Streaming header data from subprocess", [])
    % TODO: Taking the easy way and assuming the headers arrive in the first data chunk.
    % This should accumulate until \r\n\r\n or even \n\n is seen.
    , {data, Data} = receive_from_subprocess(Subprocess)
    , case binary:match(Data, <<"\r\n\r\n">>)
        of nomatch
            -> error({cgi_error, "Did not get headers in CGI script"})
        ; {Pos, Length}
            % Great. Pull out the binary with the headers, start a chunked response, and then kick off streaming the rest of the data.
            -> HeaderSize = Pos + Length
            , <<HeaderData:HeaderSize/binary, BodyData/binary>> = Data
            , MochiHeaders = mochiweb_headers:from_binary(HeaderData)
            , Status = case mochiweb_headers:get_value("Status", MochiHeaders)
                of undefined -> 200
                ; StatusStr -> list_to_integer(string:sub_word(StatusStr, 1))
                end

            % TODO: Not sure about this. The CGI script content-length is disagreeing with the chunked encoder.
            , AllHeaders = [{couch_util:to_list(K), couch_util:to_list(V)} || {K, V} <- mochiweb_headers:to_list(MochiHeaders)]
            , CouchHeaders = case lists:keytake("Content-Length", 1, AllHeaders)
                of {value, _, Stripped} -> Stripped
                ; false -> AllHeaders
                end
            , {ok, Resp} = couch_httpd:start_chunked_response(Req, Status, CouchHeaders)

            % Wonderful. Send any remainder data and then stream the rest.
            , ?LOG_DEBUG("Body: ~p", [BodyData])
            , {ok, Resp} = couch_httpd:send_chunk(Resp, BodyData)
            , stream_from_subprocess(body, Resp, Subprocess)
        end
    ;

stream_from_subprocess(body, Resp, Subprocess)
    -> case receive_from_subprocess(Subprocess)
        of done
            -> {ok, Resp}
        ; {data, Data}
            -> {ok, Resp} = couch_httpd:send_chunk(Resp, Data)
            , stream_from_subprocess(body, Resp, Subprocess)
        end
    .

receive_from_subprocess(Subprocess)
    -> ?LOG_DEBUG("Streaming response from subprocess: ~p", [self()])
    , receive
        {Subprocess, {data, Data}} when is_port(Subprocess)
            -> ?LOG_DEBUG("Sending ~p bytes from subprocess", [iolist_size(Data)])
            , ?LOG_DEBUG("Data: ~p", [Data])
            , {data, Data}
        ; {'EXIT', Subprocess, Reason} when is_port(Subprocess)
            % Done. Return the chunks in order.
            -> ?LOG_DEBUG("Subprocess exited: ~p", [Reason])
            , done
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

env_for_request(Req)
    -> ?LOG_DEBUG("Building environment for request:\n~p", [Req])
    , {ok, [{K, couch_util:to_list(V)} || {K, V} <- env_for_request(Req, raw)]}
    .

env_for_request(#httpd{mochi_req=MochiReq}=Req, raw)
    %-> [_Db, <<"_design">>, _DDName, _CgiTrigger | ScriptAndPath] = Req#httpd.path_parts % TODO: queries for /_cgi and /_cgi/ are throwing badmatch
    -> {FullPath, QueryString, _Anchor} = mochiweb_util:urlsplit_path(MochiReq:get(raw_path))
    , [ {"REQUEST_METHOD"   , Req#httpd.method}
      , {"GATEWAY_INTERFACE", "CGI/1.1"}
      , {"PATH_TRANSLATED"  , FullPath}
      , {"HTTP_COOKIE"      , couch_httpd:header_value(Req, "Cookie", "")}
      , {"QUERY_STRING"     , QueryString}
      , {"CONTENT_LENGTH"   , couch_httpd:header_value(Req, "Content-Length", "")}
      ]
    .

% vim: sw=4 sts=4 et
