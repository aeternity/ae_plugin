%% -*- mode:erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-

-module(ae_plugin_utils).

-export([ start_aecore/0
        , load_aecore/0 ]).

start_aecore() ->
    start_aecore(os:getenv("AE_ROOT")).

start_aecore(false) ->
    {error, {not_set, "AE_ROOT"}};
start_aecore(AERoot) ->
    io:fwrite("Setting up data paths~n", []),
    [application:set_env(setup, Key, V)
     || {Key, V} <- [ {home, AERoot}
                    , {data_dir, filename:join(AERoot, "data")}
                    , {log_dir, filename:join(AERoot, "log")} ]],
    io:fwrite("Loading aecore and deps~n", []),
    load_aecore(),
    io:fwrite("Running setup hooks~n", []),
    setup:run_setup(),
    io:fwrite("Starting aecore~n", []),
    application:ensure_all_started(aecore).


load_aecore() ->
    {ok, []} = setup:reload_app(aecore),
    {ok, AecoreApps} = application:get_key(aecore, applications),
    ensure_loaded(AecoreApps).

ensure_loaded(Apps) ->
    Loaded = [A || {A,_,_} <- application:loaded_applications()],
    {ok, _NewLoaded} = ensure_loaded(Apps, Loaded).

ensure_loaded([A|As], Loaded) ->
    case lists:member(A, Loaded) of
        true ->
            ensure_loaded(As, Loaded);
        false ->
            %% {A, {ok, []}} = {A, setup:reload_app(A)},
            io:fwrite("Loading ~p~n", [A]),
            load_app(A),
            ensure_loaded(As, [A|Loaded])
    end;
ensure_loaded([], Loaded) ->
    {ok, Loaded}.

load_app(A) ->
    case application:load(A) of
        {error, {already_loaded, A}} ->
            ok;
        ok ->
            ok;
        Other ->
            error({Other, [load_app, A]})
    end.
