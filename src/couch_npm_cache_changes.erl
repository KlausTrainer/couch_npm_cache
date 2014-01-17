-module(couch_npm_cache_changes).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([
    init/1, handle_call/3, handle_info/2, handle_cast/2, code_change/3,
    terminate/2
]).

-include("couch_npm_cache.hrl").

-define(CHANGES_OPTIONS, [
    continuous,
    include_docs,
    {heartbeat, 16000}
]).

-record(state, {
    ref,
    npm_db,
    local_db,
    update_doc,
    changes_options = ?CHANGES_OPTIONS
}).


start_link() ->
    NpmServerConnection = couchbeam:server_connection(
        ?DEFAULT_NPM_HOST, ?DEFAULT_NPM_PORT),
    LocalServerConnection = couchbeam:server_connection(
        ?DEFAULT_COUCH_HOST, ?DEFAULT_COUCH_PORT, "",
        ?DEFAULT_COUCH_DB_OPTIONS),
    {ok, NpmDb} = couchbeam:open_db(NpmServerConnection, ?DEFAULT_NPM_DB_NAME),
    {ok, LocalDb} = couchbeam:open_db(LocalServerConnection,
        ?DEFAULT_COUCH_DB_NAME),
    UpdateDocId = "_local/" ++ atom_to_list(?MODULE),
    case couchbeam:open_doc(LocalDb, UpdateDocId) of
    {ok, UpdateDoc} ->
        {UpdateDocProps} = UpdateDoc,
        UpdateSeq = proplists:get_value(<<"update_seq">>, UpdateDocProps),
        State = #state{
            npm_db = NpmDb,
            local_db = LocalDb,
            update_doc = UpdateDoc,
            changes_options = [{since, UpdateSeq}|?CHANGES_OPTIONS]
        },
        gen_server:start_link({local, ?MODULE}, ?MODULE, State, []);
    _ ->
        UpdateDoc = {[{<<"_id">>, list_to_binary(UpdateDocId)}]},
        State = #state{npm_db=NpmDb, local_db=LocalDb, update_doc=UpdateDoc},
        gen_server:start_link({local, ?MODULE}, ?MODULE, State, [])
    end.


init(State) ->
    {ok, State, 0}.


handle_call(_Msg, State, _From) ->
    {noreply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(timeout, #state{ref=undefined} = State) ->
    #state{npm_db=NpmDb, changes_options=ChangesOptions} = State,
    case couchbeam_changes:stream(NpmDb, self(), ChangesOptions) of
    {ok, Ref, _Pid} -> {noreply, State#state{ref=Ref}};
    {error, _} -> {noreply, State}
    end;

handle_info(timeout, State) ->
    error_logger:error_msg("~p: got timeout~n", [?MODULE]),
    {noreply, State};

handle_info({change, Ref, {done, LastSeq}}, #state{ref=Ref} = State) ->
    error_logger:warning_msg("~p: stopped at seq ~p~n", [?MODULE, LastSeq]),
    {stop, done, State};

handle_info({change, Ref, {ChangeProps}}, #state{ref=Ref, local_db=LocalDb} = State) ->
    UpdateSeq = proplists:get_value(<<"seq">>, ChangeProps),
    {DocProps} = proplists:get_value(<<"doc">>, ChangeProps),
    case proplists:get_value(<<"_deleted">>, DocProps) of
    true ->
        {noreply, State};
    _ ->
        case proplists:get_value(<<"_id">>, DocProps) of
        <<>> ->
            {noreply, State};
        _ ->
            Doc = {proplists:delete(<<"_attachments">>, DocProps)},
            #state{local_db=LocalDb, update_doc={UpdateDocProps}} = State,
            {ok, []} = couchbeam:save_docs(LocalDb, [Doc],
                [{"new_edits", false}]),
            UpdateDocProps1 = proplists:delete(<<"update_seq">>,
                UpdateDocProps),
            UpdateDoc1 = {[{<<"update_seq">>, UpdateSeq}|UpdateDocProps1]},
            {ok, UpdateDoc2} = couchbeam:save_doc(LocalDb, UpdateDoc1),
            {noreply, State#state{update_doc = UpdateDoc2}}
        end
    end;

handle_info({error, Ref, LastSeq, Error}, #state{ref=Ref} = State) ->
    error_logger:error_msg("~p: error: ~p at seq ~p~n", [?MODULE, Error, LastSeq]),
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(Reason, _State) ->
    error_logger:info_msg("~p terminating with reason ~p~n", [?MODULE, Reason]),
    ok.
