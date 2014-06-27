%% ===================================================================
%% @author Michal Sitko
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: EventsTest cluster side test driver.      
%% @end
%% ===================================================================
-module(events_test).
-include("test_common.hrl").

-export([setup/1, teardown/2, exec/1]).


setup(ccm) ->
    ok;
setup(worker) ->
    DirectIORoot = "/tmp/dio",
    os:putenv("DIO_ROOT", DirectIORoot), 
    Fuse_groups = [{fuse_group_info, "cluster_fid", {storage_helper_info, "DirectIO", [DirectIORoot]}}],
    fslogic_storage:insert_storage("ClusterProxy", [], Fuse_groups),
    
    test_common:register_user("peer.pem"),

    DirectIORoot.

teardown(ccm, _State) ->
    ok; 
teardown(worker, _State) ->
    ok.

delete_file(FilePath) ->
  dao_lib:apply(dao_vfs, remove_file, [FilePath], 1).

update_quota(UserLogin, NewQuotaInBytes) ->
  {ok, UserDoc} = user_logic:get_user({login, UserLogin}),
  user_logic:update_quota(UserDoc, {quota, NewQuotaInBytes, false}).

exec({register_mkdir_handler, FilePath}) ->
  EventHandler = fun(_) ->
    delete_file(FilePath)
  end,

  EventItem = {event_handler_item, standard, undefined, undefined, undefined, EventHandler, undefined},

  EventFilter = {eventfilterconfig, "type", "mkdir_event"},
  EventFilterConfig = {eventstreamconfig, undefined, EventFilter, undefined, undefined, undefined},
  gen_server:call({request_dispatcher, node()}, {rule_manager, 1, self(), {add_event_handler, {"mkdir_event", EventItem, EventFilterConfig}}});

exec({prepare_for_quota_case, QuotaSizeInBytes}) ->
  rule_definitions:register_for_write_events(10),
  update_quota("test_user", QuotaSizeInBytes).
