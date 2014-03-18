%% ===================================================================
%% @author Michal Sitko
%% @copyright (C): 2013 ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: PushChannelTest cluster side test driver.      
%% @end
%% ===================================================================
-module(events_test).
-include("test_common.hrl").

-export([setup/1, teardown/2, exec/1]).


setup(ccm) ->
    ok;
setup(worker) ->
    test_common:register_user("peer.pem"),
    ok.


teardown(ccm, _State) ->
    ok; 
teardown(worker, _State) ->
    ok.

delete_file(FilePath) ->
  dao_lib:apply(dao_vfs, remove_file, [FilePath], 1).

exec({register_mkdir_handler, FilePath}) ->

  EventHandler = fun(_) ->
    delete_file(FilePath)
  end,

  EventItem = {event_handler_item, standard, undefined, undefined, undefined, EventHandler, undefined},

  %EventItem = #event_handler_item{processing_method = standard, handler_fun = EventHandler}, %, map_fun = EventHandlerMapFun, disp_map_fun = EventHandlerDispMapFun, config = ProcessingConfig},
  EventFilter = {eventfilterconfig, "type", "mkdir_event"},
  EventFilterConfig = {eventstreamconfig, undefined, EventFilter, undefined},
  gen_server:call({request_dispatcher, node()}, {rule_manager, 1, self(), {add_event_handler, {mkdir_event, EventItem, EventFilterConfig}}}).