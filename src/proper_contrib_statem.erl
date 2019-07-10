%%==============================================================================
%% Helper Functions for the proper_statem module
%%==============================================================================
-module(proper_contrib_statem).

%%==============================================================================
%% API
%%==============================================================================
-export([ run/1
        , run/2
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("proper/include/proper.hrl").

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type options() :: #{ setup_fun    => fun(()      -> any())
                    , teardown_fun => fun((any()) -> ok)
                    , cleanup_fun  => fun(()      -> ok)
                    , whenfail_fun => fun(( proper_statem:command_list()
                                          , proper_statem:history()
                                          , proper_statem:dynamic_state()
                                          , proper_statem:statem_result()
                                          ) -> ok)
                                         }.

-type symbolic_call() :: {'call', atom(), atom(), [any()]}.
-type symbolic_var()  :: {'var', pos_integer()}.
-type command()       :: {'set', symbolic_var(), symbolic_call()}
                       | {'init', any()}.
-type command_list()  :: [command()].
-type history()       :: [{any(), any()}].

%%==============================================================================
%% API
%%==============================================================================
-spec run(atom()) -> ok.
run(Module) ->
  run(Module, #{}).

-spec run(atom(), options()) -> ok.
run(Module, Options) ->
  SetupFun    = maps:get(setup_fun    , Options, fun() -> ok end),
  TearDownFun = maps:get(teardown_fun , Options, fun(_) -> ok end),
  CleanupFun  = maps:get(cleanup_fun  , Options, fun() -> ok end),
  WhenFail    = maps:get(whenfail_fun , Options, fun whenfail_fun/4),
  ?SETUP( fun() ->
              Data = SetupFun(),
              fun() -> TearDownFun(Data) end
          end
        , ?FORALL( Cmds
                 , proper_statem:commands(Module)
                 , begin
                     CleanupFun(),
                     { History
                     , State
                     , Result
                     } = proper_statem:run_commands(Module, Cmds),
                     ?WHENFAIL(
                        WhenFail(Cmds, History, State, Result),
                        proper:aggregate( proper_statem:command_names(Cmds)
                                        , Result =:= ok
                                        )
                       )
                   end
                 )).

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec whenfail_fun( command_list()
                  , history()
                  , any()
                  , any()
                  ) -> ok.
whenfail_fun(Cmds, History, State, Result) ->
  Names = proper_statem:command_names(Cmds),
  AnnotatedHistory = lists:zip(lists:sublist(Names, 1, length(History)), History),
  io:format("Annotated History: ~p\nState: ~p\nResult: ~p\nCmd Names: ~p\n",
            [ AnnotatedHistory
            , State
            , Result
            , Names
            ]).
