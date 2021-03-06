%%==============================================================================
%% Write PropEr Statem Modules a' la QuickCheck
%%==============================================================================
%% This module contains some boilerplate code that allows writing more compacted
%% statem modules, resembling the QuickCheck ones, with PropEr.
%% To use this, simply include this boilerplate into your statem modules:
%%
%%   -include_lib("proper_contrib/include/proper_contrib_statem.hrl").
%%
%%==============================================================================

-ifndef(_PROPER_CONTRIB_STATEM_HRL_).
-define(_PROPER_CONTRIB_STATEM_HRL_, true).

command(State) ->
  Funs0 = [ {F, list_to_atom(atom_to_list(F) ++ "_args")}
            || {F, _} <- ?MODULE:module_info(exports)
          ],

  Funs1 = [ X || {_, FArgs} = X <- Funs0,
                 erlang:function_exported(?MODULE, FArgs, 1)
          ],

  WeightFun = case erlang:function_exported(?MODULE, weight, 2) of
                true  -> fun ?MODULE:weight/2;
                false -> fun(_, _) -> 1 end
              end,

  proper_types:frequency([ { WeightFun(State, F)
                           , {call, ?MODULE, F, ?MODULE:FArgs(State)}
                           }
                           || {F, FArgs} <- Funs1
                         ]).

precondition(S, {call, M, F, Args}) ->
  Pre = list_to_atom(atom_to_list(F) ++ "_pre"),
  case erlang:function_exported(M, Pre, 1) of
    true  -> M:Pre(S);
    false -> true
  end
    andalso
    case erlang:function_exported(M, Pre, 2) of
      true  -> M:Pre(S, Args);
      false -> true
    end.

next_state(S, Res, {call, M, F, Args}) ->
  Next = list_to_atom(atom_to_list(F) ++ "_next"),
  case erlang:function_exported(M, Next, 3) of
    true  -> M:Next(S, Res, Args);
    false -> S
  end.

postcondition(S, {call, M, F, Args}, Res) ->
  Post = list_to_atom(atom_to_list(F) ++ "_post"),
  case erlang:function_exported(M, Post, 3) of
    true  -> M:Post(S, Args, Res);
    false -> true
  end.

-endif.
