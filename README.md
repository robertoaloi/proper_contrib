# proper-contrib

[![Build Status](https://travis-ci.org/robertoaloi/proper_contrib.svg?branch=master)](https://travis-ci.org/robertoaloi/proper_contrib)

A collection of goodies for the property-based testing framework PropEr.

## Write your PropEr `statem` modules a' la QuickCheck

### Rationale

Both
[PropEr](https://proper-testing.github.io/apidocs/proper_statem.html)
and [QuickCheck](http://quviq.com/documentation/eqc/eqc_statem.html)
offer their users the ability to "test stateful systems whose internal
state and side-effects are specified via an abstract state
machine". They do so via so-called _statem_ behaviours. Whilst the
basic idea is the same in both frameworks, QuickCheck's _statem_
offers a few extra functionalities that are currently not available in
_PropEr_. Among other things, the QuickCheck _statem_ behaviour allows
users to define an abstract state machines in a more convenient,
organized format than _PropEr_.

The `proper_contrib_statem.hrl` header file contains some trivial
boilerplate that allows _PropEr_ users to define their abstract state
machines with a syntax which is more similar to the _QuickCheck_ one.

Specifically, the boilerplate allows the definition of a few
_optional_ callback functions in a `proper_statem` callback module:

* `[COMMAND]_args/1`
* `[COMMAND]_next/3`
* `[COMMAND]_post/3`
* `[COMMAND]_pre/1`
* `[COMMAND]_pre/2`

Those calls will replace the following ones:

* `next_state/3`
* `postcondition/3`
* `precondition/2`

This makes the definition of abstract state machines a lot simpler,
allowing all the logic (i.e. preconditions, postconditions, etc.)
related to the same _COMMAND_ to be grouped together, rather than be
scattered across the entire _statem_ callback module.

The boilerplate also makes the `command/1` not needed
anymore. Commands will, instead, be dynamically found _by convention_
(i.e. by looking at exported functions of arity `1` within the same
module whose name ends with `_args`). An optional `weight/1` function
allows users to specify a different frequency for each command.

### Getting Started

To get started, just include the `proper_contrib` repo as a test
dependency for your project. For example, if you are using the
`rebar3` build tool, you can add the following to your `rebar.config`:

```erlang
{profiles, [{test, {deps, [{proper_contrib, {git, "https://github.com/robertoaloi/proper_contrib.git", {tag, "0.1.0"}}}]}}]}.
```

Then, include the `proper_contrib_statem.hrl` file in your
`proper_statem` callback module:

```erlang
-module(prop_sample).

-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("proper_contrib_statem.hrl"). %% Yes, this is all!

prop_sample() ->
  ?FORALL(Cmds, proper_statem:commands(?MODULE),
          begin
            {_History, _S, Result} = proper_statem:run_commands(?MODULE, Cmds),
            Result =:= ok
          end).

```

And you are ready to go. The header will define the `proper_statem`
callback functions for you and will proxy calls to the new callback
functions whenever appropriate.

### Example

A _traditional_ `proper_statem` may look like the following (extracted
from the _PropEr_ [official
documentation](https://proper-testing.github.io/tutorials/PropEr_testing_of_generic_servers.html)):

```erlang
[...]

command(_S) ->
  oneof([{call,?MODULE,create_account,[name()]},
         {call,?MODULE,delete_account,[password()]}]).

[...]

next_state(S, Res, {call,_,create_account,[_Name]}) ->
    S#state{users = [Res|S#state.users]};
next_state(S, _Res, {call,_,delete_account,[Password]}) ->
    S#state{users = lists:delete(Password, S#state.users)}.

[...]

precondition(_, _) -> true.

[...]

postcondition(S, {call,_,create_account,[_Name]}, Result) ->
    not lists:member(Result, S#state.users);
postcondition(_S, {call,_,delete_account,[_Password]}, Result) ->
    Result =:= account_deleted;
[...]
```

Thanks to the `proper_contrib_statem.hrl` boilerplate, the same
callback module will look like:

```erlang
[...]

create_account_pre(_S, _Args) ->
  true.

create_account_args(_S) ->
  [name()].

create_account_next(S, Res, [_Name]) ->
  S#state{users = [Res|S#state.users]}.

create_account_post(_S, [_Name], Res) ->
  not lists:member(Res, S#state.users).

[...]

delete_account_pre(_S, _Args) ->
  true.

delete_account_args(_S) ->
  [password()].

delete_account_next(S, Res, [Password]) ->
  S#state{users = lists:delete(Password, S#state.users)}.

delete_account_post(_S, [Password], Res) ->
  Result =:= account_deleted.
```

# Authors

* Roberto Aloi
* Juan Facorro
