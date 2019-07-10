# proper-contrib

[![Build Status](https://travis-ci.org/robertoaloi/proper_contrib.svg?branch=master)](https://travis-ci.org/robertoaloi/proper_contrib)
[![Coverage Status](https://coveralls.io/repos/github/robertoaloi/proper_contrib/badge.svg?branch=master)](https://coveralls.io/github/robertoaloi/proper_contrib?branch=master)

A collection of goodies for the property-based testing framework PropEr.

## Getting Started

To get started, simply include the `proper_contrib` repo as a test
dependency for your project. For example, if you are using the
`rebar3` build tool, you can add the following to your `rebar.config`:

```erlang
{profiles, [{test, {deps, [{proper_contrib, "0.1.0"}]}}]}.
```

## Features

### Write your PropEr `statem` modules a' la QuickCheck

#### Rationale

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

The boilerplate also makes the `command/1` function not needed
anymore. The list of _commands_, in fact, are dynamically inferred _by
convention_ (i.e. by looking at exported functions of arity `1` within
the same module whose name ends with `_args`). An optional `weight/1`
callback function allows users to specify a different _frequency_ for
each command.

#### Usage

Simply include the `proper_contrib_statem.hrl` file in your
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

#### Example

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

### Simpler `proper_statem` properties

#### Rationale

When implementing a _statem_ callback module in _PropEr_ (or even
_QuickCheck for what matters) you often end up copy-pasting a bunch of
boilerplate code in your property. This boilerplate code includes
calls to your setup/teardown functions, some `?WHENFAIL` actions, some
statistical aggregations. Why not to refactor all of this boilerplate
into a separate, re-usable module? This is what the
`proper_contrib_statem` provides.

### Usage

From the property in your _proper\_statem_ callback module, simply
invoke the `proper_contrib_statem:run/1` function, instead of
implementing the `?FORALL` boilerplate yourself:

```erlang
prop_sample() ->
  proper_contrib_statem:run(?MODULE).
```

The above will give you out-of-the-box:

* The `?FORALL` boilerplate
* A categorization of commands by command name
* A more readable _history_ printout, annotated with _command names_
* A more readbale _state evolution_

A `proper_contrib_statem:run/2` function is also available to override
some of the above, default behaviours:

```erlang
prop_sample() ->
  Config = #{ setup_fun    -> fun setup/0
            , teardown_fun -> fun teardown/1
            , cleanup_fun  -> fun cleanup/0
            , whenfail_fun -> fun whenfail/4
            },
  proper_contrib_statem:run(?MODULE, Config).
```

The `setup/0` function is executed _once_ before the first test and
its return value is passed to the `teardown/1` function. The
`teardown/1` function is executed _once_ at the end of all tests. The
`cleanup/0` function is executed between each sequence of commands.
Finally, the `whenfail/4` function (used by the `?WHENFAIL` macro)
takes four arguments: a `proper_statem:command_list()`, a
`proper_statem:history()`, a `proper_statem:dynamic_state()` and a
`proper_statem:statem_result()`.

# Authors

* Roberto Aloi
* Juan Facorro
