%%%-------------------------------------------------------------------
%%% @author Mark McElroy
%%% @copyright (C) 2018
%%% @doc
%%%
%%% To test the collision detection methods within the point and
%%% line_segment modules.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tester).
-author("Mark McElroy").

%% API
-export([run/0]).

run() ->
  Line1 = {{0,0},{5,5}},
  Line2 = {{0,0},{-5,-5}},

  Line1_process = line_segment:create_line_segment(Line1),

  Line1_process ! {self(), line_collision, Line2},
  Result =
    receive
      {line_collision_response, Answer} ->
        Answer
    after
      2000 ->
        {error, timeout}
    end,
  io:format("Line ~p on Line ~p collision: ~p ~n", [Line1, Line2, Result]),
  ok.
