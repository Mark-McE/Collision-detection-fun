%%%-------------------------------------------------------------------
%%% @author Mark McElroy
%%% @copyright (C) 2018
%%% @doc
%%%
%%% Represents a 2d point in space.
%%% Use the create_point/1 function to create a process which can take
%%% many requests.
%%% To make a request, send this process the following:
%%%   {Origin_Process, get_point}
%%%     process will return this point to Origin_Process
%%%   {Origin_Process, point_collision, Point = {_,_}}
%%%     process will return {point_collision_response, true} or {point_collision_response, false}
%%%     if this point collides with the passed point, or does not, respectively.
%%%   {Origin_Process, line_collision, Line = {{_,_},{_,_}}}
%%%     process will return {line_collision_response, true} or {line_collision_response, false}
%%%     if this point collides with the passed line, or does not, respectively.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(point).
-author("Mark McElroy").

%% API
-export([create_point/1, point_on_point/2, wait_request/1]).

%% creates a point
create_point(This = {_X, _Y}) ->
  spawn(point, wait_request, [This]).


wait_request(This = {_X, _Y}) ->
  receive
    terminate ->
      true;

    {Process, get_point} ->
      Process ! {get_point_response, This},
      wait_request(This);

    {Process, point_collision, Point = {_,_}} ->
      Answer = point:point_on_point(This, Point),
      Process ! {point_collision_response, Answer},
      wait_request(This);

    {Process, line_collision, Line = {{_,_},{_,_}}} ->
      Answer = line_segment:point_on_line_segment(This, Line),
      Process ! {line_collision_response, Answer},
      wait_request(This)
  end.


%% determines whether two points are colliding.
%% returns the atom 'true' or 'false'
point_on_point(_Point_A = {X,Y}, _Point_B = {X2,Y2}) ->
  X == X2 andalso Y == Y2.
















