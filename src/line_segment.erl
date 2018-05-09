%%%-------------------------------------------------------------------
%%% @author Mark McElroy
%%% @copyright (C) 2018
%%% @doc
%%%
%%% Represents a 2d line in space.
%%% Use the create_line/1 function to create a process which can take
%%% many requests.
%%% To make a request, send this process the following:
%%%   {Origin_Process, get_line}
%%%     process will return this point to Origin_Process
%%%   {Origin_Process, point_collision, Point = {_,_}}
%%%     process will return {point_collision_response, true} or {point_collision_response, false}
%%%     if this line collides with the passed point, or does not, respectively.
%%%   {Origin_Process, line_collision, Line = {{_,_},{_,_}}}
%%%     process will return {line_collision_response, true} or {line_collision_response, false}
%%%     if this line collides with the passed line, or does not, respectively.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(line_segment).
-author("Mark McElroy").

%% API
-export([create_line_segment/1, line_segment_on_line_segment/2, point_on_line_segment/2, wait_request/1]).

%% creates a line segment
create_line_segment(This = {{_,_}, {_,_}}) ->
  spawn(line_segment, wait_request, [This]).


wait_request(This = {{_,_}, {_,_}}) ->
  receive
    terminate ->
      true;

    {Process, get_line_segment} ->
      Process ! {get_line_segment_response, This},
      wait_request(This);

    {Process, point_collision, Point = {_,_}} ->
      Answer = line_segment:point_on_line_segment(Point, This),
      Process ! {point_collision_response, Answer},
      wait_request(This);

    {Process, line_collision, Line = {{_,_},{_,_}}} ->
      Answer = line_segment:line_segment_on_line_segment(This, Line),
      Process ! {line_collision_response, Answer},
      wait_request(This)
  end.


%% determines whether two line segments are colliding.
%% returns the atom 'true' or 'false'
line_segment_on_line_segment(_Line_A = {A1 = {Xa1,Ya1}, A2 = {Xa2,Ya2}},
                             _Line_B = {B1 = {Xb1,Yb1}, B2 = {Xb2,Yb2}}) ->
  %% The following algorithm models points on a line as
  %% Point = Line_A_Point_1 + Ua * (Line_A_Point_2 - Line_A_Point_1)
  %% Point = Line_B_Point_1 + Ub * (Line_B_Point_2 - Line_B_Point_1)
  %% Where Points are 2d vectors [ax, by], leading to two equations, for Point_x and point_y
  %%
  %% we can model each of two lines as such, then equate them to find the point which they intersect
  %% in equating them in this way, we solve for Ua or Ub
  %%
  %% in solving for Ua or Ub we must do a single division, the denominator of this division
  %% may be 0, in which case, the lines do not intersect.
  %%
  DeltaX_Line_A = Xa2 - Xa1,
  DeltaY_Line_A = Ya2 - Ya1,
  DeltaX_Line_B = Xb2 - Xb1,
  DeltaY_Line_B = Yb2 - Yb1,
  Denominator = DeltaY_Line_B * DeltaX_Line_A - DeltaX_Line_B * DeltaY_Line_A,

  if
    Denominator == 0 ->
      if
        A1 == B1 orelse A1 == B2 orelse A2 == B1 orelse A2 == B2 ->
          true; % tangential connection at a single point, therefor collision
        true ->
          io:format("divide by 0, no intercept~n"),
          false % divide by 0, no intercept, therefor return false
      end;
    true ->
      DeltaY_Lines_AB = Ya1 - Yb1,
      DeltaX_Lines_AB = Xa1 - Xb1,

      Ua_numerator = DeltaX_Line_B * DeltaY_Lines_AB - DeltaY_Line_B * DeltaX_Lines_AB,
      Ua = Ua_numerator / Denominator,
      
      Ub_numerator = DeltaX_Line_A * DeltaY_Lines_AB - DeltaY_Line_A * DeltaX_Lines_AB,
      Ub = Ub_numerator / Denominator,

      if
        Ua >= 0 andalso Ua =< 1 andalso Ub >= 0 andalso Ub =< 1 ->
          true; %% 0 <= Ua <= 1 infers that the point is on the line segment for line A,
                %% and similar for B. Therefor lines intercept, return true.
        true ->
          io:format("intercept happens on elongated line. Ua = ~p, Ub = ~p~n", [Ua,Ub]),
          false %% Otherwise, point is on the infinite line, but not the line segment.
                %% Therefor, return false.
      end
  end.


%% determines whether a point and a line segment are colliding.
%% returns the atom 'true' or 'false'
point_on_line_segment(Point, _Line = {Point_A, Point_B}) ->
  tuple_addition(abs_distance(Point_A, Point), abs_distance(Point_B, Point))
    == abs_distance(Point_A, Point_B).


%% determines the absolute distance between two points
%% returns a tuple of {x_distance, y_distance}
abs_distance({X,Y}, {X2,Y2}) ->
  {abs(X-X2), abs(Y-Y2)}.

%% adds two, 2d tuples together
tuple_addition({A,B}, {C,D}) ->
  {A+C, B+D}.