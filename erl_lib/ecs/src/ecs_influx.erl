-module(ecs_influx).

-export([write/3]).

% ===== Public

% Sends measurements to the 'ecs' database.
% Returns ok upon success, {error, _} upon failure.
write([], _, _) -> ok;
write(Stats, Uri, Authorization) when is_list(Stats) ->
    case
        httpc:request(
          post,
          {Uri ++ "/write?db=ecs",
           [{"Authorization", "Basic " ++ Authorization}],
           "text/plain",
           lists:flatten(lists:join(io_lib:nl(), lists:map(fun (S) -> measurement_to_string(translate(S)) end, Stats)))},
          [{timeout, 1000}],
          [])
    of
        {ok, Response} -> map_response(Response);
        E = {error, _} -> E
    end.

% Convert generic statistics into Influx-compatible data.
translate({Name, Value, Time, Origin, Tags}) ->
    add_tags(new_measurement(Name, Value, Time), Tags ++ get_basic_tags(Origin)).

% ===== Private

% Create a new Influx measurement.
new_measurement(Name, Value, Time) -> {Name, Value, Time, []}.

% Convert a tag and its value to a string.
tag_to_string({Name, Value}) when is_list(Name), is_list(Value) -> Name ++ "=" ++ Value.

% Convert a measurement to a string.
measurement_to_string({Name, Value, Time, Tags}) ->
    Name ++ ","
        ++ lists:flatten(lists:join(",", lists:map(fun tag_to_string/1, Tags)))
        ++ " value="
        ++ if
               erlang:is_float(Value) -> erlang:float_to_list(Value);
               erlang:is_integer(Value) -> erlang:integer_to_list(Value)
           end
        ++ " " ++ erlang:integer_to_list(Time).

% Add a tag to a measurement.
add_tag(Name, Value, {MeasurementName, MeasurementValue, Time, Tags})
  when is_list(Name), is_list(Value) ->
    {MeasurementName, MeasurementValue, Time, [{Name, Value}|Tags]}.

% Add information to identify the node measurements are taken from.
get_basic_tags(Origin) when is_atom(Origin) ->
    [{"node_name", ecs_util:name(Origin)},
     {"node_host", ecs_util:host(Origin)},
     {"node", erlang:atom_to_list(Origin)}].

add_tags(Measurement, []) -> Measurement;
add_tags(Measurement, [{N, V}|Rest]) -> add_tags(add_tag(N, V, Measurement), Rest).

% Translates an HTTP response to an ok or error.
map_response({{_, Code, _}, _, _}) ->
    if
        Code >= 200, Code =< 299 -> ok;
        true -> {error, Code}
    end.
