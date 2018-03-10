-module(ecs_influx).

-export([new_measurement/2,
         new_measurement/3,
         add_tag/3,
         write/3,
         write_all/3]).

-compile(export_all).

% ===== Public

% Create a new Influx measurement.
new_measurement(Name, Value) -> new_measurement(Name, Value, []).
new_measurement(Name, Value, Tags) -> {Name, Value, Tags}.

% Add a tag to a measurement.
add_tag(Name, Value, {MeasurementName, MeasurementValue, Tags}) when is_list(Name), is_list(Value) ->
    {MeasurementName, MeasurementValue, [{Name, Value}|Tags]}.

% Send a measurement to the 'ecs' database.
% Actually just calls write_all/3, but same effect.
write(Measurement, Uri, Authorization) -> write_all([Measurement], Uri, Authorization).

% Sends measurements to the 'ecs' database.
% Returns ok upon success, {error, _} upon failure.
write_all(Measurements, Uri, Authorization) when is_list(Measurements) ->
    case
        httpc:request(
          post,
          {Uri ++ "/write?db=ecs",
           [{"Authorization", "Basic " ++ Authorization}],
           "text/plain",
           lists:flatten(lists:join(io_lib:nl(), lists:map(fun measurement_to_string/1, Measurements)))},
          [{timeout, 500}],
          [])
    of
        {ok, _} -> ok;
        E = {error, _} -> E
    end.

% ===== Private

% Convert a tag and its value to a string.
tag_to_string({Name, Value}) when is_list(Name), is_list(Value) -> Name ++ "=" ++ Value.

% Convert a measurement to a string.
measurement_to_string({Name, Value, Tags}) ->
    Name ++ ","
        ++ lists:flatten(lists:join(",", lists:map(fun tag_to_string/1, Tags)))
        ++ " value="
        ++ if
               erlang:is_float(Value) -> erlang:float_to_list(Value);
               erlang:is_integer(Value) -> erlang:integer_to_list(Value)
           end.
