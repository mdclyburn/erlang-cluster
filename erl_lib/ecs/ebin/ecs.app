{application, ecs,
 [{description, "Erlang cluster service"},
  {vsn, "0.1"},
  {mod, {ecs, []}},
  {modules, [ecs,
             ecs_master,
             ecs_config,
             ecs_connectivity,
			 ecs_influx,
			 ecs_statistics,
			 ecs_util]},
  {applications, [ssl]}
 ]}.
