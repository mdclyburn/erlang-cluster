{application, ecs,
 [{description, "Erlang cluster service"},
  {vsn, "0.1"},
  {mod, {ecs, []}},
  {modules, [ecs,
             ecs_master,
             ecs_connectivity]},
  {applications, []}
 ]}.
