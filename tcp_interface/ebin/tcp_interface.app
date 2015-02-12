{application, tcp_interface,
 [
  {description, ""},
  {vsn, "0.1.0"},
  {registered, [ti_sup]},
  {modules, [ti_app,
             ti_sup,
             ti_server]},
  {applications, [
                  kernel,
                  stdlib,
                  sasl
                 ]},
  {mod, { ti_app, []}},
  {env, []}
 ]}.
