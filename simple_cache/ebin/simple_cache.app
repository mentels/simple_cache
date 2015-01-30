{application, simple_cache,
 [
  {description, ""},
  {vsn, "0.1.0"},
  {registered, [sc_sup]},
  {modules, [sc_app,
             sc_element,
             sc_element_sup,
             sc_event,
             sc_event_logger,
             sc_store,
             sc_sup,
             simple_cache]},
  {applications, [
                  kernel,
                  stdlib,
                  sasl,
                  stdlib,
                  mnesia,
                  resource_discovery
                 ]},
  {mod, { sc_app, []}},
  {env, []}
 ]}.
