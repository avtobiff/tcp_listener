{application, echo_app,
 [{description, "ECHO server"},
  {vsn, "1"},
  {modules, [echo_app, echo_fsm]},
  {registered, [echo_app]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {echo_app,[]}}
 ]}.
