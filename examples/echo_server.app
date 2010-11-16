{application, echo_server,
 [{description, "ECHO server"},
  {vsn, "1"},
  {modules, [echo_server, echo_fsm]},
  {registered, [echo_server]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {echo_server,[]}}
 ]}.
