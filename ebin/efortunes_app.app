{application, efortunes_app,
 [
  {description, "Prototype efortunes"},
  {vsn, "1"},
  {modules, 
   [
    efortunes_app, 
    efortunes_super, 
    efortunes_facade,
    efortunes_messagesTrader
   ]
  },
  {registered, [efortunes_facade]},
  {applications, [kernel, stdlib]},
  {env, 
   [
    {logs_dir, "../logs"},
    {log_prefix, "efortunes_log"},
    {log_tty, false}
   ]
  }, 
  {mod, {efortunes_app,[]}}
 ]
}.
	
