-record(role, {
                role_id, 
                account_id, 
                name,
                sex,
                level,
                diamond,
                
                runtime
  } ).

-record(role_runtime, {
                       session,
                       reconnectionTimer
                       }).