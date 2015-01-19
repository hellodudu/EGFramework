-record( account, {account_id, 
                   role_id_list = [], 
                   max_allowd_role_num = 2} ).
-define(ACCOUNT_BUCKET,{<<"default">>, <<"account">>}).
-define(ROLE_BUCKET, {<<"default">>, <<"role">>}).