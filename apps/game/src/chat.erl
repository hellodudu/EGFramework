-module(chat).

-include("../../include/chat_pb.hrl").
-include("../../include/role.hrl").
-include("../../include/session.hrl").
-include("../../include/error_code.hrl").

-export([handle/1]).


handle({CsChatDeliverTo,Role}) when erlang:is_record(CsChatDeliverTo,cs_chat_deliver_to) ->
    #role{account_id=AccountId} = Role,
    #cs_chat_deliver_to{recipient_account_id=ReceipentAccountId, message=Message} = CsChatDeliverTo,
    ValidatedMessage = validate_message(Message),
    case global:whereis_name(ReceipentAccountId) of
        Pid when erlang:is_pid(Pid) -> 
            connector:send_to_role(Pid,#sc_chat_one_message{sender_account_id=ReceipentAccountId,
                                                            message=ValidatedMessage}),
            connector:send_to_role(AccountId, #sc_chat_deliver_to{result=?SUCCESS});
        _ -> 
            connector:send_to_role(AccountId, #sc_chat_deliver_to{result=?ROLE_NOT_ONLINE})
    end.
  
validate_message(Message) -> %% fuck you --transate-to--> ** you
    Message.
