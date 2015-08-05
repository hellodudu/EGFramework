-module(redis_query).
-define(REDIS_POOL, redis_pool).

-export([
         q/1,
         qp/1
        ]).

%% query
q(Command) ->
    poolboy:transaction(?REDIS_POOL, fun(Worker) ->
                                    gen_server:call(Worker, {q, Command})
                                  end).

%% query pipeline
qp(Command) ->
    poolboy:transaction(?REDIS_POOL, fun(Worker) ->
                                    gen_server:call(Worker, {qp, Command})
                                  end).
