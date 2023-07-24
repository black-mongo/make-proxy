%%%-------------------------------------------------------------------
%%% @author wang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Oct 2016 下午7:46
%%%-------------------------------------------------------------------
-author("wang").

-include_lib("kernel/include/logger.hrl").

-record(client,
        {key :: string(),
         ref :: ranch:ref(),
         socket :: any(),
         transport :: module(),
         ok,
         closed,
         error,
         remote :: port() | undefined,
         protocol :: module() | undefined,
         buffer :: binary(),
         keep_alive = flase :: boolean(),
         handle_state = #{},
         enable_https = false :: boolean()}).

-type mp_target() :: {inet:ip_address() | nonempty_string(), inet:port_number()}.

-define(Debug(A, B), ?LOG_DEBUG(A, B)).
