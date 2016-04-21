%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (c) 2012-2016 Kivra
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%% @doc Erlang SFTP Utils
%%%
%%%      This library is designed to simplify some common SFTP operations
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(sftp_utils).

%%%_* Exports ==========================================================
%%%_ * API -------------------------------------------------------------
-export([read_file/3]).
-export([write_file/3]).
-export([delete/2]).
-export([rename/3]).
-export([list_dir/2]).
-export([read_file_info/2]).
-export([ensure_dir/2]).

-include_lib("kernel/include/file.hrl").

%%%_ * Types -----------------------------------------------------------
-type opt()  :: {host,                  string()}
              | {port,                  non_neg_integer()}
              | {user,                  string()}
              | {password,              string()}
              | {timeout,               non_neg_integer()}
              | {connect_timeout,       non_neg_integer()}
              | {user_interaction,      boolean()}
              | {silently_accept_hosts, boolean()}.
-type opts() :: list(opt()).

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
-spec read_file(string(), string(), opts()) -> ok | {error, term()}.
read_file(From, To, Config) ->
  with_connection(
    fun(Pid, Timeout) ->
        case ssh_sftp:open(Pid, From, [read, binary], Timeout) of
          {ok, Handle} ->
            case file:open(To, [write, binary]) of
              {ok, FD} ->
                Res = case ssh_sftp:read_file_info(Pid, From, Timeout) of
                        {ok, #file_info{size = Size}} ->
                          do_read_file(Pid, Timeout, Handle, FD, Size);
                        {error, _} = Err ->
                          Err
                      end,
                file:close(FD),
                ssh_sftp:close(Pid, Handle, Timeout),
                Res;
              {error, _} = Err ->
                ssh_sftp:close(Pid, Handle, Timeout),
                Err
            end;
          {error, _} = Err -> Err
        end
    end, Config).

-spec write_file(string(), string(), opts()) -> ok | {error, term()}.
write_file(To, From, Config) ->
  with_connection(
    fun(Pid, Timeout) ->
        case file:open(From, [read, binary]) of
          {ok, FD} ->
            case ssh_sftp:open(Pid, To, [write, binary], Timeout) of
              {ok, Handle} ->
                Res = do_write_file(Pid, Timeout, Handle, FD),
                file:close(FD),
                ssh_sftp:close(Pid, Handle, Timeout),
                Res;
              {error, _} = Err ->
                file:close(FD),
                Err
            end;
          {error, _} = Err -> Err
        end
    end, Config).

-spec delete(string(), opts()) -> ok | {error, term()}.
delete(Path, Config) ->
  with_connection(
    fun(Pid, Timeout) ->
        ssh_sftp:delete(Pid, Path, Timeout)
    end, Config).

-spec rename(string(), string(), opts()) -> ok | {error, term()}.
rename(From, To, Config) ->
  with_connection(
    fun(Pid, Timeout) ->
        ssh_sftp:rename(Pid, From, To, Timeout)
    end, Config).

-spec list_dir(string(), opts()) -> {ok, string()} | {error, term()}.
list_dir(Dir, Config) ->
  with_connection(
    fun(Pid, Timeout) ->
        ssh_sftp:list_dir(Pid, Dir, Timeout)
    end, Config).

-spec read_file_info(string(), opts()) -> {ok, #file_info{}} | {error, term()}.
read_file_info(Path, Config) ->
  with_connection(
    fun(Pid, Timeout) ->
        ssh_sftp:read_file_info(Pid, Path, Timeout)
    end, Config).

-spec ensure_dir(string(), opts()) -> ok | {error, term()}.
ensure_dir(Path, Config) ->
  with_connection(
    fun(Pid, Timeout) ->
        [Hd|Tl] = filename:split(filename:dirname(Path)),
        %% NOTE: assume root exist
        lists:foldl(
          fun(Component, P0) ->
              P = filename:join(P0, Component),
              case ssh_sftp:read_file_info(Pid, P, Timeout) of
                {ok, _} -> ok;
                {error, Rsn}
                  when Rsn =:= enoent;
                       Rsn =:= no_such_file ->
                  ok = ssh_sftp:make_dir(Pid, P, Timeout)
              end,
              P
          end, Hd, Tl),
        ok
    end, Config).

%%%_* Private functions ================================================
%% NOTE: Buffer sizes larger than 65536 seems to corrupt the data,
%% figure out why.
%%-define(buf_size, 262144).
%% -define(buf_size, 131072).
-define(buf_size, 65536).

do_read_file(Pid, Timeout, Handle, FD, Size) ->
  case send_async_reads(Pid, Handle, Size, 0, queue:new()) of
    {ok, Refs}       -> receive_async_reads(FD, Timeout, Refs);
    {error, _} = Err -> Err
  end.

send_async_reads(_, _, Size, Pos, Refs) when Size < (Pos - ?buf_size) ->
  {ok, Refs};
send_async_reads(Pid, Handle, Size, Pos, Refs) ->
  case ssh_sftp:apread(Pid, Handle, Pos, ?buf_size) of
    {async, N} ->
      send_async_reads(Pid, Handle, Size, Pos+?buf_size, queue:in(N, Refs));
    {error, Error} ->
      {error, Error}
  end.

receive_async_reads(FD, Timeout, Refs0) ->
  {{value, Ref}, Refs} = queue:out(Refs0),
  receive
    {async_reply, Ref, {ok, Data}} ->
      case file:write(FD, Data) of
        ok               -> receive_async_reads(FD, Timeout, Refs);
        {error, _} = Err -> Err
      end;
    {async_reply, Ref, {error, _} = Err} ->
      Err;
    {async_reply, Ref, eof} ->
      ok
  after
    Timeout -> {error, timeout}
  end.

do_write_file(Pid, Timeout, Handle, FD) ->
  case send_async_writes(Pid, Handle, FD, 0, queue:new()) of
    {ok, Refs}       -> receive_async_writes(Timeout, Refs);
    {error, _} = Err -> Err
  end.

send_async_writes(Pid, Handle, FD, Pos, Refs0) ->
  case file:read(FD, ?buf_size) of
    {ok, Data} ->
      case ssh_sftp:apwrite(Pid, Handle, Pos, Data) of
        {async, Ref} ->
          NextPos = Pos + byte_size(Data),
          Refs    = queue:in(Ref, Refs0),
          send_async_writes(Pid, Handle, FD, NextPos, Refs);
        {error, _} = Err ->
          Err
      end;
    {error, _} = Err ->
      Err;
    eof ->
      {ok, Refs0}
  end.

receive_async_writes(Timeout, Refs0) ->
  case queue:out(Refs0) of
    {empty, _} ->
      ok;
    {{value, Ref}, Refs} ->
      receive
        {async_reply, Ref, ok} ->
          receive_async_writes(Timeout, Refs);
        {async_reply, Ref, {error, _} = Err} ->
          Err
      after
        Timeout -> {error, timeout}
      end
  end.

with_connection(F, Config) ->
  {ok, Host}    = lget(Config, host),
  {ok, Port}    = lget(Config, port),
  {ok, Timeout} = lget(Config, timeout),
  Retries       = lget(Config, retries, 2), %% Two retries by default
  ConnConf      =
    lists:foldl(fun({user, _} = E, Acc)                  -> [E|Acc];
                   ({password, _} = E, Acc)              -> [E|Acc];
                   ({connect_timeout, _} = E, Acc)       -> [E|Acc];
                   ({silently_accept_hosts, _} = E, Acc) -> [E|Acc];
                   (_, Acc)                              -> Acc
                end,
                [], Config),
  retry(
    fun() ->
      case ssh:connect(Host, Port, ConnConf, Timeout) of
        {ok, ConnectionRef} ->
          case ssh_sftp:start_channel(ConnectionRef) of
            {ok, Pid} ->
              Res = F(Pid, Timeout),
              ok = ssh_sftp:stop_channel(Pid),
              ok = ssh:close(ConnectionRef),
              {ok, Res};
            {error, _} = Err ->
              ok = ssh:close(ConnectionRef),
              Err
          end;
        {error, _} = Err ->
          Err
      end
    end, timer:seconds(1), Retries).

retry(F, T, N) ->
  retry(F(), F, T, N).

retry({ok, Res}, _F, _T, _N) ->
  Res;
retry({error, _} = Err, _F, _T, 0) ->
  Err;
retry({error, _}, F, T, N) when N > 0 ->
  timer:sleep(T),
  retry(F(), F, T, N-1).

lget(L, K) ->
  case lists:keyfind(K, 1, L) of
    {K, V} -> {ok, V};
    false  -> {error, notfound}
  end.

lget(L, K, Def) ->
  case lget(L, K) of
    {ok, V}           -> V;
    {error, notfound} -> Def
  end.

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
