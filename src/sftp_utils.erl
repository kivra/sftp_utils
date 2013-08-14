
%%%_* Module declaration ===============================================
-module(sftp_utils).

%%%_* Exports ==========================================================
-export([ read_file/2
        , write_file/2
        , delete/1
        , rename/2
        , list_dir/1
        , read_file_info/1
        ]).

%%%_* Macros ===========================================================
-define(host, "uc.ftp.kivra.com").
-define(port, 22).
-define(user, "kivra").
-define(pass, "whatever").
-define(timeout, 5000).

%%%_ * API -------------------------------------------------------------
read_file(From, To) ->
  e(fun(Pid) ->
        case ssh_sftp:open(Pid, From, [read, binary], ?timeout) of
          {ok, Handle} ->
            case file:open(To, [write, binary]) of
              {ok, FD} ->
                do_read_file(Pid, Handle, FD);
              {error, _} = Err ->
                ssh_sftp:close(Pid, Handle, ?timeout),
                Err
            end;
          {error, _} = Err -> Err
        end
    end).

write_file(To, From) ->
  e(fun(Pid) ->
        case file:open(From, [read, binary]) of
          {ok, FD} ->
            case ssh_sftp:open(Pid, To, [write, binary], ?timeout) of
              {ok, Handle} ->
                do_write_file(Pid, Handle, FD);
              {error, _} = Err ->
                file:close(FD),
                Err
            end;
          {error, _} = Err -> Err
        end
    end).

delete(Path) ->
  e(fun(Pid) -> ssh_sftp:delete(Pid, Path, ?timeout) end).

rename(From, To) ->
  e(fun(Pid) -> ssh_sftp:rename(Pid, From, To, ?timeout) end).

list_dir(Dir) ->
  e(fun(Pid) -> ssh_sftp:list_dir(Pid, Dir, ?timeout) end).

read_file_info(Path) ->
  e(fun(Pid) -> ssh_sftp:read_file_info(Pid, Path, ?timeout) end).

%%%_ * Internals -------------------------------------------------------
do_read_file(Pid, Handle, FD) ->
  case ssh_sftp:read(Pid, Handle, 65536) of
    {ok, Data} ->
      case file:write(FD, Data) of
        ok ->
          do_read_file(Pid, Handle, FD);
        {error, _} = Err ->
          file:close(FD, Data),
          ssh_sftp:close(Pid, Handle, ?timeout),
          Err
      end;
    {error, _} = Err ->
      file:close(FD),
      ssh_sftp:close(Pid, Handle, ?timeout),
      Err;
    eof ->
      ok = file:close(FD),
      ok = ssh_sftp:close(Pid, Handle, ?timeout),
      ok
  end.

do_write_file(Pid, Handle, FD) ->
  case file:read(FD, 65536) of
    {ok, Data} ->
      case ssh_sftp:write(Pid, Handle, Data, ?timeout) of
        ok ->
          do_write_file(Pid, Handle, FD);
        {error, _} = Err ->
          file:close(FD),
          ssh_sftp:close(Pid, Handle, ?timeout),
          Err
      end;
    {error, _} = Err ->
      file:close(FD),
      ssh_sftp:close(Pid, Handle, ?timeout),
      Err;
    eof ->
      ok = file:close(FD),
      ok = ssh_sftp:close(Pid, Handle, ?timeout),
      ok
  end.

e(F) ->
  _ = application:start(crypto),
  _ = application:start(ssh),
  case ssh:connect(?host, ?port,
                   [{user, ?user},
                    {password, ?pass},
                    {silently_accept_hosts, true},
                    {connect_timeout, ?timeout}]) of
    {ok, ConnectionRef} ->
      case ssh_sftp:start_channel(ConnectionRef) of
        {ok, Pid} ->
          Res = F(Pid),
          ok = ssh_sftp:stop_channel(Pid),
          ok = ssh:close(ConnectionRef),
          Res;
        {error, _} = Err ->
          ok = ssh:close(ConnectionRef),
          Err
      end;
    {error, _} = Err ->
      Err
  end.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
