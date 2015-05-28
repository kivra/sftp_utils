# sftp_utils (v0.1.0)
Wrapper around ssh_sftp to simplify some common tasks. A new connection
is set up for each command.

sftp_utils is released under the terms of the [MIT](http://en.wikipedia.org/wiki/MIT_License) license

copyright 2012-2014 Kivra

## tl;dr
By default this library will retry *two* times resulting in a total of *three* tries.

### Examples
Example usage:
```erlang
Config = [ {host,                  "somehost"}
         , {port,                  22}
         , {user,                  "foo"}
         , {password,              "bar"}
         , {timeout,               5000}
         , {connect_timeout,       5000}
         , {user_interaction,      false}
         , {silently_accept_hosts, true}
         , {retries,               2}
         ],

%% list a dir
{ok, Dir} = sftp_utils:list_dir("/tmp", Config),

%% ensure dir exists
ok = sftp_utils:ensure_dir("/home/foo/bar/", Config).

%% upload /tmp/blob.bin
ok = sftp_utils:write_file("/home/foo/bar/blob.bin", "/tmp/blob.bin", Config),

%% download /home/foo/bar/blob.bin"
ok = sftp_utils:read_file("/home/foo/bar/blob.bin", "/tmp/blob.bin", Config),

%% read file info
{ok, Info} = sftp_utils:read_file_info("/home/foo/bar/blob.bin", Config),

%% delete a file
ok = sftp_utils:delete("/home/foo/bar/blob.bin", Config),
```
