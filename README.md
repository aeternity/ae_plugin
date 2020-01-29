ae_plugin
=====

Simple AE node bootstrapping application, based on `setup`,
to enable node plugin development.

The application is meant to be added as an application dependency
to a plugin project. When started, it locates an existing `aeternity`
node installation (through the mandatory OS env `AE_ROOT`), loads
the necessary applications, runs `setup` hooks, and then starts
the `aecore` application.

You need to either point to a `sys.config` in the `aeternity`
installation, or copy settings from there into your own `sys.config`.

The `ae_plugin` application sets the `setup` directories (`home()`,
`data_dir()` and `log_dir()` to the corresponding locations in the
`aeternity` node installation. This may be possible to override in
future versions. **Known bug:** Currently, the logs, keys and db end
up in the `ae_plugin` directory instead.

Build
-----

    $ rebar3 compile

Example
-------

This example illustrates starting a rebar3 shell where the
`aeternity node` is automatically bootstrapped while starting
the `ae_plugin` application.

```
export AE_ROOT=<path_to_ae_node_installation>
AETERNITY_CONFIG=`pwd`/ae-uat.yaml \
 ERL_LIBS=$AE_ROOT/lib \
 rebar3 shell --config $AE_ROOT/releases/5.4.1/sys.config
 
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe] [kernel-poll:false]

===> The rebar3 shell is a development tool; to deploy applications in production, consider using releases (http://www.rebar3.org/docs/releases)
Eshell V9.3  (abort with ^G)
1> 
=INFO REPORT==== 29-Jan-2020::15:29:49 ===
Setup running ...

=INFO REPORT==== 29-Jan-2020::15:29:49 ===
Directories verified. Res = ok

=INFO REPORT==== 29-Jan-2020::15:29:49 ===
Setup finished processing hooks (Mode=normal)...
Setting up data paths
Loading aecore and deps
Loading sext
Loading parse_trans
...
Running setup hooks

=INFO REPORT==== 29-Jan-2020::15:29:57 ===
Setup running ...

=INFO REPORT==== 29-Jan-2020::15:29:57 ===
Directories verified. Res = ok

=INFO REPORT==== 29-Jan-2020::15:29:57 ===
Setup phase 100
...
=INFO REPORT==== 29-Jan-2020::15:29:58 ===
aec_mining:check_env()-> ok

=INFO REPORT==== 29-Jan-2020::15:29:58 ===
Setup phase 200
Starting aecore
15:30:13.562 [info] aec_peers started for "pp_XnVYYbN...LnPExMH"
===> Booted setup
===> Booted ae_plugin
```
