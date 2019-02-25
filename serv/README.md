# serv

An OTP application


## Build and Run

Before building, we should set a macro in `rebar.config`.
Please insert an absolute path to this repository as follows:

```erlang
{erl_opts,
    [ debug_info
    , {d, 'ROOT', "/path/to/SamiDare/"} %% <----- This
    ]
}.
```

Then,

```
wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3
rebar3 compile
rebar3 run
```
