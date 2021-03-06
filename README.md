# erl_playground

An OTP application to start coding without the boring stuff.

## Prerequisites
This project has been written for Mac and Linux environments, theoretically speaking it can run on any environment where a Erlang system is correcty installed, but consider that MS Windows and Erlang are not best buddies. Nowadays it is pretty easy to have Linux systems running in minutes using Virtual Machines, Containers, USB distro or simply double booting your laptop.

In case you use a Mac system, we strongly recommend using [homebrew](https://brew.sh/) to manage all your packages.

**OpenSSL**

Check the correct installation process for you environment.

**Erlang/OTP 21.3**

If you are on Mac, we strongly suggest using [kerl](https://github.com/kerl/kerl) to build and install the proper Erlang version on your system. For other environments you can easily find your installation package on [ErlangSolutions](https://www.erlang-solutions.com/).

## Build & Run

This is a [rebar3](https://www.rebar3.org/) project.

## Compile GPB

Google Protocol Buffer is automatically compiled starting from the included proto file.
[Here](https://developers.google.com/protocol-buffers/) you can find all the information about it.

## What you have out of the box
This is a playgrounf application that allows you to focus on the logic of your system, rather than the boring technical stuff. It includes a basic Erlang/OTP application structure with a TCP client and a TCP server.

# Candidate comments
These features has been implemented:
* Automatic responder
* Jokes of the day
* My called id
* Ask the operator
And also:
* Limited operators
* Chat
* Client
* Tests

## Messages
A new proto message has been implemented (`user_request`) to send input to the `automatron_fsm`.
Another message has been implemented (`test_pid`) to ask for the pid to the `automatron_fsm` during the tests.
## Client
### Integrated client
You can connect to the server performing:

```bash
rebar shell
```

and

```erlang
> sockclient:connect().
> sockclient:send_create_session().
```

After this you can perform user interactions with

```erlang
> sockclient:send_user_request(Input).
```

### Optional client in Python
You can find the code [here](https://sberto@github.com/sberto/mc-challenge-client.git).

## Tests
Run tests with

```bash
rebar shell
```

and

```erlang
> ct:run_test([{spec, "spec.spec"}]).
```
