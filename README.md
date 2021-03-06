# rebar3_grisp

Rebar plug-in for the GRiSP project.

## Installation

### Globally

To install the plug-in globally, add the plug-in to your plug-ins list in
`~/.config/rebar3/rebar.config`:

```erlang
{plugins, [
    rebar3_hex,
    rebar3_grisp
]}.
```

CAVEAT: global rebar plugins are  kind of hard to upgrade to a newer version.
The way *not* to do it is `rebar plugins upgrade rebar3_grisp`  what worked for us is removing it from the `.cache` dir `rm -rf  ~/.cache/rebar3/plugins/rebar3_grisp/` and keeping it in the config.  The next call to `rebar3 new grispapp` it gets reinstalled in the latest version configure in `~/.config/rebar3/rebar.config` .

### For an Existing Project

Add the plug-in to your rebar config:

```erlang
{plugins, [rebar3_grisp]}.
```

Then just call your plug-in directly in the root of the existing application:

```
$ rebar3 grisp
===> Fetching grisp
===> Compiling grisp
<Plugin Output>
```

***Note!*** This does not work together with the global installation of the
plug-in with Rebar versions 3.4.7 and above, see [Rebar 3 crashes when deploying
my project. What should I do?](https://github.com/grisp/grisp/wiki /Frequently-
Asked- Questions#rebar-3-crashes-when-deploying-my-project-what- should-i-do) in
the [FAQ](https://github.com/grisp/grisp/wiki/Frequently-Asked- Questions) for
more information.

## Create New Application

Prerequisites:

* [Install Plug-In Globally](#globally)

To create a new GRiSP project:

```
$ rebar3 new grispapp name=mygrispproject dest=/path/to/SD-card
```

The specific variables provided by this plug-in are:

* **`name`** is the name of the OTP application
* **`dest`** is the destination path for deployment. This should point to where
  your SD-card is mounted (e.g. on macOS it is `/Volumes/<NAME>` where `<NAME>`
  is the name of the SD-card partition)
* **`otp_release`** is the target Erlang/OTP version used on the GRiSP (defaults
  to `19`)

For a full list of customizable variables, run `rebar3 new help grispapp`.

## Compile the project

```rebar3 compile```

Make sure you do that with Erlang 19.3.6. If you compiled rebar3 yourself with a more recent version of Erlang it will give errors, you will need to recompile rebar3 as well in that case.

For further information have a look at the [GRiSP Wiki](https://github.com/grisp/grisp/wiki)

## Build OTP for GRiSP

Add the path to your build toolchain to the `rebar.config` file so the section looks something like this:

```erlang
{grisp, [
    {otp_release, "19"},
    
    {toolchain, [{root,"/home/USERNAME/GRiSP/grisp-software/rtems-install/rtems-4.12"}]},
    
    {deploy, [
        {destination, "/run/media/MYGRISPSD/"}
    ]}
]}.
```

Then execute `rebar3 grisp build`. This will take some time, because Erlang/OTP is cross-compiled for the GRiSP board.

You only need to do that again if you updated and rebuilt the `grisp-software` repository or if you wrote new drivers in C. If you need to build OTP for a second time you can speed it up by using `rebar3 grisp build --configure false`.

The built Erlang distribution and its runtime system is located in the project
folder, under the path `_grisp/otp/<version>/install`.

## Deploy an Application

To deploy a GRiSP application, use the command `rebar3 grisp deploy`. The
command requires the release name and version to be provided. The deployment
destination can be set in `rebar.config` or be given as an additional argument.

Example:

```
rebar3 grisp deploy --relname my_release --relvsn 0.7.8
```

Run `rebar3 help grisp deploy` for information on all arguments.

### Configuration

`rebar.config`:

```erlang
{grisp, [
    {deploy, [
        % Path to put deployed release in
        {destination, "/path/to/destination"},

        % Shell script to run before deploying begins
        {pre_script, "rm -rf /path/to/destination/*"},

        % Shell script to run after deploying has finished
        {post_script, "unmount /path/to/destination"}
    ]}
]}.
```
