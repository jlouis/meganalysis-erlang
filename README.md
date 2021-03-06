# 101companies, Meganalysis

This is a very quick Erlang implementation of the basics of the
101companies tasks. See further down for a description of the implementation.

Current list of things that should be done:

* Implement persistence
* Implement distribution
* Consider outsourcing the data store to Mnesia
* Add spec constraints
* Implement some of the more specialized versions
* Parallelize the work done.
* Improve roboustness.

# Prerequisites, building, testing

First, in order to run this code, you need three tools:

* Make, preferably GNU make, though the makefile should be BSD-make
  compatible. It can be skipped if you want. The Makefiles only serves
  as a canonical interface that makes Emacs and Vim's build-triggers
  work nicely.

* A Recent Erlang/OTP distribution. I used R14B02, but anything from
  R13B04 and up ought to work for this. Avoid R14A if possible. It is
  a beta-release.

* Rebar. Rebar is an Erlang build tool. It is available from

     https://github.com/basho/rebar

  When rebar has been boot-strapped, you place it into your path after
  which you have access to a build system that is bearable to work with.

## Building

Once the above requirements are satisfied, the code can be built with

     make compile

It should complete with no warnings.

## Testing

To test the code, we use

     make console

which will spawn a console in the Erlang system, but with an altered
code-load path so it can find our compiled bytecode .beam files. Now
you can test it by:

     1> meganalysis:run(). % Press return here :)
     {399747,199873.5}
     2> q().
     ok


Note the '.', which marks the end of the expression to the parser.

# Implementation details

The code makes extensive use of the OTP (Open Telecom Platform). Aptly
misnamed, this is a library of common concurrency tasks which most
concurrent programs will use. Rather than writing code that uses the
primitive send (`!`) and receive (`receive .. end`) constructs, these
are wrapped in OTP.

A common OTP idiom is that OTP provides a *generic* version of a task,
a server, a supervisor, and so on. This generic version is then
specialized by injecting the name of a module. Whenever specific hook
points in the generic server is reached, a callback is made to the
module according to a protocol. Thus our code is call-backed to handle
the specialization. A consequence as that we don't have to handle all
the gritty parts of message passing concurrency.

The code consist of several modules:

* [`meganalysis.app.src`] This file describes a manifest of the
  maganalysis application.

* [`meganalysis_app`] A callback interface to the generic
  `application` system in OTP. An application is a collection of
  processes that so some task together. The Erlang VM is an
  application server in which you store several applications. Should
  we want to provide a web interface for instance, we can spawn a
  webserver application later.

* [`meganalysis_sup`] Provides the callbacks for the generic
  `supervisor` construction. The supervisor is built to mostly live in
  hibernation supervising concrete workers. If the worker dies due to
  a program error or some unforseen event, then the supervisor will
  restart its worker according to some rules. We use the `one_for_one`
  strategy here, but there are several possible. If the worker crashes
  too often however, the supervisor itself dies. This trigger
  application death in our case, but usually a nested tree of
  supervisors can withstand supervisor crashes as the error usually
  only propagates up the tree to a certain point.

* [`meganalysis_process`] A generic server implementation. That is, we
  provide a set of callbacks for the `gen_server` OTP generic server
  *behaviour*. Note that we export a neat interface which hides the
  `gen_server` parts. Specifically, we export `total()` and
  `cut()`. Total is a *call* meaning that it is synchronous. A caller
  will wait on the server to return an answer. Cut is a *cast* on the
  other hand. It is asynchronous and the caller is not going to wait
  on its completion.

  the `handle_*` parts are the protocol for the `gen_server`
  callbacks. That is actually doing the meat of the work.

* [`meganalysis`] It is common to provide a nice API to the rest of
  the Erlang system, to hide the complexities of processes. An user of
  the application will only use this API to work with the
  application. Thus, we decouple the internal design of the
  application with its interface.


