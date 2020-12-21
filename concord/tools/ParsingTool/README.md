# CLI: a parsing tool for Concord's logs

CLI offers an easy to use command-line interface to parse concord's logs.
It is developed in order to enhance our ability to understand what and why things happened in our system.
We want to be able to track which events are [causally related](https://scattered-thoughts.net/writing/causal-ordering/)
and produce causal ordered logs,
which will help us to get a view from above on the entire system and not only from a replica point of view.

## Libraries

### Click

We decided to build our command-line applications with Click.
Click is a Python package for creating command-line interfaces in a composable way.

Advantages:
* Easy-to-use alternative to the standard libraries.
* Utilizes decorators for code reduction.
* Allows arbitrary nesting of commands.
* Automatic help page generation.
* Well documented.

Commands are the basic building blocks of an application.
Commands are created with a click.command() decorator. Values are passed via options or arguments.
Options are added with the click.option() decorator, arguments with the click.argument().
Values in options follow the name of the option while arguments are taken positionally.

### tqmd

Instantly make your loops show a smart progress meter by wrapping any iterable.
A text progress bar used to display the progress of a long-running operation,
in our case parsing large log files,
providing a visual cue that processing is underway.

## Commands
CLI parse each protocol in our system separately, hence implements a dedicated parser for each case.

For information about the tool commands:

`cli --help`

For information about specific command:

`cli <name_of_the_command> --help`

CLI includes the following parsers:

**DAML parser** - This basic parser provides two options to output failed DAML requests.

* The following command will output DAML requests that weren't submitted or processed.

   `cli daml_parser <daml_log_file> failed-requests`

* The following command will output a DAML requests that took a longer time than the threshold to submit.
  Threshold default value is 500 millisecond, can be configured explicitly.

  `cli daml_parser <daml_log_file> exceeded-threshold`

  `cli daml_parser <daml_log_file> exceeded-threshold --thld <threshold>`

**State Transfer parser** - This parser parses State Transfer logs in a causal order. The parser requires two obligatory arguments and one optional:
1. in_path = <some_path> A path to the location of the logs.
2. out_path = <some_path> A path to the location where the output will be generated.
3. --is_deploy=<true/false> -  A flag that tells the parser which kind of log format will be parsed. Deployment or local format. (Optional: default value deploy=true)

- The following command will causal order State Transfer logs with deployment format.

  `cli st_parser <in_path> <out_path>`

- The following command will causal order State Transfer logs with local run format.

  `cli st_parser <in_path> <out_path> --is_deploy=false`


## Installation
CLI tool comes with setup.py file, which allows us to install it easily.

Run the following commands in your console:

`cd` vmwathena_blockchain/concord/tools/ParsingTool/

For basic usage run, `pip install .`

For developers usage run, `pip install -e .`

You would use this when trying to install a package locally, most often in the case when you are developing it on your system.
It will just link the package to the original location,
meaning any changes to the original package would reflect directly in your environment.
Thus, prevent the dev from re-installing the tool after every change in the source.

## Notes
- The causal order parser will generate a summary of the events detected in the logs. Parsing different logging levels logs might produce a summary with missing information.
- Although adding new parameters to the logs won't break the parser, changing or removing columns will.