# Time Tracking tool

Use [todo.txt](https://github.com/todotxt/todo.txt) in `~/todo.txt` and
`~/done.txt`. Tt will operate on these two files.

Tt will read both `todo.txt` and `done.txt` files for analysis, but it will
always insert new items in the `todo.txt` file, even if they are items already
marked as done with the `x ` prefix. This is so that you have a chance to edit
the lines generated by tt before archiving them and treating the `done.txt`
file as more of a write-only store.

Tt has logic that depends on the ordering of items, but it always sorts the
items itself based on their timestamps and ignores the physical order the
lines are found in the `done.txt` and `todo.txt` log files.

## TODO items

Tt is not a complete todo application. You are assumed to use a text editor or
a dedicated todo.txt tool like Simpletask to maintain your todo list. However,
tt does provide you with `todo` and `done` commands to add a new open or
completed item to your `todo.txt` file from the command line.

    tt todo "Buy milk @shop"

Most of the entries tt deals with are entered directly as done items. You can
also enter done items to add a daily summary of what you did. See
[wj](http://tylerneylon.com/a/wj/) for an explanation of such system.

    tt done "+day Finished painting the deck"

## Time tracking

Clock in and out of projects, this generates lines in your todo.txt file:

    tt in my-project
    tt out "Frobnicated the macguffin"

Comments after `tt in [project]` and `tt out` are optional.

Tt can convert its clock entries into [Emacs
timeclock.el](http://hledger.org/timeclock.html) compatible format understood
by [hledger](http://hledger.org/) with the `timeclock` command.

You can then use hledger to analyze your hours and generate reports:

    tt timeclock | hledger -f - balance

Use tt to show the name of the currently clocked project and the hours clocked
on it today:

    tt current

This is useful for status displays like a custom command prompt or a desktop
status bar to have your clock situation visible at a glance.

## Goal tracking

You can use tt to track goals that you work towards cumulatively. Write
entries using the GOAL tag as follows:

    x 2017-10-01 GOAL run 50 km due:2017-12-01  Half-marathon training

This sets the start date (2017-10-01), the end date (2017-12-01), the goal
project name (run) and the target value with an optional unit (50 km).

You can then log contributions to the goal with subsequent datapoint entries:

    x 2017-10-05 run 4 km  Trying out new shoes.

If the `GOAL` target specified an unit, the datapoint must have the same unit.
Goals can also be specified without a unit for items that are simple counts.
For these items, datapoints can omit the number and have an implicit count of
1:

    x 2017-07-01 GOAL floss 30 due:2017-08-01
    x 2017-07-02 floss

The goal and datapoint entries can have trailing comments, these are ignored
when parsing the goal.

Some goals track a measurement instead of accumulation. You can use =
(always whitespace separated) to declare an absolute datapoint and set the
goal value to a specific number instead of adding to the existing value:

    x 2017-03-01 GOAL weight 72 kg due:2017-06-01
    x 2017-04-02 weight = 76.5 kg

(If you regularly sort the files in your done file, you probably only want to
have one absolute datapoint per day. Sorting will not preserve the order of
the datapoints entered during the same day.)

You can view ongoing goals and recently completed ones with

    tt goals

This will list your ongoing goals, how far along to completion they are and
whether you're ahead or behind your expected schedule of steadily completing
them.
