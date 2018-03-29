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

## Installation

Install the [Haskell Tool Stack](https://www.haskellstack.org/) to build and
install tt:

    cd tt
    stack build
    stack install

## TODO items

Tt is not a complete todo application. You are assumed to use a text editor or
a dedicated todo.txt tool like
[Simpletask](https://github.com/mpcjanssen/simpletask-android) to maintain
your todo list. However, tt does provide you with `todo` and `done` commands
to quickly add a new open or completed items to your `todo.txt` file from the
command line.

    tt todo "Buy milk @shop"

Most of the entries tt deals with are entered directly as done items. You can
also enter done items to add a daily summary of what you did. See
[wj](http://tylerneylon.com/a/wj/) for an explanation of such system.

    tt done "+day Finished painting the deck"

You can add a "^" to the start of the message to backdate it to yesterday.

    tt done "^run 4 km  -- Late night run"

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

Again, you are not expected to use tt as your only application when managing
your lists. If you make mistakes filing clock items, there are no tt commands
to change existing entries and you need to use a text editor and correct the
entries by hand in your `todo.txt` file. The reason tt files even the "done"
entries in `todo.txt` instead of `done.txt` is that this allows you to use the
`done.txt` file as a frozen append-only storage that you file done items from
todo.txt in once you're happy with them.

There are two internal syntaxes for tracked time. The `tt in` and `tt out`
commands generate start and end entry pairs:

    x 2018-01-05 10:00:00+0200 s project
    x 2018-01-05 10:30:00+0200 e Did some work on project

You can also use a single line in the goal tracker datapoint style with either
`min` or `h` as the unit:

    x 2018-01-05 10:00 project 30 min  Did some work on project

There is no internal difference between the styles. You probably want to use
the `tt in` style when you clock into a task and aren't sure when you'll
stop, and the goal datapoint style when you're planning time blocking or
entering work hours done after the fact.

## Time blocking

You can plan your [future time
use](http://calnewport.com/blog/2013/12/21/deep-habits-the-importance-of-planning-every-minute-of-your-work-day/)
with open todo items that have both date and time and have a duration unit.

    2018-01-10 09:00 block-project 2 h  -- Plan to work on block-project for two hours
    2018-02-10 12:00 lunch-break 30 min  -- Time in minutes

An ongoing planned session will show up in `tt current`, but unless you mark
them up with the `x`, they won't show up in your log of completed work. The
assumption is that the plan is expected to change and you only mark the lines
done when you have completed that planned work block.

## Goal tracking

You can use tt to track goals that you work towards cumulatively. Write
entries using the GOAL tag as follows:

    x 2017-10-01 GOAL run 10 km  -- Half-marathon training

This sets the start date (2017-10-01), the goal project name (run) and the
target weekly goal with an optional unit (10 km). Then system will now expect
you to log an average of 10 km run every week from now on to keep the goal
from failing.

You can log contributions to the goal with subsequent datapoint entries:

    x 2017-10-05 run 4 km  Trying out new shoes.

If the `GOAL` target specified an unit, the datapoint must have the same unit.
Goals can also be specified without a unit for items that are simple counts.
For these items, datapoints can omit the number and have an implicit count of
1:

    x 2017-07-01 GOAL floss 6
    x 2017-07-02 floss

The goal and datapoint entries can have trailing comments, these are ignored
when parsing the goal.

Some goals track a measurement instead of accumulation. You can use =
(always whitespace separated) to declare an absolute datapoint and set the
goal value to a specific number instead of adding to the existing value:

    x 2017-03-01 GOAL weight -0.2 kg
    x 2017-04-02 weight = 76.5 kg

If you have multiple absolute datapoints for the same day in your file, you
need to make sure sorting the lines of the file won't rearrange their order.
A simple way to ensure this is to add times of the day to the datapoints:

    x 2017-03-02 08:00 falling-goal = 7
    x 2017-03-02 10:00 falling-goal = 6

If you want to give up on a goal, use the DROP GOAL directive.

    x 2017-11-02 DROP GOAL run  -- Broken leg

You can view ongoing goals and recently completed ones with

    tt goals

This will list your ongoing goals and whether you're ahead or behind your
expected schedule of steadily completing them.

## Examples

The actual [done.txt](examples/done.txt) from developing this project is
included as an example for what the result of a todo.txt and timeclock
workflow looks like.

## Bugs

Time tracking currently throws away time zone information, so clock in / clock
out pairs that change time zone offset will result in wrong interval sizes and
may cause parse errors if the local time of the clock out is earlier than the
local time of the clock in. This will happen if the work session spans a
daylight saving time transition.
