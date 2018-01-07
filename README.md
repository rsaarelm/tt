Time Tracking tool

Use [todo.txt](https://github.com/todotxt/todo.txt) in `~/todo.txt` and
`~/done.txt`.

Clock in and out of projects, this generates lines in your todo.txt file:

    tt in my-project
    tt out Frobnicated the macguffin

Using hledger to analyze summary:

    tt timeclock | hledger -f - balance
