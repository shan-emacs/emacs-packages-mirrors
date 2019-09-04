
Start hacking time with

    {M-x hack-time-mode RET}

Then Choose a date.  E.g. enter

  - "+" for tomorrow 11:55AM.  11:55AM has been chosen as default.
  - "-1" for yesterday.  Time gets set to 11:55AM.
  - "-10 12:05" for 10 days ago at 12:05 PM"

After this action the current time is frozen to the chosen time.

Watch out for 'HACK-TIME-MODE' in the modeline which indicates that
hack-time-mode is on.
Turn the mode off with

    {M-x hack-time-mode RET}

again.  The time is back to normal.

* Use cases

- Mark Org-todo-items done at another date.
- View the Org agenda as if it was tomorrow.
- Bulk scatter (keys BS in Org agenda) items starting at a certain
  date.

* Warning

Possibly some functionalities behave weird when hack-time-mode is on.

Watch out!

'hack-time-mode' has limitted control over time.  There are time
sources in Emacs which can _not_ be controlled by 'hack-time-mode'.

* Dependencies

'hack-time-mode' depends on function 'org-read-date' of
Orgmode.

* Vision

`hack-time-mode` gives the user full control about every aspect of
time in this universe and all parallel universes.
