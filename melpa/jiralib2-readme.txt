This file provides a programatic interface to JIRA.  It provides access to
JIRA from other programs, but no user level functionality.

jiralib2.el uses cookie authentication instead of basic auth for performance
reasons. JIRA API has an artificial delay of ~second in basic auth queries.
The session cookie is stored in an Emacs global variable, and it is
automatically used in each query. If the user has not logged in, or the
session has expired, a new login is performed and the password queried from
the user. jiralib2 DOES NOT store user's password anywhere like jiralib did.
Only the session token is saved, and user credentials cannot be extracted
from it.

Jira References:

Primary reference (on current Jira, only REST is supported):
https://docs.atlassian.com/jira/REST/cloud/
