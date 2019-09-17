This file provides a programatic interface to JIRA.  It provides access to
JIRA from other programs, but no user level functionality.

jiralib2 supports three methods of authentication: cookie, basic and token.
Cookie auth is the same which is used in the browser, and works by
requesting a session id from the API. Basic auth works by including the
Authorization-header in all the requests. Token authentication is similar
to the basic authentication, but uses a server-side generated token instead
of the password, and is only available with JIRA Cloud.
OAuth login is not supported.

Jira References:

Primary reference (on current Jira, only REST is supported):
https://docs.atlassian.com/jira/REST/cloud/
