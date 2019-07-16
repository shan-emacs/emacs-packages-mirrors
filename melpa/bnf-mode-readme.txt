  GNU Emacs major mode for editing BNF grammars.  Currently this mode
provides basic syntax and font-locking for BNF files.  BNF notation is
supported exactly form as it was first announced in the ALGOL 60 report.

When developing this mode, the following documents were taken into account:

- RFC822: Standard for ARPA Internet Text Messages
  (see URL `https://www.ietf.org/rfc/rfc822.txt')
- RFC5234: Augmented BNF for Syntax Specifications: ABNF
  (see URL `https://www.ietf.org/rfc/rfc5234.txt')
- FRC7405: Case-Sensitive String Support in ABNF
  (see URL `https://www.ietf.org/rfc/rfc7405.txt')
- Revised Report on the Algorithmic Language Algol 60
  (see URL `https://www.masswerk.at/algol60/report.htm')

Installation:

The recommended way is to use ELPA (see URL `https://elpa.gnu.org'),
MELPA (see URL `https://melpa.org') or MELPA Stable
(see URL `https://stable.melpa.org').  If either is in your
`package-archives', do:

  M-x package-install RET bnf-mode RET

To learn on how to use any other installation methods refer to relevant
documentation.

Usage:

To toggle the mode in the current buffer:

  M-x bnf-mode RET

By default any file that matches the glob *.bnf is automatically opened
in `bnf-mode'.

Customization:

To customize various options, use command as follows:

  M-x customize-group bnf RET

Bugs:

Bug tracking is currently handled using the GitHub issue tracker
(see URL `https://github.com/sergeyklay/bnf-mode/issues')

History:

History is tracked in the Git repository rather than in this file.
(see URL `https://github.com/sergeyklay/bnf-mode/blob/master/NEWS')
