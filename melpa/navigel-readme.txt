This library makes it simpler for Emacs Lisp developers to define
user-interfaces based on tablists (also known as tabulated-lists).
Overriding a few (CL) methods and calling `navigel-open' is all
that's required to get a nice UI to navigate your domain objects
(e.g., files, music library, database).

Features include :

- pressing RET to open the entity at point in another buffer;
- pressing ^ to open the current entity's parent;
- marking entities for bulk operations (e.g., delete);
- `imenu' support for quick navigation;
