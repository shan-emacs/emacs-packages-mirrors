This package offers a development system for the Haxe programming language.
Haxe code completion is activated using the `company-mode' package.
Code navigation options like "go to definition" and "find all references" are available.
There is support for `eldoc' to show the type of current variable, the arguments of current function.
All of those features are triggered in `battle-haxe-mode' which also spawns a Haxe server to perform them.
The tools rely on the Haxe "compiler services" feature ( https://haxe.org/manual/cr-completion-overview.html ).
The main quirk is that the system has to force automatic saving of the Haxe code file buffer as you edit it.
If this is a problem for you, don't use the package.
See the project home page for more information.
