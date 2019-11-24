Purpose:

 Maintain a small and changing subset of your org-nodes to visit with ease.

 The working-set is a small set of nodes, among which you can switch
 rapidly; it is expected to change on a daily or even hourly basis.  Put
 nodes into your working set in order to return easily after any
 interruption.

 Once you have added nodes to your working set, there are two ways to
 traverse them (both are accessible through the central function
 `org-working-set'): cycling through your working set is the quickest
 way to return to the current node or go to others; alternatively,
 invoking the working set menu allows for better control but may require
 more keystrokes.

 Please note, that org-working-set adds an id-property to all nodes in
 the working-set.

 The list of ids from the nodes of your working-set is stored within the
 property-drawer of a distinguished node specified via
 `org-working-set-id'; this can be any node you choose and is itself not
 part of the working-set.

 Remark: Depending on your needs you might also find these packages
 interesting for providing somewhat similar functionality: org-now and
 org-mru-clock.

Setup:

 - org-working-set can be installed with package.el
 - Invoke `org-working-set', it will explain and assist you to set the
   customizable variable `org-working-set-id'
