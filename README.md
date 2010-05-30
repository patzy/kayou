Dependencies
------------

You'll need:

 - [glaw](http://github.com/patzy/glaw)
 - [glop](http://github.com/patzy/glop)
 - [imago](http://common-lisp.net/project/imago/)
 
and all their dependencies. 
 
Getting started
---------------

Not enough space on github for game data so get them
[here](http://appart.kicks-ass.net/patzy/files/kayou_data.tar.bz2),
uncompress in the `kayou/` directory.

Then get a lisp with asdf. Get all dependencies and:

    (asdf:operate 'asdf:load-op :kayou)
    (kayou:run)
    
Use arrow to move the ship and space to fire.
