cabal-base
==========

A skeleton from which you can start your own projects.


usage
-----

Suppose you want to create a project named "my-project".

First, clone the repository:

    git clone cabal-base my-project

Second, rename the .cabal file to use the name of your project, substituting any occurence of cabal-base with the name of your project.

    cat cabal-base.cabal | sed 's/cabal-base/my-project/g' > my-project.cabal
    git rm cabal-base.cabal

You might also want to edit the description, the category, and the hierarchical module in which your source files are located.

Finally, edit this README file to describe your project, not this skeleton!
