Comonad-Transformers Demo
=========================

    > ./conway-demo
     #     
      #    
    ###    
             
             
    (animates forever, press Ctrl-C to stop)

If a comonad is an environment for cellular automata, then a comonad transformer adds an axis to that environment.

In this demo, we first construct a comonad representing a 1D neighborhood. We then write a slightly more complicated version which adds an extra 1D neighborhood to an existing comonad: that is our comonad transformer. If we apply the 1D neighborhood transformer to another 1D neighborhood, we obtain a 2D neighborhood.

We end the demo by implementing Conway's Game of Life on top of this 2D grid. The comonadic nature of the grid makes it very easy to specify the rules of the automata, because the code is run uniformly on each cell, and because the indexing is always relative to the current cell. Those two features are central to comonads.

Our particular version happens to use numerical indices which wrap around past the edge of the grid, but that is in no way typical of comonads.
