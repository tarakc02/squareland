In these simulations, events are laid out unevenly on a 5x5 grid of cells, and reports of events are also biased by location, as observation probability varies based on which clump of events are being sampled from. I fit various MSE models, with increasing amounts of information about the spatial structure of the events. I'd like to see:

- whether models that do not explicitly account for the spatial structure can still result in decent estimates of the number of unobserved events
- if a model will result in biased estimates of the unobserved totals, what tests of model fitness (without access to the ground truth data) will reveal the shortcomings of the model?
- some cells have too few observations to provide estimates for -- but should we still use those cells in modeling, since they provide additional information about adjacent, more densely populated, cells?
