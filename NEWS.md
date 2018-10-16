
# queuecomputer 0.8.3

## New features

* Information about final state of servers is returned as `state` list element from queue_step output

## Notes

* adjust argument is removed
* updated `citation()` information
* Changed license from GPL (>= 2) to GPL-2 only  

# queuecomputer 0.8.2

## New features:

* Much faster queue lengths computation. This means `queue_step()` is faster. The speed of `queue()` is unaffected. 

## Bug fixes
* `queue_step` no longer errors if arrivals is an integer vector
* `summary.queue_list` now returns a warning rather than an error if a `server.list` object is inputted. 


# queuecomputer 0.8.1

## New features: 

* Much faster summary method
* Plotting methods for queues
* New function `create_batches` to easily create batch arrival systems. 

# queuecomputer 0.6.1

## New features:
* It's faster. We accomplish this by checking whether arrival times are already ordered (in which case we skip reordering!) and by using the `"radix"` method within the `order` function. 
* Internal `queue` function for high level users
* Customers can now miss the queue if they arrive too late. 

## Bug fixes:
* `queue_step` with `as.server.stepfun` now checks if departure times force move to next epoch. Previously (in some cases) new customers could be served by servers in previous epochs. 
* Added extra checks in `queue_step` and `as.server.stepfun` for correct input. 

# queuecomputer 0.5.1

* Released to CRAN
