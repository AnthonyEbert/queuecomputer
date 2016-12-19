

# queuecomputer 0.6.0.9002

* Fixed serious bug where servers from previous epoch were serving customers who had just arrived. 

# queuecomputer 0.6.0.9001

* Changed method of order function to radix. Improved speed. 

# queuecomputer 0.5.1.9002

* Added a check within queue_step for is.unsorted so that time isn't wasted sorting arrival times which are already sorted. This can improve speed by 1 order of magnitude (important!). 
* Changed CRAN URL in github badges in README to canonical form. 

# queuecomputer 0.5.1.9001

* Added extra stopifnot conditions in queue_step function. 
* Added URL to DESCRIPTION

# queuecomputer 0.5.1.9000

* Minor edits in readme
* Added a `NEWS.md` file to track changes to the package.

# queuecomputer 0.5.1

* Released to CRAN
