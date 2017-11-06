
# Version 0.8.2


## Test environments
* local Debian "stretch" install, R 3.4.1
* local MacOS install, R 3.4.2
* win-builder 

## R CMD check results

0 errors | 0 warnings | 0 note

## Reverse dependencies

There are no reverse dependencies.

---

## Round 1

Error on incoming checks. 

```
* using log directory 'd:/RCompile/CRANincoming/R-devel/queuecomputer.Rcheck'
* using R Under development (unstable) (2017-09-12 r73242)
* using platform: x86_64-w64-mingw32 (64-bit)
* using session charset: ISO8859-1
* checking for file 'queuecomputer/DESCRIPTION' ... OK
* this is package 'queuecomputer' version '0.8.2'
* package encoding: UTF-8
* checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
Maintainer: 'Anthony Ebert <anthonyebert+CRAN@gmail.com>'
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking whether package 'queuecomputer' can be installed ... ERROR
Installation failed.
See 'd:/RCompile/CRANincoming/R-devel/queuecomputer.Rcheck/00install.out' for details.
* DONE
Status: 1 ERROR
```

The tail of `00install.out` is shown below. 

```
...
d:/Compiler/gcc-4.9.3/mingw_64/bin/g++ -m64 -shared -s -static-libgcc -o queuecomputer.dll tmp.def RcppExports.o init.o loops.o -Ld:/Compiler/gcc-4.9.3/local330/lib/x64 -Ld:/Compiler/gcc-4.9.3/local330/lib -LD:/RCompile/recent/R/bin/x64 -lR
installing to d:/RCompile/CRANincoming/R-devel/lib/queuecomputer/libs/x64
** R
** inst
** preparing package for lazy loading
Warning: S3 methods '[.fun_list', '[.grouped_df', 'all.equal.tbl_df', 'anti_join.data.frame', 'anti_join.tbl_df', 'arrange.data.frame', 'arrange.default', 'arrange.grouped_df', 'arrange.tbl_df', 'arrange_.data.frame', 'arrange_.tbl_df', 'as.data.frame.grouped_df', 'as.data.frame.rowwise_df', 'as.data.frame.tbl_cube', 'as.data.frame.tbl_df', 'as.table.tbl_cube', 'as.tbl.data.frame', 'as.tbl.tbl', 'as.tbl_cube.array', 'as.tbl_cube.data.frame', 'as.tbl_cube.matrix', 'as.tbl_cube.table', 'as_data_frame.grouped_df', 'as_data_frame.tbl_cube', 'auto_copy.tbl_cube', 'auto_copy.tbl_df', 'cbind.grouped_df', 'collapse.data.frame', 'collect.data.frame', 'common_by.NULL', 'common_by.character', 'common_by.default', 'common_by.list', 'compute.data.frame', 'copy_to.DBIConnection', 'copy_to.src_local', 'default_missing.data.frame', 'default_missing.default', 'dim.tbl_cube', 'distinct.data.frame', 'distinct.default', 'distinct.grouped_df', 'distinct.tbl_df', 'distinct_.data.frame', 'distinct_.grouped_df', 'dist [... truncated]
Error in library.dynam(lib, package, package.lib) : 
  DLL 'dplyr' not found: maybe not installed for this architecture?
ERROR: lazy loading failed for package 'queuecomputer'
* removing 'd:/RCompile/CRANincoming/R-devel/lib/queuecomputer'
```


This is strange because it passed all the win_builder checks ("R-devel" and "R-release"). Previous warnings on other packages like cvequality-0.1.1 went away with no change to the source code. 

See also https://github.com/ropensci/git2r/issues/277 

I checked the diff between this version and 0.8.1 . There is no change which could cause a problem -- I think. 

For these reasons I've decided to resubmit to CRAN with no change to the source code. Hopefully it works! 


