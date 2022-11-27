# Revision history for Search

## 0.1.0.0 -- 2021-05-28

* First version. Released on an unsuspecting world.

## 0.1.0.1 -- 20121-05-28

* Minor internal changes to relax bounds on bytestring. Enables building with ghc-8.10.2. 

## 0.1.1.0 -- 20121-06-05

* FileTree is now a record.
* Some internal changes to get it to build on somewhat older versions of ghc. Tested with 8.2.2 

## 0.1.1.1 -- 2021-06-06

* Removed the spurious dependency on containers.
* Loosened some bounds on the packages.

## 0.3 -- 2022-09-25

* Use a new chunked interface which gives much better code sharing and improves performance too.
* Various internal refactors. 
* A lot of documentation either outdated/doesn't exist as a result and needs to be revamped.
