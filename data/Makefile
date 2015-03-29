#
# data - for the trial, remember that github has 100MiB limits
#   on file size so we do *NOT* want to commit any raw files
#   
# as an example KalkPvAvailP.1s is 155M whilst the bzip2
# version is 636K
#   

%.1s: %.1s.bz2
	bunzip2 --keep $<
