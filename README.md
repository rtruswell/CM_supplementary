# CM_supplementary

## Rob Truswell, University of Edinburgh

This repository contains corpus queries for investigating word order in Old and Middle English texts. The queries are designed to be used in conjunction with the [York-Toronto-Helsinki Parsed Corpus of Old English Prose (YCOE)](https://www-users.york.ac.uk/~lang22/YCOE/YcoeHome.htm), the [Penn-Helsinki Parsed Corpus of Middle English, 2nd edition (PPCME2)](https://www.ling.upenn.edu/hist-corpora/PPCME2-RELEASE-4/), and the [Parsed Linguistic Atlas of Early Middle English (PLAEME)](https://github.com/rtruswell/PLAEME_current).  I distribute the query files, rather than the results of those queries, because not all the corpora have licenses which would permit distribution of the results.

Included in this repository are:

* Competition_mat_OE_full.c, Competition_sub_OE_full.c : coding query files for use with YCOE
* Competition_mat_PPCME_full.c, Competition_sub_PPCME_full.c : coding query files for use with PPCME2
* Competition_mat_PLAEME_full.c, Competition_sub_PLAEME_full.c : coding query files for use with PLAEME
* OoosIds.q : generic query file for extracting codes and IDs from the output of coding queries
* CM_maps.R, Competition_plots.R, CompetitionMapsNew.R

.c and .q files should be run using [CorpusSearch](http://corpussearch.sourceforge.net).

Workflow is as follows:

1. Run coding queries on relevant corpora.
2. Run OoosIds.q on .cod files output by coding queries.
3. Perform minor formatting edits on .cod.ooo files output by OoosIds.q (replace `@` symbol with `:`; delete `,` and everything following on each line).
4. Run R scripts (scripts assume that .cod.ooo files are accessible in the working directory.
