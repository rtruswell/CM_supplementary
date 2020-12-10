# CM_supplementary

## Rob Truswell, University of Edinburgh

This repository contains corpus queries for investigating word order in Old and Middle English texts, and R scripts for producing figures based on the outputs of those corpus queries. The queries are designed to be used in conjunction with the [York-Toronto-Helsinki Parsed Corpus of Old English Prose (YCOE)](https://www-users.york.ac.uk/~lang22/YCOE/YcoeHome.htm), the [Penn-Helsinki Parsed Corpus of Middle English, 2nd edition (PPCME2)](https://www.ling.upenn.edu/hist-corpora/PPCME2-RELEASE-4/), and the [Parsed Linguistic Atlas of Early Middle English (PLAEME)](https://github.com/rtruswell/PLAEME_current).  I distribute the query files, rather than the results of those queries, because not all the corpora have licenses which would permit distribution of the results.

Included in this repository are:

* Competition_mat_OE_full.c, Competition_sub_OE_full.c : coding query files for use with YCOE
* Competition_mat_PPCME_full.c, Competition_sub_PPCME_full.c : coding query files for use with PPCME2
* Competition_mat_PLAEME_full.c, Competition_sub_PLAEME_full.c, Competition_mat_PLAEME_full_v3.c, Competition_sub_PLAEME_full_v3.c, V2.c : coding query files for use with PLAEME
* WhRel.def : Definitions file referred to by the coding queries
* OoosIds.q : generic query file for extracting codes and IDs from the output of coding queries
* CM_maps_final.R, Competition_maps_final.R, Competition_plots_final.R : R scripts for generating maps and figures.
* PLAEME_more_info.csv : metadata for PLAEME texts.
* CM_grammar_comparison.csv : CSV file created by manual triage + summarization of the first six coding queries.  It would be desirable, and possible in principle to automate the manual triage, but this research was performed in lockdown over a flaky SSH connection, and it wasn't practical under those circumstances.

.c and .q files should be run using [CorpusSearch](http://corpussearch.sourceforge.net).

Workflow is as follows (assumes all files in the same directory):

1. Run coding queries on relevant corpora.
2. Run OoosIds.q on .cod files output by coding queries.
3. Perform minor edits on .cod.ooo files output by OoosIds.q (globally replace `@` symbol with `:`; globally delete token IDs while retaining text IDs &mdash; for YCOE and PPCME2 queries this involves globally deleting the regex string `,.*$`; for PLAEME queries, delete `\..*$`).
4. Run R scripts (scripts assume that .cod.ooo files are accessible in the working directory).

NB the outputs of the first six coding queries are not called by any R script.  I have included these queries because they are the basis for the summary counts in `CM_grammar_comparison.csv`.
