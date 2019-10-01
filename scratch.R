# Scratch Code Pad for:
# A Bibliographic Analysis of librarian assistance on SRs at CUAnschutz
# (continuation of Craven/Palmer/Piper project)
# 
# Using the easyPubMed package here. Look at the following documentation:
# http://www.biotechworld.it/bioinf/2016/01/05/querying-pubmed-via-the-easypubmed-package-in-r/
# http://www.biotechworld.it/bioinf/2016/01/21/scraping-pubmed-data-via-easypubmed-xml-and-regex-in-r-for-a-targeting-campaign/
# https://www.data-pulse.com/projects/Rlibs/vignettes/easyPubMed_01_getting_started.html
# https://www.data-pulse.com/projects/Rlibs/vignettes/easyPubMed_02_advanced_tutorial.html
#
# See also maybe the bibliometrix package


library(tidyverse)
library(easyPubMed)
library(readxl)
library(tidyxl)

AMC_pubmed_hedge <- '((80045[ad]) OR (80218[ad]) OR (80206[ad]) OR (80220[ad]) OR (80262[ad]) OR (ucdenver[ad] AND (Aurora or Anschutz)) OR  (uchealth[ad] AND (Aurora or Anschutz)) OR ("University of Colorado Health Sciences Center"[ad]) OR ("University of Colorado"[ad] AND (Medicine[ad] OR SOM[ad])) OR ("University of Colorado"[ad] AND ("College of Nursing"[ad] OR "school of nursing"[ad] OR CON[ad] or SON[ad])) OR ("University of Colorado"[ad] AND "Public Health"[ad]) OR ("University of Colorado"[ad] AND "Dental Medicine"[ad]) OR ("University of Colorado"[ad] AND Pharmacy[ad]) OR (Colorado[ad] AND Denver[ad] AND AMC[ad]) OR (Colorado[ad] AND Denver[ad] AND Anschutz[ad]) OR (Colorado[ad] AND Denver[ad] AND Aurora[ad]) OR ("University of Colorado AMC"[ad]) OR (CU[ad] AND Anschutz[ad]) OR (CU[ad] AND Aurora[ad]) OR (CU[ad] AND AMC[ad]) OR  ("University of Colorado Denver" AND Aurora[ad]) OR  (Fitzsimons[ad]) OR ("University of Colorado Denver"[ad] AND Aurora[ad]) OR ("University of Colorado Hospital"[ad]) OR ("University of Colorado Health"[ad]) OR ("UC Health"[ad]) OR  ("Children\'s Hospital"[ad] AND Aurora[ad]) OR ("Childrens Hospital"[ad] AND Aurora[ad]) OR ("The Children\'s Hospital"[ad] AND Aurora[ad]) OR ("Veterans Affairs"[ad] AND (Denver[ad] OR Aurora[ad])) OR  (VAMC[ad] AND (Denver[ad] OR Aurora[ad])) OR ("VA Eastern" AND Denver[ad]) OR  ("National Jewish"[ad]) OR (NJH[ad] AND Denver[ad]) OR ("Denver Health"[ad]))'
SR_pubmed_hedge <- '(((systematic review[ti] OR systematic literature review[ti] OR systematic scoping review[ti] OR systematic narrative review[ti] OR systematic qualitative review[ti] OR systematic evidence review[ti] OR systematic quantitative review[ti] OR systematic meta-review[ti] OR systematic critical review[ti] OR systematic mixed studies review[ti] OR systematic mapping review[ti] OR systematic cochrane review[ti] OR systematic search and review[ti] OR systematic integrative review[ti]) NOT comment[pt] NOT (protocol[ti] OR protocols[ti])) NOT MEDLINE [subset]) OR (Cochrane Database Syst Rev[ta] AND review[pt]) OR systematic review[pt] '
MA_pubmed_hedge <- '(Meta-Analysis[ptyp] OR meta-analysis[ti] OR "meta analysis"[ti] OR metaanalysis[ti])'
TIME_pubmed_hedge <- '2014:2018[CRDT]'

query <- paste("(",AMC_pubmed_hedge,") AND ((",SR_pubmed_hedge,") OR (",MA_pubmed_hedge,")) AND (",TIME_pubmed_hedge,")")

query_results <- get_pubmed_ids(query)

query_records <- fetch_pubmed_data(query_results)

query_list <- articles_to_list(query_records)

rec <- list()

for (y in query_list) { rec[[(1+length(rec))]] <- y }

rectable <- do.call(rbind,lapply(rec,article_to_df,max_chars=-1))

### call for exporting csv missing

### Christi imported csv into Excel and color-coded records:
### Green: confirmed SR or lit-based MA
### Yellow: genome-wide or patient data MA
### Red: confirmed not SR/MA

pipertable <- read_excel("pubmed-SRMA-AMC-2014-2018.xlsx")
piperformats <- xlsx_formats("pubmed-SRMA-AMC-2014-2018.xlsx")



### for WoS try https://vt-arc.github.io/wosr/articles/getting-started.html
### for WoS try https://github.com/juba/rwos/
### for WoS try https://github.com/Clarivate-SAR/wos-amr

### Please use the following credentials to access the service:
### ID:  colorado_HG
### Password:  Welcome$287








# Deprecated
## Now for dealing with KP and HC spreadsheets
srr_librarians <- read_excel("SRR_Librarians.xlsx",col_names=c("authors","year","title","journal","jif","extra","extra1"))
srr_lib_table <- data.frame()
for (i in 1:nrow(srr_librarians)) {
    query <- paste(srr_librarians$title[i],"[ti]",sep="")
    entrez_id <- get_pubmed_ids(query)
    record <- fetch_pubmed_data(entrez_id)
    srr_lib_table <- rbind(srr_lib_table,article_to_df(record))
}
## For the full "non-lib" list of KP/HC
srr_table <- read_excel("SRR_Final.xlsx")
srr_table
srr_pubmed_table <-data.frame()
for (i in 1:nrow(srr_table)) {
    query <- paste(srr_table$Title[i],"[ti]",sep="")
    entrez_id <- get_pubmed_ids(query)
    record <- fetch_pubmed_data(entrez_id)
    srr_pubmed_table <- rbind(srr_pubmed_table,article_to_df(record))
}
