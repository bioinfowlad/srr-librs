# srr-collect
#
# downloads and collects records for systematic and other reviews
# with authors from Anschutz from 2014-2018
#
# script is part of:
# A Bibliographic Analysis of librarian assistance on SRs at CUAnschutz
# (continuation of Craven/Palmer/Piper project)


# Initialize
library(tidyverse)
library(magrittr)
library(easyPubMed)
library(rwos)
library(fuzzyjoin)
library(RefManageR)
library(readxl)

## Hedges (Search Query Elements)
# PubMed
SR_pubmed_hedge <- '((((Meta-Analysis[ptyp] OR meta-analysis[ti] OR "meta analysis”[ti] OR metaanalysis[ti] OR meta-synthesis[ti] OR metasynthesis[ti] OR “meta synthesis”[ti] OR umbrella review[tiab] OR living review[tiab] OR integrative review[ti] OR scoping review[tiab] OR rapid review[ti] OR qualitative evidence synthesis[ti] OR integrative research review[tiab]) OR (((systematic review[ti] OR systematic literature review[ti] OR systematic scoping review[ti] OR systematic narrative review[ti] OR systematic qualitative review[ti] OR systematic evidence review[ti] OR systematic quantitative review[ti] OR systematic meta-review[ti] OR systematic critical review[ti] OR systematic mixed studies review[ti] OR systematic mapping review[ti] OR systematic cochrane review[ti] OR systematic search and review[ti] OR systematic integrative review[ti]) NOT comment[pt] NOT (protocol[ti] OR protocols[ti])) NOT MEDLINE [subset]) OR (Cochrane Database Syst Rev[ta] AND review[pt]) OR systematic review[pt]))) NOT (((genome wide meta analysis[tiab] OR individual patient data meta analysis[ti] OR epigenome wide meta analysis[ti] OR genome wide association study[ti] OR individual patient data metaanalysis[ti] OR genome wide association studies[tiab] OR genome wide analysis[ti] OR genome wide association meta analysis[ti] OR genome wide association metaanalysis[ti] OR genomewide association meta analysis[ti])))'
AMC_pubmed_hedge <- '((80045[ad]) OR (80218[ad]) OR (80206[ad]) OR (80220[ad]) OR (80262[ad]) OR (ucdenver[ad] AND (Aurora or Anschutz)) OR  (uchealth[ad] AND (Aurora or Anschutz)) OR ("University of Colorado Health Sciences Center"[ad]) OR ("University of Colorado"[ad] AND (Medicine[ad] OR SOM[ad])) OR ("University of Colorado"[ad] AND ("College of Nursing"[ad] OR "school of nursing"[ad] OR CON[ad] or SON[ad])) OR ("University of Colorado"[ad] AND "Public Health"[ad]) OR ("University of Colorado"[ad] AND "Dental Medicine"[ad]) OR ("University of Colorado"[ad] AND Pharmacy[ad]) OR (Colorado[ad] AND Denver[ad] AND AMC[ad]) OR (Colorado[ad] AND Denver[ad] AND Anschutz[ad]) OR (Colorado[ad] AND Denver[ad] AND Aurora[ad]) OR ("University of Colorado AMC"[ad]) OR (CU[ad] AND Anschutz[ad]) OR (CU[ad] AND Aurora[ad]) OR (CU[ad] AND AMC[ad]) OR  ("University of Colorado Denver" AND Aurora[ad]) OR  (Fitzsimons[ad]) OR ("University of Colorado Denver"[ad] AND Aurora[ad]) OR ("University of Colorado Hospital"[ad]) OR ("University of Colorado Health"[ad]) OR ("UC Health"[ad]) OR  ("Children\'s Hospital"[ad] AND Aurora[ad]) OR ("Childrens Hospital"[ad] AND Aurora[ad]) OR ("The Children\'s Hospital"[ad] AND Aurora[ad]) OR ("Veterans Affairs"[ad] AND (Denver[ad] OR Aurora[ad])) OR  (VAMC[ad] AND (Denver[ad] OR Aurora[ad])) OR ("VA Eastern" AND Denver[ad]) OR  ("National Jewish"[ad]) OR (NJH[ad] AND Denver[ad]) OR ("Denver Health"[ad]))'
TIME_pubmed_hedge <- '2014:2018[CRDT]'

# Web of Science
SR_wos_hedge <- 'TI=(“Meta-Analysis” OR “meta analysis” OR metaanalysis OR “meta-synthesis” OR “meta synthesis” OR metasynthesis OR “umbrella review” OR “living review” OR “integrative review” OR “scoping review” OR “rapid review” OR “qualitative evidence synthesis” OR “integrative research review”) OR AB=(“umbrella review” OR “living review” OR “scoping review” OR “integrative research review”) OR (TS=(“systematic search”) and TI=(review)) OR TI=(“systematic review” OR “systematic literature review” OR “systematic scoping review” OR “systematic narrative review” OR “systematic qualitative review” OR “systematic evidence review” OR “systematic quantitative review” OR “systematic meta-review” OR “systematic critical review” OR “systematic mixed studies review” OR “systematic mapping review” OR “systematic cochrane review” OR “systematic integrative review”) OR SO=(COCHRANE DATABASE OF SYSTEMATIC REVIEWS) NOT (TI=(protocol OR protocols OR “genome wide meta analysis” OR “individual patient data meta analysis" OR “epigenome wide meta analysis” OR “genome wide association study” OR “individual patient data metaanalysis” OR “genome wide association studies” OR “genome wide analysis” OR “genome wide association meta analysis” OR “genome wide association metaanalysis” OR “genomewide association meta analysis”) OR AB=(“genome wide meta analysis” OR “genome wide association studies”))'
AMC_wos_hedge <- '((ZP=(80045 OR 80218 OR 80206 OR 80220 OR 80262 OR ucdenver) AND AD=(Aurora OR Anschutz)) OR (AD=uchealth AND AD=(Aurora or Anschutz)) OR OG=”University of Colorado Health Science Center” OR (OG=”University of Colorado System” AND AD=(Medicine OR SOM)) OR (AD=University Colorado AND AD=(medicine or SOM)) OR (OG=”University of Colorado System” AND AD=(College Nursing OR School nursing OR CON OR SON OR “Coll Nursing”)) OR (AD=University Colorado AND AD=(College Nursing OR school nursing OR CON OR SON OR Coll Nursing)) OR (OG=”University of Colorado System” AND (AD=(”Public Health” OR “Publ Hlth”) OR OG=”Colorado School of Public Health”)) OR (AD=University Colorado AND (AD=(Public Health OR Publ Hlth) OR OG= Colorado School of Public Health)) OR (OG=”University of Colorado System” AND AD=(”dental medicine” OR “dent med”)) OR (AD=University Colorado AND AD=(dental medicine OR dent med)) OR (OG=”University of Colorado System” AND AD=(pharmacy OR “Sch Pharm”)) OR (AD=University Colorado AND AD=(pharmacy OR Sch Pharm)) OR AD=(Colorado AND Denver AND AMC) OR AD=(Colorado AND Denver AND Anschutz) OR AD=(Colorado AND Denver AND Aurora) OR AD=University Colorado AMC OR AD=(CU AND Anschutz) OR AD=(CU AND Aurora) OR AD=(CU AND AMC) OR (OG=University of Colorado Denver AND AD=Aurora) OR AD=Fitzsimons OR (OG=University of Colorado Hospital OR AD=University Colorado Hospital) OR AD=(University Colorado Health OR Univ Colorado Hlth OR UC Health) OR (OG=Children’s Hospital Colorado) OR (AD=(Children’s Hospital OR Childrens Hospital OR Childrens Hosp Colorado) AND AD=Aurora) OR (AD=(Veterans Affairs OR VAMC OR VA Eastern OR Vet Affairs) AND AD=(Denver OR Aurora)) OR OG=Veterans Affairs Medical Center – Denver OR AD=NATL JEWISH HLTH UNIV COLORADO DENVER OR (AD=(National Jewish OR NJH) AND AD=Denver) OR OG=(Denver Health Medical Center) OR AD=Denver Health)'
TIME_wos_hedge <- 'PY=(2014-2018)'

## Search Queries
# Pubmed
pubmed_query <- paste("(",AMC_pubmed_hedge,") AND (",SR_pubmed_hedge,") AND (",TIME_pubmed_hedge,")")

# Web of Science
wos_query <- paste("(",AMC_wos_hedge,") AND (",SR_wos_hedge,") AND (",TIME_wos_hedge,")")

## Get Records
# Pubmed (using easypubmed package)
pubmed_query_results <- get_pubmed_ids(pubmed_query)
pubmed_query_records <- fetch_pubmed_data(pubmed_query_results)
pubmed_query_list <- articles_to_list(pubmed_query_records)
pubmed_records <- list()
for (y in pubmed_query_list) { pubmed_records[[(1+length(pubmed_records))]] <- y }
pubmed_table <- do.call(rbind,lapply(pubmed_records,article_to_df,max_chars=-1))
write.csv(pubmed_table,file="datapull_pubmed.csv",row.names=FALSE) # save to not have to regenerate all the time

# Web of Science (using wosr package -- Lite API only)
sid <- wos_authenticate(username = "colorado_HG", password = "Welcome$287")
woslite_results <- wos_search(sid, wos_query)
woslite_records <- wos_retrieve_all(woslite_results) 
write.csv(woslite_records,file="datapull_woslite.csv",row.names=FALSE) # save to not have to regenerate all the time

## If skipping the downloading part, run these before running the rest of the code below...
# pubmed_table <- read.csv("datapull_pubmed.csv")
# woslite_records <- read.csv("datapull_woslite.csv")

## Wrangle downloaded data
# Make tables with one row per paper and filter out wos records that are in pubmed
pubmed_papers <- unique(select(pubmed_table,one_of(c("pmid","doi","title","year","journal"))))
woslite_papers <- select(woslite_records,one_of(c("uid","doi","title","year","journal")))
pubmed_papers %<>% mutate(doi=as.character(doi),pmid,uid="",title=as.character(title),year,journal=as.character(journal))
pubmed_papers <- pubmed_papers[,c(2,1,6,3,4,5)] # reorder columns
woslite_papers %<>% mutate(doi=as.character(doi),pmid="",uid=as.character(uid),title=as.character(title),year,journal=as.character(journal))
woslite_papers <- woslite_papers[,c(2,6,1,3,4,5)] # reorder columns
pubmed_papers %<>% mutate(doi=toupper(doi),title=toupper(title)) # to make matching easier
woslite_papers %<>% mutate(doi=toupper(doi),title=toupper(title))  # to make matching easier

## Deduping
# Match by doi
doimatch <- inner_join(select(pubmed_papers,-uid),select(woslite_papers,-pmid),by="doi")
# Get not matching doi papers
ante <- anti_join(select(pubmed_papers,-uid),select(woslite_papers,-pmid),by="doi")
ante2 <- anti_join(select(woslite_papers,-pmid),select(pubmed_papers,-uid),by="doi")
# Match by title
titlematch <- stringdist_inner_join(ante,ante2,by="title")
# Get not matching titles
antetitle <- stringdist_anti_join(ante,ante2,by="title")
antetitle2 <- stringdist_anti_join(ante2,ante,by="title")
notitlenodoimatch <- full_join(antetitle,antetitle2,by=c("title","doi","year","journal"))
# Combine all unique papers
doimatch %<>% select(doi,pmid,uid,title=title.x,year=year.x,journal=journal.x)
titlematch %<>% select(doi=doi.x,pmid,uid,title=title.x,year=year.x,journal=journal.x)
notitlenodoimatch %<>% select(doi,pmid,uid,title,year,journal)
raw_papers_table <- bind_rows(doimatch,titlematch,notitlenodoimatch)
write.csv(raw_papers_table,"raw_papers_table.csv")
# Create queries to get RIS files from pubmed and wos
pmidvector <- raw_papers_table$pmid
ris_pubmed <- paste(pmidvector,collapse=" ") # copy and paste into Pubmed to get RIS file
onlywos <- raw_papers_table %>% select(pmid,uid) %>% filter(is.na(pmid)) %>% select(uid)
ris_wos <- gsub("\", \""," OR ",toString(onlywos))
ris_wos2 <- gsub("\", \n\""," OR ",ris_wos) # copy and paste into Web of Science to get CIW file

## RIS files were imported by CP into Endnote to manually curate and eliminate false positives
## (see her notes) -- true positives go from 499 to 371 and CP exported them from Endnote as a bib file
##
# Import and tidy list from Endnote:
truepos_table <- as.data.frame(ReadBib("FinalSRProjectSet(326)_10032019.bib"))

# List relevant journals/years to get JIF info from elsewhere
truepos_journals <- select(truepos_table, journal,year) %>%
    mutate(journal=toupper(journal), year=as.numeric(year)) %>%
    distinct() %>%
    arrange(journal,year)

# Import and tidy JIF data from previous poster (Craven/Palmer/Piper)
previous_srr_final <- read_excel("SRR_Final.xlsx")
previous_srr_librarians <- read_excel("SRR_Librarians.xlsx",col_names=c("Author","Year","Title","Journal","JIF","Notes","JIFx"))

previous_srr_final %<>% select(one_of(c("Journal","Year","JIF")))
previous_srr_librarians %<>% select(one_of(c("Journal","Year","JIF")))

previous_srr_final %<>% rename(journal=Journal,year=Year) %>%
    mutate(journal=toupper(journal),JIF=as.numeric(JIF))

previous_srr_librarians %<>% rename(journal=Journal,year=Year) %>%
    mutate(journal=toupper(journal))

previous_srr <- bind_rows(previous_srr_final,previous_srr_librarians) %>%
    arrange(journal,year) %>%
    distinct() %>%
    filter(is.na(JIF)==FALSE) %>%
    filter(is.na(journal)==FALSE) %>%
    filter(is.na(year)==FALSE)

# Import previous JIF data    
# (comment out the line below if re-running code)
truepos_journals_jifs <- left_join(truepos_journals,previous_srr,by=c("journal","year"))

# Export joined data into a spreadsheet to fill the rest of the JIFs manually as we
# don't have access to InCites API
# (comment out the line below is re-running code)
write_csv(truepos_journals_jifs,"truepos_journals_jifs.csv")
    
# Re-import table with journals,jifs,maxRank after manually entering
truepos_journals_jifs <- read.csv("truepos_journals_jifs.csv")

## Start combining data after manual curation
papers_table <- select(truepos_table,url,author,title,journal,year,doi)
papers_table %<>% mutate(journal=toupper(journal),year=as.numeric(year))
truepos_journals_jifs %<>% mutate(journal=as.character(journal))
papers_table <- left_join(papers_table,truepos_journals_jifs,by=c("journal","year"))    
# Extract identifier from URL
m <- regexpr("pubmed/[0-9]+|WOS:[0-9]+",papers_table$url)
papers_table <- mutate(papers_table,id=regmatches(url,m))
# Output DOIs to get citation data from WoS/AMR
write.csv(data.frame(papers_table$doi),"sr-dois.csv")
# See https://github.com/Clarivate-SAR/wos-amr for citations script
citations <- read.csv("sr-citation-results.csv") %>% select(doi,times.cited) %>% mutate(doi=as.character(doi))
papers_table <- left_join(papers_table,citations,by="doi") # save a copy for testing
# Import data as categorized by CP (bibtex exports from Endnote)
lib_coauthor <- as.data.frame(ReadBib("lib-coauthor.txt"))
lib_intext <- as.data.frame(ReadBib("lib-intext.txt"))
nolibs <- as.data.frame(ReadBib("nolibs.txt"))
# Matching by title
papers_table$coauthor[papers_table$title %in% lib_coauthor$title] <- TRUE
papers_table$intext[papers_table$title %in% lib_intext$title] <- TRUE
libassist <- bind_rows(lib_coauthor,lib_intext)
papers_table$librarian[papers_table$title %in% libassist$title] <- TRUE
papers_table$coauthor[papers_table$title %in% nolibs$title] <- FALSE
papers_table$intext[papers_table$title %in% nolibs$title] <- FALSE
papers_table$librarian[papers_table$title %in% nolibs$title] <- FALSE

