# srr-analyze
#
# plots and stats after collecting SR-AMC data
#
# script is part of:
# A Bibliographic Analysis of librarian assistance on SRs at CUAnschutz
# (continuation of Craven/Palmer/Piper project)

# Initialize
library(tidyverse)

# Read dataset
srr_dataset <- read.csv("combo_papers_table.csv")
srr_dataset <- mutate(srr_dataset,url=as.character(url),author=as.character(author),title=as.character(title),journal=as.character(journal),doi=as.character(doi),id=as.character(id))

# Look at histograms of the whole dataset
ggplot(data=srr_dataset,aes(JIF))+
    geom_histogram(col="blue",fill="blue",alpha=0.5)

ggplot(data=srr_dataset,aes(maxRank))+
    geom_histogram(col="blue",fill="blue",alpha=0.5)

ggplot(data=srr_dataset,aes(times.cited))+
    geom_histogram(col="blue",fill="blue",alpha=0.5)

# Compare these histograms among librarian versus non-librarian records

ggplot(data=(srr_dataset %>% filter(librarian==FALSE)),aes(JIF))+
    geom_histogram(col="blue",fill="blue",alpha=0.5)+
    geom_histogram(data=(srr_dataset %>% filter(librarian==TRUE)),aes(JIF),col="red",fill="red",alpha=0.5)

ggplot(data=(srr_dataset %>% filter(librarian==FALSE)),aes(maxRank))+
    geom_histogram(col="blue",fill="blue",alpha=0.5)+
    geom_histogram(data=(srr_dataset %>% filter(librarian==TRUE)),aes(maxRank),col="red",fill="red",alpha=0.5)

ggplot(data=(srr_dataset %>% filter(librarian==FALSE)),aes(times.cited))+
    geom_histogram(col="blue",fill="blue",alpha=0.5)+
    geom_histogram(data=(srr_dataset %>% filter(librarian==TRUE)),aes(times.cited),col="red",fill="red",alpha=0.5)

# Let's see if densities make any difference more visible

ggplot(data=(srr_dataset %>% filter(librarian != "NA")), aes(JIF, stat(density), colour = librarian)) +
    geom_freqpoly(na.rm=TRUE)

ggplot(data=(srr_dataset %>% filter(librarian != "NA")), aes(maxRank, stat(density), colour = librarian)) +
    geom_freqpoly(na.rm=TRUE)

ggplot(data=(srr_dataset %>% filter(librarian != "NA")), aes(times.cited, stat(density), colour = librarian)) +
    geom_freqpoly(na.rm=TRUE)

## Let's move away from histograms
# Violins
ggplot(srr_dataset,aes(factor(librarian),JIF)) + geom_violin(scale="count")

ggplot(srr_dataset,aes(factor(librarian),maxRank)) + geom_violin(scale="count")

ggplot(srr_dataset,aes(factor(librarian),times.cited)) + geom_violin(scale="count")

# Boxplots
ggplot(srr_dataset %>% filter(librarian != "NA"),aes(factor(librarian),JIF)) + geom_boxplot(outlier.shape=NA) + geom_jitter(width=0.15,alpha=0.5)

ggplot(srr_dataset %>% filter(librarian != "NA"),aes(factor(librarian),maxRank)) + geom_boxplot(outlier.shape=NA) + geom_jitter(width=0.15,alpha=0.5)

ggplot(srr_dataset %>% filter(librarian != "NA"),aes(factor(librarian),times.cited)) + geom_boxplot(outlier.shape=NA) + geom_jitter(width=0.15,alpha=0.5)

# Boxplots comparing nonlibs to coauthor libs
ggplot(srr_dataset %>% filter(librarian != "NA"),aes(factor(coauthor),JIF)) + geom_boxplot(outlier.shape=NA) + geom_jitter(width=0.15,alpha=0.5)

ggplot(srr_dataset %>% filter(librarian != "NA"),aes(factor(coauthor),maxRank)) + geom_boxplot(outlier.shape=NA) + geom_jitter(width=0.15,alpha=0.5)

ggplot(srr_dataset %>% filter(librarian != "NA"),aes(factor(coauthor),times.cited)) + geom_boxplot(outlier.shape=NA) + geom_jitter(width=0.15,alpha=0.5)

# Let me try a weird "normalization"
ggplot(srr_dataset %>% filter(librarian != "NA"),aes(factor(librarian),times.cited/JIF)) + geom_boxplot(outlier.shape=NA) + geom_jitter(width=0.15,alpha=0.5)

ggplot(srr_dataset %>% filter(librarian != "NA"),aes(factor(coauthor),times.cited/JIF)) + geom_boxplot(outlier.shape=NA) + geom_jitter(width=0.15,alpha=0.5)

# Let's do some t-tests (lib versus nonlib)
JIF_lib <- srr_dataset %>% filter(librarian=="TRUE") %>% select(JIF)
JIF_nonlib <- srr_dataset %>% filter(librarian=="FALSE") %>% select(JIF)
t.test(JIF_lib,JIF_nonlib) # p-value = 0.06408

RANK_lib <- srr_dataset %>% filter(librarian=="TRUE") %>% select(maxRank)
RANK_nonlib <- srr_dataset %>% filter(librarian=="FALSE") %>% select(maxRank)
t.test(RANK_lib,RANK_nonlib) # p-value = 0.00201

CIT_lib <- srr_dataset %>% filter(librarian=="TRUE") %>% select(times.cited)
CIT_nonlib <- srr_dataset %>% filter(librarian=="FALSE") %>% select(times.cited)
t.test(CIT_lib,CIT_nonlib) # p-value = 0.4686

# Let's do some t-tests (auth versus nonauth)
JIF_auth <- srr_dataset %>% filter(coauthor=="TRUE") %>% select(JIF)
JIF_nonauth <- srr_dataset %>% filter(coauthor=="FALSE") %>% select(JIF)
t.test(JIF_auth,JIF_nonauth) # p-value = 0.3563

RANK_auth <- srr_dataset %>% filter(coauthor=="TRUE") %>% select(maxRank)
RANK_nonauth <- srr_dataset %>% filter(coauthor=="FALSE") %>% select(maxRank)
t.test(RANK_auth,RANK_nonauth) # p-value = 0.1281

CIT_auth <- srr_dataset %>% filter(coauthor=="TRUE") %>% select(times.cited)
CIT_nonauth <- srr_dataset %>% filter(coauthor=="FALSE") %>% select(times.cited)
t.test(CIT_auth,CIT_nonauth) # p-value = 0.3153

#  Let's do some t-tests (auth versus nonlib)
t.test(JIF_auth,JIF_nonlib) # p-value = 0.2339
t.test(RANK_auth,RANK_nonlib) # p-value = 0.061
t.test(CIT_auth,CIT_nonlib) # p-value = 0.2991

# Wilcoxon
wilcox.test(as.numeric(unlist(JIF_lib)),as.numeric(unlist(JIF_nonlib))) # p-value = 0.038
wilcox.test(as.numeric(unlist(RANK_lib)),as.numeric(unlist(RANK_nonlib))) # p-value = 0.00458
wilcox.test(as.numeric(unlist(CIT_lib)),as.numeric(unlist(CIT_nonlib))) # p-value = 0.8387

wilcox.test(as.numeric(unlist(JIF_auth)),as.numeric(unlist(JIF_nonlib))) # p-value = 0.1323
wilcox.test(as.numeric(unlist(RANK_auth)),as.numeric(unlist(RANK_nonlib))) # p-value = 0.08859
wilcox.test(as.numeric(unlist(CIT_auth)),as.numeric(unlist(CIT_nonlib))) # p-value = 0.7282



