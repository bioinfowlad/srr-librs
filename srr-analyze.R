# srr-analyze
#
# plots and stats after collecting SR-AMC data
#
# script is part of:
# A Bibliographic Analysis of librarian assistance on SRs at CUAnschutz
# (continuation of Craven/Palmer/Piper project)

# Initialize
library(tidyverse)
library(magrittr)
library(RefManageR)

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

# Boxplots comparing nonlibs to coauthor libs
ggplot(srr_dataset %>% filter(librarian != "NA"),aes(factor(coauthor),JIF)) + geom_boxplot(outlier.shape=NA) + geom_jitter(width=0.15,alpha=0.5)

ggplot(srr_dataset %>% filter(librarian != "NA"),aes(factor(coauthor),maxRank)) + geom_boxplot(outlier.shape=NA) + geom_jitter(width=0.15,alpha=0.5)

ggplot(srr_dataset %>% filter(librarian != "NA"),aes(factor(coauthor),times.cited)) + geom_boxplot(outlier.shape=NA) + geom_jitter(width=0.15,alpha=0.5)

# Let me try a weird "normalization"
ggplot(srr_dataset %>% filter(librarian != "NA"),aes(factor(librarian),times.cited/JIF)) + geom_boxplot(outlier.shape=NA) + geom_jitter(width=0.15,alpha=0.5)

ggplot(srr_dataset %>% filter(librarian != "NA"),aes(factor(coauthor),times.cited/JIF)) + geom_boxplot(outlier.shape=NA) + geom_jitter(width=0.15,alpha=0.5)

## Pie Chart of dataset
srr_chart <- data.frame(
    group <- c("No Librarian Involvement","Librarian is Co-author","Librarian Acknowledged"),
    value <- c(243,37,46))
srr_chart <- srr_chart %>%
    arrange(desc(group)) %>%
    mutate(prop = value / sum(srr_chart$value) * 100) %>%
    mutate(ypos = cumsum(prop) - 0.5*prop)
srr_pie <- ggplot(srr_chart,aes(x="", y=prop, fill=group)) +
    geom_bar(stat="identity",width=1,color="white") +
    coord_polar("y",start=pi/2) +
    theme_void() +
    theme(legend.position="bottom",legend.direction="vertical",legend.text=element_text(face="bold",size=14)) +
    scale_fill_manual("",values=c("#007C7C","#773D84","#215297")) +
    geom_text(aes(y=ypos,label=value),color="white",size=8)
        
## Stacked Histograms of dataset
# JIF
jifhist <- ggplot(data=(srr_dataset %>% filter(librarian==FALSE)),aes(JIF))+
    geom_histogram(col="#215297",fill="#215297",alpha=0.65,binwidth=1)+
    geom_histogram(data=(srr_dataset %>% filter(librarian==TRUE)),
                   aes(JIF),col="#BE5039",fill="#BE5039",alpha=0.65,binwidth=1) +
    theme_minimal() +
    ggtitle("Histogram of Journal Impact Factors in dataset") +
    theme(plot.title = element_text(size=20,face="bold")) +
    annotate("text",x=30,y=50,label="No Librarian",colour="#215297",size=7,fontface="bold") +
    annotate("text",x=30,y=45,label="Librarian",colour="#BE5039",size=7,fontface="bold")

# maxRank
rankhist <- ggplot(data=(srr_dataset %>% filter(librarian==FALSE)),aes(maxRank))+
    geom_histogram(col="#215297",fill="#215297",alpha=0.65,binwidth=2.5)+
    geom_histogram(data=(srr_dataset %>% filter(librarian==TRUE)),
                   aes(maxRank),col="#BE5039",fill="#BE5039",alpha=0.65,binwidth=2.5) +
    theme_minimal() +
    ggtitle("Histogram of Journal Category Rankings in dataset") +
    theme(plot.title = element_text(size=20,face="bold")) +
    annotate("text",x=40,y=15,label="No Librarian",colour="#215297",size=7,fontface="bold") +
    annotate("text",x=40,y=14,label="Librarian",colour="#BE5039",size=7,fontface="bold")

# citations
cithist <- ggplot(data=(srr_dataset %>% filter(librarian==FALSE)),aes(times.cited))+
    geom_histogram(col="#215297",fill="#215297",alpha=0.65,binwidth=2)+
    geom_histogram(data=(srr_dataset %>% filter(librarian==TRUE)),
                   aes(times.cited),col="#BE5039",fill="#BE5039",alpha=0.65,binwidth=2) +
    theme_minimal() +
    ggtitle("Histogram of Article Citations in dataset") +
    theme(plot.title = element_text(size=20,face="bold")) +
    annotate("text",x=150,y=15,label="No Librarian",colour="#215297",size=7,fontface="bold") +
    annotate("text",x=150,y=12,label="Librarian",colour="#BE5039",size=7,fontface="bold")

## Box-plots Lib versus NoLib
libscale <- c("#215297","#BE5039")
jifbox <- ggplot(srr_dataset %>% filter(librarian != "NA"),aes(factor(librarian),JIF)) +
    geom_jitter(width=0.15,alpha=0.5) +
    geom_boxplot(outlier.shape=NA,alpha=0.8,colour="black",fill=libscale) +
    theme_minimal() +
    ggtitle("Journal Impact Factor versus Librarian Involvement") +
    xlab("Librarian Involved?")
rankbox <- ggplot(srr_dataset %>% filter(librarian != "NA"),aes(factor(librarian),maxRank)) +
    geom_jitter(width=0.15,alpha=0.5) +
    geom_boxplot(outlier.shape=NA,alpha=0.8,colour="black",fill=libscale) +
    theme_minimal() +
    ggtitle("Journal Category Rank versus Librarian Involvement") +
    xlab("Librarian Involved?")
citbox <- ggplot(srr_dataset %>% filter(librarian != "NA"),aes(factor(librarian),times.cited)) +
    geom_jitter(width=0.15,alpha=0.5) +
    geom_boxplot(outlier.shape=NA,alpha=0.8,colour="black",fill=libscale) +
    theme_minimal() +
    ggtitle("Number of Citations versus Librarian Involvement") +
    xlab("Librarian Involved?")

## Prep for auth vs intext-only comparisons
authscale <- c("#357A7A","#713F7E")
srr_dataset <- mutate(srr_dataset,libtype = case_when( librarian==FALSE ~ "None",
                                                       coauthor==TRUE ~ "Co-Author",
                                                       intext==TRUE ~ "Acknowledgment" ))
srr_dataset %<>% mutate(libtype=as.factor(libtype))       

## Box-plots Co-Author versus Acknowledgement
jifauthbox <- ggplot(srr_dataset %>% filter(librarian == TRUE),aes(libtype,JIF)) +
    geom_jitter(width=0.15,alpha=0.5) +
    geom_boxplot(outlier.shape=NA,alpha=0.8,colour="black",fill=authscale) +
    theme_minimal() +
    ggtitle("Journal Impact Factor versus Librarian Involvement") +
    xlab("Librarian Involvement")

rankauthbox <- ggplot(srr_dataset %>% filter(librarian == TRUE),aes(libtype,maxRank)) +
    geom_jitter(width=0.15,alpha=0.5) +
    geom_boxplot(outlier.shape=NA,alpha=0.8,colour="black",fill=authscale) +
    theme_minimal() +
    ggtitle("Journal Category Ranking versus Librarian Involvement") +
    xlab("Librarian Involvement")

citauthbox <- ggplot(srr_dataset %>% filter(librarian == TRUE),aes(libtype,times.cited)) +
    geom_jitter(width=0.15,alpha=0.5) +
    geom_boxplot(outlier.shape=NA,alpha=0.8,colour="black",fill=authscale) +
    theme_minimal() +
    ggtitle("Number of Citations versus Librarian Involvement") +
    xlab("Librarian Involvement")

# Let's do some t-tests (lib versus nonlib)
JIF_lib <- srr_dataset %>% filter(librarian=="TRUE") %>% select(JIF)
JIF_nonlib <- srr_dataset %>% filter(librarian=="FALSE") %>% select(JIF)
t.test(JIF_lib,JIF_nonlib,alt="two.sided",var.equal=FALSE) # p-value = 0.06408

RANK_lib <- srr_dataset %>% filter(librarian=="TRUE") %>% select(maxRank)
RANK_nonlib <- srr_dataset %>% filter(librarian=="FALSE") %>% select(maxRank)
t.test(RANK_lib,RANK_nonlib,alt="two.sided",var.equal=FALSE) # p-value = 0.00201

CIT_lib <- srr_dataset %>% filter(librarian=="TRUE") %>% select(times.cited)
CIT_nonlib <- srr_dataset %>% filter(librarian=="FALSE") %>% select(times.cited)
t.test(CIT_lib,CIT_nonlib,alt="two.sided",var.equal=FALSE) # p-value = 0.4686

# Wilcoxon rank-sum
wilcox.test(as.numeric(unlist(JIF_lib)),as.numeric(unlist(JIF_nonlib)),alt="two.sided") # p-value = 0.038
wilcox.test(as.numeric(unlist(RANK_lib)),as.numeric(unlist(RANK_nonlib)),alt="two.sided") # p-value = 0.00458
wilcox.test(as.numeric(unlist(CIT_lib)),as.numeric(unlist(CIT_nonlib)),alt="two.sided") # p-value = 0.8387

# Let's do some t-tests (auth versus nonauth)
JIF_auth <- srr_dataset %>% filter(coauthor=="TRUE") %>% select(JIF)
JIF_nonauth <- srr_dataset %>% filter(coauthor=="FALSE") %>% select(JIF)
JIF_intext <- srr_dataset %>% filter(intext=="TRUE") %>% select(JIF)
t.test(JIF_auth,JIF_intext) # p-value = 0.7126

RANK_auth <- srr_dataset %>% filter(coauthor=="TRUE") %>% select(maxRank)
RANK_nonauth <- srr_dataset %>% filter(coauthor=="FALSE") %>% select(maxRank)
RANK_intext <- srr_dataset %>% filter(intext=="TRUE") %>% select(maxRank)
t.test(RANK_auth,RANK_intext) # p-value = 0.6542

CIT_auth <- srr_dataset %>% filter(coauthor=="TRUE") %>% select(times.cited)
CIT_nonauth <- srr_dataset %>% filter(coauthor=="FALSE") %>% select(times.cited)
CIT_intext <- srr_dataset %>% filter(intext=="TRUE") %>% select(times.cited)
t.test(CIT_auth,CIT_intext) # p-value = 0.6874

# Wilcoxon rank-sum between librarian involvement types
wilcox.test(as.numeric(unlist(JIF_auth)),as.numeric(unlist(JIF_intext)),alt="two.sided")
wilcox.test(as.numeric(unlist(RANK_auth)),as.numeric(unlist(RANK_intext)),alt="two.sided")
wilcox.test(as.numeric(unlist(CIT_auth)),as.numeric(unlist(CIT_intext)),alt="two.sided")

## For funsies....
strauss_table <- as.data.frame(ReadBib("StraussLibrarians.txt"))
strauss_table <- left_join(strauss_table,srr_dataset,by="title")                      
JIFstrauss <- strauss_table %>% select(JIF)
t.test(JIFstrauss,JIF_nonlib) # p-value = 0.6473
wilcox.test(as.numeric(unlist(JIFstrauss)),as.numeric(unlist(JIF_nonlib)),alt="two.sided") # p-value = 0.6196
RANKstrauss <- strauss_table %>% select(maxRank)
t.test(RANKstrauss,RANK_nonlib) # p-value = 0.6519
wilcox.test(as.numeric(unlist(RANKstrauss)),as.numeric(unlist(RANK_nonlib)),alt="two.sided") # p-value = 0.9661
CITstrauss <- strauss_table %>% select(times.cited)
t.test(CITstrauss,CIT_nonlib) # p-value = 0.1012
wilcox.test(as.numeric(unlist(CITstrauss)),as.numeric(unlist(CIT_nonlib)),alt="two.sided") # p-value = 0.3177


