library(dplyr)
library(reshape2)
library(igraph)
library(merTools)
library(rlist)

riasec_pairings <- t(apply(X = expand.grid( c("R","I","A","S","E","C"),
             c("R","I","A","S","E","C")) %>% subset(Var1 != Var2), 1, sort)) %>% unique %>% data.frame(stringsAsFactors = F)

subsetList <- function(myList, elementNames) {
  lapply(elementNames, FUN=function(x) myList[[x]])
}

apply(matrix(1:nrow(riasec_pairings)), MARGIN = 1, 
function(x){
do.call(cbind, subsetList(myList = items, elementNames = riasec_pairings[x,]) %>% lapply(FUN = function(x){sample(x, size = 1)}) )
}) %>% t

rep(c("R","I","A","S","E","C"), each=8)

items <- list(
R = c('Test the quality of parts before shipment',
'Lay brick or tile',
'Work on an offshore oil-drilling rig',
'Assemble electronic parts',
'Operate a grinding machine in a factory',
'Fix a broken faucet',
'Assemble products in a factory',
'Install flooring in houses'),

I = c('Study the structure of the human body',
'Study animal behavior',
'Do research on plants or animals',
'Develop a new medical treatment or procedure',
'Conduct biological research',
'Study whales and other types of marine life',
'Work in a biology lab',
'Make a map of the bottom of an ocean'),

A = c('Conduct a musical choir',
'Direct a play',
'Design artwork for magazines',
'Write a song',
'Write books or plays',
'Play a musical instrument',
'Perform stunts for a movie or television show',
'Design sets for plays'),

S = c('Give career guidance to people',
'Do volunteer work at a non-profit organization',
'Help people who have problems with drugs or alcohol',
'Teach an individual an exercise routine',
'Help people with family-related problems',
'Supervise the activities of children at a camp',
'Teach children how to read',
'Help elderly people with their daily activities'),

E = c('Sell restaurant franchises to individuals',
'Manage the operations of a hotel',
'Sell merchandise at a department store',
'Operate a beauty salon or barber shop',
'Manage a department within a large company',
'Manage a clothing store',
'Sell houses',
'Run a toy store'),

C = c('Generate the monthly payroll checks for an office',
'Inventory supplies using a hand-held computer',
'Use a computer program to generate customer bills',
'Maintain employee records',
'Compute and record statistical and other numerical data',
'Operate a calculator',
'Handle customer bank transactions',
'Keep shipping and receiving records')
)

names(R)[1] = "R"
names(I)[1] = "I"
names(A)[1] = "A"
names(S)[1] = "S"
names(E)[1] = "E"
names(C)[1] = "C"

rowShuffle <- function(x){x[sample(nrow(x),]}

items_sample <- apply(matrix(1:nrow(riasec_pairings)), MARGIN = 1, 
                      function(x){
                        do.call(cbind, subsetList(myList = items, elementNames = riasec_pairings[x,]) %>% lapply(FUN = function(x){sample(x, size = 1)}) )
                      }) %>% t %>% cbind(riasec_pairings)

items_sample

# The argument I would like to make is that normative interest scores are disregarded when it comes to making occupational recommendations. Regradless if you get 100,100,100 on RIC or 20,20,20 the jobs that you are recommended are the same

# Individuals choosing a field to pursue lack complete information regarding the items their asked to respond to ("How am I supposed to rate 1-5 'working in a biology lab'?"). However, they may have just enough information to be able to compare two items against each other. 

#Graph theory can provide insight into the paired comparison response process. Further, GT calculations can provide insight into the importance of different 

shuffled_rp <- apply(riasec_pairings, 1, sample) %>% t

ws <- shuffled_rp[,2]

ls <- shuffled_rp[,1]

riasec_elo <- elochoice(ws, ls, 100, 1000)$ratmat

riasec_pr <- (graph_from_data_frame(cbind(ls,ws)) %>% page_rank)$vector
riasec_pr <- riasec_pr[order(names(riasec_pr))]
cor(riasec_pr %>% matrix,
    riasec_elo %>% matrix,
    method = 'spearman')


