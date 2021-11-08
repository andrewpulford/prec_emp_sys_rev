#### ---------------------------------------------------------------------- ####
###                              PRISMA diagram                              ###
#### ---------------------------------------------------------------------- ####

## remove any existing objects from global environment
rm(list=ls()) 

## disable scientific notation printing
options(scipen=999)

library(DiagrammeR)


grViz("
digraph {
  graph [ranksep = 0.2]
  node [shape = box, width = 5]
    A [label = '21,407 studies imported for screening']
    B [label = '8,476, duplicates removed']
    C [label = '12,940 studies screened']
    D [label = '12,383 studies irrelevant']
    E [label = '555 full-text studies screened']
    F [label = '505 studies excluded:\\n
    330 Did not report persistent exposure
    55  Commentary/editorial
    36  Review
    26  Duplicate
    23  No health outcome reported
    15  Non-English language
    10  Insufficient information published
    5   No appropriate comparator
    4   Unit of analysis not at individual or household
    1   The study population is not working-age adults
    1   Qualitative study']
    G [label = '50 studies included                 ']
  edge [minlen = 2]
    A->B
    A->C
    C->D
    C->E
    E->F
    E->G
  { rank = same; A; B }
  { rank = same; C; D }
  { rank = same; E; F }
}
")


