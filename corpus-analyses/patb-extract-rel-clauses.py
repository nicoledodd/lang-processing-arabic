#!/usr/bin/env python3
# -*- coding: utf-8 -*-

## Purpose of this script:
## Extract tagged SRCs and ORCs from the Penn Arabic Treebank (PATB) Part 3 v 3.2

## sys.argv[1] = path to PATB files in nltk_data folder

import sys, os, re
from nltk.corpus import ptb
import pandas as pd


## Import data ################################################################

## get list of file ids
file_list = []
for filename in os.listdir(sys.argv[1]):
    if filename.endswith('.tree'):
        file_list.append(filename)


## Extract relative clauses from sentences ####################################

patb_rel = []
total_sents = 0

for filename in file_list:
    sentnum = 1 # sentence indexing
    sents = ptb.raw(filename).split('\n')
    total_sents += len(sents) # get total number of sentences
    sent_chunks = []
    for sent in sents:
        sent_chunks.append(list(filter(None, sent.split(')'))))

    for sent in sent_chunks: # sent_chunks = nested list of sentences
        subsentnum = 1 # indexing for sentences with more than one RC
        i = 0 # chunk indexing
        for chunk in sent: # gets at each chunk within individual sentences
        #    if re.search('\(SBAR \(WHNP-\d+ \(REL_PRON', chunk): # excludes SBAR-NOM (uses ما rel pronoun) and WHNP -NONE-
        #    if re.search('\(SBAR \(WHNP-\d+', chunk): # excludes SBAR-NOM (uses ما rel pronoun)
            if re.search('\(SBAR(?:-NOM)? \(WHNP-\d+', chunk):
                patb_rel.append([filename, #fileid
                                str(sentnum) + '.' + str(subsentnum), # index of sentence and rel clause in sentence
                                sent[i-1] + ')' + sent[i] + ')' + sent[i+1] + ')' + sent[i+2] + ')']) #estimated chunk of sentence rather than full sentence
                # append clauses to search for N-SBJ (checks for len of sentence)
                if len(sent) > i+3:
                    patb_rel[-1][-1] = patb_rel[-1][-1] + sent[i+3] + ')'
                if len(sent) > i+4:
                    patb_rel[-1][-1] = patb_rel[-1][-1] + sent[i+4] + ')'
                if len(sent) > i+5:
                    patb_rel[-1][-1] = patb_rel[-1][-1] + sent[i+5] + ')'
                if len(sent) > i+6:
                    patb_rel[-1][-1] = patb_rel[-1][-1] + sent[i+6] + ')'
                if len(sent) > i+7:
                    patb_rel[-1][-1] = patb_rel[-1][-1] + sent[i+7] + ')'
                subsentnum += 1
            i += 1
        sentnum += 1


## Tag RCs as either SRCs or ORCs & get relative clause word order #############

srcs = []
orcs = []
svo = 0
vso = 0

for clause in patb_rel:
    if re.search('\(WHNP-(\d+).*\(NP-SBJ-\\1 \(-NONE- \*T\*', clause[2]): # if subject gap is labeled and refers back to WHNP index
        if re.search('\(WHNP-\d+ .* (\(S )+\(VP', clause[2]):
            srcs.append([clause[0], clause[1], 'VSO', clause[2]])
            vso += 1
        else:
            srcs.append([clause[0], clause[1], 'SVO', clause[2]])
            svo += 1
    elif re.search('\(WHNP-(\d+).*\(NP-OBJ.*\(NP-\\1 \(-NONE- \*T\*', clause[2]):
        if re.search('\(WHNP-\d+ .* (\(S )+\(VP', clause[2]):
            orcs.append([clause[0], clause[1], 'VSO', clause[2]])
            vso += 1
        else:
            orcs.append([clause[0], clause[1], 'SVO', clause[2]])
            svo += 1


## Calculate proportions in corpus ############################################

prop_src = len(srcs) / (len(srcs) + len(orcs))
prop_orc = len(orcs) / (len(srcs) + len(orcs))
prop_svo = svo / (svo + vso)
prop_vso = vso / (svo + vso)

print('Total sentences: ' + str(total_sents) +
    '\nTotal relative clauses: ' +str(len(patb_rel)) +
    '\nNumber of SRCs: ' + str(len(srcs)) +
    '\nNumber of ORCs: ' + str(len(orcs)) +
    '\nProportion of SRCs: ' + str(round(prop_src * 100)) + '%' +
    '\nProportion of ORCs: ' + str(round(prop_orc * 100)) + '%' +
    '\nNumber of SVO clauses: ' + str(svo) +
    '\nNumber of VSO clauses: ' + str(vso) +
    '\nProportion of SVO clauses: ' + str(round(prop_svo * 100)) + '%' +
    '\nProportion of VSO clauses: ' + str(round(prop_vso * 100)) + '%')


## Determine if any ORCs don't explicitly mark RP #############################
not_blank = 0

for clause in orcs:
    if re.search('\(WHNP-(\d+).*\(NP-OBJ \(NP', clause[2]):
        not_blank += 1

print('Number of unmarked RPs in ORCs: ' + str(len(orcs) - not_blank))


## Export to Excel ############################################################

file_ids = []
sent_index = []
rc_type = []
clause_order = []
rc_chunks = []

for line in srcs:
    file_ids.append(line[0])
    sent_index.append(line[1])
    rc_type.append('SRC')
    clause_order.append(line[2])
    rc_chunks.append(line[3])

for line in orcs:
    file_ids.append(line[0])
    sent_index.append(line[1])
    rc_type.append('ORC')
    clause_order.append(line[2])
    rc_chunks.append(line[3])

rc_dict = {'file-id': file_ids,
            'sent-index': sent_index,
            'rc-type': rc_type,
            'clause-order': clause_order,
            'clause': rc_chunks}

rcs = pd.DataFrame(rc_dict)

# rcs.to_excel('data/patb-rel-clauses-word-order.xlsx')
