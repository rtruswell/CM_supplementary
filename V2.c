node: IP-MAT|IP-SUB|IP-MAT-*|IP-SUB-*
define: WhRel.def
add_to_ignore: CODE|PUNC|LINEBREAK|MISC|CNJCTR|INS

coding_query:
1: {
EarlyPP: ((PP* HasSister NP-SBJ*) AND (PP* precedes NP-SBJ*) AND (NP-SBJ* iDoms ![1]\**) AND (PP* HasSister FiniteVerbPLAEME) AND (PP* precedes FiniteVerbPLAEME) AND (PP* iDomsMod PP NP|ADJP|ADVP|ADVP-TMP|ADV+P|P+ADJ|P+D|P+D-RH|P+N|P+WADV|P+RP) AND (NP|ADJP|ADVP|ADVP-TMP|ADV+P|P+ADJ|P+D|P+D-RH|P+N|P+WADV|P+RP iDoms ![2]\**))
EarlyNP: ((NP-O*|NP-1|NP-2|NP-3 HasSister NP-SBJ*) AND (NP-O*|NP-1|NP-2|NP-3 precedes NP-SBJ*) AND (NP-SBJ* iDoms ![1]\**) AND (NP-O*|NP-1|NP-2|NP-3 HasSister FiniteVerbPLAEME) AND (NP-O*|NP-1|NP-2|NP-3 precedes FiniteVerbPLAEME) AND (NP-O*|NP-1|NP-2|NP-3 iDoms ![2]\**))
EarlyAP: ((ADJP* HasSister NP-SBJ*) AND (ADJP* precedes NP-SBJ*) AND (NP-SBJ* iDoms ![1]\**) AND (ADJP* HasSister FiniteVerbPLAEME) AND (ADJP* precedes FiniteVerbPLAEME) AND (ADJP* iDoms ![2]\**))
EarlyThen: ((ADVP* HasSister NP-SBJ*) AND (ADVP* precedes NP-SBJ*) AND (NP-SBJ* iDoms !\**) AND (ADVP* HasSister FiniteVerbPLAEME) AND (ADVP* precedes FiniteVerbPLAEME) AND (ADVP* iDomsMod ADV* *-Ya:*|*-then))
EarlyNow: ((ADVP* HasSister NP-SBJ*) AND (ADVP* precedes NP-SBJ*) AND (NP-SBJ* iDoms !\**) AND (ADVP* HasSister FiniteVerbPLAEME) AND (ADVP* precedes FiniteVerbPLAEME) AND (ADVP* iDomsMod ADV* *-now))
EarlyADVP: ((ADVP* HasSister NP-SBJ*) AND (ADVP* precedes NP-SBJ*) AND (NP-SBJ* iDoms ![1]\**) AND (ADVP* HasSister FiniteVerbPLAEME) AND (ADVP* precedes FiniteVerbPLAEME) AND (ADVP* iDoms ![2]\**))
EarlyNPAdj: ((NP-TMP*|NP-ADV*|NP-MSR*|NP-DIR*|NP-ADT* HasSister NP-SBJ*) AND (NP-TMP*|NP-ADV*|NP-MSR*|NP-DIR*|NP-ADT* precedes NP-SBJ*) AND (NP-SBJ* iDoms ![1]\**) AND (NP-TMP*|NP-ADV*|NP-MSR*|NP-DIR*|NP-ADT* HasSister FiniteVerbPLAEME) AND (NP-TMP*|NP-ADV*|NP-MSR*|NP-DIR*|NP-ADT* precedes FiniteVerbPLAEME) AND (NP-LOC*|NP-TMP*|NP-ADV*|NP-MSR*|NP-DIR*|NP-SPR*|NP-ADT* iDoms ![2]\**))
EarlyNEG: ((NEG* HasSister NP-SBJ*) AND (NEG* precedes NP-SBJ*) AND (NP-SBJ* iDoms !\**) AND (NEG* HasSister FiniteVerbPLAEME) AND (NEG* precedes FiniteVerbPLAEME))
EarlyNEG: ((NegFiniteVerbPLAEME HasSister NP-SBJ*) AND (NegFiniteVerbPLAEME precedes NP-SBJ*) AND (NP-SBJ* iDoms !\**))
EarlyPRT: ((RP|RP-* HasSister NP-SBJ*) AND (RP|RP-* precedes NP-SBJ*) AND (NP-SBJ* iDoms !\**) AND (RP|RP-* HasSister FiniteVerbPLAEME) AND (RP|RP-* precedes FiniteVerbPLAEME))
EarlyFiniteVerb: (IP-MAT*|IP-SUB* iDomsFirst FiniteVerbPLAEME)
EarlyFiniteVerb: ((IP-MAT*|IP-SUB* iDomsFirst InitialFluff) AND (IP-MAT*|IP-SUB* iDomsNumber 2 FiniteVerbPLAEME))
EarlyFiniteVerb: ((IP-MAT*|IP-SUB* iDomsFirst [1]InitialFluff) AND (IP-MAT*|IP-SUB* iDomsNumber 2 [2]InitialFluff) AND (IP-MAT*|IP-SUB* iDomsNumber 3 FiniteVerbPLAEME))
EarlyInfinitiveVerb: (IP-MAT*|IP-SUB* iDomsFirst VB|HV|BE|DO)
EarlyInfinitiveVerb: (IP-MAT*|IP-SUB* iDomsFirst InitialFluff) AND (IP-MAT*|IP-SUB* iDomsNumber 2 VB|HV|BE|DO)
EarlyParticiple: ((VAN*|VBN*|IP-PPL* HasSister NP-SBJ*) AND (VAN*|VBN*|IP-PPL* precedes NP-SBJ*) AND (NP-SBJ* iDoms !\**) AND (VAN*|VBN*|IP-PPL* HasSister FiniteVerbPLAEME) AND (VAN*|VBN*|IP-PPL* precedes FiniteVerbPLAEME))
EarlyVP: ((VP|VP-* HasSister NP-SBJ*) AND (VP|VP-* precedes NP-SBJ*) AND (NP-SBJ* iDoms !\**) AND (VP|VP-* HasSister FiniteVerbPLAEME) AND (VP|VP-* precedes FiniteVerbPLAEME))
EarlyINF: (IP-MAT*|IP-SUB* iDomsFirst IP-INF*)
EarlyINF: ((IP-MAT*|IP-SUB* iDomsFirst CONJ*) AND (IP-MAT*|IP-SUB* iDomsNumber 2 IP-INF*))
EarlySMC: ((IP-SMC* HasSister NP-SBJ*) AND (IP-SMC* precedes NP-SBJ*) AND (NP-SBJ* iDoms !\**) AND (IP-SMC* HasSister FiniteVerbPLAEME) AND (IP-SMC* precedes FiniteVerbPLAEME))
EarlySBJ: ((IP-MAT*|IP-SUB* iDomsFirst NP-SBJ*) AND (NP-SBJ* iDoms !\**))
EarlySBJ: ((IP-MAT*|IP-SUB* iDomsFirst InitialFluff) AND (IP-MAT*|IP-SUB* iDomsNumber 2 NP-SBJ*) AND (NP-SBJ* iDoms !\**))
EarlySBJ: ((IP-MAT*|IP-SUB* iDomsFirst [1]InitialFluff) AND (IP-MAT*|IP-SUB* iDomsNumber 2 [2]InitialFluff) AND (IP-MAT*|IP-SUB* iDomsNumber 3 NP-SBJ*) AND (NP-SBJ* iDoms !\**))
EarlySBJ: ((IP-MAT*|IP-SUB* iDomsFirst [1]InitialFluff) AND (IP-MAT*|IP-SUB* iDomsNumber 2 [2]InitialFluff) AND (IP-MAT*|IP-SUB* iDomsNumber 3 [3]InitialFluff) AND (IP-MAT*|IP-SUB* iDomsNumber 4 NP-SBJ*) AND (NP-SBJ* iDoms !\**))
EarlyCPADV: ((CP-ADV* HasSister NP-SBJ*) AND (CP-ADV* precedes NP-SBJ*) AND (NP-SBJ* iDoms !\**) AND (CP-ADV* HasSister FiniteVerbPLAEME) AND (CP-ADV* precedes FiniteVerbPLAEME))
EarlyQUE: ((CP-QUE* HasSister NP-SBJ*) AND (CP-QUE* precedes NP-SBJ*) AND (NP-SBJ* iDoms !\**) AND (CP-QUE* HasSister FiniteVerbPLAEME) AND (CP-QUE* precedes FiniteVerbPLAEME))
EarlyTHT: ((CP-THT* HasSister NP-SBJ*) AND (CP-THT* precedes NP-SBJ*) AND (NP-SBJ* iDoms !\**) AND (CP-THT* HasSister FiniteVerbPLAEME) AND (CP-THT* precedes FiniteVerbPLAEME))
EarlyClausalPP: ((PP* HasSister NP-SBJ*) AND (PP* precedes NP-SBJ*) AND (NP-SBJ* iDoms !\**) AND (PP* HasSister FiniteVerbPLAEME) AND (PP* precedes FiniteVerbPLAEME) AND (PP* iDomsMod PP* CP-ADV*))
EarlyLFD: ((NP-LFD* HasSister NP-SBJ*) AND (NP-LFD* precedes NP-SBJ*) AND (NP-SBJ* iDoms !\**) AND (NP-LFD* HasSister FiniteVerbPLAEME) AND (NP-LFD* precedes FiniteVerbPLAEME))
EarlyINTJ: (IP-MAT*|IP-SUB* iDomsFirst INTJ*)
EarlyQTP: (IP-MAT*|IP-SUB* iDomsFirst QTP)
Trace: ((IP-MAT*|IP-SUB* iDomsFirst .*) AND (.* iDoms \**))
NullSBJ: ((IP-MAT*|IP-SUB* iDoms NP-SBJ*) AND (NP-SBJ* iDoms \**))
INFSBJ: (IP-MAT*|IP-SUB* iDoms IP-INF-SBJ*)
QUESBJ: (IP-MAT*|IP-SUB* iDoms CP-QUE-SBJ*)
THTSBJ: (IP-MAT*|IP-SUB* iDoms CP-THT-SBJ*)
QTPSBJ: (IP-MAT*|IP-SUB* iDoms QTP-SBJ*)
PPSBJ: (IP-MAT*|IP-SUB* iDoms PP-SBJ*)
CONJ: (IP-MAT*|IP-SUB* iDoms CONJP)
Gapping: ((IP-MAT*|IP-SUB* iDomsFirst .*) AND (.*=* iDomsFirst .*))
}

2: {
Inv: ((FiniteVerbPLAEME|NegFiniteVerbPLAEME HasSister NP-SBJ*) AND (FiniteVerbPLAEME|NegFiniteVerbPLAEME precedes NP-SBJ*) AND (NP-SBJ* iDoms !\**))
NoInv: ((FiniteVerbPLAEME|NegFiniteVerbPLAEME HasSister NP-SBJ*) AND (NP-SBJ* precedes FiniteVerbPLAEME|NegFiniteVerbPLAEME) AND (NP-SBJ* iDoms !\**))
Trace: ((IP-MAT*|IP-SUB* iDomsFirst .*) AND (.* iDoms \**))
NullSBJ: ((IP-MAT*|IP-SUB* iDoms NP-SBJ*) AND (NP-SBJ* iDoms \**))
INFSBJ: (IP-MAT*|IP-SUB* iDoms IP-INF-SBJ*)
QUESBJ: (IP-MAT*|IP-SUB* iDoms CP-QUE-SBJ*)
THTSBJ: (IP-MAT*|IP-SUB* iDoms CP-THT-SBJ*)
QTPSBJ: (IP-MAT*|IP-SUB* iDoms QTP-SBJ*)
PPSBJ: (IP-MAT*|IP-SUB* iDoms PP-SBJ*)
CONJ: (IP-MAT*|IP-SUB* iDoms CONJP)
Gapping: ((IP-MAT*|IP-SUB* iDomsFirst .*) AND (.*=* iDomsFirst .*))
   }

3: {
FullNP: (NP-SBJ* iDomsMod NP|CONJP N|NS|NPR|NPRS|N-RH|NS-RH|NPR-RH|NPRS-RH|N+N|N+N-RH|N+NS|N+NS-RH|Q+N|Q+N-RH|Q+NS|Q+NS-RH|Q+ONE|Q+ONE-RH|D+OTHER|D+OTHER-RH|Q+WPRO|Q+WPRO-RH|NPR+NPR|NPR$+NPR|NPR$+NPR-RH|NPR+NS|NPR+ADJ|NPR+ADJ-RH|CP-FRL*|ADJ+N|ADJ+N-RH|NUM+N|ADJ+NS|D+ADJ|ADJP|NUMP|ADJS+N)
FullNP: ((NP-SBJ* iDomsMod NP|CONJP D|PRO$|SUCH|ADV) AND (D|PRO$|SUCH|ADV HasSister CP-REL*|ONE|ONE-RH|OTHER|OTHER-RH|OTHERS|ADJ|ADJ-RH|ADJR|ADJS|Q|Q-RH|NUM|NUM-RH|NP-COM) AND (D|PRO$|SUCH|ADV precedes CP-REL*|ONE|ONE-RH|OTHER|OTHER-RH|OTHERS|ADJ|ADJ-RH|ADJR|ADJS|Q|Q-RH|NUM|NUM-RH|NP-COM) AND (CP-REL*|ONE|ONE-RH|OTHER|OTHER-RH|OTHERS|ADJ|ADJ-RH|ADJR|ADJS|Q|Q-RH|NUM|NUM-RH|NP-COM iDoms !\**))
FullNP: ((NP-SBJ* iDomsMod NP|CONJP FP) AND (FP HasSister ADJ) AND (FP Precedes ADJ))
FullNP: (NP-SBJ* iDomsOnly ADJ|ADV)
FullNP: ((NP-SBJ* iDomsFirst ADJ) AND (NP-SBJ* iDomsLast .*) AND (.* iDoms \**) AND (ADJ iPrecedes \**))
FullNP: ((NP-SBJ* iDomsMod NP|CONJP Q|ONE|D+ADJ|NUM|NUM+NUM|D+NUM+CONJ+NUM-RH) AND (Q|ONE|D+ADJ|NUM|NUM+NUM|D+NUM+CONJ+NUM-RH HasSister CP-REL*|RRC|PP*|NP-COM|OTHER|OTHERS|NUM|SUCH|ADJ) AND (Q|ONE|D+ADJ|NUM|NUM+NUM|D+NUM+CONJ+NUM-RH precedes CP-REL*|RRC|PP*|NP-COM|OTHER|OTHERS|NUM|SUCH|ADJ))
FullNP: ((NP-SBJ* iDoms [1]NP) AND ([1]NP iDoms [2]ADJ) AND (NP-SBJ* iDoms CONJP) AND (CONJP iDoms [3]NP) AND ([3]NP iDoms [4]ADJ))
FullNP: ((NP-SBJ* iDoms [1]QS) AND (NP-SBJ* iDoms CONJ) AND (NP-SBJ* iDoms [2]QS))
Pronoun: (NP-SBJ* iDomsOnly PRO|PRO-RH)
ProTrace: ((NP-SBJ* iDomsFirst PRO|PRO-RH) AND (NP-SBJ* iDomsLast \**) AND (PRO|PRO-RH iPrecedes \**))
ProTrace: ((NP-SBJ* iDomsFirst PRO|PRO-RH) AND (NP-SBJ* iDomsLast .*) AND (.* iDoms \**) AND (PRO|PRO-RH iPrecedes \**))
ProTrace: ((NP-SBJ* iDomsFirst NP) AND (NP iDomsOnly PRO|PRO-RH) AND (NP-SBJ* iDomsLast CONJP) AND (CONJP iDoms \**) AND (PRO|PRO-RH iPrecedes \**))
PossPro: (NP-SBJ* iDomsOnly PRO\$)
ProSelf: ((NP-SBJ* iDomsOnly PRO+N) AND (PRO+N iDoms *self))
DemPro: (NP-SBJ* iDomsOnly D|D-RH)
DemProTrace: ((NP-SBJ* iDomsFirst D|D-RH) AND (NP-SBJ* iDomsLast \**) AND (D|D-RH iPrecedes \**))
DemProTrace: ((NP-SBJ* iDomsFirst D) AND (NP-SBJ* iDomsLast .*) AND (.* iDoms \**) AND (D iPrecedes \**))
DemProTrace: ((NP-SBJ* iDomsFirst NP) AND (NP iDomsOnly D) AND (NP-SBJ* iDomsLast CONJP) AND (CONJP iDoms \**) AND (D iPrecedes \**))
Man: (NP-SBJ* iDomsOnly MAN)
ManTrace: ((NP-SBJ* iDomsFirst MAN) AND (NP-SBJ* iDomsLast .*) AND (.* iDoms \**) AND (MAN iPrecedes \**))
Num: (NP-SBJ* iDomsOnly NUM|D+NUM)
NumTrace: ((NP-SBJ* iDomsFirst NUM|D+NUM) AND (NP-SBJ* iDomsLast .*) AND (.* iDoms \**) AND (NUM|D+NUM iPrecedes \**))
One: (NP-SBJ* iDomsOnly ONE|ONE-RH)
OneTrace: ((NP-SBJ* iDomsFirst ONE|ONE-RH) AND (NP-SBJ* iDomsLast .*) AND (.* iDoms \**) AND (ONE|ONE-RH iPrecedes \**))
Other: (NP-SBJ* iDomsOnly OTHER|OTHERS)
Such: (NP-SBJ* iDomsOnly SUCH)
SuchTrace: ((NP-SBJ* iDomsFirst SUCH) AND (NP-SBJ* iDomsLast .*) AND (.* iDoms \**) AND (SUCH iPrecedes \**))
There: (NP-SBJ* iDomsOnly EX)
BareQ: (NP-SBJ* iDomsOnly Q|Q-RH|QR)
BareQTrace: ((NP-SBJ* iDomsFirst Q|Q-RH|QR) AND (NP-SBJ* iDomsLast .*) AND (.* iDoms \**) AND (Q|Q-RH|QR iPrecedes \**))
BareQTrace: ((NP-SBJ* iDomsLast Q|Q-RH) AND (NP-SBJ* iDomsFirst .*) AND (.* iDoms \**) AND (\** iPrecedes Q|Q-RH))
WhInd: (NP-SBJ* iDomsOnly WQ)
ProPlus: (NP-SBJ* iDomsMod NP PRO|PRO-RH)
DemPlus: (NP-SBJ* iDomsLast D)
Trace: ((IP-MAT*|IP-SUB* iDomsFirst .*) AND (.* iDoms \**))
NullSBJ: ((IP-MAT*|IP-SUB* iDoms NP-SBJ*) AND (NP-SBJ* iDoms \**))
INFSBJ: (IP-MAT*|IP-SUB* iDoms IP-INF-SBJ*)
QUESBJ: (IP-MAT*|IP-SUB* iDoms CP-QUE-SBJ*)
THTSBJ: (IP-MAT*|IP-SUB* iDoms CP-THT-SBJ*)
QTPSBJ: (IP-MAT*|IP-SUB* iDoms QTP-SBJ*)
PPSBJ: (IP-MAT*|IP-SUB* iDoms PP-SBJ*)
CONJ: (IP-MAT*|IP-SUB* iDoms CONJP)
Gapping: ((IP-MAT*|IP-SUB* iDomsFirst .*) AND (.*=* iDomsFirst .*))
   }


4: {
Mat: ((IP-SUB*|IP-MAT* iDoms NP-SBJ*) AND (IP-MAT* iDoms NP-SBJ*))
Sub: ((IP-SUB*|IP-MAT* iDoms NP-SBJ*) AND (IP-SUB* iDoms NP-SBJ*))
Trace: ((IP-MAT*|IP-SUB* iDomsFirst .*) AND (.* iDoms \**))
NullSBJ: ((IP-MAT*|IP-SUB* iDoms NP-SBJ*) AND (NP-SBJ* iDoms \**))
INFSBJ: (IP-MAT*|IP-SUB* iDoms IP-INF-SBJ*)
QUESBJ: (IP-MAT*|IP-SUB* iDoms CP-QUE-SBJ*)
THTSBJ: (IP-MAT*|IP-SUB* iDoms CP-THT-SBJ*)
QTPSBJ: (IP-MAT*|IP-SUB* iDoms QTP-SBJ*)
PPSBJ: (IP-MAT*|IP-SUB* iDoms PP-SBJ*)
CONJ: (IP-MAT*|IP-SUB* iDoms CONJP)
Gapping: ((IP-MAT*|IP-SUB* iDomsFirst .*) AND (.*=* iDomsFirst .*))
   }
