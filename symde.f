*DANISH
      SUBROUTINE DANISH
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. DANISH V 7.00  4/ 5/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     TRANSLATION GENEROUSLY PROVIDED BY FRANK BASON.   MARCH 1971.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION
C                      CENTER FOR COMPUTING ANDAPPLIED MATHEMATICS
C                      A337 ADMINISTRATION BUILDING
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION - SEPTEMBER 1975.
C                   CURRENT VERSION -     APRIL 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION L(282), LO(25), LD( 8), LW(35,2), LU(10), LCXX(6)
      DIMENSION LC( 8), LF(30), LP( 5),   LT(14), LCH(7)
C
C     ==================================================================
C
C                        ***   TYPE STATEMENTS   ***
C
      CHARACTER LCH*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
C     (1)   ONE WORD COMMANDS IN IR(.). 
C
      DATA L(  1) /   76003069 /
      DATA L(  2) /   92310589 /
      DATA L(  3) /   80200000 /
      DATA L(  4) /   80211280 /
      DATA L(  5) / 1061607337 /
      DATA L(  6) /   82501058 /
      DATA L(  7) /   82513851 /
      DATA L(  8) /   82514040 /
      DATA L(  9) /   82514067 /
      DATA L( 10) /   82514580 /
      DATA L( 11) /   82514769 /
      DATA L( 12) /   82514796 /
      DATA L( 13) /   84104131 /
      DATA L( 14) /   84204631 /
      DATA L( 15) /   84616038 /
      DATA L( 16) /   84616285 /
      DATA L( 17) /  130016300 /
      DATA L( 18) /  105401605 /
      DATA L( 19) /   90318765 /
      DATA L( 20) /   91910395 /
      DATA L( 21) /   91910395 /
      DATA L( 22) /  112706900 /
      DATA L( 23) /   87104042 /
      DATA L( 24) /   94604239 /
      DATA L( 25) /  125110206 /
      DATA L( 26) /  125110395 /
      DATA L( 27) /  125110422 /
      DATA L( 28) /  128700875 /
      DATA L( 29) /  128700875 /
      DATA L( 30) /  127010206 /
      DATA L( 31) /  127010395 /
      DATA L( 32) /  127010422 /
      DATA L( 33) /  128409522 /
      DATA L( 34) /  128701126 /
      DATA L( 35) /  525210354 /
      DATA L( 36) /  112808748 /
      DATA L( 37) /  160018607 /
      DATA L( 38) /  161206939 /
      DATA L( 39) /  161207668 /
      DATA L( 40) /  161208397 /
      DATA L( 41) /  161214677 /
      DATA L( 42) /  171610341 /
      DATA L( 43) /  172704146 /
      DATA L( 44) /  174310341 /
      DATA L( 45) /  175404146 /
      DATA L( 46) /  177010341 /
      DATA L( 47) /  178104146 /
      DATA L( 48) /  187504797 /
      DATA L( 49) /  195303807 /
      DATA L( 50) /  214810341 /
      DATA L( 51) /  215904146 /
      DATA L( 52) /  221803069 /
      DATA L( 53) /  230416285 /
      DATA L( 54) /  233111318 /
      DATA L( 55) /  233119107 /
      DATA L( 56) /  233311318 /
      DATA L( 57) /  233319107 /
      DATA L( 58) /  233614274 /
      DATA L( 59) /  235407614 /
      DATA L( 60) /  239500000 /
      DATA L( 61) /   87803402 /
      DATA L( 62) /  241811453 /
      DATA L( 63) /  244510341 /
      DATA L( 64) /  245604146 /
      DATA L( 65) /  249910341 /
      DATA L( 66) /  251004146 /
      DATA L( 67) /  237710395 /
      DATA L( 68) /  259603645 /
      DATA L( 69) / 1389109626 /
      DATA L( 70) /  260614729 /
      DATA L( 71) /  260614837 /
      DATA L( 72) /  261013269 /
      DATA L( 73) /  261100000 /
      DATA L( 74) /  261105103 /
      DATA L( 75) /  261105832 /
      DATA L( 76) /  261106959 /
      DATA L( 77) /  261200000 /
      DATA L( 78) /  261205103 /
      DATA L( 79) /  261205832 /
      DATA L( 80) / 1461208748 /
      DATA L( 81) /  263111475 /
      DATA L( 82) /  263408793 /
      DATA L( 83) /  267802728 /
      DATA L( 84) /  269300000 /
      DATA L( 85) /  274500875 /
      DATA L( 86) /  273608910 /
      DATA L( 87) /  296813851 /
      DATA L( 88) /  305706944 /
      DATA L( 89) /  306304190 /
      DATA L( 90) /  305813153 /
      DATA L( 91) /  307002682 /
      DATA L( 92) /  316504527 /
      DATA L( 93) /  317200000 /
      DATA L( 94) /  317204042 /
      DATA L( 95) /  318100000 /
      DATA L( 96) /  318103165 /
      DATA L( 97) /  318106674 /
      DATA L( 98) /  349909002 /
      DATA L( 99) /  380411921 /
      DATA L(100) /  390214396 /
      DATA L(101) /  390214722 /
      DATA L(102) / 1543907085 /
      DATA L(103) /  451908748 /
      DATA L(104) /  424009316 /
      DATA L(105) /  215300000 /
      DATA L(106) / 1543504185 /
      DATA L(107) /  430211318 /
      DATA L(108) /  430219107 /
      DATA L(109) /  430411318 /
      DATA L(110) /  430419107 /
      DATA L(111) /  431200000 /
      DATA L(112) / 1542616285 /
      DATA L(113) /  430906959 /
      DATA L(114) / 1483511710 /
      DATA L(115) /  442014580 /
      DATA L(116) /  470317741 /
      DATA L(117) / 1618702916 /
      DATA L(118) /  471301278 /
      DATA L(119) /  480013566 /
      DATA L(120) /  195908132 /
      DATA L(121) /  652311914 /
      DATA L(122) /  492308829 /
      DATA L(123) /  495308748 /
      DATA L(124) /  514309504 /
      DATA L(125) /  525204136 /
      DATA L(126) /  587709896 /
      DATA L(127) /  592814108 /
      DATA L(128) /  598509740 /
      DATA L(129) /  608013167 /
      DATA L(130) /  609414992 /
      DATA L(131) /  635410463 /
      DATA L(132) /  643514184 /
      DATA L(133) /  643514376 /
      DATA L(134) /  643514384 /
      DATA L(135) /  643514214 /
      DATA L(136) /  643514211 /
      DATA L(137) /  643514106 /
      DATA L(138) /  479711077 /
      DATA L(139) /  694313883 /
      DATA L(140) /  597914619 /
      DATA L(141) /  695904132 /
      DATA L(142) /  695904147 /
      DATA L(143) /  695907695 /
      DATA L(144) /  696104151 /
      DATA L(145) / 1138614835 /
      DATA L(146) /  708908901 /
      DATA L(147) /  710613275 /
      DATA L(148) /  808211318 /
      DATA L(149) /  808219107 /
      DATA L(150) /  808411318 /
      DATA L(151) /  808419107 /
      DATA L(152) /  817806980 /
      DATA L(153) /  817807268 /
      DATA L(154) /  817808438 /
      DATA L(155) /  817808726 /
      DATA L(156) /  877703969 /
      DATA L(157) /  878215462 /
      DATA L(158) /  879304637 /
      DATA L(159) /  889004027 /
      DATA L(160) /  889705651 /
      DATA L(161) /  901014580 /
      DATA L(162) /  915601053 /
      DATA L(163) /  916000000 /
      DATA L(164) /  916003645 /
      DATA L(165) /  916014823 /
      DATA L(166) /  950803069 /
      DATA L(167) /  624902916 /
      DATA L(168) /  952800000 /
      DATA L(169) /  951514107 /
      DATA L(170) /  951509734 /
      DATA L(171) /  959004631 /
      DATA L(172) /  959400933 /
      DATA L(173) /  961606602 /
      DATA L(174) /  961904023 /
      DATA L(175) / 1004816300 /
      DATA L(176) /  972404043 /
      DATA L(177) /  973400000 /
      DATA L(178) /  973406933 /
      DATA L(179) /  973416191 /
      DATA L(180) /  979211318 /
      DATA L(181) /  980201605 /
      DATA L(182) /  982915179 /
      DATA L(183) /  965118765 /
      DATA L(184) /  966710395 /
      DATA L(185) /  966710395 /
      DATA L(186) /  989416578 /
      DATA L(187) /  990014985 /
      DATA L(188) /  472314580 /
      DATA L(189) /  961904042 /
      DATA L(190) /  969404239 /
      DATA L(191) / 1000101054 /
      DATA L(192) / 1003500875 /
      DATA L(193) / 1003500875 /
      DATA L(194) / 1003501126 /
      DATA L(195) / 1003503943 /
      DATA L(196) /  514405103 /
      DATA L(197) /  514405103 /
      DATA L(198) / 1007602304 /
      DATA L(199) / 1007602539 /
      DATA L(200) /  987608748 /
      DATA L(201) / 1030309173 /
      DATA L(202) / 1034803902 /
      DATA L(203) / 1034804312 /
      DATA L(204) / 1043114406 /
      DATA L(205) / 1062909802 /
      DATA L(206) / 1065011475 /
      DATA L(207) / 1074701787 /
      DATA L(208) / 1499800000 /
      DATA L(209) / 1129514580 /
      DATA L(210) /  379409505 /
      DATA L(211) / 1142504023 /
      DATA L(212) / 1170912165 /
      DATA L(213) / 1170914431 /
      DATA L(214) /  306304955 /
      DATA L(215) / 1181702336 /
      DATA L(216) / 1543504185 /
      DATA L(217) /  307914998 /
      DATA L(218) /  308411440 /
      DATA L(219) /   89306889 /
      DATA L(220) / 1208118774 /
      DATA L(221) / 1216503494 /
      DATA L(222) / 1137413153 /
      DATA L(223) / 1216512087 /
      DATA L(224) /  586416038 /
      DATA L(225) / 1316305238 /
      DATA L(226) / 1316305103 /
      DATA L(227) / 1326007011 /
      DATA L(228) / 1326011048 /
      DATA L(229) / 1327011480 /
      DATA L(230) / 1543504185 /
      DATA L(231) / 1327308778 /
      DATA L(232) / 1327615003 /
      DATA L(233) / 1327700986 /
      DATA L(234) / 1348400000 /
      DATA L(235) /   90915691 /
      DATA L(236) / 1315408173 /
      DATA L(237) / 1534105252 /
      DATA L(238) / 1393310206 /
      DATA L(239) / 1540513613 /
      DATA L(240) / 1396804541 /
      DATA L(241) / 1396816155 /
      DATA L(242) / 1426105103 /
      DATA L(243) / 1607008937 /
      DATA L(244) /  306300000 /
      DATA L(245) / 1388314580 /
      DATA L(246) / 1585809181 /
      DATA L(247) /  479708442 /
      DATA L(248) / 1409400000 /
      DATA L(249) / 1410800000 /
      DATA L(250) / 1410805103 /
      DATA L(251) / 1410805832 /
      DATA L(252) / 1410806959 /
      DATA L(253) / 1566505252 /
      DATA L(254) /  915813851 /
      DATA L(255) / 1544910558 /
      DATA L(256) / 1427414733 /
      DATA L(257) / 1370200000 /
      DATA L(258) / 1648509443 /
      DATA L(259) /  861403403 /
      DATA L(260) /  861403407 /
      DATA L(261) / 1438401278 /
      DATA L(262) / 1439214842 /
      DATA L(263) / 1439215024 /
      DATA L(264) / 1439503073 /
      DATA L(265) / 1586409505 /
      DATA L(266) / 1506703942 /
      DATA L(267) / 1506703942 /
      DATA L(268) / 1443109630 /
      DATA L(269) / 1460908883 /
      DATA L(270) / 1462100000 /
      DATA L(271) / 1462105103 /
      DATA L(272) / 1462105832 /
      DATA L(273) / 1462303645 /
      DATA L(274) / 1466903724 /
      DATA L(275) / 1473309734 /
      DATA L(276) / 1521612003 /
      DATA L(277) / 1499800760 /
      DATA L(278) / 1539803724 /
      DATA L(279) / 1569614580 /
      DATA L(280) / 1620914580 /
      DATA L(281) / 1648509429 /
      DATA L(282) / 1701414796 /
C
C     ..................................................................
C
C     (3)   COMMANDS EXECUTED BY OMNIT IN NL(.).
C
      DATA LO( 1), LO( 2), LO( 3), LO( 4), LO( 5), LO( 6), LO( 7) /
     1      11300,   7102,  14196,  14580,  14392,  10333,  10631 /
      DATA LO( 8), LO( 9), LO(10), LO(11), LO(12), LO(13), LO(14) /
     1       4131,  14843,   3969,   4797,   9524,  15091,   8019 /
      DATA LO(15), LO(16), LO(17), LO(18), LO(19) /
     1       1613,   3848,   3994,   3993,   9477 /
C
C     ..................................................................
C
C     (4)   RESET, PRINT, ETC. IN ID(.) 
C
      DATA LD( 1), LD( 2) /  525203173, 1509108019 /
      DATA LD( 3), LD( 4) / 1509108590,  878013851 /
      DATA LD( 5), LD( 6) /   80106676,  128718522 /
      DATA LD( 7), LD( 8) / 1003518522, 1076418522 /
C
C     ..................................................................
C
C     (5)   TWO-WORD COMMANDS IN IRD(.).
C
      DATA LW( 1,1), LW( 1,2) / 1035100000,  901014580 /
      DATA LW( 2,1), LW( 2,2) / 1506703942, 1389109626 /
      DATA LW( 3,1), LW( 3,2) / 1088100000, 1409803645 /
      DATA LW( 4,1), LW( 4,2) /  947700000, 1814400000 /
      DATA LW( 5,1), LW( 5,2) /  947700000, 1749600000 /
      DATA LW( 6,1), LW( 6,2) /  947700000, 1754700000 /
      DATA LW( 7,1), LW( 7,2) /  947700000,   83700000 /
      DATA LW( 8,1), LW( 8,2) /  947700000,  294300000 /
      DATA LW( 9,1), LW( 9,2) /  947700000,  132300000 /
      DATA LW(10,1), LW(10,2) /  947700000, 1603800000 /
      DATA LW(11,1), LW(11,2) /  398107013,  478413655 /
      DATA LW(12,1), LW(12,2) /  398107013,  111104023 /
      DATA LW(13,1), LW(13,2) / 1409803645,   89306889 /
      DATA LW(14,1), LW(14,2) / 1440915908, 1078500000 /
      DATA LW(15,1), LW(15,2) / 1440915908, 1131800000 /
      DATA LW(16,1), LW(16,2) / 1315408321, 1443100000 /
      DATA LW(17,1), LW(17,2) / 1078500000,  174215645 /
      DATA LW(18,1), LW(18,2) / 1078504131,  174310341 /
      DATA LW(19,1), LW(19,2) /  515114364,  858703403 /
      DATA LW(20,1), LW(20,2) / 1439609477,  888404374 /
      DATA LW(21,1), LW(21,2) / 1438403996,  888404374 /
      DATA LW(22,1), LW(22,2) /  437400000, 1389203454 /
      DATA LW(23,1), LW(23,2) / 1429507101, 1200314580 /
      DATA LW(24,1), LW(24,2) / 1045203645, 1200314580 /
      DATA LW(25,1), LW(25,2) / 1045203645, 1065011475 /
      DATA LW(26,1), LW(26,2) / 1045203645,  263111475 /
      DATA LW(27,1), LW(27,2) / 1045203645, 1030309173 /
      DATA LW(28,1), LW(28,2) / 1389111993, 1703006318 /
      DATA LW(29,1), LW(29,2) / 1389111993, 1703006258 /
      DATA LW(30,1), LW(30,2) / 1472615081, 1200314580 /
      DATA LW(31,1), LW(31,2) / 1472615081,  138613851 /
      DATA LW(32,1), LW(32,2) / 1472615081, 1473309734 /
      DATA LW(33,1), LW(33,2) /  879404131, 1200314580 /
      DATA LW(34,1), LW(34,2) /  879404131,  138613851 /
      DATA LW(35,1), LW(35,2) /  879404131, 1473309734 /
C
C     ..................................................................
C
C     (6)   UNIT COMMANDS IN ITP(.).
C
      DATA LU( 1), LU( 2) / 1325802916,  267800837 /
      DATA LU( 3), LU( 4) / 1726214715, 1400600000 /
      DATA LU( 5), LU( 6) /  270514580,  402704629 /
      DATA LU( 7), LU( 8) / 1328006943, 1415711664 /
      DATA LU( 9), LU(10) /  148808548,          0 /
C
C     ..................................................................
C
C     (7)   CENSOR XX IN ICP(.).
C
      DATA LCXX(1), LCXX(2), LCXX(3), LCXX(4), LCXX(5), LCXX(6)/
     1        8883,    4104,    5238,    5643,    9288,   10341/
C
C     ..................................................................
C
C     (8)   CALCOMP IN ICL(.).
C
      DATA LC( 1), LC( 2) / 1462303645, 1428803753 /
      DATA LC( 3), LC( 4) / 1412003645, 1200314580 /
      DATA LC( 5), LC( 6) / 1419016767,  442014580 /
      DATA LC( 7), LC( 8) / 1170704131,  138613851 /
C
C     ..................................................................
C
C     (9)   DISTRIBUTIONS IN IDIST(.).
C
      DATA LF( 1), LF( 2) / 1062909516,  916010629 /
      DATA LF( 3), LF( 4) /  587104767, 1458000000 /
      DATA LF( 5), LF( 6) /  241214331,  514309504 /
      DATA LF( 7), LF( 8) /  437400000,  161300729 /
      DATA LF( 9), LF(10) / 1433810400,  404200000 /
      DATA LF(11), LF(12) /  223502428,  878801567 /
      DATA LF(13), LF(14) /  951509734, 1691102037 /
      DATA LF(15), LF(16) / 1317408892,  431212083 /
      DATA LF(17), LF(18) /  307512083,  916007094 /
      DATA LF(19), LF(20) / 1030700000, 1029006094 /
      DATA LF(21), LF(22) / 1029300000,  161110632 /
      DATA LF(23), LF(24) /  171511295, 1034801715 /
      DATA LF(25), LF(26) / 1207814379,  525309632 /
      DATA LF(27), LF(28) /  652304138,  317802678 /
      DATA LF(29), LF(30) / 1171002226, 1005614837 /
C
C     ..................................................................
C
C     (10)   PROPERTIES OF DISTRIBUTIONS IN IPROP(.).
C
      DATA LP( 1), LP( 2) /  306514114,  276715634 /
      DATA LP( 3), LP( 4) / 1181702336, 1483504406 /
      DATA LP( 5)         /   89306889             /
C
C     ..................................................................
C
C     (11)   TABLE MAKING IN ITB(.).
C
      DATA LT( 1), LT( 2) /  486512965, 1443100000 /
      DATA LT( 3), LT( 4) /  132813156, 1439503073 /
      DATA LT( 5), LT( 6) /  973406933,  952806933 /
      DATA LT( 7), LT( 8) / 1316305238,  961606602 /
      DATA LT( 9), LT(10) / 1181702336, 1216512087 /
      DATA LT(11), LT(12) / 1355913208,  262413208 /
      DATA LT(13), LT(14) / 1357211382,  263711382 /
C
C     ..................................................................
C
C     (13)   COLUMN IN ICOLHD(.).
C
      DATA LCH(1), LCH(2), LCH(3), LCH(4), LCH(5), LCH(6), LCH(7)/
     1        'S',    'O',    'E',    'J',    'L',    'E',    ' '/
C
C     ==================================================================
C
      CALL MVELNG (L, LO, LD, LW, LU, LCXX, LC, LF, LP, LT, LCH)
C
      RETURN
C
C     ==================================================================
C
      END 
*DASHLN
      SUBROUTINE DASHLN (X,Y,N,K)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. DASHLN V 7.00  5/15/90. **
C **  MODFIED JULY 1992.  SUPPORT NON-PROPRIETARY VERSON.  USE DASHS
C **  AND PLOT ROUTINES TO IMPLEMENT.  CURRENTLY ONLY SUPPORT ONE 
C**   DASH STYLE.
C
C     ==================================================================
C
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /TEKOPS/ ITEK(10,6), ITEKSW, TEKHGT, TEKXDH
C
      REAL ARRCAL(10)
      REAL X(*), Y(*)
C
      IF (N.LT.2)RETURN
      LINTYP=0
      ISPACE=32
      IF (ITEKSW.EQ.IZERO)THEN
        ARRCAL(1)=0.1
        ARRCAL(2)=0.1
        ICNT=2
        CALL DASHS(ARRCAL,ICNT)
        CALL LINE(X,Y,N,K,LINTYP,ISPACE)
        ARRCAL(1)=0.0
        ARRCAL(2)=0.0
        ICNT=0
        CALL DASHS(ARRCAL,ICNT)
C  FOR CALCOMP COMPATIBLE
       ELSE
        ARRCAL(1)=0.0
        ARRCAL(2)=0.0
        ICNT=1
        CALL DASHST(ARRCAL,ICNT)
        CALL LINET(X,Y,N,K,LINTYP,ISPACE)
        ICNT=0
        CALL DASHST(ARRCAL,ICNT)
       ENDIF
C
      RETURN
      END
*DATA1
      BLOCK  DATA   DATA1
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  DATA1 V 7.00  4/28/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SET CONSTANTS WHICH MAY HAVE TO BE CHANGED BECAUSE ...
C
C        (1) LOGICAL INPUT OR OUTPUT UNITS ARE REASSIGNED.
C        (2) CONSTANTS USED IN PRINTING ARE CHANGED.
C        (3) NUMBER OF DECIMAL DIGITS IN REAL NUMBER IS DIFFERENT.
C        (4) NORMAL WIDTH OF CARD (STATEMENT) SCANNED IS CHANGED.
C        (5) SIZE OF WORKSHEET AND SCRATCH AREA IS CHANGED.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -
C                   CURRENT VERSION - APRIL, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT
      COMMON /IOUNIT/ LPTAPE
      COMMON /PRTCTS/ IPLACE, NCHTIT(4), NLENGT, NLSWT, NSIGD, NCWIDE
      COMMON /SCNLCD/ LENCRD, LKARD, KARD(83), KRDEND     
C
C     ==================================================================
C
C                    ***   TYPE STATEMENTS   ***
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***

C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
C     (1)   THE FOLLOWING SEVEN DATA STATEMENTS DEFINE LOGICAL UNITS
C
C         INUNIT = LOGICAL INPUT UNIT  - USUALLY CARD READER
C          NPRNT = LOGICAL OUTPUT UNIT - FOR BATCH OR TERMINAL USE
C          MPRNT = LOGICAL OUTPUT UNIT - FOR PRINTER WHEN USING TERMINAL
C         IPUNCH = LOGICAL CARD PUNCH UNIT
C          ISCRT = LOGICAL AUXILIARY (TEMPORARY) OUTPUT UNIT
C         KBDOUT = LOGICAL ON-LINE UNIT OR KEYBOARD
C         LPTAPE = LOGICAL UNIT OR TAPE TO STORE INTERMIDIATE OUTPUT FOR
C                  PLOTTING ON DIFFERENT DEVICES. (I.E. CALCOM,
C                  TEKTRONIX, ZETA, VERSATEC, ETC.).
C         LTAPE  = FIRST LOGICAL UNIT TO BE USED WITH OMNITAB COMMANDS
C                     READ  TAPE
C                     READ  UNIT
C                     WRITE TAPE
C                     WRITE UNIT
C            THE LOGICAL UNIT NUMBER AND  SUCCEEDING SIX (6) LOGICAL
C            UNIT NUMBERS ARE RESERVED FOR LTAPE.  (I.E. TAPE OR UNIT
C            HAS QUALIFIER  A THRU F.  QUALIFIER  A IS AS SIGNED  TO
C            LOGICAL UNIT LTAPE, AND QUALIFIER  F  TO LTAPE+6.
C
      DATA INUNIT /  5 /
      DATA  NPRNT /  6 /
      DATA  MPRNT / 48 /
      DATA IPUNCH /  1 /
      DATA  ISCRT / 45 /
      DATA KBDOUT /  6 /
      DATA LPTAPE /  9 /
      DATA LTAPE  /  7 /
C
C     ..................................................................
C
C     (2)   THE FOLLOWING DATA STATEMENT DEFINES IPLACE, THE MAXIMUM
C              WIDTH OF PRINTED COLUMNS.
C
      DATA IPLACE / 15 /
C
C           THE FOLLOWING DATA STATEMENTS DEFINES NLENGT AND NCWIDE,
C              THE MAXIMUM LENGTH AND WIDTH
C              OF A PRINTED PAGE FOR INTERACTIVE USE.
C
      DATA NLENGT / 18 /
      DATA NCWIDE / 72 /
C
C     ..................................................................
C
C     (3)   THE FOLLOWING DATA STATEMENT DEFINES NSIGD, THE NUMBER OF
C              DECIMAL DIGITS IN A REAL NUMBER IN THE COMPUTER.
C
C                 NSIGD =  7, FOR A 32 BIT WORD COMPUTER (IBM)
C                       =  8, FOR A 36 BIT WORD COMPUTER (UNIVAC)
C                       = 10, FOR A 48 BIT WORD COMPUTER (BURROUGHS)
C                       = 13, FOR A 60 BIT WORD COMPUTER (CDC).
C
C                 CAUTION.  NSIGD MUST BE SMALL ENOUGH SO THAT
C                    10**(NSIGD+1) IS A VALID MACHINE INTEGER.
C                    (THIS IS WHY NSIGD EQUALS 13 AND NOT 14 FOR A
C                       60 BIT WORD COMPUTER.)
C
      DATA NSIGD / 8 /
C
C     ..................................................................
C
C     (4)   THE FOLLOWING DATA STATEMENT DEFINES LENCRD, THE NORMAL
C              LENGTH OF CARD (OR STATEMENT) ALLOWED.
C
C              CAUTION.  AT THIS TIME THE VALUE OF LENCRD SHOULD
C                 NOT BE CHANGED.
C
      DATA LENCRD / 80 /
C
C     ==================================================================
C
      END
*DATA2    
      BLOCK  DATA DATA2       
C         
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  DATA2 V 7.00  2/26/91. **        
C         
C     ==================================================================        
C         
C                        ***   GENERAL COMMENTS   ***       
C         
C     THIS BLOCK DATA PROCEDURE SETS VALUES FOR LABELED COMMON /PRHEAD/,        
C        WHICH CONTAINS VARIABLES NEEDED FOR STORING HEADINGS, LABELS 
C           AND FORMATS.      
C         
C     CHANGES MAY BE NECESSARY AT TIME OF IMPLEMENTATION, IF
C          THE NUMBER OF HEADINGS WHICH CAN BE STORED IS CHANGED.   
C         
C     THE BLOCK DATA PROCEDURE ALSO SETS THE VALUE FOR KMES IN        
C        LABELED COMMON /ERRMES/.  IF THE VALUE OF KMES IS CHANGED,   
C           THE DIMENSION OF MESS(.) MUST ALSO BE CHANGED.  
C         
C     ==================================================================        
C         
C                    ***   SPECIFICATION STATEMENTS   ***   
C         
      COMMON /ERRMES/ ISE, KMES, LLIST, MESS(15), MNOE, NERR, NRM, NROLD        
      COMMON /PRHEAD/ IHEAD(6,50), NHEADS         
C         
C     IFMT CONTAINS THE INFORMATION FOR  6 FORMATS (I.E.,   
C        FORMAT A THROUGH F).  THE MAXIMUM LENGTH FOR EACH FORMAT IS  
C        80 CHARACTERS INCLUDING LEFT AND RIGHT PARENTHESES.
C         
C     IHEAD CONTAINS THE HEADINGS FOR (NHEADS) COLUMNS.     
C        THE MAXIMUM NUMBER OF CHARACTERS PER HEADING IS 12.
C         
C     ==================================================================        
C         
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C         
C        KMES IS THE MAXIMUM NUMBER OF TYPES OF ARITHMETIC FAULTS     
C           ALLOWED.
C         
      DATA KMES / 15 /        
C         
C        NHEADS = NUMBER OF HEADINGS OR LABELS WHICH CAN BE STORED.   
C         
C           IF NHEADS IS REDEFINED, THEN ARRAY IHEAD (6,50) IN LABELED
C           COMMON /PRHEAD/ MUST BE REDIMENSIONED AS IHEAD(6,LHTP),   
C           WHERE LHTP IS NUMERIC VALUE ASSIGNED TO VARIABLE NHEADS.  
C         
      DATA NHEADS / 50 /      
C         
C     ==================================================================        
C         
      END 
*DATA3
      BLOCK DATA DATA3
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  DATA3 V 7.00  2/20/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THESE CONSTANTS MAY HAVE TO BE CHANGED FOR OTHER COMPUTERS OR
C        LIBRARY ROUTINES.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /CONSTS/ DEG, E, HALFPI, PI, RAD
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /DMCONS/ DMAXDP, DMXINT, DSNCOS, DXEXP
      COMMON /DPICON/ DHLFPI, DPI, DSQRPI, D2BYSP
      COMMON /DTCONS/ DALOG2, DEULER
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /IMCONS/ IEXP, MMXINT, MXINT, NBC, NBM, NSBB
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
      COMMON /SLCONS/ MXLIN, MXWDTH
C
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DHLFPI, DPI, DSQRPI, D2BYSP
      DOUBLE PRECISION DMAXDP, DMXINT, DSNCOS, DXEXP
      DOUBLE PRECISION DALOG2, DEULER
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
C     (1)   CONSTANTS FOR LABELED COMMON CONSTS -
C
C     THIS BLOCK DEFINES CONSTANTS TO BE USED THROUGHOUT OMNITAB WHOSE
C        VALUE (ACCURACY) MAY HAVE TO BE CHANGED FOR OTHER COMPUTERS.
C
C           DEG    = 57.2957795    (NUMBER OF DEGREES IN ONE RADIAN)
C           E      =  2.71821818   (BASE OF NATURAL LOGS)
C           HALFPI =  1.5707963    (VALUE OF PI/2)
C           PI     =  3.14159265   (VALUE OF PI)
C           RAD    =  0.0174532925 (NUMBER OF RADIANS IN ONE DEGREE)
C
      DATA DEG    / 57.2957795     /
      DATA E      /  2.7182818     /
      DATA HALFPI /  1.5707963     /
      DATA PI     /  3.14159265    /
      DATA RAD    /  1.74532925E-2 /
C
C     ..................................................................
C
C     (2)   CONSTANTS FOR LABELED COMMON DCONST -
C
C     FREQUENTLY USED DOUBLE PRECISION CONSTANTS.
C
      DATA DEHT,  DFOR, DHALF,  DONE,  DSIX, DTHRE,  DTWO, DZERO /
     1    8.0D0, 4.0D0, 0.5D0, 1.0D0, 6.0D0, 3.0D0, 2.0D0, 0.0D0 /
C
C     ..................................................................
C
C     (3)   CONSTANTS FOR LABELED COMMON DMCONS -
C
C        DMAXDP IS THE LARGEST MACHINE DOUBLE PRECISION VALUE
C
C        DMXINT IS THE LARGEST MANTISSA OF A REAL EXPRESSED AS A
C           DOUBLE PRECISION NUMBER.  VALUE SET IN SETUP.
C
C        DSNCOS IS USED BY DOUBLE PRECISION SIN AND COS FUNCTIONS TO
C           TRAP WHEN ARGUMENT BECOMES TOO LARGE.
C
C        DXEXP  IS USED BY FDEXP FUNCTION TO TRAP WHEN ARGUMENT
C           BECOMES TOO LARGE.
C
      DATA DMAXDP / 1.7D38 /
      DATA DSNCOS / 0.7205759403792794D17  /
      DATA  DXEXP /               704.0D0  /
C
C     ..................................................................
C
C     (4)   CONSTANTS FOR LABELED COMMON DPICON -
C
C      DOUBLE PRECISION CONSTANTS USING PI.
C
C         DHLFPI = DOUBLE PRECISION VALUE P OF PI/2.0
C            DPI = DOUBLE PRECISION VALUE OF PI
C         DSQRPI = DOUBLE PRECISION VALUE OF SQUARE ROOT OF PI
C         D2BYSP = DOUBLE PRECISION VALUE OF 2.0/SQUARE ROOT (PI)
C
C         NUMBER OF DIGITS MAY HAVE TO BE CHANGED.
C
      DATA DHLFPI / 1.570796326794897D0 /
      DATA    DPI / 3.141592653589793D0 /
      DATA DSQRPI / 1.772453850905516D0 /
      DATA D2BYSP / 1.128379167095513D0 /
C
C     ..................................................................
C
C     (5)   CONSTANTS FOR LABELED COMMON DTCONS -
C
C        DALOG2  = LOG BASE 10 0F 2 IN DOUBLE PRECISION
C        DEULER  = EULER VALUE IN DOUBLE PRECISION
C                   IN DOUBLE PRECISION.
C
      DATA DALOG2 / 0.6931471805599453D0 /
      DATA DEULER / 0.5772156649015329D0 /
C
C     ..................................................................
C
C     (6)   CONSTANTS FOR LABELED COMMON ICONST -
C
C     FREQUENTLY USED INTEGER CONSTANTS.
C
      DATA IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO /
     1         5,     4,  100,    1,   10,     3,    2,     0 /
C
C     ..................................................................
C
C     (7)   CONSTANTS FOR LABELED COMMON IMCONS -
C
C     MACHINE INTEGER CONSTANTS.
C
C         IEXP   IS USED BY SUBROUTINE FEXP2 FOR SELECTING COMPUTATIONAL
C                   METHOD.  IT MAY BE DESIRABLE, BUT NOT NECESSARY, FOR
C                   GREATEST EFFICIENCY, TO CHANGE ITS VALUE.
C
C         MMXINT IS MANTISSA OF LARGEST REAL (2**27-1).
C
C         MXINT  IS LARGEST MACHINE INTEGER (2**35 - 1).
C
C         NBC    IS USED BY SUBROUTINE ERRINT AND IS THE NUMBER OF BINARY
C                   BITS IN THE CHARACTERISTIC OF A DOUBLE PRECISION
C                   NUMBER.
C
C         NBM    IS USED BY SUBROUTINE ERRINT AND IS THE NUMBER OF BINARY
C                   BITS IN THE MANTISSA OF A DOUBLE PRECISION NUMBER.
C
C         NSBB   IS THE NUMBER OF SIGNIFICANT BINARY BITS IN THE MANTISSA
C                   OF A REAL NUMBER (I.E., FLOATING NO.).
C
C
      DATA   IEXP / 55 /
      DATA MMXINT /   134217727 /
      DATA  MXINT / 2147483647  /
      DATA    NBC /  8 /
      DATA    NBM / 55 /
      DATA   NSBB / 27 /
C
C     ..................................................................
C
C     (8)   CONSTANTS FOR LABELED COMMON RCONST -
C
C     FREQUENTLY USED REAL CONSTANTS.
C
      DATA RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO /
     1       5.0,  4.0,   0.5,  1.0, 10.0,   3.0,  2.0,   0.0 /
C
C     ..................................................................
C
C     (9)   CONSTANTS FOR LABELED COMMON RMCONS -
C
C        RALOG  IS THE EXPONENT UPPER BOUND.
C
C        RER   IS USED TO CHECK VALUES CLOSE TO ZERO.
C
C        REXP   IS USED BY FEXP FUNCION IN ORDER TO TRAP WHEN ARGUMENT
C                     BECOMES TOO LARGE.
C
C        RMIFY  IS THE SMALLEST REAL NUMBER. (-INFINITY)
C
C        RMXINT IS THE LARGEST MANTISSA OF A REAL, SET IN SETUP.
C
C        RPIFY  IS THE VALUE OF LARGEST REAL NUMBER. (+ INFINITY)
C
C        RSD    IS THE MAXIMUM NUMBER OF SIGNIGICANT DIGITS IN A REAL
C                     NUMBER.  THE VALUE OF RSD IS SET IN SETUP.
C
C        RTRG   IS USED BY FSIN AND FCOS FUNCTIONS IN ORDER TO TRAP
C                     WHEN ARGUMENT BECOMES TOO LARGE.
C
      DATA RALOG / 38.0       /
      DATA RER   /  1.0E-8    /
      DATA REXP  / 88.0       /
      DATA RMIFY / -1.0E37    /
      DATA RPIFY /  1.0E38    /
      DATA RTRG  / 2.097152E6 /
C
C    ..................................................................
C
C    (10)   CONSTANTS FOR LABELED COMMON SLCONS -
C
C     MXLIN = MAXIMUM NUMBER OF LINES PRINTED FOR STEM
C               AND LEAVES WHEN ICASE = 0
C
C    MXWDTH = MAXIMUM FIELD WIDTH FOR CHARACTER REPRESENTATION OF A
C                NUMBER.
C
      DATA  MXLIN / 99 /
      DATA MXWDTH / 19 /
C
C    ==================================================================
C
      END
*DATA4
      BLOCK  DATA DATA4
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  DATA4 V 7.00  2/20/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     BLOCK DATA PROCEDURE FOR PHYSICAL CONSTANTS
C        (THEIR VALUES AND NUMBER REPRESENTATION).
C
C     NO CHANGES ARE NECESSARY AT TIME OF IMPLEMENTATION.  EXCEPT THE
C        CONSTANTS SHOULD HAVE DIGITS ADDED FOR COMPUTERS WITH
C           MORE THAN 36 BITS PER WORD AND
C        CONSTANTS SHOULD BE ROUNDED TO FEWER DIGITS FOR COMPUTERS WITH
C           LESS THAN 36 BITS PER WORD.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /PCONST/ PC(40), JPC, NT(40)
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
C            PCONST  DEFINES PHYSICAL CONSTANT VALUES
C
C                           SI UNITS                   CGS UNITS
C
C        PI        PC( 1) = 3.1415926535898   PC( 2) = 3.1415926535898
C        E         PC( 3) = 2.7182818284590   PC( 4) = 2.7182818284590
C        C         PC( 5) = 2.997925E8        PC( 6) = 2.997925E10
C        Q         PC( 7) = 1.60210E-19       PC( 8) = 1.60210E-20
C        N         PC( 9) = 6.02252E23        PC(10) = 6.02252E23
C        ME        PC(11) = 9.1091E-31        PC(12) = 9.1091E-28
C        MP        PC(13) = 1.67252E-27       PC(14) = 1.67252E-24
C        F         PC(15) = 9.64870E4         PC(16) = 9648.70
C        H         PC(17) = 6.6256E-34        PC(18) = 6.6256E-27
C        ALPHA     PC(19) = 7.29720E-3        PC(20) = 7.29720E-3
C        QME       PC(21) = 1.758796E11       PC(22) = 17587960.
C        RINF      PC(23) = 10973731.         PC(24) = 109737.31
C        GAMMA     PC(25) = 2.67519E8         PC(26) = 26751.9
C        MUB       PC(27) = 9.2732E-24        PC(28) = 9.2732E-21
C        R         PC(29) = 8.3143            PC(30) = 8.3143E7
C        K         PC(31) = 1.38054E-23       PC(32) = 1.38054E-16
C        CONE      PC(33) = 3.7415E-16        PC(34) = 3.7415E-5
C        CTWO      PC(35) = 1.43879E-2        PC(36) = 1.43879
C        SIGMA     PC(37) = 5.6697E-8         PC(38) = 5.6697E-5
C        G         PC(39) = 6.670E-11         PC(40) = 6.670E-8
C
      DATA PC( 1), PC( 2), PC( 3), PC( 4), PC( 5), PC( 6) /
     1    3.1415926535898,
     2    3.1415926535898,
     3    2.7182818284590,
     4    2.7182818284590,
     5    2.997925E8,
     6    2.997925E10/
C
      DATA PC( 7), PC( 8), PC( 9), PC(10), PC(11), PC(12) /
     1    1.60210E-19,
     2    1.60210E-20,
     3    6.02252E23,
     4    6.02252E23,
     5    9.1091E-31,
     6    9.1091E-28/
C
      DATA PC(13), PC(14), PC(15), PC(16), PC(17), PC(18) /
     1    1.67252E-27,
     2    1.67252E-24,
     3    9.64870E4,
     4    9648.70,
     5    6.6256E-34,
     6    6.6256E-27/
C
      DATA PC(19), PC(20), PC(21), PC(22), PC(23), PC(24) /
     1    7.29720E-3,
     2    7.29720E-3,
     3    1.758796E11,
     4    17587960.,
     5    10973731.,
     6    109737.31/
C
      DATA PC(25), PC(26), PC(27), PC(28), PC(29), PC(30) /
     1    2.67519E8,
     2    26751.9,
     3    9.2732E-24,
     4    9.2732E-21,
     5    8.3143,
     6    8.3143E7/
C
      DATA PC(31), PC(32), PC(33), PC(34), PC(35), PC(36) /
     1    1.38054E-23,
     2    1.38054E-16,
     3    3.7415E-16,
     4    3.7415E-5,
     5    1.43879E-2,
     6    1.43879/
C
      DATA PC(37), PC(38), PC(39), PC(40)/
     1    5.6697E-8,
     2    5.6697E-5,
     3    6.670E-11,
     4    6.670E-8/
C
C     ..................................................................
C
C     THE VALUE OF JPC IS SET BY THE XOMNIT AND PHYCON SUBROUTINES.
C
C     ..................................................................
C
C        PHYSICAL CONSTANTS INTEGER REPRESENTATION.
C
C NT (1) = 11907 = PI    (PI)
C NT (2) =  3645 = E     (BASE OF NATURAL LOGS)
C NT (3) =  2187 = C     (SPEED OF LIGHT IN VACUUM)
C NT (4) = 12393 = Q     (ELEMENTARY CHARGE)
C NT (5) = 10206 = N     (AVOGADRO CONSTANT)
C NT (6) =  9612 = ME    (ELECTRON REST MASS)
C NT (7) =  9909 = MP    (PROTON REST MASS)
C NT (8) =  4374 = F     (FARADAY CONSTANT)
C NT (9) =  5832 = H     (PLANCK CONSTANT)
C NT(10) =  1069 = ALPHA (FINE STRUCTURE CONSTANT)
C NT(11) = 12749 = QME   (CHARGE TO MASS RATIO FOR ELECTRON)
C NT(12) = 13379 = RINF  (RYDBERG CONSTANT)
C NT(13) =  5143 = GAMMA (PROTON GROMAGNETIC RATIO-CORRECTED FOR H20)
C NT(14) = 10046 = MUB   (BOHR MAGNETON)
C NT(15) = 13122 = R     (GAS CONSTANT)
C NT(16) =  8019 = K     (BOLTZMANN CONSTANT)
C NT(17) =  2606 = CONE  (FIRST RADIATION CONSTANT)
C NT(18) =  2750 = CTWO  (SECOND RADIATION CONSTANT)
C NT(19) = 14101 = SIGMA (STEPHAN-BOLTZMANN CONSTANT
C NT(20) =  5103 = G     (GRAVITATIONAL CONSTANT)
C
      DATA NT( 1), NT( 2), NT( 3), NT( 4), NT( 5),
     1     NT( 6), NT( 7), NT( 8), NT( 9), NT(10)/
     2      11907,   3645,   2187,  12393,  10206,
     3       9612,   9909,   4374,   5832,   1069/
C
      DATA NT(11), NT(12), NT(13), NT(14), NT(15),
     1     NT(16), NT(17), NT(18), NT(19), NT(20)/
     2      12749, 13379,   5143,  10046,   13122,
     3       8019,  2606,   2750,  14101,    5103/
C
C     ==================================================================
C
      END
*DATA5
      BLOCK DATA DATA5
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  DATA5 V 7.00  2/20/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS BLOCK DATA PROCEDURE SETS VALUES FOR NAME(1), NAME(2),
C        NAME(3), NAME(4), L1 AND L2 FOR EVERY ONE-WORD COMMAND IN IR(.)
C
C     NO CHANGES ARE NECESSARY AT TIME OF IMPLEMENTATION.
C
C   THERE ARE 509 OMNITAB 1978 COMMANDS INCL ABBREVIATIONS AND SYNONYMS.
C
C    525 =  282-2 (280)    FOR ONE WORD COMMANDS IN IR*         (I)
C         + 1              FOR PRINT NOTE                       (II)
C         + 14             FOR COMMANDS EXEC. BY OMNIT IN NL(.) (III)
C         + 2*8 (16)       FOR ID(.)  , IALPH(.) AND NALPH(.)   (IV)
C         + 31+1 (32)      FOR TWO WORD COMMANDS IN IRD(.)**    (V)
C         + 2*(9+3) (24)   FOR TAPE IN ITP(.)                   (VI)
C         + 6              FOR CENSOR IN ICP(.)                 (VII)
C         + 8              FOR CALCOMP IN ICL(.)                (VIII)
C         + 17+21+19+21
C             +14 (92)     FOR DIST. IN IDIST AND IPROP***      (IX),(X)
C         + 2*14 (28)      FOR TABLE ITB(.)                     (XI)
C         + 14             FOR LANGUAGES IN IL(.)               (XII)
C
C     *   NOTE - TABLE AND NTABLE ARE COUNTED IN (XI).
C     **  NOTE - M(X'X) IS TREATED SEPARATELY.
C     *** NOTE - NOT ALL COMBINATIONS OF IDIST AND IPROP ARE AVAILABLE.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARRAYA/ IR(282,2), NIRMID, NIRQTR, NIRTRD
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
C     (I)   NIR COMMANDS IN IR(I,J).
C
C       1. AADD      2. AAVERAG   3. ABS       4. ABSOLUT   5. ACCURAC
C       6. ACOALES   7. ACOS      8. ACOSD     9. ACOSH    10. ACOT
C      11. ACOTD    12. ACOTH    13. ADD      14. ADEFINE  15. ADIV
C      16. ADIVIDE  17. AERASE   18. ALABEL   19. AMOVE    20. AMULT
C      21. AMULTIP  22. ANTILOG  23. APROPER  24. ARAISE   25. ASIN
C      26. ASIND    27. ASINH    28. ASUB     29. ASUBTRA  30. ATAN
C      31. ATAND    32. ATANH    33. ATOMIC   34. ATRANSP  35. AVERAGE
C      36. AZERO    37. BEGIN    38. BESIN    39. BESJN    40. BESKN
C      41. BESTCP   42. BIONE    43. BIZERO   44. BJONE    45. BJZERO
C      46. BKONE    47. BKZERO   48. BOLDIST  49. BRIEF    50. BYONE
C      51. BYZERO   52. CADD     53. CDIVIDE  54. CEIONE   55. CEIZERO
C      56. CEKONE.  57. CEKZERO  58. CENSOR   59. CERF     60. CGS
C      61. CHANGE   62. CHOOSE   63. CIONE    64. CIZERO   65. CKONE
C      66. CKZERO   67. CMULTIP  68. CODE     69. COMPARE  70. CONTENT
C      71. CONTING  72. CORRELA  73. COS      74. COSD     75. COSH
C      76. COSINT   77. COT      78. COTD     79. COTH     80. COUNT
C      81. CPLOT    82. CPOLAR   83. CRECTAN  84. CRT      85. CSUBTRA
C      86. CTOF     87. DAYS     88. DEFINE   89. DELETE   90. DEMOTE
C      91. DESCRIB  92. DIFFERE  93. DIM      94. DIMENSI  95. DIV
C      96. DIVDIFF  97. DIVIDE   98. DUPLICA  99. EEXPINT 100. EINSTEI
C
      DATA IR(  1,1), IR(  1,2) /   76002916, 1804 /
      DATA IR(  2,1), IR(  2,2) /   77804132, 1810 /
      DATA IR(  3,1), IR(  3,2) /   80200000, 1231 /
      DATA IR(  4,1), IR(  4,2) /   80211280, 1231 /
      DATA IR(  5,1), IR(  5,2) /   81315796, 1106 /
      DATA IR(  6,1), IR(  6,2) /   82501058, 1809 /
      DATA IR(  7,1), IR(  7,2) /   82513851, 1206 /
      DATA IR(  8,1), IR(  8,2) /   82513959, 1214 /
      DATA IR(  9,1), IR(  9,2) /   82514067, 1228 /
      DATA IR( 10,1), IR( 10,2) /   82514580, 1208 /
      DATA IR( 11,1), IR( 11,2) /   82514688, 1216 /
      DATA IR( 12,1), IR( 12,2) /   82514796, 1230 /
      DATA IR( 13,1), IR( 13,2) /   84100000, 1101 /
      DATA IR( 14,1), IR( 14,2) /   84204631, 1501 /
      DATA IR( 15,1), IR( 15,2) /   84616038, 1807 /
      DATA IR( 16,1), IR( 16,2) /   84616285, 1807 /
      DATA IR( 17,1), IR( 17,2) /   88201247, 1502 /
      DATA IR( 18,1), IR( 18,2) /  105401605, 1604 /
      DATA IR( 19,1), IR( 19,2) /  109516173, 2306 /
      DATA IR( 20,1), IR( 20,2) /  110109288, 1806 /
      DATA IR( 21,1), IR( 21,2) /  110109297, 1806 /
      DATA IR( 22,1), IR( 22,2) /  112706900, 1222 /
      DATA IR( 23,1), IR( 23,2) /  117911372, 2702 /
      DATA IR( 24,1), IR( 24,2) /  121607079, 1808 /
      DATA IR( 25,1), IR( 25,2) /  125110206, 1205 /
      DATA IR( 26,1), IR( 26,2) /  125110314, 1213 /
      DATA IR( 27,1), IR( 27,2) /  125110422, 1227 /
      DATA IR( 28,1), IR( 28,2) /  126301458, 1805 /
      DATA IR( 29,1), IR( 29,2) /  126302016, 1805 /
      DATA IR( 30,1), IR( 30,2) /  127010206, 1207 /
      DATA IR( 31,1), IR( 31,2) /  127010314, 1215 /
      DATA IR( 32,1), IR( 32,2) /  127010422, 1229 /
      DATA IR( 33,1), IR( 33,2) /  128409723, 3103 /
      DATA IR( 34,1), IR( 34,2) /  128701126, 1803 /
      DATA IR( 35,1), IR( 35,2) /  132813156, 1711 /
      DATA IR( 36,1), IR( 36,2) /  143613527, 1502 /
      DATA IR( 37,1), IR( 37,2) /  160006939, 1401 /
      DATA IR( 38,1), IR( 38,2) /  161206939, 3038 /
      DATA IR( 39,1), IR( 39,2) /  161207668, 3032 /
      DATA IR( 40,1), IR( 40,2) /  161208397, 3039 /
      DATA IR( 41,1), IR( 41,2) /  161214677, 2206 /
      DATA IR( 42,1), IR( 42,2) /  171610341, 3006 /
      DATA IR( 43,1), IR( 43,2) /  172704146, 3005 /
      DATA IR( 44,1), IR( 44,2) /  174310341, 3002 /
      DATA IR( 45,1), IR( 45,2) /  175404146, 3001 /
      DATA IR( 46,1), IR( 46,2) /  177010341, 3008 /
      DATA IR( 47,1), IR( 47,2) /  178104146, 3007 /
      DATA IR( 48,1), IR( 48,2) /  187503178, 3109 /
      DATA IR( 49,1), IR( 49,2) /  195303807, 1417 /
      DATA IR( 50,1), IR( 50,2) /  214810341, 3004 /
      DATA IR( 51,1), IR( 51,2) /  215904146, 3003 /
      DATA IR( 52,1), IR( 52,2) /  221802916, 3201 /
      DATA IR( 53,1), IR( 53,2) /  230416285, 3204 /
      DATA IR( 54,1), IR( 54,2) /  233111318, 3026 /
      DATA IR( 55,1), IR( 55,2) /  233119107, 3025 /
      DATA IR( 56,1), IR( 56,2) /  233311318, 3028 /
      DATA IR( 57,1), IR( 57,2) /  233319107, 3027 /
      DATA IR( 58,1), IR( 58,2) /  233614274, 2503 /
      DATA IR( 59,1), IR( 59,2) /  234004374, 2119 /
      DATA IR( 60,1), IR( 60,2) /  239500000, 1310 /
      DATA IR( 61,1), IR( 61,2) /  240410400, 2113 /
      DATA IR( 62,1), IR( 62,2) /  241811453, 1708 /
      DATA IR( 63,1), IR( 63,2) /  244510341, 3022 /
      DATA IR( 64,1), IR( 64,2) /  245604146, 3021 /
      DATA IR( 65,1), IR( 65,2) /  249910341, 3024 /
      DATA IR( 66,1), IR( 66,2) /  251004146, 3023 /
      DATA IR( 67,1), IR( 67,2) /  255909297, 3203 /
      DATA IR( 68,1), IR( 68,2) /  259603645, 1112 /
      DATA IR( 69,1), IR( 69,2) /  260511709, 1415 /
      DATA IR( 70,1), IR( 70,2) /  260614729, 1506 /
      DATA IR( 71,1), IR( 71,2) /  260614837, 2415 /
      DATA IR( 72,1), IR( 72,2) /  261013269, 2411 /
      DATA IR( 73,1), IR( 73,2) /  261100000, 1202 /
      DATA IR( 74,1), IR( 74,2) /  261102916, 1210 /
      DATA IR( 75,1), IR( 75,2) /  261105832, 1224 /
      DATA IR( 76,1), IR( 76,2) /  261106959, 2121 /
      DATA IR( 77,1), IR( 77,2) /  261200000, 1204 /
      DATA IR( 78,1), IR( 78,2) /  261202916, 1212 /
      DATA IR( 79,1), IR( 79,2) /  261205832, 1226 /
      DATA IR( 80,1), IR( 80,2) /  261310746, 2302 /
      DATA IR( 81,1), IR( 81,2) /  263111475, 1315 /
      DATA IR( 82,1), IR( 82,2) /  263408793, 3206 /
      DATA IR( 83,1), IR( 83,2) /  267802728, 3205 /
      DATA IR( 84,1), IR( 84,2) /  269300000, 1423 /
      DATA IR( 85,1), IR( 85,2) /  272102016, 3202 /
      DATA IR( 86,1), IR( 86,2) /  274204374, 3101 /
      DATA IR( 87,1), IR( 87,2) /  296813851, 1416 /
      DATA IR( 88,1), IR( 88,2) /  305706944, 2103 /
      DATA IR( 89,1), IR( 89,2) /  306304190, 1707 /
      DATA IR( 90,1), IR( 90,2) /  306411480, 2311 /
      DATA IR( 91,1), IR( 91,2) /  307002682, 1507 /
      DATA IR( 92,1), IR( 92,2) /  316504527, 1107 /
      DATA IR( 93,1), IR( 93,2) /  317200000, 2312 /
      DATA IR( 94,1), IR( 94,2) /  317204042, 2312 /
      DATA IR( 95,1), IR( 95,2) /  318100000, 1104 /
      DATA IR( 96,1), IR( 96,2) /  318103165, 1108 /
      DATA IR( 97,1), IR( 97,2) /  318106674, 1104 /
      DATA IR( 98,1), IR( 98,2) /  349908994, 2305 /
      DATA IR( 99,1), IR( 99,2) /  380411921, 2127 /
      DATA IR(100,1), IR(100,2) /  390214396, 3105 /
C
C     101. EINTEGR 102. ERASE   103. ERROR   104. EVALUAT 105. EXCHANG
C     106. EXECUTE 107. EXIONE  108. EXIZERO 109. EXKONE  110. EXKZERO
C     111. EXP     112. EXPAND  113. EXPINT  114. FIT     115. FIXED
C     116. FLEXIBL 117. FLIP    118. FLOATIN 119. FOURPLO 120. FRACTIO
C     121. FREQUEN 122. FTOC    123. FULL    124. GAMMA   125. GENERAT
C     126. HARMONI 127. HCOSINT 128. HERMITE 129. HIERARC 130. HISTOGR
C     131. HSININT 132. IFEQ    133. IFGE    134. IFGT    135. IFLE
C     136. IFLT    137. IFNE    138. INCREME 139. INSERT  140. INTEGER
C     141. INTERAC 142. INTERPO 143. INTJO   144. INVERT  145. ISETUP
C     146. ISOLATE 147. ITERATE 148. KBIONE  149. KBIZERO 150. KBKONE
C     151. KBKZERO 152. KEXIONE 153. KEXIZER 154. KEXKONE 155. KEXKZER
C     156. LABEL   157. LAGUERR 158. LARFIT  159. LEGENDR 160. LENGTH
C     161. LIST    162. LOCAL   163. LOG     164. LOGE    165. LOGTEN
C     166. MADD    167. MATCH   168. MAX     169. MAXIMUM 170. MAXMIN
C     171. MDEFINE 172. MDIAGON 173. MEDIAN  174. MEIGEN  175. MERASE
C     176. MIDENTI 177. MIN     178. MINIMUM 179. MINVERT 180. MKRONEC
C     181. MLABEL  182. MMATVEC 183. MMOVE   184. MMULT   185. MMULTIP
C     186. MOLWT   187. MORTHO  188. MOVE    189. MPROPER 190. MRAISE
C     191. MSCALAR 192. MSUB    193. MSUBTRA 194. MTRANSP 195. MTRIANG
C     196. MULT    197. MULTIPL 198. MVECDIA 199. MVECMAT 200. MZERO
C
      DATA IR(101,1), IR(101,2) /  390214722, 2122 /
      DATA IR(102,1), IR(102,2) /  413213986, 2110 /
      DATA IR(103,1), IR(103,2) /  414911421, 2118 /
      DATA IR(104,1), IR(104,2) /  424009316, 1606 /
      DATA IR(105,1), IR(105,2) /  429605873, 2111 /
      DATA IR(106,1), IR(106,2) /  429802774, 1403 /
      DATA IR(107,1), IR(107,2) /  430211318, 3010 /
      DATA IR(108,1), IR(108,2) /  430219107, 3009 /
      DATA IR(109,1), IR(109,2) /  430411318, 3012 /
      DATA IR(110,1), IR(110,2) /  430419107, 3011 /
      DATA IR(111,1), IR(111,2) /  430900000, 1218 /
      DATA IR(112,1), IR(112,2) /  430901111, 2304 /
      DATA IR(113,1), IR(113,2) /  430906959, 2126 /
      DATA IR(114,1), IR(114,2) /  463700000, 2203 /
      DATA IR(115,1), IR(115,2) /  464103753, 1303 /
      DATA IR(116,1), IR(116,2) /  470317741, 1312 /
      DATA IR(117,1), IR(117,2) /  470711664, 2112 /
      DATA IR(118,1), IR(118,2) /  471301278, 1304 /
      DATA IR(119,1), IR(119,2) /  480013566, 1318 /
      DATA IR(120,1), IR(120,2) /  486102736, 1233 /
      DATA IR(121,1), IR(121,2) /  486512965, 2410 /
      DATA IR(122,1), IR(122,2) /  492902187, 3102 /
      DATA IR(123,1), IR(123,2) /  495308748, 1418 /
      DATA IR(124,1), IR(124,2) /  514309504, 1508 /
      DATA IR(125,1), IR(125,2) /  525204132, 1301 /
      DATA IR(126,1), IR(126,2) /  587709896, 3037 /
      DATA IR(127,1), IR(127,2) /  592814108, 2125 /
      DATA IR(128,1), IR(128,2) /  598509740, 1903 /
      DATA IR(129,1), IR(129,2) /  608013167, 2114 /
      DATA IR(130,1), IR(130,2) /  609414992, 2408 /
      DATA IR(131,1), IR(131,2) /  635410463, 2124 /
      DATA IR(132,1), IR(132,2) /  672812393, 1410 /
      DATA IR(133,1), IR(133,2) /  673003645, 1412 /
      DATA IR(134,1), IR(134,2) /  673014580, 1411 /
      DATA IR(135,1), IR(135,2) /  673503645, 1414 /
      DATA IR(136,1), IR(136,2) /  673514580, 1409 /
      DATA IR(137,1), IR(137,2) /  673703645, 1413 /
      DATA IR(138,1), IR(138,2) /  694213270, 1406 /
      DATA IR(139,1), IR(139,2) /  695804151, 2903 /
      DATA IR(140,1), IR(140,2) /  695903839, 1232 /
      DATA IR(141,1), IR(141,2) /  695904132, 1404 /
      DATA IR(142,1), IR(142,2) /  695904147, 2504 /
      DATA IR(143,1), IR(143,2) /  695907695, 3029 /
      DATA IR(144,1), IR(144,2) /  696104151, 1601 /
      DATA IR(145,1), IR(145,2) /  707915163, 2802 /
      DATA IR(146,1), IR(146,2) /  708908795, 2803 /
      DATA IR(147,1), IR(147,2) /  710613169, 2801 /
      DATA IR(148,1), IR(148,2) /  808211318, 3014 /
      DATA IR(149,1), IR(149,2) /  808219107, 3013 /
      DATA IR(150,1), IR(150,2) /  808411318, 3016 /
      DATA IR(151,1), IR(151,2) /  808419107, 3015 /
      DATA IR(152,1), IR(152,2) /  817806980, 3018 /
      DATA IR(153,1), IR(153,2) /  817807268, 3017 /
      DATA IR(154,1), IR(154,2) /  817808438, 3020 /
      DATA IR(155,1), IR(155,2) /  817808726, 3019 /
      DATA IR(156,1), IR(156,2) /  877703969, 1603 /
      DATA IR(157,1), IR(157,2) /  878215462, 1902 /
      DATA IR(158,1), IR(158,2) /  879304637, 2207 /
      DATA IR(159,1), IR(159,2) /  889004027, 1905 /
      DATA IR(160,1), IR(160,2) /  889705651, 1420 /
      DATA IR(161,1), IR(161,2) /  901014580, 2115 /
      DATA IR(162,1), IR(162,2) /  915601053, 1422 /
      DATA IR(163,1), IR(163,2) /  916000000, 1220 /
      DATA IR(164,1), IR(164,2) /  916003645, 1220 /
      DATA IR(165,1), IR(165,2) /  916014729, 1221 /
      DATA IR(166,1), IR(166,2) /  950802916, 1801 /
      DATA IR(167,1), IR(167,2) /  952402403, 2505 /
      DATA IR(168,1), IR(168,2) /  952800000, 2105 /
      DATA IR(169,1), IR(169,2) /  952806933, 2105 /
      DATA IR(170,1), IR(170,2) /  952809734, 2904 /
      DATA IR(171,1), IR(171,2) /  959004631, 1501 /
      DATA IR(172,1), IR(172,2) /  959400933, 1504 /
      DATA IR(173,1), IR(173,2) /  961606602, 1714 /
      DATA IR(174,1), IR(174,2) /  962105252, 1705 /
      DATA IR(175,1), IR(175,2) /  963001247, 1502 /
      DATA IR(176,1), IR(176,2) /  972404043, 1503 /
      DATA IR(177,1), IR(177,2) /  973400000, 2106 /
      DATA IR(178,1), IR(178,2) /  973406933, 2106 /
      DATA IR(179,1), IR(179,2) /  973416191, 1601 /
      DATA IR(180,1), IR(180,2) /  979211318, 1703 /
      DATA IR(181,1), IR(181,2) /  980201605, 1605 /
      DATA IR(182,1), IR(182,2) /  982915179, 2603 /
      DATA IR(183,1), IR(183,2) /  984316173, 2306 /
      DATA IR(184,1), IR(184,2) /  984909288, 1701 /
      DATA IR(185,1), IR(185,2) /  984909297, 1701 /
      DATA IR(186,1), IR(186,2) /  989417307, 3104 /
      DATA IR(187,1), IR(187,2) /  990014811, 2205 /
      DATA IR(188,1), IR(188,2) /  990403645, 2306 /
      DATA IR(189,1), IR(189,2) /  992711372, 2701 /
      DATA IR(190,1), IR(190,2) /  996407079, 1702 /
      DATA IR(191,1), IR(191,2) /  999301054, 1806 /
      DATA IR(192,1), IR(192,2) / 1001101458, 1802 /
      DATA IR(193,1), IR(193,2) / 1001102016, 1802 /
      DATA IR(194,1), IR(194,2) / 1003501126, 1803 /
      DATA IR(195,1), IR(195,2) / 1003506602, 1704 /
      DATA IR(196,1), IR(196,2) / 1005614580, 1103 /
      DATA IR(197,1), IR(197,2) / 1005614839, 1103 /
      DATA IR(198,1), IR(198,2) / 1007602304, 2601 /
      DATA IR(199,1), IR(199,2) / 1007602539, 2602 /
      DATA IR(200,1), IR(200,2) / 1018413527, 1502 /
C
C     201. NCPLOT  202. NEGEINT 203. NEGEXP  204. NHISTOG 205. NORMLAG
C     206. NPLOT   207. NTABLE  208. NULL    209. OMIT    210. ONEWAY
C     211. ORDER   212. PARPROD 213. PARSUM  214. PARTFUN 215. PERCENT
C     216. PERFORM 217. PFATOMI 218. PFTRANS 219. PLOT    220. POLYFIT
C     221. PRODUCT 222. PROMOTE 223. PROPORT 224. RAISE   225. RANGE
C     226. RANKS   227. RECIPRO 228. RECODE  229. REMOTE  230. REPEAT
C     231. REPLACE 232. RESTORE 233. RETAIN  234. RMS     235. ROUND
C     236. ROWSUM  237. SAPROPE 238. SCAN    239. SCORREL 240. SDIFFER
C     241. SDIVDIF 242. SEARCH  243. SELECT  244. SEPARAT 245. SET
C     246. SFIT    247. SHORTEN 248. SI      249. SIN     250. SIND
C     251. SINH    252. SININT  253. SMPROPE 254. SOLVE   255. SONEWAY
C     256. SORT    257. SPACE   258. SPOLYFI 259. SQRT    260. SQUARE
C     261. SSTATIS 262. STATIST 263. STATPLO 264. STDDEV  265. STWOWAY
C     266. SUB     267. SUBTRAC 268. SUM     269. TABLE   270. TAN
C     271. TAND    272. TANH    273. TAPE    274. TCHEBYS 275. TERMINA
C     276. TWOPLOT 277. TWOWAY  278. UCHEBYS 279. UNIT    280. VFIT
C     281. VPOLYFI 282. WIDTH
C
      DATA IR(201,1), IR(201,2) / 1030309173, 1316 /
      DATA IR(202,1), IR(202,2) / 1034803902, 2123 /
      DATA IR(203,1), IR(203,2) / 1034804309, 1219 /
      DATA IR(204,1), IR(204,2) / 1043114406, 2409 /
      DATA IR(205,1), IR(205,2) / 1062909802, 1901 /
      DATA IR(206,1), IR(206,2) / 1065011475, 1307 /
      DATA IR(207,1), IR(207,2) / 1074701787, 3514 /
      DATA IR(208,1), IR(208,2) / 1078508748, 2117 /
      DATA IR(209,1), IR(209,2) / 1129514580, 1706 /
      DATA IR(210,1), IR(210,2) / 1131816819, 2413 /
      DATA IR(211,1), IR(211,2) / 1142504131, 2109 /
      DATA IR(212,1), IR(212,2) / 1170912165, 2002 /
      DATA IR(213,1), IR(213,2) / 1170914431, 2001 /
      DATA IR(214,1), IR(214,2) / 1170914763, 3108 /
      DATA IR(215,1), IR(215,2) / 1181702336, 1715 /
      DATA IR(216,1), IR(216,2) / 1181704797, 1403 /
      DATA IR(217,1), IR(217,2) / 1182714998, 3107 /
      DATA IR(218,1), IR(218,2) / 1184613163, 3106 /
      DATA IR(219,1), IR(219,2) / 1200314580, 1305 /
      DATA IR(220,1), IR(220,2) / 1208118396, 2201 /
      DATA IR(221,1), IR(221,2) / 1216503486, 2102 /
      DATA IR(222,1), IR(222,2) / 1216509902, 2310 /
      DATA IR(223,1), IR(223,2) / 1216512087, 1716 /
      DATA IR(224,1), IR(224,2) / 1315813986, 1105 /
      DATA IR(225,1), IR(225,2) / 1316305238, 1713 /
      DATA IR(226,1), IR(226,2) / 1316308532, 2403 /
      DATA IR(227,1), IR(227,2) / 1326007011, 1235 /
      DATA IR(228,1), IR(228,2) / 1326011048, 1111 /
      DATA IR(229,1), IR(229,2) / 1327011480, 1421 /
      DATA IR(230,1), IR(230,2) / 1327303692, 1403 /
      DATA IR(231,1), IR(231,2) / 1327308778, 1710 /
      DATA IR(232,1), IR(232,2) / 1327615003, 1408 /
      DATA IR(233,1), IR(233,2) / 1327700986, 1709 /
      DATA IR(234,1), IR(234,2) / 1349200000, 2003 /
      DATA IR(235,1), IR(235,2) / 1354810314, 1314 /
      DATA IR(236,1), IR(236,2) / 1355014431, 2101 /
      DATA IR(237,1), IR(237,2) / 1389413543, 2704 /
      DATA IR(238,1), IR(238,2) / 1393310206, 1402 /
      DATA IR(239,1), IR(239,2) / 1394713613, 2412 /
      DATA IR(240,1), IR(240,2) / 1396804541, 1109 /
      DATA IR(241,1), IR(241,2) / 1396816155, 1110 /
      DATA IR(242,1), IR(242,2) / 1398713211, 2502 /
      DATA IR(243,1), IR(243,2) / 1399803746, 2501 /
      DATA IR(244,1), IR(244,2) / 1400201216, 2902 /
      DATA IR(245,1), IR(245,2) / 1400600000, 1302 /
      DATA IR(246,1), IR(246,2) / 1402214580, 2204 /
      DATA IR(247,1), IR(247,2) / 1408213667, 2303 /
      DATA IR(248,1), IR(248,2) / 1409400000, 1311 /
      DATA IR(249,1), IR(249,2) / 1410800000, 1201 /
      DATA IR(250,1), IR(250,2) / 1410802916, 1209 /
      DATA IR(251,1), IR(251,2) / 1410805832, 1223 /
      DATA IR(252,1), IR(252,2) / 1410806959, 2120 /
      DATA IR(253,1), IR(253,2) / 1421813543, 2703 /
      DATA IR(254,1), IR(254,2) / 1426816173, 1602 /
      DATA IR(255,1), IR(255,2) / 1427004267, 2414 /
      DATA IR(256,1), IR(256,2) / 1427414580, 2108 /
      DATA IR(257,1), IR(257,2) / 1428402322, 1309 /
      DATA IR(258,1), IR(258,2) / 1429809429, 2202 /
      DATA IR(259,1), IR(259,2) / 1432814580, 1217 /
      DATA IR(260,1), IR(260,2) / 1433101220, 1234 /
      DATA IR(261,1), IR(261,2) / 1438401278, 2402 /
      DATA IR(262,1), IR(262,2) / 1439214842, 2401 /
      DATA IR(263,1), IR(263,2) / 1439215024, 1505 /
      DATA IR(264,1), IR(264,2) / 1439503073, 1712 /
      DATA IR(265,1), IR(265,2) / 1441411557, 2407 /
      DATA IR(266,1), IR(266,2) / 1442000000, 1102 /
      DATA IR(267,1), IR(267,2) / 1442015067, 1102 /
      DATA IR(268,1), IR(268,2) / 1443100000, 2004 /
      DATA IR(269,1), IR(269,2) / 1460908883, 3500 /
      DATA IR(270,1), IR(270,2) / 1462100000, 1203 /
      DATA IR(271,1), IR(271,2) / 1462102916, 1211 /
      DATA IR(272,1), IR(272,2) / 1462105832, 1225 /
      DATA IR(273,1), IR(273,2) / 1462303645, 5029 /
      DATA IR(274,1), IR(274,2) / 1466903724, 1906 /
      DATA IR(275,1), IR(275,2) / 1473309734, 1419 /
      DATA IR(276,1), IR(276,2) / 1521612003, 1317 /
      DATA IR(277,1), IR(277,2) / 1521616819, 2406 /
      DATA IR(278,1), IR(278,2) / 1539803724, 1904 /
      DATA IR(279,1), IR(279,2) / 1569614580, 5029 /
      DATA IR(280,1), IR(280,2) / 1620914580, 2208 /
      DATA IR(281,1), IR(281,2) / 1648509429, 2209 /
      DATA IR(282,1), IR(282,2) / 1701414796, 1405 /
C
C     ==================================================================
C
      END
*DATA6
      BLOCK DATA DATA6
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  DATA6 V 7.00  1/ 9/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS BLOCK DATA PROCEDURE SETS VALUES FOR NAME(1), NAME(2),
C        NAME(3), NAME(4), L1 AND L2 FOR EVERY COMMAND, AS NEEDED,
C           EXCEPT FOR IR(.) WHICH IS TAKEN CARE OF IN DATA5.
C
C     NO CHANGES ARE NECESSARY AT TIME OF IMPLEMENTATION.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARRAYB/ IALPH(6), ICL(10,2), ICP(6), ID(8,2) 
      COMMON /ARRYBC/ ICOLHD(7)
      COMMON /ARRAYC/ IDIST(30), IL(14,2), IPROP(5), IRD(35,3)
      COMMON /ARRAYD/ ITB(14), ITP(9,2), NALPH(5), NL(25)
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      CHARACTER  ICOLHD*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
C     (II)   1 COMMAND EXECUTED BY LOOKUP
C
C      1. PRINT NOTE
C
C     ------------------------------------------------------------------
C
C     (III)   15 COMMANDS EXECUTED BY SPINST FROM NL(.).
C
C      1. OMNITAB   2. FINISH    3. STOP      4. NOTE      5. NOTE1
C      6. NOTE2     7. TITLE1    8. TITLE2    9. TITLE3   10. TITLE4
C     11. FORMAT A 12. HEAD     13. TITLEX   14. TITLEY   15. FILE
C
C     OMNITAB IS IN NL( 1), NL( 2)      FINISH IS IN NL( 3), NL( 4)
C     STOP    IS IN NL( 5), NL( 6)      NOTE   IS IN NL( 7), NL( 8)
C     TITLE   IS IN NL( 9), NL(10)      FORMAT IS IN NL(11), NL(12)
C     PRINT   IS IN NL(13), NL(14)      HEAD   IS IN NL(15), NL(16)
C     LEX FOR TITLEX IS IN NL(17)
C     LEY FOR TITLEY IS IN NL(18)
C     M FOR SPECIAL MATRIX COMMANDS IS IN NL (19)
C     FILE    IS IN NL(20), NL(21)
C
      DATA NL( 1), NL( 2), NL( 3), NL( 4), NL( 5),
     1     NL( 6), NL( 7), NL( 8), NL( 9), NL(10),
     2     NL(11), NL(12), NL(13), NL(14), NL(15),
     3     NL(16), NL(17), NL(18), NL(19), NL(20),
     4     NL(21)/
     5      11300,   7102,   4631,   7082,  14406,
     6      11664,  10631,   3645,  14843,   8883,
     7       4797,   9524,  12159,  10746,   5968,
     8       2916,   8908,   8907,   9477,   4629,
     9       3645/
C
C     ------------------------------------------------------------------
C
C     (IV)   2*NID COMMANDS FROM ID(I,J), IALPH(J) AND NALPH(J).
C
C      1. RESET        2. RESET V      3. PRINT        4. PRINT A
C      5. PUNCH        6. PUNCH A      7. READ         8. READ A
C      9. ABRIDGE     10. ABRIDGE A   11. APRINT      12. APRINT A
C     13. MPRINT      14. MPRINT A    15. NPRINT      16. NPRINT A
C
C     RESET, PRINT, PUNCH, READ, ABRIDGE, APRINT, MPRINT, AND NPRINT
C        ARE IN ID(I,J).
C
C     V, W, X, Y, AND Z ARE IN NALPH(I).
C
C     A, B, C, D, E, AND F ARE IN IALPH(I).
C
      DATA ID(1,1), ID(1,2) / 1327604185,  1 /
      DATA ID(2,1), ID(2,2) / 1215910746,  2 /
      DATA ID(3,1), ID(3,2) / 1224502403,  3 /
      DATA ID(4,1), ID(4,2) / 1325802916,  5 /
      DATA ID(5,1), ID(5,2) /   80106676,  6 /
      DATA ID(6,1), ID(6,2) /  117906959,  4 /
      DATA ID(7,1), ID(7,2) /  992706959,  7 /
      DATA ID(8,1), ID(8,2) / 1065606959,  8 /
C
      DATA     IALPH(1),IALPH(2),IALPH(3),IALPH(4),IALPH(5),IALPH(6)/
     1              729,    1458,    2187,    2916,    3645,    4374/
C
      DATA     NALPH(1),NALPH(2),NALPH(3),NALPH(4),NALPH(5)/
     1            16038,   16767,   17496,   18225,   18954/
C
C     ------------------------------------------------------------------
C
C     (V)   NIRD TWO-WORD AND SPECIAL MATRIX COMMANDS IN IRD(I,J).
C
C      1. NO LIST            2. CLOSE UP           3. NEW PAGE
C      4. M(XX')             5. M(X'X)             6. M(XAX')
C      7. M(AD)              8. M(DA)              9. M(AV)
C     10. M(V'A)            11. ELLIPTICAL FIRST  12. ELLIPTICAL SECOND
C     13. PAGE PLOT         14. STRUVE ZERO       15. STRUVE ONE
C     16. ROW SUM           17. ZEROS BJZERO      18. ZEROS BJONE
C     19. GAUSS QUADRATURE  20. STEM LEAF         21. SSTEM LEAF
C     22. F PROBABILITY     23. SPLIT PLOT        24. NICE PLOT
C     25. NICE NPLOT        26. NICE CPLOT        27. NICE NCPLOT
C     28. SAMPLE WITHR      29. SAMPLE WITHOUTR   30. TEKTRONIX PLOT
C     31. TEKTRONIX AXIS    32. TEKTRONIX OPTIONS 33. LASER PLOT
C     34. LASER AXIS        35. LASER OPTIONS
C
C     NOTE, FOR NO. 5., M(X'X) = M(X'AX).
C
      DATA  IRD( 1,1),  IRD( 1,2),  IRD( 1,3) /
     1     1061100000,  901014580,       2116 /
      DATA  IRD( 2,1),  IRD( 2,2),  IRD( 2,3) /
     1      252613986, 1574100000,       2301 /
      DATA  IRD( 3,1),  IRD( 3,2),  IRD( 3,3) /
     1     1036400000, 1169803645,       1308 /
      DATA  IRD( 4,1),  IRD( 4,2),  IRD( 4,3) /
     1      947700000, 1814400000,       5101 /
      DATA  IRD( 5,1),  IRD( 5,2),  IRD( 5,3) /
     1      947700000, 1749600000,       5102 /
      DATA  IRD( 6,1),  IRD( 6,2),  IRD( 6,3) /
     1      947700000, 1754700000,       5103 /
      DATA  IRD( 7,1),  IRD( 7,2),  IRD( 7,3) /
     1      947700000,   83700000,       5201 /
      DATA  IRD( 8,1),  IRD( 8,2),  IRD( 8,3) /
     1      947700000,  294300000,       5202 /
      DATA  IRD( 9,1),  IRD( 9,2),  IRD( 9,3) /
     1      947700000,  132300000,       5301 /
      DATA  IRD(10,1),  IRD(10,2),  IRD(10,3) /
     1      947700000, 1603800000,       5302 /
      DATA  IRD(11,1),  IRD(11,2),  IRD(11,3) /
     1      398107013,  463514391,       3030 /
      DATA  IRD(12,1),  IRD(12,2),  IRD(12,3) /
     1      398107013, 1398911317,       3031 /
      DATA  IRD(13,1),  IRD(13,2),  IRD(13,3) /
     1     1169803645, 1200314580,       1306 /
      DATA  IRD(14,1),  IRD(14,2),  IRD(14,3) /
     1     1440915908, 1910710935,       3035 /
      DATA  IRD(15,1),  IRD(15,2),  IRD(15,3) /
     1     1440915908, 1131800000,       3036 /
      DATA  IRD(16,1),  IRD(16,2),  IRD(16,3) /
     1     1355000000, 1443100000,       2101 /
      DATA  IRD(17,1),  IRD(17,2),  IRD(17,3) /
     1     1910711448,  175404146,       3033 /
      DATA  IRD(18,1),  IRD(18,2),  IRD(18,3) /
     1     1910711448,  174310341,       3034 /
      DATA  IRD(19,1),  IRD(19,2),  IRD(19,3) /
     1      515114364, 1296103403,       2404 /
      DATA  IRD(20,1),  IRD(20,2),  IRD(20,3) /
     1     1439609477,  888404374,       3301 /
      DATA  IRD(21,1),  IRD(21,2),  IRD(21,3) /
     1     1438403996,  888404374,       3302 /
      DATA  IRD(22,1),  IRD(22,2),  IRD(22,3) /
     1      437400000, 1216501487,       2405 /
      DATA  IRD(23,1),  IRD(23,2),  IRD(23,3) /
     1     1429507101, 1200314580,       2416 /
      DATA  IRD(24,1),  IRD(24,2),  IRD(24,3) /
     1     1045203645, 1200314580,       1319 /
      DATA  IRD(25,1),  IRD(25,2),  IRD(25,3) /
     1     1045203645, 1065011475,       1320 /
      DATA  IRD(26,1),  IRD(26,2),  IRD(26,3) /
     1     1045203645,  263111475,       1321 /
      DATA  IRD(27,1),  IRD(27,2),  IRD(27,3) /
     1     1045203645, 1030309173,       1322 /
      DATA  IRD(28,1),  IRD(28,2),  IRD(28,3) /
     1     1389111993, 1703006318,       1509 /
      DATA  IRD(29,1),  IRD(29,2),  IRD(29,3) /
     1     1389111993, 1703006258,       1510 /
      DATA  IRD(30,1),  IRD(30,2),  IRD(30,3) /
     1     1472615081, 1200314580,       3216 /
      DATA  IRD(31,1),  IRD(31,2),  IRD(31,3) /
     1     1472615081,  138613851,       3217 /
      DATA  IRD(32,1),  IRD(32,2),  IRD(32,3) /
     1     1472615081, 1473309734,       3218 /
      DATA  IRD(33,1),  IRD(33,2),  IRD(33,3) /
     1      879404131, 1200314580,       3219 /
      DATA  IRD(34,1),  IRD(34,2),  IRD(34,3) /
     1      879404131,  138613851,       3220 /
      DATA  IRD(35,1),  IRD(35,2),  IRD(35,3) /
     1      879404131, 1473309734,       3221 /
C
C     ------------------------------------------------------------------
C
C     (VI)   2*(NITP+3) UNIT AND TAPE OPERATION COMMANDS FROM ITP(.).
C
C      1. READ UNIT A        2. CREAD UNIT A       3. WRITE UNIT A
C      4. SET UNIT A         5. CSET UNIT A        6. ENDFILE UNIT A
C      7. REWIND UNIT A      8. SKIP UNIT A        9. BACKSPACE UNIT A
C     10. READ UNIT A A     11. CREAD UNIT A A    12. WRITE UNIT A A
C     13. READ TAPE A       14. CREAD TAPE A      15. WRITE TAPE A
C     16. SET TAPE A        17. CSET TAPE A       18. ENDFILE TAPE A
C     19. REWIND TAPE A     20. SKIP TAPE A       21. BACKSPACE TAPE A
C     22. READ TAPE A A     23. CREAD TAPE A A    24. WRITE TAPE A A
C
      DATA ITP(1,1), ITP(1,2) / 1325802916, 4500 /
      DATA ITP(2,1), ITP(2,2) /  267800837, 4600 /
      DATA ITP(3,1), ITP(3,2) / 1726214715, 4700 /
      DATA ITP(4,1), ITP(4,2) / 1400600000, 4800 /
      DATA ITP(5,1), ITP(5,2) /  270514580, 4900 /
      DATA ITP(6,1), ITP(6,2) /  402704629, 5001 /
      DATA ITP(7,1), ITP(7,2) / 1328006943, 5008 /
      DATA ITP(8,1), ITP(8,2) / 1415711664, 5015 /
      DATA ITP(9,1), ITP(9,2) /  148808548, 5022 /
C
C     ------------------------------------------------------------------
C
C     (VII)   6 CENSOR XX COMMANDS IN ICP(I).
C
C     1. LE     2. EQ     3. GE     4. GT     5. LT     6. NE
C
      DATA ICP(1), ICP(2), ICP(3), ICP(4), ICP(5), ICP(6) /
     1       8883,   4104,   5238,   5643,   9288,  10341 /
C
C     ------------------------------------------------------------------
C
C     (VIII)   8 CALCOMP XXXXXX COMMANDS IN ICL (I,J).
C
C     1. TAPE      2. SPEED     3. SIZE      4. PLOT.
C     5. SLOW      6. FAST      7. PAPER     8. AXIS
C
      DATA ICL(1,1), ICL(1,2) / 1462303645, 3215 /
      DATA ICL(2,1), ICL(2,2) / 1428803753, 3208 /
      DATA ICL(3,1), ICL(3,2) / 1412003645, 3209 /
      DATA ICL(4,1), ICL(4,2) / 1200314580, 3210 /
      DATA ICL(5,1), ICL(5,2) / 1419016767, 3211 /
      DATA ICL(6,1), ICL(6,2) /  442014580, 3212 /
      DATA ICL(7,1), ICL(7,2) / 1170704131, 3213 /
      DATA ICL(8,1), ICL(8,2) /  138613851, 3214 /
C
C     ------------------------------------------------------------------
C
C     (IX)   NDIST DISTRIBUTION NAMES IN IDIST(I,J).
C
C      1. NORMAL    2. LOGNORMA  3. HALFNORM  4. T         5. CHISQU
C      6. GAMMA     7. F         8. BETA      9. SRANGE   10. UNIFORM
C     11. CAUCHY   12. LAMBDA   13. EXTREME  14. WEIBULL  15. PARETO
C     16. EXPONENT 17. DEXPONEN 18. LOGISTIC 19. NCT      20. NCCHISQ
C     21. NCF      22. BERNOULL 23. BINOMIAL 24. NEGBINOM 25. POISSON
C     26. GEOMETRI 27. HYPERGEO 28. DISCRETE 29. PASCAL   30. MULTINOM
C
      DATA IDIST( 1) / 1062909516 /
      DATA IDIST( 2) /  916010629 /
      DATA IDIST( 3) /  587104767 /
      DATA IDIST( 4) / 1458000000 /
      DATA IDIST( 5) /  241214331 /
      DATA IDIST( 6) /  514309504 /
      DATA IDIST( 7) /  437400000 /
      DATA IDIST( 8) /  161300729 /
      DATA IDIST( 9) / 1433810400 /
      DATA IDIST(10) / 1569604797 /
      DATA IDIST(11) /  223502428 /
      DATA IDIST(12) /  878801567 /
      DATA IDIST(13) /  431313270 /
      DATA IDIST(14) / 1691102037 /
      DATA IDIST(15) / 1170904200 /
      DATA IDIST(16) /  430911318 /
      DATA IDIST(17) /  307512083 /
      DATA IDIST(18) /  916007094 /
      DATA IDIST(19) / 1030700000 /
      DATA IDIST(20) / 1029006094 /
      DATA IDIST(21) / 1029300000 /
      DATA IDIST(22) /  161110632 /
      DATA IDIST(23) /  171511295 /
      DATA IDIST(24) / 1034801715 /
      DATA IDIST(25) / 1207814379 /
      DATA IDIST(26) /  525309632 /
      DATA IDIST(27) /  652304138 /
      DATA IDIST(28) /  317802678 /
      DATA IDIST(29) / 1171002226 /
      DATA IDIST(30) / 1005614837 /
C
C     ------------------------------------------------------------------
C
C     (X)   NPROP COMMANDS FOR PROB. DIST. PROPERTIES IN IPROP(I,J).
C
C     1. DENSITY     2. CUMULATIVE    3. PERCENTILE     4. RANDOM
C     5. PLOT
C
      DATA IPROP(1) /  306514114 /
      DATA IPROP(2) /  276715634 /
      DATA IPROP(3) / 1181702336 /
      DATA IPROP(4) / 1316303334 /
      DATA IPROP(5) / 1200314580 /
C
C     TWO-WORD PROBABILITY DISTRIBUTION COMMANDS AVAILABLE.
C
C      1. NORMAL       DENSITY   CUMULATIVE   PERCENTILE   RANDOM   PLOT
C      2. LOGNORMAL    DENSITY   CUMULATIVE   PERCENTILE   RANDOM   PLOT
C      3. HALFNORMAL   DENSITY   CUMULATIVE   PERCENTILE   RANDOM   PLOT
C      4. T                      CUMULATIVE   PERCENTILE   RANDOM
C      5. CHISQUARED             CUMULATIVE   PERCENTILE   RANDOM
C      6. GAMMA                  CUMULATIVE   PERCENTILE   RANDOM   PLOT
C      7. F                      CUMULATIVE   PERCENTILE   RANDOM
C      8. BETA         DENSITY   CUMULATIVE                RANDOM
C     10. UNIFORM      DENSITY   CUMULATIVE   PERCENTILE   RANDOM   PLOT
C     11. CAUCHY       DENSITY   CUMULATIVE   PERCENTILE   RANDOM   PLOT
C     12. LAMBDA       DENSITY   CUMULATIVE   PERCENTILE   RANDOM   PLOT
C     13. EXTREME      DENSITY   CUMULATIVE   PERCENTILE   RANDOM   PLOT
C     14. WEIBULL      DENSITY   CUMULATIVE   PERCENTILE   RANDOM   PLOT
C     15. PARETO       DENSITY   CUMULATIVE   PERCENTILE   RANDOM   PLOT
C     16. EXPONENTIAL  DENSITY   CUMULATIVE   PERCENTILE   RANDOM   PLOT
C     17. DEXPONENTIAL DENSITY   CUMULATIVE   PERCENTILE   RANDOM   PLOT
C     18. LOGISTIC     DENSITY   CUMULATIVE   PERCENTILE   RANDOM   PLOT
C     23. BINOMIAL     DENSITY   CUMULATIVE   PERCENTILE   RANDOM
C     24. NEGBINOMIAL  DENSITY   CUMULATIVE   PERCENTILE   RANDOM
C     25. POISSON      DENSITY   CUMULATIVE   PERCENTILE   RANDOM   PLOT
C     26. GEOMETRIC    DENSITY   CUMULATIVE   PERCENTILE   RANDOM
C         ------------ -------   ----------   ----------   ------   ----
C            (21)       (17)        (21)         (19)       (21)    (14)
C     TOTAL OF 17 + 21 + 19 + 21 + 14 = 92 INSTRUCTIONS AVAILABLE.
C
C     ------------------------------------------------------------------
C
C     (XI)   NITB TABLE COMMANDS IN ITB(I,J).
C
C      1. FREQUENCY 2. SUM       3. AVERAGE   4. STDDEV    5. MINIMUM
C      6. MAXIMUM   7. RANGE     8. MEDIAN    9. PERCENTA 10. PROPORTION
C     11. RPERCENT 12. CPERCENT 13. RPROPORT 14. CPROPORT
C
      DATA ITB( 1) /  486512965 /
      DATA ITB( 2) / 1443100000 /
      DATA ITB( 3) /  132813156 /
      DATA ITB( 4) / 1439503073 /
      DATA ITB( 5) /  973406933 /
      DATA ITB( 6) /  952806933 /
      DATA ITB( 7) / 1316305238 /
      DATA ITB( 8) /  961606602 /
      DATA ITB( 9) / 1181702336 /
      DATA ITB(10) / 1216512087 /
      DATA ITB(11) / 1355913208 /
      DATA ITB(12) /  262413208 /
      DATA ITB(13) / 1357211382 /
      DATA ITB(14) /  263711382 /
C
C     ------------------------------------------------------------------
C
C     (XII)   NIL LANGUAGES IN IL(I,J).
C
C      1. ENGLISH   2. FRANCAIS  3. DEUTSCH   4. ESPANOL   5. ITALIANO
C      6. NORSK     7. DANSK     8. JAPANESE  9. YUGOSLAV 10. PORTUGESE
C     11. NEDERLAN 12. SVENSKA  13. SLOVENE  14. VOCABULA
C
      DATA IL( 1,1), IL( 1,2) /  403009010, 3401 /
      DATA IL( 2,1), IL( 2,2) /  486110288, 3402 /
      DATA IL( 3,1), IL( 3,2) /  307215096, 3403 /
      DATA IL( 4,1), IL( 4,2) /  417401122, 3404 /
      DATA IL( 5,1), IL( 5,2) /  710208992, 3405 /
      DATA IL( 6,1), IL( 6,2) / 1062914148, 3406 /
      DATA IL( 7,1), IL( 7,2) /  295714148, 3407 /
      DATA IL( 8,1), IL( 8,2) /  733301112, 3408 /
      DATA IL( 9,1), IL( 9,2) / 1879911460, 3409 /
      DATA IL(10,1), IL(10,2) / 1208715154, 3410 /
      DATA IL(11,1), IL(11,2) / 1034504143, 3411 /
      DATA IL(12,1), IL(12,2) / 1445010730, 3412 /
      DATA IL(13,1), IL(13,2) / 1419016187, 3413 /
      DATA IL(14,1), IL(14,2) / 1644600804, 3414 /
C
C     ------------------------------------------------------------------
C
C     (XIII)   COLUMN (HEAD) IN ICOLHD(I) LANGUA USES IS NOT A COMMAND.
C
      DATA ICOLHD(1), ICOLHD(2), ICOLHD(3), ICOLHD(4), ICOLHD(5),
     1     ICOLHD(6), ICOLHD(7)/
     2    'C',   'O',   'L',   'U',   'M',   'N',   ' '/
C
C     ------------------------------------------------------------------
C
C     ADDITIONAL COMMENTS CONCERNING NUMBER OF COMMANDS ...
C
C     TWO IS SUBTRACTED FROM THE NUMBER OF COMMANDS IN IR(.) FOR TABLE
C        AND NTABLE BECAUSE THERE ARE NO ONE-WORD COMMANDS WITH THESE
C        NAMES AND THEY ARE COUNTED IN THE NUMBER OF COMMANDS IN ITB(.).
C
C     THE LETTER A AFTER A COMMAND STANDS FOR A FORMAT OR UNIT QUALIFIER
C        WHICH CAN BE EITHER A, B, C, D, E, OR F.
C
C     THE LETTER V AFTER RESET STANDS FOR THE QUALIFIER V, W, X, Y, OR Z
C
C     THE LETTER Q AFTER THE UNIT COMMANDS READ, CREAD, AND WRITE
C        STANDS FOR BOTH THE UNIT QUALIFIER A AND THE FORMAT QUALIFIER A
C            AS IN CREAD UNIT A A.
C
C     A COMMAND WITH A QUALIFIER IS TREATED AS A SINGLE COMMAND,
C        EVEN THOUGH THE QUALIFIER CAN TAKE ON ONE OF SEVERAL VALUES.
C
C     ==================================================================
C
      END
*DATA7
      BLOCK  DATA DATA7
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/19/81.  DATA7 V 7.00  1/18/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS BLOCK DATA PROCEDURE DEFINES THE CHARACTERS USED BY
C        THE OMNITAB 1977 COMPUTING SYSTEM.
C
C     NO CHANGE IS NECESSARY AT TIME OF IMPLEMENTATION.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ABCDEF/ LA(74)
C
C     ==================================================================
C
C                    ***   TYPE STATEMENTS   ***
      CHARACTER LA*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
C           ABCDEF
C
C   LA( 1) =  0  LA( 2) =  1  LA( 3) =  2  LA( 4) =  3  LA( 5) =  4
C   LA( 6) =  5  LA( 7) =  6  LA( 8) =  7  LA( 9) =  8  LA(10) =  9
C   LA(11) =  A  LA(12) =  B  LA(13) =  C  LA(14) =  D  LA(15) =  E
C   LA(16) =  F  LA(17) =  G  LA(18) =  H  LA(19) =  I  LA(20) =  J
C   LA(21) =  K  LA(22) =  L  LA(23) =  M  LA(24) =  N  LA(25) =  O
C   LA(26) =  P  LA(27) =  Q  LA(28) =  R  LA(29) =  S  LA(30) =  T
C   LA(31) =  U  LA(32) =  V  LA(33) =  W  LA(34) =  X  LA(35) =  Y
C   LA(36) =  Z  LA(37) =  /  LA(38) =  .  LA(39) =  -  LA(40) =  +
C   LA(41) =  *  LA(42) =  (  LA(43) =  )  LA(44) =  ,  LA(45) =  
C   LA(46) =  =  LA(47) =  $  LA(48) =  '  LA(49) =  a  LA(50) =  b
C   LA(51) =  c  LA(52) =  d  LA(53) =  e  LA(54) =  f  LA(55) =  g
C   LA(56) =  h  LA(57) =  i  LA(58) =  j  LA(59) =  k  LA(60) =  l
C   LA(61) =  m  LA(62) =  n  LA(63) =  o  LA(64) =  p  LA(65) =  q
C   LA(66) =  r  LA(67) =  s  LA(68) =  t  LA(69) =  u  LA(70) =  v
C   LA(71) =  w  LA(72) =  x  LA(73) =  y  LA(74) =  z
C
      DATA LA( 1), LA( 2), LA( 3), LA( 4), LA( 5),
     1     LA( 6), LA( 7), LA( 8), LA( 9), LA(10)/
     2        '0',    '1',    '2',    '3',    '4',
     3        '5',    '6',    '7',    '8',    '9'/
C
      DATA LA(11), LA(12), LA(13), LA(14), LA(15),
     1     LA(16), LA(17), LA(18), LA(19), LA(20)/
     2        'A',    'B',    'C',    'D',    'E',
     3        'F',    'G',    'H',    'I',    'J'/
C
      DATA LA(21), LA(22), LA(23), LA(24), LA(25),
     1     LA(26), LA(27), LA(28), LA(29), LA(30)/
     2        'K',    'L',    'M',    'N',    'O',
     3        'P',    'Q',    'R',    'S',    'T'/
C
      DATA LA(31), LA(32), LA(33), LA(34), LA(35),
     1     LA(36), LA(37), LA(38), LA(39), LA(40)/
     2        'U',    'V',    'W',    'X',    'Y',
     3        'Z',    '/',    '.',    '-',    '+'/
C
      DATA LA(41), LA(42), LA(43), LA(44), LA(45),
     1     LA(46), LA(47), LA(48), LA(49), LA(50)/
     2        '*',    '(',    ')',    ',',    ' ',
     3        '=',    '$',   '''',    'a',    'b'/
C
      DATA LA(51), LA(52), LA(53), LA(54), LA(55),
     1     LA(56), LA(57), LA(58), LA(59), LA(60)/
     2        'c',    'd',    'e',    'f',    'g',
     3        'h',    'i',    'j',    'k',    'l'/
C
      DATA LA(61), LA(62), LA(63), LA(64), LA(65),
     1     LA(66), LA(67), LA(68), LA(69), LA(70)/
     2        'm',    'n',    'o',    'p',    'q',
     3        'r',    's',    't',    'u',    'v'/
C
      DATA LA(71), LA(72), LA(73), LA(74)/
     2        'w',    'x',    'y',    'z'/
C
C     ==================================================================
C
      END
*DATA8    
      BLOCK  DATA DATA8       
C         
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  DATA8 V 7.00  2/26/91. **        
C         
C     ==================================================================        
C         
C                        ***   GENERAL COMMENTS   ***       
C         
C     THIS BLOCK DATA PROCEDURE SETS VALUES FOR NOCARD AND IFMTS ARRAYS,        
C        LENGTH OF TABLES USED BY LOOKUP, AND VALUES OF LANGP AND LANGC.        
C         
C        NO CHANGES ARE NECESSARY AT TIME OF IMPLEMENTATION.
C         
C     NOCARD CONTAINS THE INFORMATION SAVED FROM THE OMNITAB CONTROL  
C        INSTRUCTION.         
C         
C     IFMTS CONTAINS THE FORMAT USED BY PRINT.    
C         
C     ==================================================================        
C         
C                    ***   SPECIFICATION STATEMENTS   ***   
C         
      COMMON /FRMATP/ IFMT(6), IFMTPR, IFMTS(12), IFMTX(12) 
      COMMON /HEADCH/ ITLE(60,6), NOCARD(80), NOMNIT(80)
      COMMON /LANGUE/ LANGC, LANGP      
      COMMON /LARRAY/ NDIST, NID, NIL, NIR, NIRD, NITB, NITP, NPROP   
C         
C     ==================================================================        
C         
C                    ***   TYPE STATEMENTS   ***   
C         
      CHARACTER IFMT*80, IFMTPR*5, IFMTS*1, IFMTX*1         
      CHARACTER ITLE*1, NOCARD*1, NOMNIT*1
C         
C     ==================================================================        
C         
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C         
C     THE FOLLOWING EIGHT DATA STATEMENTS DEFINE THE SIZE OF TABLES USED        
C        IN BLOCK DATA PROCEDURE 1 FOR DEFINING NAME(.), L1 AND L2 FOR
C        ALL THE COMMANDS IN THE OMNITAB COMPUTING SYSTEM.  
C         
C              NDIST = ACTUAL LENGTH OF ARRAY  IDIST        
C              NID   = ACTUAL LENGTH OF ARRAY  ID 
C              NIL   = ACTUAL LENGTH OF ARRAY  IL 
C              NIR   = ACTUAL LENGTH OF ARRAY  IR 
C              NIRD  = ACTUAL LENGTH OF ARRAY  IRD
C              NITB  = ACTUAL LENGTH OF ARRAY  ITB
C              NITP  = ACTUAL LENGTH OF ARRAYS IC AND ITP   
C              NPROP = ACTUAL LENGTH OF ARRAY  IPROP        
C         
      DATA NDIST /  30 /      
      DATA NID   /   8 /      
      DATA NIL   /  14 /      
      DATA NIR   / 282 /      
      DATA NIRD  /  35 /      
      DATA NITB  /  14 /      
      DATA NITP  /   9 /      
      DATA NPROP /   5 /      
C         
C     ..................................................................        
C         
C     THE FOLLOWING TWO DATA STATEMENTS ARE NEEDED FOR MULTILINGUAL   
C        CAPABILITY.
C         
      DATA LANGP / 1 /        
      DATA LANGC / 1 /        
C         
C        LANGP = 0, IF MULTILINGUAL CAPABILITY NOT USED.    
C                L2 OF LANGUAGE DETERMINED BY OMNITAB.      
C        LANGC = L2 FOR LANGUAGE CURRENTLY BEING USED.      
C                    SHOULD BE SET EQUAL TO LANGP INITIALLY.
C         
C     ..................................................................        
C         
      DATA NOCARD( 1), NOCARD( 2), NOCARD( 3), NOCARD( 4), NOCARD( 5),
     1     NOCARD( 6), NOCARD( 7), NOCARD( 8), NOCARD( 9), NOCARD(10),
     2     NOCARD(11), NOCARD(12), NOCARD(13), NOCARD(14), NOCARD(15),
     3     NOCARD(16), NOCARD(17), NOCARD(18), NOCARD(19), NOCARD(20)/
     4            ' ',        ' ',        ' ',        ' ',        ' ',
     5            ' ',        ' ',        ' ',        ' ',        ' ',
     6            ' ',        ' ',        ' ',        ' ',        ' ',
     7            ' ',        ' ',        ' ',        ' ',        ' '/
      DATA NOCARD(21), NOCARD(22), NOCARD(23), NOCARD(24), NOCARD(25),
     1     NOCARD(26), NOCARD(27), NOCARD(28), NOCARD(29), NOCARD(30),
     2     NOCARD(31), NOCARD(32), NOCARD(33), NOCARD(34), NOCARD(35),
     3     NOCARD(36), NOCARD(37), NOCARD(38), NOCARD(39), NOCARD(40)/
     4            ' ',        ' ',        ' ',        ' ',        ' ',
     5            ' ',        ' ',        ' ',        ' ',        ' ',
     6            ' ',        ' ',        ' ',        ' ',        ' ',
     7            ' ',        'O',        'M',        'N',        'I'/
      DATA NOCARD(41), NOCARD(42), NOCARD(43), NOCARD(44), NOCARD(45),
     1     NOCARD(46), NOCARD(47), NOCARD(48), NOCARD(49), NOCARD(50),
     2     NOCARD(51), NOCARD(52), NOCARD(53), NOCARD(54), NOCARD(55),
     3     NOCARD(56), NOCARD(57), NOCARD(58), NOCARD(59), NOCARD(60)/
     4            'T',        'A',        'B',        ' ',        ' ',
     5            ' ',        ' ',        ' ',        ' ',        ' ',
     6            ' ',        ' ',        ' ',        ' ',        ' ',
     7            ' ',        ' ',        ' ',        ' ',        ' '/
      DATA NOCARD(61), NOCARD(62), NOCARD(63), NOCARD(64), NOCARD(65),
     5     NOCARD(66), NOCARD(67), NOCARD(68), NOCARD(69), NOCARD(70),
     6     NOCARD(71), NOCARD(72), NOCARD(73), NOCARD(74), NOCARD(75),
     7     NOCARD(76), NOCARD(77), NOCARD(78), NOCARD(79), NOCARD(80)/
     4            ' ',        ' ',        ' ',        ' ',        ' ',
     5            ' ',        ' ',        ' ',        ' ',        ' ',
     6            ' ',        ' ',        ' ',        ' ',        ' ',
     7            ' ',        ' ',        ' ',        ' ',        ' '/
C         
C     ..................................................................        
C         
C        IFMTPR(.) CONTAINS THE FORMAT (A80), WHERE         
C         
      DATA IFMTPR / '(A80)' / 
C         
C         
      DATA IFMTS(1),IFMTS(2),IFMTS(3), IFMTS(4),IFMTS( 5), IFMTS(6),  
     1     IFMTS(7),IFMTS(8),IFMTS(9),IFMTS(10),IFMTS(11),IFMTS(12)/  
     2          '(',     '1',     'P',      ' ',      '8',      'E',  
     3          '1',     '5',     '.',      ' ',      '6',      ')'/  
C         
C     ==================================================================        
C         
      END    
*DECOMP   
      SUBROUTINE DECOMP (A,NASIZE,UL,NN,SCALES,APS,KEY)     
C         
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. DECOMP V 7.00  5/30/91. **        
C         
C     ==================================================================        
C         
C                        ***   GENERAL COMMENTS   ***       
C         
C      PROGRAM UNIT MODIFIED FROM       
C     CHAPTER 17 OF G. E. FORSYTHE AND C. B. MOLER'S 'COMPUTER        
C     SOLUTION OF LINEAR ALGEBRAIC SYSTEMS', PRENTICE-HALL (1967).    
C         
C     THIS SUBROUTINE COMPUTES MATRICES L AND U AND PERMUTATION MATRIX P        
C     SO THAT LU = PA.  IT STORES L - I AND U IN UL.  ARRAY IPS CONTAINS        
C     PERMUTED ROW INDICES.   
C         
C               ADAPTED TO OMNITAB BY - 
C                      ROY H.WAMPLER,   
C                      STATISTICAL ENGINEERING DIVISION,    
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,      
C                      A337 ADMINISTRATION BUILDING,        
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,        
C                      GAITHERSBURG, MD  20234    
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION - FEBRUARY, 1977.       
C                   CURRENT VERSION -      MAY, 1991.       
C         
C     ==================================================================        
C         
C                    ***   SPECIFICATION STATEMENTS   ***   
C         
      DIMENSION APS(*)        
C         
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO      
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW  
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO        
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG         
C         
      REAL             A(*), UL(*), SCALES(*)     
      REAL             BIG, EM, PIVOT, ROWNRM, SIZE         
C         
C     ==================================================================        
C         
      N = NN        
      KEY = IZERO   
C         
C     INITIALIZE APS, UL AND SCALES.    
C         
      DO 50 I=1,N   
        APS(I) = I  
        ROWNRM = RZERO
        IJUL = I
        IJA  = I
        DO 20 J=1,N 
          UL(IJUL) = A(IJA)    
          IF (ROWNRM-ABS(UL(IJUL))) 10,15,15       
  10      ROWNRM = ABS(UL(IJUL))         
  15      IJUL = IJUL + NN
          IJA  = IJA  + NASIZE
  20    CONTINUE    
        IF (ROWNRM) 30,40,30  
  30    SCALES(I) = FDIV (RONE,ROWNRM,IND)        
        GO TO 50    
  40    KEY = IONE  
        RETURN      
  50  CONTINUE      
C         
C     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING. 
C         
      NM1 = N-IONE  
      DO 130 K=1,NM1
        BIG    = RZERO        
        IDXPIV = K  
        DO 70 I=K,N 
          IP = APS(I) + 0.05
          IPK = IP + (K - IONE) * NN
          SIZE = ABS(UL(IPK))*SCALES(IP)         
          IF (SIZE-BIG) 70,70,60        
  60        BIG = SIZE        
            IDXPIV = I        
  70    CONTINUE    
        IF (BIG) 90,80,90     
  80    KEY = IONE  
        RETURN      
  90    IF (IDXPIV-K) 100,110,100       
 100      J = APS(K) + 0.05
          APS(K) = APS(IDXPIV)
          APS(IDXPIV) = J     
 110    KP    = APS(K) + 0.05 
        KPK = KP + (K - IONE) * NN 
        PIVOT = UL(KPK)      
        KP1   = K + IONE      
        DO 120 I=KP1,N        
          IP = APS(I) + 0.05  
          IPK = IP + (K - IONE) * NN
          EM = FDIV (-UL(IPK),PIVOT,INDA)        
          UL(IPK) = -EM      
          DO 120 J=KP1,N      
             IPJ = IP + (J - IONE) * NN
             KPJ = KP + (J - IONE) * NN
             UL(IPJ) = UL(IPJ) + EM*UL(KPJ)    
C         
C            INNER LOOP.  USE MACHINE CODING IF COMPILER    
C               DOES NOT PRODUCE EFFICIENT CODE.  
C         
 120    CONTINUE    
 130  CONTINUE      
C         
      KP = APS(N) + 0.05
      KPN = KP + (N -IONE) * NN
      IF (ABS(UL(KPN)).GT.RER) RETURN  
      KEY = IONE    
      RETURN        
C         
C     ==================================================================        
C         
      END 
*DESC1
      SUBROUTINE DESC1
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  DESC1 V 7.00 12/ 7/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE DESCRIBE INSTRUCTION FOR ...
C        L1 = 1 TO  11.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING ANDAPPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -  OCTOBER, 1978.
C                   CURRENT VERSION - DECEMBER, 1989.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
C     ==================================================================
C
      IF (L1.LT.1 .OR. L1.GT.11) GO TO 50
C
      GO TO ( 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11), L1
C
   1  GO TO (111, 50,112), L2
   2  GO TO (121,122), L2
   3  GO TO (131,132), L2
   4  GO TO (141,142), L2
   5  GO TO (151,152), L2
   6  GO TO (161,162), L2
   7  GO TO (171,172), L2
   8  GO TO (181,182), L2
   9  GO TO 50
  10  GO TO 50
  11  GO TO (201,202,203,204,205,206,207,208,209,210,211,212), L2
C
C     ..................................................................
C
  50  CALL ERROR (1)
      RETURN
C
C     ..................................................................
C
C     L1 = 1.
C
 111  WRITE (IPRINT,1101)
      RETURN
C
 112  WRITE (IPRINT,1102)
      RETURN
C
C     ..................................................................
C
C     L1 = 2.
C
 121  WRITE (IPRINT,1201)
      RETURN
C
 122  WRITE (IPRINT,1202)
      RETURN
C
C     ..................................................................
C
C     L1 = 3.
C
 131  WRITE (IPRINT,1301)
      RETURN
C
 132  WRITE (IPRINT,1302)
      RETURN
C
C     ..................................................................
C
C     L1 = 4.
C
 141  WRITE (IPRINT,1401)
      RETURN
C
 142  WRITE (IPRINT,1402)
      RETURN
C
C     ..................................................................
C
C     L1 = 5.
C
 151  WRITE (IPRINT,1501)
      RETURN
C
 152  WRITE (IPRINT,1502)
      RETURN
C
C     ..................................................................
C
C     L1 = 6.
C
 161  WRITE (IPRINT,1601)
      RETURN
C
 162  WRITE (IPRINT,1602)
      RETURN
C
C     ..................................................................
C
C     L1 = 7.
C
 171  WRITE (IPRINT,1701)
      RETURN
C
 172  WRITE (IPRINT,1702)
      RETURN
C
C     ..................................................................
C
C     L1 = 8.
C
 181  WRITE (IPRINT,1801)
      RETURN
C
 182  WRITE (IPRINT,1802)
      RETURN
C
C     ..................................................................
C
C     L1 = 11.
C
 201  WRITE (IPRINT,2101)
      RETURN
C
 202  WRITE (IPRINT,2102)
      RETURN
C
 203  WRITE (IPRINT,2103)
      RETURN
C
 204  WRITE (IPRINT,2104)
      RETURN
C
 205  WRITE (IPRINT,2105)
      RETURN
C
 206  WRITE (IPRINT,2106)
      RETURN
C
 207  WRITE (IPRINT,2107)
      RETURN
C
 208  WRITE (IPRINT,2108)
      RETURN
C
 209  WRITE (IPRINT,2109)
      RETURN
C
 210  WRITE (IPRINT,2110)
      RETURN
C
 211  WRITE (IPRINT,2111)
      RETURN
C
 212  WRITE (IPRINT,2112)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
1101  FORMAT (1X,69HRESET NRMAX TO EQUAL (R) ROWS  $ SETS NEW WORKING LE
     1NGTH OF WORKSHEET)
1102  FORMAT (1X,62HRESET ''V'' EQUAL TO (K)   $ ''V'' IS VARIABLE V, W,
     1 X, Y OR Z)
1201  FORMAT (1X,30HPRINT COLUMNS (C), (C) ... (C)/
     2        1X,53HPRINT COLUMNS (C) ... (C) WITH (K) SIGNIFICANT DIGIT
     2S/
     2        1X,71HPRINT COLS (C)...(C), (K) S. DIGITS, (C)...(C) WITH 
     2(K) S. DIGITS, ETC./
     2        1X,71HPRINT (K) COLS,(C), (S) S.D., (C) WITH (S), ETC $ MA
     2X WIDTH 22,3 BLANKS/
     2        1X,64HPRINT (K) COLS, (C) WITH (S) AND MAX WIDTH (M), (C),
     2(S),(M) ETC./
     3        1X,71HPRINT (K) COLS, (C), (S) S.D. (M) MAX W (B) BLANKS, 
     3(C),(S),(M),(B) ETC)
1202  FORMAT (1X,44HPRINT ''L'' FORMAT, COLUMNS (C), (C) ... (C))
1301  FORMAT (1X,69HPUNCH DATA IN COLS (C), (C)...(C) ON HOLLERITH CARDS
     1 $ 4 COLUMN LIMIT)
1302  FORMAT (1X,63HPUNCH ''L'' FORMAT, DATA IN COLS (C) ... (C) ON HOLL
     1ERITH CARDS)
1401  FORMAT (1X,43HAPRINT THE ARRAY IN (R),(C) OF SIZE (R)X(C))
1402  FORMAT (1X,57HAPRINT ''L'' FORMAT, THE ARRAY IN (R),(C) OF SIZE (R
     1)X(C))
1501  FORMAT (1X,70HREAD DATA ON FOLLOWING CARDS INTO COLS (C)...(C) ONE
     1 CARD FOR EACH ROW)
1502  FORMAT (1X,67HREAD ''L'' FORMAT, (N) CARDS OR ROWS, INTO COLUMNS (
     1C), (C) ... (C))
1601  FORMAT (1X,44HABRIDGE ROW (R) OF COLUMNS (C), (C), ... (C)/
     2        1X,71HABRIDGE ROW (R) OF COLUMNS (C), (C) ... (C) WITH (K)
     2 SIGNIFICANT DIGITS/
     2        1X,71HABRIDGE ROW (R) OF (C)...(C) WITH (K) S. DIGITS, (C)
     2...(C) WITH (K) .../
     2        1X,71HABRIDGE ROW (R),(K) COLS,(C) (S) S.D.,(C) (S)...$ MA
     2X WIDTH 22,3 BLANKS/
     2        1X,71HABRIDGE ROW (R),(K) COLS, (C) (S) (M) MAX WIDTH, (C)
     2(S)(M)...$ 3 BLANKS/
     3        1X,71HABRIDGE ROW (R) OF (K) COLS, (C) (S) (M) (B) BLANKS,
     3 (C) (S) (M) (B)...)
1602  FORMAT (1X,57HABRIDGE ''L'' FORMAT, ROW (R) OF COLUMNS (C), (C) ..
     1. (C))
1701  FORMAT (1X,44HMPRINT THE MATRIX IN (R),(C) OF SIZE (R)X(C))
1702  FORMAT (1X,58HMPRINT ''L'' FORMAT, THE MATRIX IN (R),(C) OF SIZE (
     1R)X(C))
1801  FORMAT (1X,69HNPRINT COLS (C), (C), ... (C)  $  NO NEW PAGE, COL H
     1EADINGS OR TITLES/
     2        1X,60HNPRINT COLUMNS (C), (C), ... (C) WITH (K) SIGNIFICAN
     2T DIGITS/
     2        1X,70HNPRINT COLS (C)..(C) WITH (K) S. DIGITS, (C)...(C) W
     2ITH (K) S. D. ETC./
     2        1X,69HNPRINT (K) COLS (C) (S) SD, (C) WITH (S), ETC $ MAX 
     2WIDTH 22,3 BLANKS/
     2        1X,69HNPRINT (K) COLS, (C) WITH (S) S.D. AND (M) MAX WIDTH
     2, (C),(S),(M) .../
     3        1X,71HNPRINT (K) COLS, (C) (S) S.D. (M) MAX W (B) BLANKS, 
     3(C),(S),(M),(B) ...)
1802  FORMAT (1X,45HNPRINT ''L'' FORMAT, COLUMNS (C), (C) ... (C))
2101  FORMAT (1X,36HADD (E) TO (E) AND PUT IN COLUMN (C)/
     3        1X,62HADD (E) TO (E), MULTIPLY BY (E), ADD TO (E), PUT IN 
     3COLUMN (C))
2102  FORMAT (1X,38HSUB (E) FROM (E) AND PUT IN COLUMN (C)/
     2        1X,64HSUB (E) FROM (E), MULTIPLY BY (E), ADD TO (E), PUT I
     2N COLUMN (C)/
     2        1X,43HSUBTRACT (E) FROM (E) AND PUT IN COLUMN (C)/
     3        1X,69HSUBTRACT (E) FROM (E), MULTIPLY BY (E), ADD TO (E), 
     3PUT IN COLUMN (C))
2103  FORMAT (1X,37HMULT (E) BY (E) AND PUT IN COLUMN (C)/
     2        1X,63HMULT (E) BY (E), MULTIPLY BY (E), ADD TO (E), PUT IN
     2 COLUMN (C)/
     2        1X,41HMULTIPLY (E) BY (E) AND PUT IN COLUMN (C)/
     3        1X,67HMULTIPLY (E) BY (E), MULTIPLY BY (E), ADD TO (E), PU
     3T IN COLUMN (C))
2104  FORMAT (1X,36HDIV (E) BY (E) AND PUT IN COLUMN (C)/
     2        1X,62HDIV (E) BY (E), MULTIPLY BY (E), ADD TO (E), PUT IN 
     2COLUMN (C)/
     2        1X,39HDIVIDE (E) BY (E) AND PUT IN COLUMN (C)/
     3        1X,65HDIVIDE (E) BY (E), MULTIPLY BY (E), ADD TO (E), PUT 
     3IN COLUMN (C))
2105  FORMAT (1X,44HRAISE (E) TO POWER (E) AND PUT IN COLUMN (C)/
     3        1X,70HRAISE (E) TO POWER (E), MULTIPLY BY (E), ADD TO (E),
     3 PUT IN COLUMN (C))
2106  FORMAT (1X,49HACCURACY OF (E) COMPARED TO (E) PUT IN COLUMN (C))
2107  FORMAT (1X,30HDIFFERENCES OF Y IN COLUMN (C)/
     3        1X,65HDIFFERENCES OF Y IN COLUMN (C) PUT IN COLUMNS (C), (
     3C), ... , (C))
2108  FORMAT (1X,48HDIVDIFFERENCES FOR X IN COL (C) AND Y IN COL (C)/
     3        1X,70HDIVDIFFERENCES FOR X IN COL (C) AND Y IN COL (C) PUT
     3 IN (C), ... , (C))
2109  FORMAT (1X,66HSDIFFERENCES OF Y IN COLUMN (C) PUT IN COLUMNS (C), 
     1(C), ... , (C))
2110  FORMAT (1X,68HSDIVDIFFERENCES FOR X IN (C) AND Y IN (C) PUT IN COL
     1S (C), (C)...(C))
2111  FORMAT (1X,33HRECODE COLUMN (C) INTO COLUMN (C))
2112  FORMAT (1X,50HCODE COLUMN (C) USING LENGTH (K) PUT IN COLUMN (C)/
     3        1X,66HCODE COLUMN (C) STARTING AT (K) USING LENGTH (K) PUT
     3 IN COLUMN (C))
C
C     ==================================================================
C
      END
*DESC10
      SUBROUTINE DESC10
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. DESC10 V 7.00 12/ 7/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE DESCRIBE INSTRUCTION FOR ...
C        L1 = 50 TO  55.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHAERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - NOVEMBER, 1978.
C                   CURRENT VERSION - DECEMBER, 1989.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
C     ==================================================================
C
      IF (L1.LT.50 .OR. L1.GT.55) GO TO 50
C
      L = L1 - 49
      GO TO ( 1, 2, 3, 4,50, 6), L
C
   1  LL = IDIV (L2-1,7,IND) + IONE
        GO TO (101,102,103,104,105), LL
   2  GO TO (201,202,203), L2
   3  GO TO (301,302), L2
   4  GO TO (401,402), L2
   6  GO TO (601,602,603, 50, 50, 50, 50,608, 50,610,
     1       611,612,613,614,615,616,617,618, 50, 50,
     2        50, 50,623,624,625,626, 50, 50, 50, 50,
     3        50,632,633,634,635,636,637,638,639, 50,
     4       641,642,643,644,645,646,647,648,649, 50,
     5        50, 50, 50,654,655,656,657), L2
C
C     ..................................................................
C
  50  CALL ERROR (1)
      RETURN
C
C     ..................................................................
C
C     L1 = 50.
C
 101  WRITE (IPRINT,6001)
      RETURN
C
 102  WRITE (IPRINT,6008)
      RETURN
C
 103  WRITE (IPRINT,6015)
      RETURN
C
 104  WRITE (IPRINT,6023)
      RETURN
C
 105  WRITE (IPRINT,6029)
      RETURN
C
C     ..................................................................
C
C     L1 = 51.
C
 201  WRITE (IPRINT,6101)
      RETURN
C
 202  WRITE (IPRINT,6102)
      RETURN
C
 203  WRITE (IPRINT,6103)
      RETURN
C
C     ..................................................................
C
C     L1 = 52.
C
 301  WRITE (IPRINT,6201)
      RETURN
C
 302  WRITE (IPRINT,6202)
      RETURN
C
C     ..................................................................
C
C     L1 = 53.
C
 401  WRITE (IPRINT,6301)
      RETURN
C
 402  WRITE (IPRINT,6302)
      RETURN
C
C     ..................................................................
C
C     L1 = 55.
C
 601  WRITE (IPRINT,6501)
      RETURN
C
 602  WRITE (IPRINT,6502)
      RETURN
C
 603  WRITE (IPRINT,6503)
      RETURN
C
 608  WRITE (IPRINT,6508)
      RETURN
C
 610  WRITE (IPRINT,6510)
      RETURN
C
 611  WRITE (IPRINT,6511)
      RETURN
C
 612  WRITE (IPRINT,6512)
      RETURN
C
 613  WRITE (IPRINT,6513)
      RETURN
C
 614  WRITE (IPRINT,6514)
      RETURN
C
 615  WRITE (IPRINT,6515)
      RETURN
C
 616  WRITE (IPRINT,6516)
      RETURN
C
 617  WRITE (IPRINT,6517)
      RETURN
C
 618  WRITE (IPRINT,6518)
      RETURN
C
 623  WRITE (IPRINT,6523)
      RETURN
C
 624  WRITE (IPRINT,6524)
      RETURN
C
 625  WRITE (IPRINT,6525)
      RETURN
C
 626  WRITE (IPRINT,6526)
      RETURN
C
 632  WRITE (IPRINT,6532)
      RETURN
C
 633  WRITE (IPRINT,6533)
      RETURN
C
 634  WRITE (IPRINT,6534)
      RETURN
C
 635  WRITE (IPRINT,6535)
      RETURN
C
 636  WRITE (IPRINT,6536)
      RETURN
C
 637  WRITE (IPRINT,6537)
      RETURN
C
 638  WRITE (IPRINT,6538)
      RETURN
C
 639  WRITE (IPRINT,6539)
      RETURN
C
 641  WRITE (IPRINT,6541)
      RETURN
C
 642  WRITE (IPRINT,6542)
      RETURN
C
 643  WRITE (IPRINT,6543)
      RETURN
C
 644  WRITE (IPRINT,6544)
      RETURN
C
 645  WRITE (IPRINT,6545)
      RETURN
C
 646  WRITE (IPRINT,6546)
      RETURN
C
 647  WRITE (IPRINT,6547)
      RETURN
C
 648  WRITE (IPRINT,6548)
      RETURN
C
 649  WRITE (IPRINT,6549)
      RETURN
C
 654  WRITE (IPRINT,6554)
      RETURN
C
 655  WRITE (IPRINT,6555)
      RETURN
C
 656  WRITE (IPRINT,6556)
      RETURN
C
 657  WRITE (IPRINT,6557)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
6001  FORMAT (1X,18HENDFILE TAPE ''L''/
     3        1X,18HENDFILE UNIT ''L'')
6008  FORMAT (1X,17HREWIND TAPE ''L''/
     3        1X,17HREWIND UNIT ''L'')
6015  FORMAT (1X,35HSKIP TAPE ''L'' FORWARD (N) RECORDS/
     3        1X,43HSKIP UNIT ''L'' FORWARD (N) LOGICAL RECORDS)
6023  FORMAT (1X,32HBACKSPACE TAPE ''L'' (N) RECORDS/
     3        1X,40HBACKSPACE UNIT ''L'' (R) LOGICAL RECORDS)
6029  FORMAT (1X,43HTAPE WITH (C) CHARACTERS PER LOGICAL RECORD/
     2        1X,69HTAPE WITH (C) CHARACTERS/LOGICAL RECORD AND (R) LOGI
     2CAL RECORDS/BLOCK/
     2        1X,43HUNIT WITH (C) CHARACTERS PER LOGICAL RECORD/
     3        1X,69HUNIT WITH (C) CHARACTERS/LOGICAL RECORD AND (R) LOGI
     3CAL RECORDS/BLOCK)
6101  FORMAT (1X,53HM(XX') MATRIX IN (R),(C) OF SIZE (R)X(C) INTO (R),(C
     1))
6102  FORMAT (1X,69HM(X'AX) A IN (R),(C) SIZE (R)X(C), X (R),(C) SIZE (R
     1)X(C), IN (R),(C)/
     3        1X,53HM(X'X) MATRIX IN (R),(C) OF SIZE (R)X(C) INTO (R),(C
     3))
6103  FORMAT (1X,71HM(XAX') A IN (R),(C) SIZE (R)X(C), X (R),(C) SIZE (R
     1)X(C), INTO (R),(C))
6201  FORMAT (1X,69HM(AD) MAT (R),(C) SIZE (R)X(C) X MAT WITH (C) IN DIA
     1G, PUT IN (R),(C))
6202  FORMAT (1X,70HM(DA) (R),(C) SIZE (R)X(C) PREMULT BY MAT WITH (C) I
     1N DIAG, IN (R),(C))
6301  FORMAT (1X,68HM(AV) MATRIX (R),(C) SIZE (R)X(C) BY VECTOR IN (C) P
     1UT VECTOR IN (C)/
     3        1X,69HM(AV) MATRIX (R),(C) SIZE (R)X(C) BY COLUMN (C) PUT 
     3VECTOR IN (R),(C))
6302  FORMAT (1X,71HM(V'A) MATRIX (R),(C) SIZE (R)X(C), VECTOR IN (C) PU
     1T VECTOR IN ROW (R)/
     3        1X,69HM(V'A) (R),(C) SIZE (R)X(C), VECTOR IN COL (C), ROW 
     3VECTOR IN (R),(C))
6501  FORMAT (1X,39HNORMAL DENSITY OF (E) PUT IN COLUMN (C))
6502  FORMAT (1X,42HLOGNORMAL DENSITY OF (E) PUT IN COLUMN (C))
6503  FORMAT (1X,43HHALFNORMAL DENSITY OF (E) PUT IN COLUMN (C))
6508  FORMAT (1X,65HBETA DENSITY OF (E) WITH PARAMETERS (E) AND (E) PUT 
     1IN COLUMN (C))
6510  FORMAT (1X,40HUNIFORM DENSITY OF (E) PUT IN COLUMN (C))
6511  FORMAT (1X,39HCAUCHY DENSITY OF (E) PUT IN COLUMN (C))
6512  FORMAT (1X,58HLAMBDA DENSITY OF (E) WITH PARAMETER (E) PUT IN COLU
     1MN (C))
6513  FORMAT (1X,40HEXTREME DENSITY OF (E) PUT IN COLUMN (C)/
     3        1X,59HEXTREME DENSITY OF (E) WITH PARAMETER (E) PUT IN COL
     3UMN (C))
6514  FORMAT (1X,59HWEIBULL DENSITY OF (E) WITH PARAMETER (E) PUT IN COL
     1UMN (C))
6515  FORMAT (1X,58HPARETO DENSITY OF (E) WITH PARAMETER (E) PUT IN COLU
     1MN (C))
6516  FORMAT (1X,44HEXPONENTIAL DENSITY OF (E) PUT IN COLUMN (C))
6517  FORMAT (1X,45HDEXPONENTIAL DENSITY OF (E) PUT IN COLUMN (C))
6518  FORMAT (1X,41HLOGISTIC DENSITY OF (E) PUT IN COLUMN (C))
6523  FORMAT (1X,63HBINOMIAL DENSITY OF (E) WITH N = (E) AND P = (E) PUT
     1 IN COL (C))
6524  FORMAT (1X,66HNEGBINOMIAL DENSITY OF (E) WITH N = (E) AND P = (E) 
     1PUT IN COL (C))
6525  FORMAT (1X,59HPOISSON DENSITY OF (E) WITH PARAMETER (E) PUT IN COL
     1UMN (C))
6526  FORMAT (1X,61HGEOMETRIC DENSITY OF (E) WITH PARAMETER (E) PUT IN C
     1OLUMN (C))
6532  FORMAT (1X,42HNORMAL CUMULATIVE OF (E) PUT IN COLUMN (C))
6533  FORMAT (1X,45HLOGNORMAL CUMULATIVE OF (E) PUT IN COLUMN (C))
6534  FORMAT (1X,46HHALFNORMAL CUMULATIVE OF (E) PUT IN COLUMN (C))
6535  FORMAT (1X,62HT CUMULATIVE OF (E) WITH (E) DEGREES OF FREEDOM PUT 
     1IN COL (C))
6536  FORMAT (1X,71HCHISQUARED CUMULATIVE OF (E) WITH (E) DEGREES OF FRE
     1EDOM PUT IN COL (C))
6537  FORMAT (1X,60HGAMMA CUMULATIVE OF (E) WITH PARAMETER (E) PUT IN CO
     1LUMN (C))
6538  FORMAT (1X,70HF CUMULATIVE OF (E) WITH (E) AND (E) DEGREES OF FREE
     1DOM PUT IN COL (C))
6539  FORMAT (1X,65HBETA CUMULATIVE OF (E) WITH PARAMETERS (E) AND (E) P
     1UT IN COL (C))
6541  FORMAT (1X,43HUNIFORM CUMULATIVE OF (E) PUT IN COLUMN (C))
6542  FORMAT (1X,42HCAUCHY CUMULATIVE OF (E) PUT IN COLUMN (C))
6543  FORMAT (1X,61HLAMBDA CUMULATIVE OF (E) WITH PARAMETER (E) PUT IN C
     1OLUMN (C))
6544  FORMAT (1X,43HEXTREME CUMULATIVE OF (E) PUT IN COLUMN (C)/
     3        1X,62HEXTREME CUMULATIVE OF (E) WITH PARAMETER (E) PUT IN 
     3COLUMN (C))
6545  FORMAT (1X,62HWEIBULL CUMULATIVE OF (E) WITH PARAMETER (E) PUT IN 
     1COLUMN (C))
6546  FORMAT (1X,61HPARETO CUMULATIVE OF (E) WITH PARAMETER (E) PUT IN C
     1OLUMN (C))
6547  FORMAT (1X,47HEXPONENTIAL CUMULATIVE OF (E) PUT IN COLUMN (C))
6548  FORMAT (1X,48HDEXPONENTIAL CUMULATIVE OF (E) PUT IN COLUMN (C))
6549  FORMAT (1X,44HLOGISTIC CUMULATIVE OF (E) PUT IN COLUMN (C))
6554  FORMAT (1X,66HBINOMIAL CUMULATIVE OF (E) WITH N = (E) AND P = (E) 
     1PUT IN COL (C))
6555  FORMAT (1X,69HNEGBINOMIAL CUMULATIVE OF (E) WITH N = (E) AND P = (
     1E) PUT IN COL (C))
6556  FORMAT (1X,62HPOISSON CUMULATIVE OF (E) WITH PARAMETER (E) PUT IN 
     1COLUMN (C))
6557  FORMAT (1X,64HGEOMETRIC CUMULATIVE OF (E) WITH PARAMETER (E) PUT I
     1N COLUMN (C))
C
C     ==================================================================
C
      END
*DESC11
      SUBROUTINE DESC11
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. DESC11 V 7.00 12/ 7/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE DESCRIBE INSTRUCTION FOR ...
C        L1 = 56 TO  57.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - NOVEMBER, 1978.
C                   CURRENT VERSION - DECEMBER, 1989.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
C     ==================================================================
C
      IF (L1.LT.56 .OR. L1.GT.57) GO TO 50
C
      L = L1 - 55
      GO TO ( 1, 2), L
C
   1  GO TO (101,102,103,104,105,106, 50, 50, 50,110,
     1       111,112,113,114,115,116,117,118, 50, 50,
     2        50, 50,123,124,125,126, 50, 50, 50, 50,
     3        50,132,133,134,135,136,137,138,139, 50,
     4       141,142,143,144,145,146,147,148,149, 50,
     5        50, 50, 50,154,155,156,157), L2
   2  GO TO (201,202,203, 50, 50,206, 50, 50, 50,210,
     1       211,212,213,214,215,216,217,218, 50, 50,
     2        50, 50, 50, 50,225), L2
C
C     ..................................................................
C
  50  CALL ERROR (1)
      RETURN
C
C     ..................................................................
C
C     L1 = 56.
C
 101  WRITE (IPRINT,6601)
      RETURN
C
 102  WRITE (IPRINT,6602)
      RETURN
C
 103  WRITE (IPRINT,6603)
      RETURN
C
 104  WRITE (IPRINT,6604)
      RETURN
C
 105  WRITE (IPRINT,6605)
      RETURN
C
 106  WRITE (IPRINT,6606)
      RETURN
C
 110  WRITE (IPRINT,6610)
      RETURN
C
 111  WRITE (IPRINT,6611)
      RETURN
C
 112  WRITE (IPRINT,6612)
      RETURN
C
 113  WRITE (IPRINT,6613)
      RETURN
C
 114  WRITE (IPRINT,6614)
      RETURN
C
 115  WRITE (IPRINT,6615)
      RETURN
C
 116  WRITE (IPRINT,6616)
      RETURN
C
 117  WRITE (IPRINT,6617)
      RETURN
C
 118  WRITE (IPRINT,6618)
      RETURN
C
 123  WRITE (IPRINT,6623)
      RETURN
C
 124  WRITE (IPRINT,6624)
      RETURN
C
 125  WRITE (IPRINT,6625)
      RETURN
C
 126  WRITE (IPRINT,6626)
      RETURN
C
 132  WRITE (IPRINT,6632)
      RETURN
C
 133  WRITE (IPRINT,6633)
      RETURN
C
 134  WRITE (IPRINT,6634)
      RETURN
C
 135  WRITE (IPRINT,6635)
      RETURN
C
 136  WRITE (IPRINT,6636)
      RETURN
C
 137  WRITE (IPRINT,6637)
      RETURN
C
 138  WRITE (IPRINT,6638)
      RETURN
C
 139  WRITE (IPRINT,6639)
      RETURN
C
 141  WRITE (IPRINT,6641)
      RETURN
C
 142  WRITE (IPRINT,6642)
      RETURN
C
 143  WRITE (IPRINT,6643)
      RETURN
C
 144  WRITE (IPRINT,6644)
      RETURN
C
 145  WRITE (IPRINT,6645)
      RETURN
C
 146  WRITE (IPRINT,6646)
      RETURN
C
 147  WRITE (IPRINT,6647)
      RETURN
C
 148  WRITE (IPRINT,6648)
      RETURN
C
 149  WRITE (IPRINT,6649)
      RETURN
C
 154  WRITE (IPRINT,6654)
      RETURN
C
 155  WRITE (IPRINT,6655)
      RETURN
C
 156  WRITE (IPRINT,6656)
      RETURN
C
 157  WRITE (IPRINT,6657)
      RETURN
C
C     ..................................................................
C
C     L1 = 57.
C
 201  WRITE (IPRINT,6701)
      RETURN
C
 202  WRITE (IPRINT,6702)
      RETURN
C
 203  WRITE (IPRINT,6703)
      RETURN
C
 206  WRITE (IPRINT,6706)
      RETURN
C
 210  WRITE (IPRINT,6710)
      RETURN
C
 211  WRITE (IPRINT,6711)
      RETURN
C
 212  WRITE (IPRINT,6712)
      RETURN
C
 213  WRITE (IPRINT,6713)
      RETURN
C
 214  WRITE (IPRINT,6714)
      RETURN
C
 215  WRITE (IPRINT,6715)
      RETURN
C
 216  WRITE (IPRINT,6716)
      RETURN
C
 217  WRITE (IPRINT,6717)
      RETURN
C
 218  WRITE (IPRINT,6718)
      RETURN
C
 225  WRITE (IPRINT,6725)
      RETURN
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
6601  FORMAT (1X,42HNORMAL PERCENTILE OF (E) PUT IN COLUMN (C))
6602  FORMAT (1X,45HLOGNORMAL PERCENTILE OF (E) PUT IN COLUMN (C))
6603  FORMAT (1X,46HHALFNORMAL PERCENTILE OF (E) PUT IN COLUMN (C))
6604  FORMAT (1X,62HT PERCENTILE OF (E) WITH (E) DEGREES OF FREEDOM PUT 
     1IN COL (C))
6605  FORMAT (1X,71HCHISQUARED PERCENTILE OF (E) WITH (E) DEGREES OF FRE
     1EDOM PUT IN COL (C))
6606  FORMAT (1X,60HGAMMA PERCENTILE OF (E) WITH PARAMETER (E) PUT IN CO
     1LUMN (C))
6610  FORMAT (1X,43HUNIFORM PERCENTILE OF (E) PUT IN COLUMN (C))
6611  FORMAT (1X,42HCAUCHY PERCENTILE OF (E) PUT IN COLUMN (C))
6612  FORMAT (1X,61HLAMBDA PERCENTILE OF (E) WITH PARAMETER (E) PUT IN C
     1OLUMN (C))
6613  FORMAT (1X,43HEXTREME PERCENTILE OF (E) PUT IN COLUMN (C)/
     3        1X,62HEXTREME PERCENTILE OF (E) WITH PARAMETER (E) PUT IN 
     3COLUMN (C))
6614  FORMAT (1X,62HWEIBULL PERCENTILE OF (E) WITH PARAMETER (E) PUT IN 
     1COLUMN (C))
6615  FORMAT (1X,61HPARETO PERCENTILE OF (E) WITH PARAMETER (E) PUT IN C
     1OLUMN (C))
6616  FORMAT (1X,47HEXPONENTIAL PERCENTILE OF (E) PUT IN COLUMN (C))
6617  FORMAT (1X,48HDEXPONENTIAL PERCENTILE OF (E) PUT IN COLUMN (C))
6618  FORMAT (1X,44HLOGISTIC PERCENTILE OF (E) PUT IN COLUMN (C))
6623  FORMAT (1X,66HBINOMIAL PERCENTILE OF (E) WITH N = (E) AND P = (E) 
     1PUT IN COL (C))
6624  FORMAT (1X,69HNEGBINOMIAL PERCENTILE OF (E) WITH N = (E) AND P = (
     1E) PUT IN COL (C))
6625  FORMAT (1X,62HPOISSON PERCENTILE OF (E) WITH PARAMETER (E) PUT IN 
     1COLUMN (C))
6626  FORMAT (1X,64HGEOMETRIC PERCENTILE OF (E) WITH PARAMETER (E) PUT I
     1N COLUMN (C))
6632  FORMAT (1X,31HNORMAL RANDOM PUT IN COLUMN (C)/
     3        1X,42HNORMAL RANDOM START (N), PUT IN COLUMN (C))
6633  FORMAT (1X,34HLOGNORMAL RANDOM PUT IN COLUMN (C)/
     3        1X,45HLOGNORMAL RANDOM START (N), PUT IN COLUMN (C))
6634  FORMAT (1X,35HHALFNORMAL RANDOM PUT IN COLUMN (C)/
     3        1X,46HHALFNORMAL RANDOM START (N), PUT IN COLUMN (C))
6635  FORMAT (1X,51HT RANDOM WITH (E) DEGREES OF FREEDOM PUT IN COL (C)/
     3        1X,62HT RANDOM START (N), WITH (E) DEGREES OF FREEDOM PUT 
     3IN COL (C))
6636  FORMAT (1X,60HCHISQUARED RANDOM WITH (E) DEGREES OF FREEDOM PUT IN
     1 COL (C)/
     3        1X,71HCHISQUARED RANDOM START (N), WITH (E) DEGREES OF FRE
     3EDOM PUT IN COL (C))
6637  FORMAT (1X,49HGAMMA RANDOM WITH PARAMETER (E) PUT IN COLUMN (C)/
     3        1X,60HGAMMA RANDOM START (N), WITH PARAMETER (E) PUT IN CO
     3LUMN (C))
6638  FORMAT (1X,59HF RANDOM WITH (E) AND (E) DEGREES OF FREEDOM PUT IN 
     1COL (C)/
     3        1X,70HF RANDOM START (N), WITH (E) AND (E) DEGREES OF FREE
     3DOM PUT IN COL (C))
6639  FORMAT (1X,54HBETA RANDOM WITH PARAMETERS (E) AND (E) PUT IN COL (
     1C)/
     3        1X,65HBETA RANDOM START (N), WITH PARAMETERS (E) AND (E) P
     3UT IN COL (C))
6641  FORMAT (1X,58HUNIFORM RANDOM NUMBERS STARTING WITH (K) PUT IN COLU
     1MN (C)/
     2        1X,32HUNIFORM RANDOM PUT IN COLUMN (C)/
     3        1X,43HUNIFORM RANDOM START (N), PUT IN COLUMN (C))
6642  FORMAT (1X,31HCAUCHY RANDOM PUT IN COLUMN (C)/
     3        1X,42HCAUCHY RANDOM START (N), PUT IN COLUMN (C))
6643  FORMAT (1X,50HLAMBDA RANDOM WITH PARAMETER (E) PUT IN COLUMN (C)/
     3        1X,61HLAMBDA RANDOM START (N), WITH PARAMETER (E) PUT IN C
     3OLUMN (C))
6644  FORMAT (1X,32HEXTREME RANDOM PUT IN COLUMN (C)/
     2        1X,51HEXTREME RANDOM WITH PARAMETER (E) PUT IN COLUMN (C)/
     2        1X,43HEXTREME RANDOM START (N), PUT IN COLUMN (C)/
     3        1X,62HEXTREME RANDOM START (N), WITH PARAMETER (E) PUT IN 
     3COLUMN (C))
6645  FORMAT (1X,51HWEIBULL RANDOM WITH PARAMETER (E) PUT IN COLUMN (C)/
     3        1X,62HWEIBULL RANDOM START (N), WITH PARAMETER (E) PUT IN 
     3COLUMN (C))
6646  FORMAT (1X,50HPARETO RANDOM WITH PARAMETER (E) PUT IN COLUMN (C)/
     3        1X,61HPARETO RANDOM START (N), WITH PARAMETER (E) PUT IN C
     3OLUMN (C))
6647  FORMAT (1X,36HEXPONENTIAL RANDOM PUT IN COLUMN (C)/
     3        1X,47HEXPONENTIAL RANDOM START (N), PUT IN COLUMN (C))
6648  FORMAT (1X,37HDEXPONENTIAL RANDOM PUT IN COLUMN (C)/
     3        1X,48HDEXPONENTIAL RANDOM START (N), PUT IN COLUMN (C))
6649  FORMAT (1X,33HLOGISTIC RANDOM PUT IN COLUMN (C)/
     3        1X,44HLOGISTIC RANDOM START (N), PUT IN COLUMN (C))
6654  FORMAT (1X,55HBINOMIAL RANDOM WITH N = (E) AND P = (E) PUT IN COL 
     1(C)/
     3        1X,66HBINOMIAL RANDOM START (N), WITH N = (E) AND P = (E) 
     3PUT IN COL (C))
6655  FORMAT (1X,58HNEGBINOMIAL RANDOM WITH N = (E) AND P = (E) PUT IN C
     1OL (C)/
     3        1X,69HNEGBINOMIAL RANDOM START (N), WITH N = (E) AND P = (
     3E) PUT IN COL (C))
6656  FORMAT (1X,51HPOISSON RANDOM WITH PARAMETER (E) PUT IN COLUMN (C)/
     3        1X,62HPOISSON RANDOM START (N), WITH PARAMETER (E) PUT IN 
     3COLUMN (C))
6657  FORMAT (1X,53HGEOMETRIC RANDOM WITH PARAMETER (E) PUT IN COLUMN (C
     1)/
     3        1X,64HGEOMETRIC RANDOM START (N), WITH PARAMETER (E) PUT I
     3N COLUMN (C))
6701  FORMAT (1X,25HNORMAL PLOT OF COLUMN (C))
6702  FORMAT (1X,28HLOGNORMAL PLOT OF COLUMN (C))
6703  FORMAT (1X,29HHALFNORMAL PLOT OF COLUMN (C))
6706  FORMAT (1X,43HGAMMA PLOT WITH PARAMETER (K) OF COLUMN (C))
6710  FORMAT (1X,26HUNIFORM PLOT OF COLUMN (C))
6711  FORMAT (1X,25HCAUCHY PLOT OF COLUMN (C))
6712  FORMAT (1X,44HLAMBDA PLOT WITH PARAMETER (K) OF COLUMN (C))
6713  FORMAT (1X,26HEXTREME PLOT OF COLUMN (C)/
     3        1X,45HEXTREME PLOT WITH PARAMETER (K) OF COLUMN (C))
6714  FORMAT (1X,45HWEIBULL PLOT WITH PARAMETER (K) OF COLUMN (C))
6715  FORMAT (1X,44HPARETO PLOT WITH PARAMETER (K) OF COLUMN (C))
6716  FORMAT (1X,30HEXPONENTIAL PLOT OF COLUMN (C))
6717  FORMAT (1X,31HDEXPONENTIAL PLOT OF COLUMN (C))
6718  FORMAT (1X,27HLOGISTIC PLOT OF COLUMN (C))
6725  FORMAT (1X,45HPOISSON PLOT WITH PARAMETER (K) OF COLUMN (C))
C
C     ==================================================================
C
      END
*DESC2
      SUBROUTINE DESC2
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  DESC2 V 7.00 12/ 7/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE DESCRIBE INSTRUCTION FOR ...
C        L1 = 12.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -  OCTOBER, 1978.
C                   CURRENT VERSION - DECEMBER, 1989.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
C     ==================================================================
C
      IF (L1.NE.12) GO TO 50
C
      GO TO (101,102,103,104,105,106,107,108,109,110,
     1       111,112,113,114,115,116,117,118,119,120,
     2       121,122,123,124,125,126,127,128,129,130,
     3       131,132,133,134,135), L2
C
C     ..................................................................
C
  50  CALL ERROR (1)
      RETURN
C
C     ..................................................................
C
C     L1 = 12.
C
 101  WRITE (IPRINT,2201)
      RETURN
C
 102  WRITE (IPRINT,2202)
      RETURN
C
 103  WRITE (IPRINT,2203)
      RETURN
C
 104  WRITE (IPRINT,2204)
      RETURN
C
 105  WRITE (IPRINT,2205)
      RETURN
C
 106  WRITE (IPRINT,2206)
      RETURN
C
 107  WRITE (IPRINT,2207)
      RETURN
C
 108  WRITE (IPRINT,2208)
      RETURN
C
 109  WRITE (IPRINT,2209)
      RETURN
C
 110  WRITE (IPRINT,2210)
      RETURN
C
 111  WRITE (IPRINT,2211)
      RETURN
C
 112  WRITE (IPRINT,2212)
      RETURN
C
 113  WRITE (IPRINT,2213)
      RETURN
C
 114  WRITE (IPRINT,2214)
      RETURN
C
 115  WRITE (IPRINT,2215)
      RETURN
C
 116  WRITE (IPRINT,2216)
      RETURN
C
 117  WRITE (IPRINT,2217)
      RETURN
C
 118  WRITE (IPRINT,2218)
      RETURN
C
 119  WRITE (IPRINT,2219)
      RETURN
C
 120  WRITE (IPRINT,2220)
      RETURN
C
 121  WRITE (IPRINT,2221)
      RETURN
C
 122  WRITE (IPRINT,2222)
      RETURN
C
 123  WRITE (IPRINT,2223)
      RETURN
C
 124  WRITE (IPRINT,2224)
      RETURN
C
 125  WRITE (IPRINT,2225)
      RETURN
C
 126  WRITE (IPRINT,2226)
      RETURN
C
 127  WRITE (IPRINT,2227)
      RETURN
C
 128  WRITE (IPRINT,2228)
      RETURN
C
 129  WRITE (IPRINT,2229)
      RETURN
C
 130  WRITE (IPRINT,2230)
      RETURN
C
 131  WRITE (IPRINT,2231)
      RETURN
C
 132  WRITE (IPRINT,2232)
      RETURN
C
 133  WRITE (IPRINT,2233)
      RETURN
C
 134  WRITE (IPRINT,2234)
      RETURN
C
 135  WRITE (IPRINT,2235)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
2201  FORMAT (1X,28HSIN OF (E) PUT IN COLUMN (C)/
     3        1X,58HSIN OF (E), MULTIPLY BY (E), ADD TO (E), PUT IN COLU
     3MN (C))
2202  FORMAT (1X,28HCOS OF (E) PUT IN COLUMN (C)/
     3        1X,58HCOS OF (E), MULTIPLY BY (E), ADD TO (E), PUT IN COLU
     3MN (C))
2203  FORMAT (1X,28HTAN OF (E) PUT IN COLUMN (C)/
     3        1X,58HTAN OF (E), MULTIPLY BY (E), ADD TO (E), PUT IN COLU
     3MN (C))
2204  FORMAT (1X,28HCOT OF (E) PUT IN COLUMN (C)/
     3        1X,58HCOT OF (E), MULTIPLY BY (E), ADD TO (E), PUT IN COLU
     3MN (C))
2205  FORMAT (1X,30HASIN OF (E), PUT IN COLUMN (C)/
     3        1X,59HASIN OF (E), MULTIPLY BY (E), ADD TO (E), PUT IN COL
     3UMN (C))
2206  FORMAT (1X,29HACOS OF (E) PUT IN COLUMN (C)/
     3        1X,59HACOS OF (E), MULTIPLY BY (E), ADD TO (E), PUT IN COL
     3UMN (C))
2207  FORMAT (1X,30HATAN OF (E), PUT IN COLUMN (C)/
     3        1X,59HATAN OF (E), MULTIPLY BY (E), ADD TO (E), PUT IN COL
     3UMN (C))
2208  FORMAT (1X,30HACOT OF (E), PUT IN COLUMN (C)/
     3        1X,59HACOT OF (E), MULTIPLY BY (E), ADD TO (E), PUT IN COL
     3UMN (C))
2209  FORMAT (1X,29HSIND OF (E) PUT IN COLUMN (C)/
     3        1X,59HSIND OF (E), MULTIPLY BY (E), ADD TO (E), PUT IN COL
     3UMN (C))
2210  FORMAT (1X,29HCOSD OF (E) PUT IN COLUMN (C)/
     3        1X,59HCOSD OF (E), MULTIPLY BY (E), ADD TO (E), PUT IN COL
     3UMN (C))
2211  FORMAT (1X,29HTAND OF (E) PUT IN COLUMN (C)/
     3        1X,59HTAND OF (E), MULTIPLY BY (E), ADD TO (E), PUT IN COL
     3UMN (C))
2212  FORMAT (1X,29HCOTD OF (E) PUT IN COLUMN (C)/
     3        1X,59HCOTD OF (E), MULTIPLY BY (E), ADD TO (E), PUT IN COL
     3UMN (C))
2213  FORMAT (1X,31HASIND OF (E), PUT IN COLUMN (C)/
     3        1X,60HASIND OF (E), MULTIPLY BY (E), ADD TO (E), PUT IN CO
     3LUMN (C))
2214  FORMAT (1X,31HACOSD OF (E), PUT IN COLUMN (C)/
     3        1X,60HACOSD OF (E), MULTIPLY BY (E), ADD TO (E), PUT IN CO
     3LUMN (C))
2215  FORMAT (1X,31HATAND OF (E), PUT IN COLUMN (C)/
     3        1X,60HATAND OF (E), MULTIPLY BY (E), ADD TO (E), PUT IN CO
     3LUMN (C))
2216  FORMAT (1X,31HACOTD OF (E), PUT IN COLUMN (C)/
     3        1X,60HACOTD OF (E), MULTIPLY BY (E), ADD TO (E), PUT IN CO
     3LUMN (C))
2217  FORMAT (1X,29HSQRT OF (E) PUT IN COLUMN (C)/
     3        1X,59HSQRT OF (E), MULTIPLY BY (E), ADD TO (E), PUT IN COL
     3UMN (C))
2218  FORMAT (1X,28HEXP OF (E) PUT IN COLUMN (C)/
     2        1X,58HEXP OF (E), MULTIPLY BY (E), ADD TO (E), PUT IN COLU
     2MN (C)/
     2        1X,36HEXPONENTIAL OF (E) PUT IN COLUMN (C)/
     3        1X,66HEXPONENTIAL OF (E), MULTIPLY BY (E), ADD TO (E), PUT
     3 IN COLUMN (C))
2219  FORMAT (1X,39HNEGEXPONENTIAL OF (E) PUT IN COLUMN (C)/
     3        1X,69HNEGEXPONENTIAL OF (E), MULTIPLY BY (E), ADD TO (E), 
     3PUT IN COLUMN (C))
2220  FORMAT (1X,52HLOG OF (E) PUT IN COLUMN (C)   $   LOG TO THE BASE E
     1/
     2        1X,58HLOG OF (E), MULTIPLY BY (E), ADD TO (E), PUT IN COLU
     2MN (C)/
     2        1X,29HLOGE OF (E) PUT IN COLUMN (C)/
     3        1X,59HLOGE OF (E), MULTIPLY BY (E), ADD TO (E), PUT IN COL
     3UMN (C))
2221  FORMAT (1X,31HLOGTEN OF (E) PUT IN COLUMN (C)/
     3        1X,61HLOGTEN OF (E), MULTIPLY BY (E), ADD TO (E), PUT IN C
     3OLUMN (C))
2222  FORMAT (1X,33HANTILOG OF (E), PUT IN COLUMN (C)/
     3        1X,62HANTILOG OF (E), MULTIPLY BY (E), ADD TO (E), PUT IN 
     3COLUMN (C))
2223  FORMAT (1X,29HSINH OF (E) PUT IN COLUMN (C)/
     3        1X,59HSINH OF (E), MULTIPLY BY (E), ADD TO (E), PUT IN COL
     3UMN (C))
2224  FORMAT (1X,29HCOSH OF (E) PUT IN COLUMN (C)/
     3        1X,59HCOSH OF (E), MULTIPLY BY (E), ADD TO (E), PUT IN COL
     3UMN (C))
2225  FORMAT (1X,29HTANH OF (E) PUT IN COLUMN (C)/
     3        1X,59HTANH OF (E), MULTIPLY BY (E), ADD TO (E), PUT IN COL
     3UMN (C))
2226  FORMAT (1X,29HCOTH OF (E) PUT IN COLUMN (C)/
     3        1X,59HCOTH OF (E), MULTIPLY BY (E), ADD TO (E), PUT IN COL
     3UMN (C))
2227  FORMAT (1X,31HASINH OF (E), PUT IN COLUMN (C)/
     3        1X,60HASINH OF (E), MULTIPLY BY (E), ADD TO (E), PUT IN CO
     3LUMN (C))
2228  FORMAT (1X,31HACOSH OF (E), PUT IN COLUMN (C)/
     3        1X,60HACOSH OF (E), MULTIPLY BY (E), ADD TO (E), PUT IN CO
     3LUMN (C))
2229  FORMAT (1X,31HATANH OF (E), PUT IN COLUMN (C)/
     3        1X,60HATANH OF (E), MULTIPLY BY (E), ADD TO (E), PUT IN CO
     3LUMN (C))
2230  FORMAT (1X,31HACOTH OF (E), PUT IN COLUMN (C)/
     3        1X,60HACOTH OF (E), MULTIPLY BY (E), ADD TO (E), PUT IN CO
     3LUMN (C))
2231  FORMAT (1X,34HABS VALUE OF (E) PUT IN COLUMN (C)/
     2        1X,64HABS VALUE OF (E), MULTIPLY BY (E), ADD TO (E), PUT I
     2N COLUMN (C)/
     2        1X,39HABSOLUTE VALUE OF (E) PUT IN COLUMN (C)/
     3        1X,69HABSOLUTE VALUE OF (E), MULTIPLY BY (E), ADD TO (E), 
     3PUT IN COLUMN (C))
2232  FORMAT (1X,37HINTEGER PART OF (E) PUT IN COLUMN (C)/
     3        1X,67HINTEGER PART OF (E), MULTIPLY BY (E), ADD TO (E), PU
     3T IN COLUMN (C))
2233  FORMAT (1X,41HFRACTIONAL PART OF (E), PUT IN COLUMN (C)/
     3        1X,70HFRACTIONAL PART OF (E), MULTIPLY BY (E), ADD TO (E),
     3 PUT IN COLUMN (C))
2234  FORMAT (1X,32HSQUARE (E) AND PUT IN COLUMN (C)/
     3        1X,58HSQUARE (E), MULTIPLY BY (E), ADD TO (E), PUT IN COLU
     3MN (C))
2235  FORMAT (1X,35HRECIPROCAL OF (E) PUT IN COLUMN (C)/
     3        1X,66HRECIPROCAL OF (E), MULTIPLY BY (E), ADD (E), AND PUT
     3 IN COLUMN (C))
C
C     ==================================================================
C
      END
*DESC3
      SUBROUTINE DESC3
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  DESC3 V 7.00 12/ 7/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE DESCRIBE INSTRUCTION FOR ...
C        L1 = 13 TO  15.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - NOVEMBER, 1978.
C                   CURRENT VERSION - DECEMBER, 1989.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
C     ==================================================================
C
      IF (L1.LT.13 .OR. L1.GT.15) GO TO 50
C
      L = L1 - 12
      GO TO ( 1, 2, 3), L
C
   1  GO TO (101,102,103,104,105,106,107,108,109,110,
     1       111,112,113,114,115,116,117,118,119,120,
     2       121,122), L2
   2  GO TO (201,202,203,204,205,206,207,208,209,210,
     1       211,212,213,214,215,216,217,218,219,220,
     2       221,222,223), L2
   3  GO TO (301,302,303,304,305,306,307,308,309,310), L2
C
C     ..................................................................
C
  50  CALL ERROR (1)
      RETURN
C
C     ..................................................................
C
C     L1 = 13.
C
 101  WRITE (IPRINT,2301)
      RETURN
C
 102  WRITE (IPRINT,2302)
      RETURN
C
 103  WRITE (IPRINT,2303)
      RETURN
C
 104  WRITE (IPRINT,2304)
      RETURN
C
 105  WRITE (IPRINT,2305)
      RETURN
C
 106  WRITE (IPRINT,2306)
      RETURN
C
 107  WRITE (IPRINT,2307)
      RETURN
C
 108  WRITE (IPRINT,2308)
      RETURN
C
 109  WRITE (IPRINT,2309)
      RETURN
C
 110  WRITE (IPRINT,2310)
      RETURN
C
 111  WRITE (IPRINT,2311)
      RETURN
C
 112  WRITE (IPRINT,2312)
      RETURN
C
 113  WRITE (IPRINT,2313)
      RETURN
C
 114  WRITE (IPRINT,2314)
      RETURN
C
 115  WRITE (IPRINT,2315)
      RETURN
C
 116  WRITE (IPRINT,2316)
      RETURN
C
 117  WRITE (IPRINT,2317)
      RETURN
C
 118  WRITE (IPRINT,2318)
      RETURN
C
 119  WRITE (IPRINT,2319)
      RETURN
C
 120  WRITE (IPRINT,2320)
      RETURN
C
 121  WRITE (IPRINT,2321)
      RETURN
C
 122  WRITE (IPRINT,2322)
      RETURN
C
C     ..................................................................
C
C     L1 = 14.
C
 201  WRITE (IPRINT,2401)
      RETURN
C
 202  WRITE (IPRINT,2402)
      RETURN
C
 203  WRITE (IPRINT,2403)
      RETURN
C
 204  WRITE (IPRINT,2404)
      RETURN
C
 205  WRITE (IPRINT,2405)
      RETURN
C
 206  WRITE (IPRINT,2406)
      RETURN
C
 207  CONTINUE
      RETURN
C
 208  WRITE (IPRINT,2408)
      RETURN
C
 209  WRITE (IPRINT,2409)
      RETURN
C
 210  WRITE (IPRINT,2410)
      RETURN
C
 211  WRITE (IPRINT,2411)
      RETURN
C
 212  WRITE (IPRINT,2412)
      RETURN
C
 213  WRITE (IPRINT,2413)
      RETURN
C
 214  WRITE (IPRINT,2414)
      RETURN
C
 215  WRITE (IPRINT,2415)
      RETURN
C
 216  WRITE (IPRINT,2416)
      RETURN
C
 217  WRITE (IPRINT,2417)
      RETURN
C
 218  WRITE (IPRINT,2418)
      RETURN
C
 219  WRITE (IPRINT,2419)
      RETURN
C
 220  WRITE (IPRINT,2420)
      RETURN
C
 221  WRITE (IPRINT,2421)
      RETURN
C
 222  WRITE (IPRINT,2422)
      RETURN
C
 223  WRITE (IPRINT,2423)
      RETURN
C
C     ..................................................................
C
C     L1 = 15.
C
 301  WRITE (IPRINT,2501)
      RETURN
C
 302  WRITE (IPRINT,2502)
      RETURN
C
 303  WRITE (IPRINT,2503)
      RETURN
C
 304  WRITE (IPRINT,2504)
      RETURN
C
 305  WRITE (IPRINT,2505)
      RETURN
C
 306  WRITE (IPRINT,2506)
      RETURN
C
 307  WRITE (IPRINT,2507)
      RETURN
C
 308  WRITE (IPRINT,2508)
      RETURN
C
 309  WRITE (IPRINT,2509)
      RETURN
C
 310  WRITE (IPRINT,2510)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
2301  FORMAT (1X,71HGENERATE FROM (K) IN STEPS OF (K) TO (K) STEPS (K) T
     1O (K)... PUT IN (C))
2302  FORMAT (1X,56HSET IN ONE COLUMN (C), DATA ON FOLLOWING HOLLERITH C
     1ARDS/
     3        1X,70HSET STARTING WITH ROW (R) OF COL (C) DATA ON FOLLOWI
     3NG HOLLERITH CARDS)
2303  FORMAT (1X,41HFIXED WITH (D) DIGITS AFTER DECIMAL POINT)
2304  FORMAT (1X,36HFLOATING WITH (S) SIGNIFICANT DIGITS/
     3        1X,38HFLOATING WITH EIGHT SIGNIFICANT DIGITS)
2305  FORMAT (1X,70HPLOT COLUMNS (C), (C) ... (C) AGAINST COLUMN (C)  $ 
     1MAX OF 6 ARGUMENTS/
     2        1X,68HPLOT COLS (C) ... (C) VERTICAL SCALE FROM (K) TO (K)
     2 AGAINST COL (C)/
     2        1X,69HPLOT COLS (C)...(C) AGAINST COL (C), HORIZONTAL SCAL
     2E FROM (K) TO (K)/
     2        1X,68HPLOT COLS (C)...(C) VERTICAL (K) TO (K), VS COL (C) 
     2HORIZ (K) TO (K)/
     3        1X,65HPLOT COLS (C)...(C) VS (C) HORIZONTALLY (K) TO (K) V
     3ER (K) TO (K))
2306  FORMAT (1X,69HPAGE PLOT COLS (C), (C) ... (C) AGAINST COL (C)  $ M
     1AX OF 6 ARGUMENTS/
     2        1X,68HPAGE PLOT COLS (C) ... (C) VER SCALE FROM (K) TO (K)
     2 AGAINST COL (C)/
     2        1X,69HPAGE PLOT COLS (C)...(C) AGAINST COL (C), HORIZ SCAL
     2E FROM (K) TO (K)/
     2        1X,67HPAGE PLOT COLS (C)...(C) VER (K) TO (K) VS COL (C) H
     2ORIZ (K) TO (K)/
     3        1X,70HPAGE PLOT COLS (C)...(C) VS (C) HORIZONTALLY (K) TO 
     3(K) VER (K) TO (K))
2307  FORMAT (1X,50HNPLOT COLUMNS (C), (C), ..., (C) VERSUS COLUMN (C)/
     2        1X,71HNPLOT COLS (C), (C), ..., (C) VERTICAL SCALE FROM (K
     2) TO (K) VS COL (C)/
     3        1X,68HNPLOT COLS (C)...(C) VS COLUMN (C) WITH HORIZ. SCALE
     3 FROM (K) TO (K))
2308  FORMAT (1X,67HNEW PAGE           $ ASSURES NEXT PRINTING WILL STAR
     1T ON A NEW PAGE)
2309  FORMAT (1X,31HSPACE (P) LINES ON PRINTED PAGE/
     3        1X,30HSPACE ONE LINE ON PRINTED PAGE)
2310  FORMAT (1X,69HCGS SYSTEM OF FUNDAMENTAL PHYSICAL CONSTRANTS, CENTI
     1METER-GRAM-SECOND)
2311  FORMAT (1X,62HSI SYSTEM OF FUNDAMENTAL PHYSICAL CONSTRANTS  $  FOR
     1MERLY MKSA)
2312  FORMAT (1X,39HFLEXIBLE TO RETURN TO READABLE PRINTING)
2313  FORMAT (1X,69HPRINT NOTE  $ INFORMATION FROM NOTE1 AND NOTE2 IS PR
     1INTED IMMEDIATELY)
2314  FORMAT (1X,69HROUND THE NOS IN COL (C) TO (N) SIGNIFICANT DIGITS A
     1ND PUT IN COL (C))
2315  FORMAT (1X,66HCPLOT COL (C), SYMBOL (E) COL (C),... SYMBOL (E) VER
     1SUS COLUMN (C)/
     2        1X,71HCPLOT COL (C) SYM (E),(C),... (E) VERT SCALE FROM (K
     2) TO (K) VS COL (C)/
     2        1X,71HCPLOT COL (C) SYM (E),(C)...(E) VS COL (C) HORIZ. SC
     2ALE FROM (K) TO (K)/
     3        1X,68HCPLOT (C) (E),(C)...(E) VERTICAL (K) TO (K) VS (C) H
     3ORIZ. (K) TO (K))
2316  FORMAT (1X,46HNCPLOT (C), (E),...,(C), (E) VERSUS COLUMN (C)/
     2        1X,64HNCPLOT (C), (C),...,(C),(E) VERTICAL SCALE (K) TO (K
     2) VS COL (C)/
     2        1X,65HNCPLOT (C),(E),...,(C),(E) VS COL (C) HORIZONTAL SCA
     2LE (K) TO (K)/
     3        1X,71HNCPLOT (C),(C),...,(C),(E) VERTICAL (K) TO (K) VS (C
     3) HORIZ. (K) TO (K))
2317  FORMAT (1X,61HTWOPLOTS OF COL (C) VERSUS COL (C) AND COL (C) VERSU
     1S COL (C)/
     3        1X,70HTWOPLOTS COL (C) (K) TO (K) VS (C) (K) (K), (C) (K) 
     3(K) VS (C) (K) (K))
2318  FORMAT (1X,68HFOURPLOTS OF (C) VS (C) AND (C) VS (C) AND (C) VS (C
     1) AND (C) VS (C)/
     3        1X,67HFOURPLOTS (C),(K),(K) VS (C),(K),(K) ... (C),(K),(K)
     3 VS (C),(K),(K))
2319  FORMAT (1X,41HNICE PLOT OF COLUMN (C) VERSUS COLUMN (C)/
     3        1X,52HNICE PLOT OF COLUMNS (C), ..., (C) VERSUS COLUMN (C)
     3)
2320  FORMAT (1X,42HNICE NPLOT OF COLUMN (C) VERSUS COLUMN (C)/
     3        1X,53HNICE NPLOT OF COLUMNS (C), ..., (C) VERSUS COLUMN (C
     3))
2321  FORMAT (1X,55HNICE CPLOT OF COLUMN (C), SYMBOL (E), VERSUS COLUMN 
     1(C)/
     3        1X,68HNICE CPLOT OF COL (C) SYMBOL (E), COL (C) SYMBOL (E)
     3, ... VERSUS (C))
2322  FORMAT (1X,56HNICE NCPLOT OF COLUMN (C), SYMBOL (E), VERSUS COLUMN
     1 (C)/
     3        1X,69HNICE NCPLOT OF COL (C) SYMBOL (E), COL (C) SYMBOL (E
     3), ... VERSUS (C))
2401  FORMAT (1X,40HBEGIN STORING INSTRUCTIONS FOR LATER USE/
     3        1X,66HBEGIN STORING INSTRS START WITH INSTR NO. (N) $ NO. 
     3LESS THAN 1000)
2402  FORMAT (1X,66HSCAN ONLY THE FIRST (C) CARD COLS ON THE FOLLOWING H
     1OLLERITH CARDS)
2403  FORMAT (1X,56HEXECUTE INSTRUCTIONS NUMBERED (N) THROUGH (N), (T) T
     1IMES/
     2        1X,50HEXECUTE INSTRUCTIONS NUMBERED (N) THROUGH (N) ONCE/
     2        1X,37HEXECUTE INSTRUCTION NUMBERED (N) ONCE/
     2        1X,56HPERFORM INSTRUCTIONS NUMBERED (N) THROUGH (N), (T) T
     2IMES/
     2        1X,50HPERFORM INSTRUCTIONS NUMBERED (N) THROUGH (N) ONCE/
     2        1X,37HPERFORM INSTRUCTION NUMBERED (N) ONCE/
     2        1X,55HREPEAT INSTRUCTIONS NUMBERED (N) THROUGH (N), (T) TI
     2MES/
     2        1X,49HREPEAT INSTRUCTIONS NUMBERED (N) THROUGH (N) ONCE/
     3        1X,36HREPEAT INSTRUCTION NUMBERED (N) ONCE)
2404  FORMAT (1X,31HINTERACTIVE USE FROM A TERMINAL/
     3        1X,44HINTERACTIVE USE WITH (C) CHARACTERS PER LINE)
2405  FORMAT (1X,49HWIDTH SET TO A MAXIMUM OF (C) CHARACTERS PER LINE)
2406  FORMAT (1X,67HINCREMENT INSTR NO. (N) BY (E)...(E) $ NO. OF ARGS I
     1N INSTR (N) + 1)
2408  FORMAT (1X,70HRESTORE INSTRUCTION (N) TO (E), (E)...(E) $ NO OF AR
     1GS IN INSTR(N) + 1)
2409  FORMAT (1X,17HIFLT (E) THAN (E))
2410  FORMAT (1X,49HIFEQ (E) TO (E) WITHIN THE ABSOLUTE TOLERANCE (E)/
     3        1X,15HIFEQ (E) TO (E))
2411  FORMAT (1X,17HIFGT (E) THAN (E))
2412  FORMAT (1X,15HIFGE (E) TO (E))
2413  FORMAT (1X,43HIFNE (E) TO (E) WITH ABSOLUTE TOLERANCE (E)/
     3        1X,15HIFNE (E) TO (E))
2414  FORMAT (1X,15HIFLE (E) TO (E))
2415  FORMAT (1X,47HCOMPARE (E) TO (E) USING RELATIVE TOLERANCE (E))
2416  FORMAT (1X,57HDAYS FOR MONTH (E) DAY (E) AND YEAR (E) PUT IN COLUM
     1N (C))
2417  FORMAT (1X,27HBRIEF      $   NO ARGUMENTS)
2418  FORMAT (1X,26HFULL      $   NO ARGUMENTS)
2419  FORMAT (1X,30HTERMINAL      $   NO ARGUMENTS)
2420  FORMAT (1X,42HLENGTH EQUAL TO (N) LINES PRINTED PER PAGE)
2421  FORMAT (1X,37HREMOTE PRINTING ON HIGH-SPEED PRINTER)
2422  FORMAT (1X,26HLOCAL PRINTING AT TERMINAL)
2423  FORMAT (1X,58HCRT   $  TEMPORARILY HALT PRINTING AT THE END OF EAC
     1H PAGE)
2501  FORMAT (1X,63HADEFINE THE ARRAY IN (R),(C) OF SIZE (R)X(C) TO BE E
     1QUAL TO (K)/
     3        1X,69HMDEFINE MATRIX (R),(C) SIZE (R)X(C) TO HAVE ALL ELEM
     3ENTS EQUAL TO (K))
2502  FORMAT (1X,43HAERASE THE ARRAY IN (R),(C) OF SIZE (R)X(C)/
     2        1X,42HAZERO THE ARRAY IN (R),(C) OF SIZE (R)X(C)/
     2        1X,71HMERASE MATRIX IN (R),(C) SIZE (R)X(C)  $ SETS EVERY 
     2ELEMENT IN MATRIX=0/
     3        1X,43HMZERO THE MATRIX IN (R),(C) OF SIZE (R)X(C))
2503  FORMAT (1X,36HMIDENTITY IN (R),(C) OF SIZE (R)X(C))
2504  FORMAT (1X,69HMDIAGONAL MATRIX (R),(C) OF SIZE (R)X(C) EQUAL TO (E
     1) ON THE DIAGONAL)
2505  FORMAT (1X,23HSTATPLOTS OF COLUMN (C))
2506  FORMAT (1X,62HCONTENTS $ PRINT SECTION TITLES IN DESCRIPTION OF IN
     1STRUCTIONS/
     2        1X,55HCONTENTS (K) PRINT SUBSECTION TITLES IF K IS AN INTE
     2GER/
     3        1X,70HCONTENTS (K)  $  PRINT COMMANDS IN SUBSECTION (K), I
     3F K NOT AN INTEGER)
2507  FORMAT (1X,67HDESCRIBE FOLLOWED BY COMMAND PRINTS INSTRUCTION AS L
     1ISTED IN PART D)
2508  FORMAT (1X,30HGAMMA OF (E) PUT IN COLUMN (C))
2509  FORMAT (1X,67HSAMPLE WITHR OF SIZE (N) FROM POPULATION OF SIZE (N)
     1 PUT IN COL (C)/
     3        1X,67HSAMPLE WITHR START (N), OF SIZE (N), POP'N SIZE (N),
     3 PUT IN COL (C))
2510  FORMAT (1X,70HSAMPLE WITHOUTR OF SIZE (N) FROM POPULATION OF SIZE 
     1(N) PUT IN COL (C)/
     3        1X,70HSAMPLE WITHOUTR START (N), OF SIZE (N), POP'N SIZE (
     3N), PUT IN COL (C))
C
C     ==================================================================
C
      END
*DESC4
      SUBROUTINE DESC4
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  DESC4 V 7.00 12/ 7/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE DESCRIBE INSTRUCTION FOR ...
C        L1 = 16 TO  18.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - NOVEMBER, 1978.
C                   CURRENT VERSION - DECEMBER, 1989.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
C     ==================================================================
C
      IF (L1.LT.16 .OR. L1.GT.18) GO TO 50
C
      L = L1 - 15
      GO TO ( 1, 2, 3), L
C
   1  GO TO (101,102,103,104,105,106), L2
   2  GO TO (201,202,203,204,205,206,207,208,209,210,
     1       211,212,213,214,215,216), L2
   3  GO TO (301,302,303,304,305,306,307,308,309,310), L2
C
C     ..................................................................
C
  50  CALL ERROR (1)
      RETURN
C
C     ..................................................................
C
C     L1 = 16.
C
 101  WRITE (IPRINT,2601)
      RETURN
C
 102  WRITE (IPRINT,2602)
      RETURN
C
 103  WRITE (IPRINT,2603)
      RETURN
C
 104  WRITE (IPRINT,2604)
      RETURN
C
 105  WRITE (IPRINT,2605)
      RETURN
C
 106  WRITE (IPRINT,2606)
      RETURN
C
C     ..................................................................
C
C     L1 = 17.
C
 201  WRITE (IPRINT,2701)
      RETURN
C
 202  WRITE (IPRINT,2702)
      RETURN
C
 203  WRITE (IPRINT,2703)
      RETURN
C
 204  WRITE (IPRINT,2704)
      RETURN
C
 205  WRITE (IPRINT,2705)
      RETURN
C
 206  WRITE (IPRINT,2706)
      RETURN
C
 207  WRITE (IPRINT,2707)
      RETURN
C
 208  WRITE (IPRINT,2708)
      RETURN
C
 209  WRITE (IPRINT,2709)
      RETURN
C
 210  WRITE (IPRINT,2710)
      RETURN
C
 211  WRITE (IPRINT,2711)
      RETURN
C
 212  WRITE (IPRINT,2712)
      RETURN
C
 213  WRITE (IPRINT,2713)
      RETURN
C
 214  WRITE (IPRINT,2714)
      RETURN
C
 215  WRITE (IPRINT,2715)
      RETURN
C
 216  WRITE (IPRINT,2716)
      RETURN
C
C     ..................................................................
C
C     L1 = 18.
C
 301  WRITE (IPRINT,2801)
      RETURN
C
 302  WRITE (IPRINT,2802)
      RETURN
C
 303  WRITE (IPRINT,2803)
      RETURN
C
 304  WRITE (IPRINT,2804)
      RETURN
C
 305  WRITE (IPRINT,2805)
      RETURN
C
 306  WRITE (IPRINT,2806)
      RETURN
C
 307  WRITE (IPRINT,2807)
      RETURN
C
 308  WRITE (IPRINT,2808)
      RETURN
C
 309  WRITE (IPRINT,2809)
      RETURN
C
 310  WRITE (IPRINT,2810)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
2601  FORMAT (1X,63HINVERT THE MATRIX IN (R),(C) OF SIZE (R)X(C) AND PUT
     1 IN (R),(C)/
     3        1X,69HMINVERT THE MATRIX IN (R),(C) OF SIZE (R)X(C), PUT I
     3NVERSE IN (R),(C))
2602  FORMAT (1X,70HSOLVE LIN EQS , COEFFS IN (R),(C) SIZE (R)X(C) CONST
     1S (C), PUT SOL (C))
2603  FORMAT (1X,36HLABEL (LABEL,) (LABEL,) (LABEL,) .../
     3        1X,53HLABEL (LABEL,) (C,), (LABEL,) (C,), (LABEL,) (C,) ..
     3.)
2604  FORMAT (1X,70HALABEL (LABEL,) (R,) (C,) (R,) (C,), (LABEL,) (R,) (
     1C,) (R,) (C,), ...)
2605  FORMAT (1X,70HMLABEL (LABEL,) (R,) (C,) (R,) (C,), (LABEL,) (R,) (
     1C,) (R,) (C,), ...)
2606  FORMAT (1X,55HEVALUATE COLUMN (C) = (A FORTRAN ARITHMETIC EXPRESSI
     1ON))
2701  FORMAT (1X,71HMMULT (R),(C) SIZE (R)X(C) X MATRIX (R),(C) SIZE (R)
     1X(C) PUT IN (R),(C)/
     3        1X,69HMMULTIPLY (R),(C) SIZE (R)X(C) BY (R),(C) SIZE (R)X(
     3C) PUT IN (R),(C))
2702  FORMAT (1X,69HMRAISE MATRIX IN (R),(C) OF SIZE (R)X(C) TO POWER (K
     1), PUT IN (R),(C))
2703  FORMAT (1X,70HMKRONECKER PROD (R),(C) SIZE (R)X(C) X (R),(C) SIZE 
     1(R)X(C) IN (R),(C))
2704  FORMAT (1X,68HMTRIANGULARIZE MATRIX IN (R),(C) SIZE (R)X(C) INTO M
     1ATRIX IN (R),(C)/
     3        1X,70HMTRIANGULARIZE MAT (R),(C) SIZE (R)X(C) IN (R),(C), 
     3INVERSE IN (R),(C))
2705  FORMAT (1X,71HMEIGEN OF MATRIX IN (R),(C) OF SIZE (R)X(C), PUT EIG
     1ENVALUES IN COL (C)/
     2        1X,68HMEIGEN OF MATRIX IN (R),(C) SIZE (R)X(C) PUT EIGENVE
     2CTORS IN (R),(C)/
     3        1X,68HMEIGEN MATRIX (R),(C) SIZE (R)X(C), VALUES IN (C) VE
     3CTORS IN (R),(C))
2706  FORMAT (1X,54HOMIT ROWS WITH (K) IN COLUMN (C) AND PUT IN COLUMN (
     1C)/
     2        1X,69HOMIT ROWS WITH NUMBERS BETWEEN (K) AND (K) IN COL (C
     2), PUT IN COL (C)/
     2        1X,71HOMIT ROWS WITH (K) IN (C), CORR. ROWS OF (C),...,(C)
     2 PUT IN (C),...,(C)/
     3        1X,70HOMIT FROM (K) TO (K) IN (C), CORR. ROWS OF (C),...,(
     3C), IN (C),...,(C))
2707  FORMAT (1X,69HDELETE ROWS HAVING (K) IN ROWS OF ANY COLS (C)...(C)
     1 PUT IN (C)...(C)/
     3        1X,70HDELETE ROWS WITH NOS BETWEEN (K) AND (K) IN (C)...(C
     3) PUT IN (C)...(C))
2708  FORMAT (1X,56HCHOOSE ROWS WITH (K) IN COLUMN (C) AND PUT IN COLUMN
     1 (C)/
     2        1X,71HCHOOSE ROWS WITH NUMBERS BETWEEN (K) AND (K) IN COL 
     2(C), PUT IN COL (C)/
     2        1X,71HCHOOSE ROWS WITH (K) IN (C), CORRSP. ROWS OF (C)...(
     2C) PUT IN (C)...(C)/
     3        1X,71HCHOOSE FROM (K) TO (K) IN (C), CORR. ROWS OF (C)...(
     3C) PUT IN (C)...(C))
2709  FORMAT (1X,68HRETAIN NUMBERS BETWEEN (K) AND (K) IN (C),...,(C) PU
     1T IN (C),...,(C))
2710  FORMAT (1X,70HREPLACE THE NUMBER (K) IN COL (C) BY THE NUMBER (K) 
     1AND PUT IN COL (C)/
     3        1X,69HREPLACE THE NUMBERS FROM (K) TO (K) IN COL (C) BY (K
     3), PUT IN COL (C))
2711  FORMAT (1X,39HAVERAGE OF COLUMN (C) PUT IN COLUMN (C)/
     3        1X,62HAVERAGE OF NUMBERS IN COLS (C) ... (C) PUT IN COLS (
     3C) ... (C))
2712  FORMAT (1X,49HSTDDEV OF NUMBERS IN COLUMN (C) PUT IN COLUMN (C)/
     3        1X,61HSTDDEV OF NUMBERS IN COLS (C) ... (C) PUT IN COLS (C
     3) ... (C))
2713  FORMAT (1X,48HRANGE OF NUMBERS IN COLUMN (C) PUT IN COLUMN (C)/
     3        1X,60HRANGE OF NUMBERS IN COLS (C) ... (C) PUT IN COLS (C)
     3 ... (C))
2714  FORMAT (1X,49HMEDIAN OF NUMBERS IN COLUMN (C) PUT IN COLUMN (C)/
     3        1X,67HMEDIAN OF NUMBERS IN COLS (C), (C)...(C) PUT IN COLS
     3 (C), (C)...(C))
2715  FORMAT (1X,54HPERCENTAGES OF NUMBERS ON COLUMN (C) PUT IN COLUMN (
     1C)/
     3        1X,66HPERCENTAGES OF NUMBERS IN COLS (C) ... (C) PUT IN CO
     3LS (C) ... (C))
2716  FORMAT (1X,54HPROPORTIONS OF NUMBERS IN COLUMN (C) PUT IN COLUMN (
     1C)/
     3        1X,65HPROPORTION OF NUMBERS IN COLS (C) ... (C) PUT IN COL
     3S (C) ... (C))
2801  FORMAT (1X,71HMADD MATRIX (R),(C) SIZE (R)X(C) TO (R),(C) SIZE (R)
     1X(C) PUT IN (R),(C)/
     3        1X,68HMADD MATRIX (R),(C) SIZE (R)X(C) TO MATRIX IN (R),(C
     3) PUT IN (R),(C))
2802  FORMAT (1X,71HMSUB MAT (R),(C) SIZE (R)X(C) MINUS MAT (R),(C) SIZE
     1 (R)X(C) IN (R),(C)/
     2        1X,71HMSUB MATRIX IN (R),(C) SIZE (R)X(C) MINUS MATRIX (R)
     2,(C) PUT IN (R),(C)/
     2        1X,68HMSUBTRACT MAT (R),(C) SIZE (R)X(C) - (R),(C) SIZE (R
     2)X(C) IN (R),(C)/
     3        1X,70HMSUBTRACT MATRIX (R),(C) SIZE (R)X(C) MINUS MAT (R),
     3(C) PUT IN (R),(C))
2803  FORMAT (1X,60HATRANSPOSE THE ARRAY IN (R),(C) OF SIZE (R)X(C) INTO
     1 (R),(C)/
     3        1X,65HMTRANSPOSE THE MATRIX (R),(C) SIZE (R)X(C) INTO MATR
     3IX IN (R),(C))
2804  FORMAT (1X,70HAADD ARRAY (R),(C) SIZE (R)X(C) TO (R),(C) SIZE (R)X
     1(C) PUT IN (R),(C)/
     2        1X,68HAADD ARRAY IN (R),(C) SIZE (R)X(C), TO ARRAY (R),(C)
     2, PUT IN (R),(C)/
     3        1X,70HAADD THE ARRAY IN (R),(C) OF SIZE (R)X(C) TO (E), PU
     3T ARRAY IN (R),(C))
2805  FORMAT (1X,70HASUB ARRAY (R),(C) SIZE (R)X(C) - (R),(C) SIZE (R)X(
     1C), PUT IN (R),(C)/
     2        1X,71HASUB ARRAY (R),(C) SIZE (R)X(C) MINUS THE ARRAY (R),
     2(C), PUT IN (R),(C)/
     2        1X,69HASUB ARRAY IN (R),(C) OF SIZE (R)X(C) MINUS (E), PUT
     2 ARRAY IN (R),(C)/
     2        1X,70HASUBTRACT ARRAY (R),(C) SIZE (R)X(C) - (R),(C) SIZE 
     2(R)X(C) IN (R),(C)/
     2        1X,71HASUBTRACT ARRAY (R),(C) SIZE (R)X(C) MINUS ARRAY (R)
     2,(C) PUT IN (R),(C)/
     3        1X,71HASUBTRACT ARRAY IN (R),(C) SIZE (R)X(C) MINUS (E), P
     3UT ARRAY IN (R),(C))
2806  FORMAT (1X,71HAMULT ARRAY (R),(C) SIZE (R)X(C) BY (R),(C) SIZE (R)
     1X(C) PUT IN (R),(C)/
     2        1X,71HAMULT ARRAY IN (R),(C) SIZE (R)X(C) BY THE ARRAY (R)
     2,(C) PUT IN (R),(C)/
     2        1X,71HAMULT THE ARRAY IN (R),(C) OF SIZE (R)X(C) BY (E), P
     2UT ARRAY IN (R),(C)/
     2        1X,71HAMULTIPLY ARRAY (R),(C) SIZE (R)X(C) BY (R),(C) SIZE
     2 (R)X(C) IN (R),(C)/
     2        1X,71HAMULTIPLY ARRAY (R),(C) SIZE (R)X(C) BY ARRAY IN (R)
     2,(C) PUT IN (R),(C)/
     2        1X,71HAMULTIPLY THE ARRAY IN (R),(C) SIZE (R)X(C) BY (E) P
     2UT ARRAY IN (R),(C)/
     3        1X,67HMSCALAR MATRIX (R),(C) SIZE (R)X(C) BY CONSTANT (K),
     3 PUT IN (R),(C))
2807  FORMAT (1X,71HADIV ARRAY (R),(C) SIZE (R)X(C) BY (R),(C) SIZE (R)X
     1(C), PUT IN (R),(C)/
     2        1X,69HADIV ARRAY (R),(C) SIZE (R)X(C), BY THE ARRAY (R),(C
     2), PUT IN (R),(C)/
     2        1X,71HADIV THE ARRAY IN (R),(C) OF SIZE (R)X(C), BY (E), P
     2UT ARRAY IN (R),(C)/
     2        1X,70HADIVIDE ARRAY (R),(C) SIZE (R)X(C) BY (R),(C) SIZE (
     2R)X(C), IN (R),(C)/
     2        1X,70HADIVIDE ARRAY IN (R),(C) SIZE (R)X(C) BY ARRAY (R),(
     2C), PUT IN (R),(C)/
     3        1X,67HADIVIDE THE ARRAY (R),(C), SIZE (R)X(C) BY (E) PUT A
     3RRAY IN (R),(C))
2808  FORMAT (1X,69HARAISE ARRAY (R),(C) SIZE (R)X(C) TO (R),(C) SIZE (R
     1)X(C), IN (R),(C)/
     2        1X,69HARAISE ARRAY (R),(C) OF SIZE (R)X(C) TO ARRAY (R),(C
     2), PUT IN (R),(C)/
     3        1X,68HARAISE ARRAY IN (R),(C) OF SIZE (R)X(C) TO (E), PUT 
     3ARRAY IN (R),(C))
2809  FORMAT (1X,68HACOALESCE ON FIRST COL OF (R),(C) SIZE (R)X(C), PUT 
     1ARRAY IN (R),(C)/
     3        1X,69HACOALESCE ON (K) IN FIRST COL OF (R),(C) SIZE (R)X(C
     3), ROW IN (R),(C))
2810  FORMAT (1X,67HAAVERAGE ON FIRST COL OF (R),(C) SIZE (R)X(C), PUT A
     1RRAY IN (R),(C)/
     3        1X,71HAAVERAGE ON (K) IN FIRST COL OF (R),(C) SIZE (R)X(C)
     3 PUT ROW IN (R),(C))
C
C     ==================================================================
C
      END
*DESC5
      SUBROUTINE DESC5
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  DESC5 V 7.00 12/ 7/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE DESCRIBE INSTRUCTION FOR ...
C        L1 = 19 TO 22.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - NOVEMBER, 1978.
C                   CURRENT VERSION - DECEMBER, 1989.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
C     ==================================================================
C
      IF (L1.LT.19 .OR. L1.GT.22) GO TO 50
C
      L = L1 - 18
      GO TO ( 1, 2, 3, 4), L
C
   1  GO TO (101,102,103,104,105,106), L2
   2  GO TO (201,202,203,204), L2
   3  GO TO (301,302,303,304,305,306,307,308,309,310,
     1       311,312,313,314,315,316,317,318,319,320,
     2       321,322,323,324,325,326,327), L2
   4  GO TO (401,402,403,404,405,406,407), L2
C
C     ..................................................................
C
  50  CALL ERROR (1)
      RETURN
C
C     ..................................................................
C
C     L1 = 19.
C
 101  WRITE (IPRINT,2901)
      RETURN
C
 102  WRITE (IPRINT,2902)
      RETURN
C
 103  WRITE (IPRINT,2903)
      RETURN
C
 104  WRITE (IPRINT,2904)
      RETURN
C
 105  WRITE (IPRINT,2905)
      RETURN
C
 106  WRITE (IPRINT,2906)
      RETURN
C
C     ..................................................................
C
C     L1 = 20.
C
 201  WRITE (IPRINT,3001)
      RETURN
C
 202  WRITE (IPRINT,3002)
      RETURN
C
 203  WRITE (IPRINT,3003)
      RETURN
C
 204  WRITE (IPRINT,3004)
      RETURN
C
C     ..................................................................
C
C     L1 = 21.
C
 301  WRITE (IPRINT,3101)
      RETURN
C
 302  WRITE (IPRINT,3102)
      RETURN
C
 303  WRITE (IPRINT,3103)
      RETURN
C
 304  GO TO 50
C
 305  WRITE (IPRINT,3105)
      RETURN
C
 306  WRITE (IPRINT,3106)
      RETURN
C
 307  GO TO 50
C
 308  WRITE (IPRINT,3108)
      RETURN
C
 309  WRITE (IPRINT,3109)
      RETURN
C
 310  WRITE (IPRINT,3110)
      RETURN
C
 311  WRITE (IPRINT,3111)
      RETURN
C
 312  WRITE (IPRINT,3112)
      RETURN
C
 313  WRITE (IPRINT,3113)
      RETURN
C
 314  WRITE (IPRINT,3114)
      RETURN
C
 315  WRITE (IPRINT,3115)
      RETURN
C
 316  WRITE (IPRINT,3116)
      RETURN
C
 317  WRITE (IPRINT,3117)
      RETURN
C
 318  WRITE (IPRINT,3118)
      RETURN
C
 319  WRITE (IPRINT,3119)
      RETURN
C
 320  WRITE (IPRINT,3120)
      RETURN
C
 321  WRITE (IPRINT,3121)
      RETURN
C
 322  WRITE (IPRINT,3122)
      RETURN
C
 323  WRITE (IPRINT,3123)
      RETURN
C
 324  WRITE (IPRINT,3124)
      RETURN
C
 325  WRITE (IPRINT,3125)
      RETURN
C
 326  WRITE (IPRINT,3126)
      RETURN
C
 327  WRITE (IPRINT,3127)
      RETURN
C
C     ..................................................................
C
C     L1 = 22.
C
 401  WRITE (IPRINT,3201)
      RETURN
C
 402  WRITE (IPRINT,3202)
      RETURN
C
 403  WRITE (IPRINT,3203)
      RETURN
C
 404  WRITE (IPRINT,3204)
      RETURN
C
 405  WRITE (IPRINT,3205)
      RETURN
C
 406  WRITE (IPRINT,3206)
      RETURN
C
 407  WRITE (IPRINT,3207)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
2901  FORMAT (1X,71HNORMLAGUERRE POLYNOMIAL, ORDER (N) OF COL (C) PUT IN
     1 (C) AND SUCC. COLS)
2902  FORMAT (1X,71HLAGUERRE POLYNOMIAL ORDER (N) OF COL (C), PUT IN COL
     1 (C) AND SUCC. COLS)
2903  FORMAT (1X,70HHERMITE POLYNOMIAL ORDER (N) OF COL (C), PUT IN COL 
     1(C) AND SUCC. COLS)
2904  FORMAT (1X,68HUCHEBYSHEV POLYNOMIAL ORDER (N) OF COL (C) PUT IN (C
     1) AND SUCC. COLS)
2905  FORMAT (1X,71HLEGENDRE POLYNOMIAL ORDER (N) OF COL (C), PUT IN COL
     1 (C) AND SUCC. COLS)
2906  FORMAT (1X,71HTCHEBYSHEV POLYNOMIAL OF ORDER (N) OF COL (C) PUT IN
     1 (C) AND SUCC. COLS)
3001  FORMAT (1X,49HPARSUM COLUMN (C), PUT PARTIAL SUMS IN COLUMN (C))
3002  FORMAT (1X,60HPARPRODUCT OF COLUMN (C), PUT PARTIAL PRODUCTS IN CO
     1LUMN (C))
3003  FORMAT (1X,52HRMS OF COLUMN (C) PUT ROOT MEAN SQUARE IN COLUMN (C)
     1)
3004  FORMAT (1X,48HSUM ROWS OF COLUMN (C) AND PUT SUM IN COLUMN (C)/
     2        1X,59HSUM COLUMN (C), ROWS (R) THROUGH (R), PUT SUM IN COL
     2UMN (C)/
     3        1X,71HSUM COL (C), ROWS (R), (R)...(R), PUT SUM IN COL (C)
     3  $ AT LEAST 5 ARGS)
3101  FORMAT (1X,70HROW SUM COLS (C), (C) ... (C) PUT IN COL (C)  $ USE 
     1AT LEAST 4 COL NOS/
     2        1X,53HROW SUM COLUMNS (C) THROUGH (C) AND PUT IN COLUMN (C
     2)/
     2        1X,50HROW SUM THE ENTIRE WORKSHEET AND PUT IN COLUMN (C)/
     2        1X,70HROWSUM COLS (C), (C) ... (C) PUT IN COL (C)  $ USE A
     2T LEAST 4 COL NOS./
     2        1X,52HROWSUM COLUMNS (C) THROUGH (C) AND PUT IN COLUMN (C)
     2/
     3        1X,49HROWSUM THE ENTIRE WORKSHEET AND PUT IN COLUMN (C))
3102  FORMAT (1X,68HPRODUCT ROW BY ROW OF COLS (C)...(C) PUT IN (C) $ AT
     1 LEAST 4 COL NOS/
     3        1X,52HPRODUCT OF COLUMNS (C) THROUGH (C) PUT IN COLUMN (C)
     3)
3103  FORMAT (1X,26HDEFINE (E) INTO COLUMN (C)/
     2        1X,51HDEFINE THE CONSTRANT (K) INTO ROW (R) OF COLUMN (C)/
     2        1X,57HDEFINE THE VALUE IN ROW (R) OF COLUMN (C) INTO COLUM
     2N (C)/
     3        1X,68HDEFINE THE VALUE IN ROW (R) OF COLUMN (C) INTO ROW (
     3R) OF COLUMN (C))
3105  FORMAT (1X,41HMAX VALUE OF COLUMN (C) PUT IN COLUMN (C)/
     2        1X,70HMAX OF COL (C) PUT IN (C), CORRESPONDING VALUE OF (C
     2) INTO COL (C) .../
     2        1X,45HMAXIMUM VALUE OF COLUMN (C) PUT IN COLUMN (C)/
     3        1X,67HMAXIMUM OF COL (C) PUT IN COL (C), CORRESP VALUE OF 
     3(C) IN (C), ...)
3106  FORMAT (1X,41HMIN VALUE OF COLUMN (C) PUT IN COLUMN (C)/
     2        1X,67HMIN OF COL (C) PUT IN COL (C), CORRESP VALUE OF COL 
     2(C) IN (C), .../
     2        1X,45HMINIMUM VALUE OF COLUMN (C) PUT IN COLUMN (C)/
     3        1X,65HMINIMUM OF COL (C) PUT IN (C) CORRESP VALUE OF COL (
     3C) IN (C) ...)
3108  FORMAT (1X,71HSORT COL (C) MIN TO MAX, CARRY ALONG CORRESP VALUES 
     1IN COLS (C) ... (C)/
     3        1X,15HSORT COLUMN (C))
3109  FORMAT (1X,64HORDER INDEPENDENTLY COLUMNS (C), (C) ... (C) SMALLES
     1T TO LARGEST)
3110  FORMAT (1X,30HERASE COLUMNS (C), (C) ... (C)/
     3        1X,50HERASE THE ENTIRE WORKSHEET AND RESET NRMAX TO ZERO)
3111  FORMAT (1X,57HEXCHANGE COL (C) WITH COL (C), COL (C) WITH COL (C),
     1 ETC.)
3112  FORMAT (1X,64HFLIP COLUMN (C) INTO COLUMN (C), COLUMN (C) INTO COL
     1UMN (C), ETC/
     3        1X,37HFLIP THE ENTIRE WORKSHEET UPSIDE DOWN)
3113  FORMAT (1X,49HCHANGE SIGN OF VALUES IN COLUMNS (C), (C) ... (C))
3114  FORMAT (1X,71HHIERARCHY OF COL (C), PUT LOCATIONS OF SMALLEST THRU
     1 LARGEST IN COL (C))
3115  FORMAT (1X,65HLIST (N) $ CONTROLS LISTING OF INSTRUCTIONS, N IS 0,
     1 1, 2, 3 OR 4/
     3        1X,33HLIST INSTRUCTIONS AND DIAGNOSTICS)
3116  FORMAT (1X,55HNO LIST            $ SUPPRESSES LISTING OF INSTRUCTI
     1ONS)
3117  FORMAT (1X,38HNULL   $ THIS INSTRUCTION DOES NOTHING)
3118  FORMAT (1X,30HERROR OF (E) PUT IN COLUMN (C))
3119  FORMAT (1X,29HCERF OF (E) PUT IN COLUMN (C))
3120  FORMAT (1X,36HSININTEGRAL OF (E) PUT IN COLUMN (C))
3121  FORMAT (1X,36HCOSINTEGRAL OF (E) PUT IN COLUMN (C)/
     3        1X,69HCOSINTEGRAL OF (E) PUT REAL VALUES IN (C) AND IMAGIN
     3ARY VALUES IN (C))
3122  FORMAT (1X,35HEINTEGRAL OF (E), PUT IN COLUMN (C))
3123  FORMAT (1X,37HNEGEINTEGRAL OF (E) PUT IN COLUMN (C))
3124  FORMAT (1X,37HHSININTEGRAL OF (E) PUT IN COLUMN (C))
3125  FORMAT (1X,37HHCOSINTEGRAL OF (E) PUT IN COLUMN (C)/
     3        1X,70HHCOSINTEGRAL OF (E) PUT REAL PART IN COL (C) AND IMA
     3GINARY PART IN (C))
3126  FORMAT (1X,44HEXPINTEGRAL FOR (N) OF (E) PUT IN COLUMN (C))
3127  FORMAT (1X,45HEEXPINTEGRAL FOR (N) OF (E) PUT IN COLUMN (C))
3201  FORMAT (1X,70HPOLYFIT Y IN (C), USING WTS (E), OF DEGREE (D), PRED
     1ICTOR X IN COL (C)/
     2        1X,71HPOLYFIT Y IN (C), WTS (E), DEGREE (D), X IN (C) PUT 
     2COEFFICIENTS IN (C)/
     2        1X,71HPOLYFIT Y IN (C), WTS (E), DEG (D), X IN (C), PUT CO
     2EFS IN (C), RES (C)/
     2        1X,71HPOLYFIT (C), (E), (D), (C) PUT COEFS IN (C), RES IN 
     2(C) SD OF PV IN (C)/
     2        1X,70HPOLYFIT (C), (E), (D), (C) PUT IN (C), (C), (C), FOU
     2RIER COEFFS IN (C)/
     3        1X,70HPOLYFIT (C),(E),(D),(C) PUT IN (C), (C), (C), (C) VC
     3 MATRIX IN (R),(C))
3202  FORMAT (1X,70HSPOLYFIT Y IN (C), WTS (E), DEGREE (D), X IN (C) PUT
     1 COEFFS IN COL (C)/
     2        1X,71HSPOLYFIT Y IN (C), WTS (E), DEG (D), X IN (C), PUT C
     2OEFS IN (C) RES (C)/
     2        1X,71HSPOLYFIT (C),(E),(D),(C) PUT COEFFS IN (C), RES IN (
     2C), SD OF PV IN (C)/
     2        1X,71HSPOLYFIT (C), (E), (D), (C) PUT IN (C), (C), (C), FO
     2URIER COEFFS IN (C)/
     3        1X,70HSPOLYFIT (C),(E),(D),(C), PUT IN (C),(C),(C),(C), VC
     3 MATRIX IN (R),(C))
3203  FORMAT (1X,68HFIT Y IN COL (C), WTS (E), (K) VARIABLES IN COLUMNS 
     1(C), (C) ... (C)/
     2        1X,69HFIT Y IN (C), WTS (E), (K) VAR'S IN COLS (C)...(C), 
     2PUT COEFFS IN (C)/
     2        1X,68HFIT (C), WTS (E), TO (K) IN (C)...(C), PUT COEFFS IN
     2 (C), RES IN (C)/
     2        1X,71HFIT (C), (E), (K), (C)...(C), COEF IN (C), RES. IN (
     2C), SD OF PV IN (C)/
     2        1X,71HFIT (C), (E), (K), (C)...(C), PUT IN (C), (C), (C), 
     2FOURIER COEF IN (C)/
     3        1X,71HFIT (C),(E),(K), (C)...(C), PUT IN (C),(C),(C),(C) V
     3C MATRIX IN (R),(C))
3204  FORMAT (1X,70HSFIT Y IN (C), WTS (E), (K) VARS IN COLS (C)...(C), 
     1PUT COEFF'S IN (C)/
     2        1X,69HSFIT (C), WTS (E), TO (K) IN (C)...(C), PUT COEFFS I
     2N (C) RES. IN (C)/
     2        1X,70HSFIT (C), (E), (K), (C)...(C), COEFF IN (C) RES IN (
     2C) SD OF PV IN (C)/
     2        1X,71HSFIT (C), (E), (K), (C)...(C), PUT IN (C),(C),(C), F
     2OURIER COEFF IN (C)/
     3        1X,71HSFIT (C),(E),(K),(C)...(C), PUT IN (C),(C),(C),(C) V
     3C MATRIX IN (R),(C))
3205  FORMAT (1X,70HMORTHO MAT (R),(C) SIZE (R)X(C) WTS (E), ORTHNORMAL 
     1VECTORS IN (R),(C)/
     3        1X,68HMORTHO (R),(C) SIZE (R)X(C) WTS (E) PUT IN (R),(C) T
     3RANS MAT (R),(C))
3206  FORMAT (1X,71HBESTCP OF Y IN COL (C) WEIGHTS (E) WITH (K) VARIABLE
     1S IN COLS (C)...(C))
3207  FORMAT (1X,71HLARFIT (C), WTS 1.0, (K) VECTORS (C),...,(C), COEFS 
     1IN (C), RES. IN (C))
C
C     ==================================================================
C
      END
*DESC6
      SUBROUTINE DESC6
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  DESC6 V 7.00 12/ 7/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE DESCRIBE INSTRUCTION FOR ...
C        L1 = 23 TO  24.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - NOVEMBER, 1978.
C                   CURRENT VERSION - DECEMBER, 1989.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
C     ==================================================================
C
      IF (L1.LT.23 .OR. L1.GT.24) GO TO 50
C
      L = L1 - 22
      GO TO ( 1, 2), L
C
   1  GO TO (101,102,103,104,105,106,107,108,109,110,
     1       111,112), L2
   2  GO TO (201,202,203,204,205,206,207,208,209,210,
     1       211,212,213,214,215,216), L2
C
C     ..................................................................
C
  50  CALL ERROR (1)
      RETURN
C
C     ..................................................................
C
C     L1 = 23.
C
 101  WRITE (IPRINT,3301)
      RETURN
C
 102  WRITE (IPRINT,3302)
      RETURN
C
 103  WRITE (IPRINT,3303)
      RETURN
C
 104  WRITE (IPRINT,3304)
      RETURN
C
 105  WRITE (IPRINT,3305)
      RETURN
C
 106  WRITE (IPRINT,3306)
      RETURN
C
 107  GO TO 50
C
 108  GO TO 50
C
 109  GO TO 50
C
 110  WRITE (IPRINT,3310)
      RETURN
C
 111  WRITE (IPRINT,3311)
      RETURN
C
 112  WRITE (IPRINT,3312)
      RETURN
C
C     ..................................................................
C
C     L1 = 24.
C
 201  WRITE (IPRINT,3401)
      RETURN
C
 202  WRITE (IPRINT,3402)
      RETURN
C
 203  WRITE (IPRINT,3403)
      RETURN
C
 204  WRITE (IPRINT,3404)
      RETURN
C
 205  WRITE (IPRINT,3405)
      RETURN
C
 206  WRITE (IPRINT,3406)
      RETURN
C
 207  WRITE (IPRINT,3407)
      RETURN
C
 208  WRITE (IPRINT,3408)
      RETURN
C
 209  WRITE (IPRINT,3409)
      RETURN
C
 210  WRITE (IPRINT,3410)
      RETURN
C
 211  WRITE (IPRINT,3411)
      RETURN
C
 212  WRITE (IPRINT,3412)
      RETURN
C
 213  WRITE (IPRINT,3413)
      RETURN
C
 214  WRITE (IPRINT,3414)
      RETURN
C
 215  WRITE (IPRINT,3415)
      RETURN
C
 216  WRITE (IPRINT,3416)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
3301  FORMAT (1X,70HCLOSE UP ROWS WITH (K) IN COLS (C), (C) ... (C) $ PU
     1TS ZEROS AT BOTTOM)
3302  FORMAT (1X,71HCOUNT LENGTH OF (C) PUT IN (C) $ SEARCHES FROM BELOW
     1 FOR 1ST NOZERO NO.)
3303  FORMAT (1X,67HSHORTEN COL (C) FOR COL (C) = (K) PUT SHORTENED COLS
     1 IN (C) AND (C))
3304  FORMAT (1X,70HEXPAND (E) TO POWER (P) IN INCREMENTS OF (I), IN COL
     1 (C) AND SUCC COLS/
     3        1X,71HEXPAND (E) TO POWER (K) IN INCREMENTS OF (K), PUT IN
     3 (C) AND SUCC. COLS)
3305  FORMAT (1X,69HDUPLICATE (T) TIMES, ARRAY IN (R),(C) OF SIZE (R)X(C
     1), PUT IN (R),(C))
3306  FORMAT (1X,53HAMOVE THE ARRAY IN (R),(C) OF SIZE (R)X(C) TO (R),(C
     1)/
     2        1X,52HMOVE THE ARRAY IN (R),(C) OF SIZE (R)X(C) TO (R),(C)
     2/
     3        1X,54HMMOVE THE MATRIX IN (R),(C) OF SIZE (R)X(C) TO (R),(
     3C))
3310  FORMAT (1X,69HPROMOTE BY (R) ROWS, COL (C) INTO COL (C), COL (C) I
     1NTO COL (C), ETC./
     3        1X,47HPROMOTE ALL VALUES IN THE WORKSHEET BY (R) ROWS)
3311  FORMAT (1X,68HDEMOTE BY (R) ROWS, COL (C) INTO COL (C), COL (C) IN
     1TO COL (C), ETC./
     3        1X,46HDEMOTE ALL VALUES IN THE WORKSHEET BY (R) ROWS)
3312  FORMAT (1X,71HDIM THE WORKSHEET TO BE (R) ROWS BY (C) COLUMNS  $ R
     1 X C AT MOST 12,500/
     3        1X,69HDIMENSION WORKSHEET TO BE (R) ROWS X (C) COLS  $ R X
     3 C AT MOST 12,500)
3401  FORMAT (1X,34HSTATISTICAL ANALYSIS OF COLUMN (C)/
     2        1X,70HSTATISTICAL ANALYSIS OF COL (C), PUT STATIS IN (C) A
     2ND NEXT THREE COLS/
     2        1X,71HSTATISTICAL ANAL OF (C), WTS (C), PUT STATIS IN (C) 
     2AND NEXT THREE COLS/
     2        1X,71HSTATISTICAL ANAL OF (C) WTS (C) DON'T PUT IN (-C) $ 
     2- COL NO=NO STORAGE/
     2        1X,70HSTATISTICAL ANAL. OF COL (C), PUT STATIS IN COLS (C)
     2, (C), (C) AND (C)/
     3        1X,70HSTATISTICAL ANAL. OF (C) WTS IN (C), PUT STATIS IN (
     3C),(C),(C) AND (C))
3402  FORMAT (1X,71HSSTATISTICAL ANAL. OF COL (C) PUT STATISTICS IN (C) 
     1AND NEXT THREE COLS/
     2        1X,69HSSTATISTICAL ANAL. OF (C), WTS IN (C), PUT IN (C) AN
     2D NEXT THREE COLS/
     2        1X,71HSSTATISTICAL ANAL. OF COL (C), PUT STATIS IN COLS (C
     2), (C), (C) AND (C)/
     3        1X,71HSSTATISTICAL ANALYSIS OF (C), WTS (C), PUT IN COL (C
     3), (C), (C) AND (C))
3403  FORMAT (1X,60HRANKS OF COLUMN (C) PUT IN COLUMN (C)  $ SMALLEST HA
     1S RANK 1)
3404  FORMAT (1X,71HGAUSS QUADRATURE WITH (K) PTS FROM (K) TO (K), PUT X
     1 IN (C), WTS IN (C))
3405  FORMAT (1X,71HF PROBABILITY WITH (E) AND (E) DEG OF FREEDOM, FOR (
     1E), TAILAREA IN (C))
3406  FORMAT (1X,71HTWOWAY ANAL FOR (R)X(C) TABLE, DATA IN (C), STORE IN
     1 (C) AND SUCC. COLS/
     3        1X,71HTWOWAY ANAL. FOR (R)X(C) TABLE, DATA (C), STORE FROM
     3 (C) ON, WTS IN (C))
3407  FORMAT (1X,71HSTWOWAY ANAL. FOR (R)X(C) TABLE DATA IN (C) STORE IN
     1 (C) AND SUCC. COLS/
     3        1X,71HSTWOWAY ANAL FOR (R)X(C) TABLE, DATA (C), STORE FORM
     3 (C) ON, WTS IN (C))
3408  FORMAT (1X,71HHISTOGRAM  USING MID-POINTS IN COLUMN (C) AND FREQUE
     1NCIES IN COLUMN (C))
3409  FORMAT (1X,70HNHISTOGRAM USING MIDPOINTS IN (C) AND FREQUENCIES IN
     1 (C) $ NO NEW PAGE)
3410  FORMAT (1X,54HFREQUENCY DISTRIBUTION OF COLUMN (C) PUT IN COLUMN (
     1C)/
     2        1X,69HFREQUENCY DISTRIBUTION OF COL (C), USE (K) CLASSES, 
     2PUT IN COLUMN (C)/
     2        1X,70HFREQUENCY DIST'N OF COL (C), USE (K) CLASSES OF LENG
     2TH (K), PUT IN (C)/
     2        1X,70HFREQUENCY OF (C), USE (K) CLASSES OF LENGTH (K), STA
     2RT (K), PUT IN (C)/
     2        1X,70HFREQUENCY OF (C), LOWER BOUNDARIES IN (C), UPPER IN 
     2(C), FREQ'S IN (C)/
     2        1X,67HFREQUENCY OF (C) USING (K) CLASSES, PUT IN COLUMNS (
     2C), (C) AND (C)/
     2        1X,71HFREQUENCY OF (C) USING (K) CLASSES OF LENGTH (K) PUT
     2 IN (C),(C) AND (C)/
     3        1X,71HFREQUENCY OF (C) CLASSES (K), LENGTH (K), START (K),
     3 IN (C),(C) AND (C))
3411  FORMAT (1X,61HCORRELATION BETWEEN (P) VARIABLES IN COLUMNS (C), (C
     1) ... (C)/
     2        1X,71HCORRELATION (P) VARS IN (C)...(C), PUT ARRAY OF SIMP
     2LE COEFS IN (R),(C)/
     3        1X,69HCORRELATION FOR (P) IN (C)...(C), R COEFFS IN (R),(C
     3), RHO IN (R),(C))
3412  FORMAT (1X,68HSCORRELATION (P) VARIABLES IN (C)...(C) PUT SIMPLE C
     1OEFFS IN (R),(C)/
     3        1X,67HSCORRELATION (P) VAR'S IN (C)...(C) PUT R IN (R),(C)
     3 RHO IN (R),(C))
3413  FORMAT (1X,70HONEWAY ANALYSIS FOR DATA IN COLUMN (C) WITH GROUP NU
     1MBER IN COLUMN (C)/
     2        1X,68HONEWAY FOR (C), TAG IN (C) PUT STATISTICS IN (C) AND
     2 NEXT THREE COLS/
     3        1X,70HONEWAY FOR (C) WITH (C), TAG IN (C), NUMBER (C), MEA
     3NS (C) S.D. IN (C))
3414  FORMAT (1X,71HSONEWAY ANAL. FOR (C), GROUP NUMBER (C), PUT IN (C) 
     1AND NEXT THREE COLS/
     3        1X,67HSONEWAY FOR DATA (C) GROUP NO (C) PUT IN COLS (C), (
     3C), (C) AND (C))
3415  FORMAT (1X,68HCONTINGENCY TABLE ANALYSIS OF (R) X (C) TABLE STARTI
     1NG IN COLUMN (C))
3416  FORMAT (1X,71HSPLIT PLOT ANAL. OF (C), REP NOS (C), WHOLE PLOT NOS
     1 (C), S. P. NOS (C))
C
C     ==================================================================
C
      END
*DESC7
      SUBROUTINE DESC7
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  DESC7 V 7.00 12/ 7/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE DESCRIBE INSTRUCTION FOR ...
C        L1 = 25 TO  29.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - NOVEMBER, 1978.
C                   CURRENT VERSION - DECEMBER, 1989.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
C     ==================================================================
C
      IF (L1.LT.25 .OR. L1.GT.29) GO TO 50
C
      L = L1 - 24
      GO TO ( 1, 2, 3, 4, 5), L
C
   1  GO TO (101,102,103,104,105,106,107,108,109,110,
     1       111), L2
   2  GO TO (201,202,203), L2
   3  GO TO (301,302,303,304), L2
   4  GO TO (401,402,403), L2
   5  GO TO (501,502,503,504), L2
C
C     ..................................................................
C
  50  CALL ERROR (1)
      RETURN
C
C     ..................................................................
C
C     L1 = 25.
C
 101  WRITE (IPRINT,3501)
      RETURN
C
 102  WRITE (IPRINT,3502)
      RETURN
C
 103  WRITE (IPRINT,3503)
      RETURN
C
 104  WRITE (IPRINT,3504)
      RETURN
C
 105  WRITE (IPRINT,3505)
      RETURN
C
 106  WRITE (IPRINT,3506)
      RETURN
C
 107  WRITE (IPRINT,3507)
      RETURN
C
 108  WRITE (IPRINT,3508)
      RETURN
C
 109  WRITE (IPRINT,3509)
      RETURN
C
 110  WRITE (IPRINT,3510)
      RETURN
C
 111  WRITE (IPRINT,3511)
      RETURN
C
C     ..................................................................
C
C     L2 = 26.
C
 201  WRITE (IPRINT,3601)
      RETURN
C
 202  WRITE (IPRINT,3602)
      RETURN
C
 203  WRITE (IPRINT,3603)
      RETURN
C
C     ..................................................................
C
C     L1 = 27.
C
 301  WRITE (IPRINT,3701)
      RETURN
C
 302  WRITE (IPRINT,3702)
      RETURN
C
 303  WRITE (IPRINT,3703)
      RETURN
C
 304  WRITE (IPRINT,3704)
      RETURN
C
C     ..................................................................
C
C     L2 = 28.
C
 401  WRITE (IPRINT,3801)
      RETURN
C
 402  WRITE (IPRINT,3802)
      RETURN
C
 403  WRITE (IPRINT,3803)
      RETURN
C
C     ..................................................................
C
C     L1 = 29.
C
 501  GO TO 50
C
 502  WRITE (IPRINT,3902)
      RETURN
C
 503  WRITE (IPRINT,3903)
      RETURN
C
 504  WRITE (IPRINT,3904)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
3501  FORMAT (1X,70HSELECT IN (C) NOS APPROX'G COL (C) WITHIN ABS TOLERE
     1NCE (K) PUT IN (C)/
     2        1X,68HSELECT IN (C) NOS APPROX (C) TO WITHIN ABS TOL (K) P
     2UT IN (C) TO (C)/
     3        1X,71HSELECT IN (C) NOS APPROX (C) WITHIN ABS TOL (K) IN (
     3C) TO (C) COUNT (C))
3502  FORMAT (1X,71HSEARCH COL (C) EQUAL (C) MOVE CORRESP NOS IN (C) TO 
     1(C), (C) TO (C) ETC)
3503  FORMAT (1X,71HCENSOR COL (C) FOR VALUES LESS THAN OR = (E) REPLACE
     1 BY (E), PUT IN (C))
3504  FORMAT (1X,68HINTERPOLATE XY IN (C)(C) LENGTH (N) VALUES (V) IN (C
     1) PTS (P) IN (C))
3505  FORMAT (1X,65HMATCH COLUMN (C) WITH (E), EXTRACT FROM (E) AND PUT 
     1IN COLUMN (C))
3506  FORMAT (1X,71HCENSOR LE COL (C) NOS LESS THAN OR EQ TO (E) REPLACE
     1 BY (E), PUT IN (C))
3507  FORMAT (1X,69HCENSOR EQ COL (C) FOR VALUES EQUAL TO (E), REPLACE B
     1Y (E), PUT IN (C))
3508  FORMAT (1X,71HCENSOR GE COL (C) NOS GREATER THAN OR EQ (E) REPLACE
     1 BY (E), PUT IN (C))
3509  FORMAT (1X,70HCENSOR GT COL (C), VALUES GREATER THAN (E), REPLACE 
     1BY (E), PUT IN (C))
3510  FORMAT (1X,71HCENSOR LT COL (C), VALUES LESS THAN (E), REPLACE BY 
     1(E), PUT IN COL (C))
3511  FORMAT (1X,71HCENSOR NE COL (C), VALUES NOT EQUAL (E), REPLACE BY 
     1(E), PUT IN COL (C))
3601  FORMAT (1X,71HMVECDIAGONAL MATRIX IN (R),(C) SIZE (R)X(C), PUT DIA
     1GONAL IN COLUMN (C)/
     3        1X,67HMVECDIAGONAL MATRIX (R),(C) SIZE (R)X(C), PUT COL VE
     3CTOR IN (R),(C))
3602  FORMAT (1X,70HMVECMAT VECTORIZE ROW BY ROW MATRIX IN (R),(C) SIZE 
     1(R)X(C) IN COL (C)/
     3        1X,70HMVECMAT MATRIX (R),(C) SIZE (R)X(C), PUT VECTOR INTO
     3 (R),(C) AND BELOW)
3603  FORMAT (1X,71HMMATVEC MAKE BY ROWS COL (C) INTO THE MATRIX IN (R),
     1(C) OF SIZE (R)X(C)/
     3        1X,68HMMATVEC COL VECTOR IN (R),(C) INTO MATRIX IN (R),(C)
     3 OF SIZE (R)X(C))
3701  FORMAT (1X,52HMPROPERTIES OF THE MATRIX IN (R),(C) OF SIZE (R)X(C)
     1/
     2        1X,71HMPROPERTIES OF MATRIX IN (R),(C) SIZE (R)X(C) PUT PR
     2OPERTIES IN COL (C)/
     2        1X,63HMPROPERTIES OF (R),(C) SIZE (R)X(C) PUT COLUMN AVE'S
     2 IN (R),(C)/
     2        1X,71HMPROPERTIES OF (R),(C) SIZE (R)X(C), PROP'S IN (C) C
     2OL AVE'S IN (R),(C)/
     2        1X,70HMPROPERTIES OF (R),(C) SIZE (R)X(C), COL AVES (R),(C
     2) ROW AVES (R),(C)/
     3        1X,71HMPROPERTIES OF (R),(C) SIZE (R)X(C) PUT IN (C) AVES  
     3(R),(C) AND (R),(C))
3702  FORMAT (1X,51HAPROPERTIES OF THE ARRAY IN (R),(C) OF SIZE (R)X(C)/
     2        1X,70HAPROPERTIES OF THE ARRAY IN (R),(C) OF SIZE (R)X(C),
     2 PUT IN COLUMN (C)/
     2        1X,70HAPROPERTIES OF ARRAY (R),(C) SIZE (R)X(C), PUT COLUM
     2N AVE'S IN (R),(C)/
     2        1X,70HAPROPERTIES OF (R),(C) SIZE (R)X(C), PROP IN (C), CO
     2L AVE'S IN (R),(C)/
     2        1X,71HAPROPERTIES OF (R),(C) SIZE (R)X(C) COL AVE'S (R),(C
     2) ROW AVE'S (R),(C)/
     3        1X,70HAPROPERTIES ARRAY (R), IN(C) SIZE (R)X(C), (C), AVE'
     3S (R),(C), (R),(C))
3703  FORMAT (1X,68HSMPROPERTIES OF MATRIX IN (R),(C) SIZE (R)X(C) PUT P
     1ROPERTIES IN (C)/
     2        1X,64HSMPROPERTIES OF (R),(C) SIZE (R)X(C) PUT COLUMN AVE'
     2S IN (R),(C)/
     2        1X,71HSMPROPERTIES OF (R),(C) SIZE (R)X(C), PROPS IN (C) C
     2OL AVE'S IN (R),(C)/
     2        1X,71HSMPROP OF (R),(C) SIZE (R)X(C), COL AVES IN (R),(C) 
     2ROW AVES IN (R),(C)/
     3        1X,71HSMPROP OF (R),(C) SIZE (R)X(C) IN (C) COL AVES (R),(
     3C) ROW AVES (R),(C))
3704  FORMAT (1X,71HSAPROPERTIES OF ARRAY (R),(C) SIZE (R)X(C) PUT PROPE
     1RTIES IN COLUMN (C)/
     2        1X,64HSAPROPERTIES OF (R),(C) SIZE (R)X(C) PUT COLUMN AVE'
     2S IN (R),(C)/
     2        1X,70HSAPROP OF (R),(C) SIZE (R)X(C), PROPERTIES IN (C) CO
     2L AVE'S IN (R),(C)/
     2        1X,70HSAPROP OF (R),(C) SIZE (R)X(C), COL AVE'S IN (R),(C)
     2 ROW AVE'S (R),(C)/
     3        1X,71HSAPROP OF (R),(C) SIZE (R)X(C) IN (C) COL AVES (R),(
     3C) ROW AVES (R),(C))
3801  FORMAT (1X,70HITERATE X,Y IN (C),(C) DESIRED Y IN (C) PUT IN (C) A
     1ND NEXT THREE COLS)
3802  FORMAT (1X,71HISETUP X,Y IN (C),(C), DESIRED Y IN (C), PUT IN (C) 
     1AND NEXT THREE COLS)
3803  FORMAT (1X,67HISOLATE X IN (C), Y IN (C) DESIRED Y = (K), PUT IN C
     1OLS (C) AND (C)/
     3        1X,71HISOLATE X IN (C), Y IN (C), FOR (K), USE (P) POINTS,
     3 PUT IN (C) AND (C))
3902  FORMAT (1X,70HSEPARATE FROM COL (C) EVERY (R) TH ROW, START WITH R
     1OW (R), PUT IN (C))
3903  FORMAT (1X,71HINSERT IN (C) FROM (C) AT EVERY (I) ROW, START WITH 
     1ROW (R), PUT IN (C))
3904  FORMAT (1X,71HEXTREMA FOR X IN (C) Y IN (C) PUT MAX X,Y IN (C),(C)
     1 MIN X,Y IN (C),(C)/
     3        1X,71HMAXMIN X IN (C) Y IN (C) PUT MAX X IN (C) MAX Y (C) 
     3MIN X (C) MIN Y (C))
C
C     ==================================================================
C
      END
*DESC8
      SUBROUTINE DESC8
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  DESC8 V 7.00 12/ 8/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE DESCRIBE INSTRUCTION FOR ...
C        L1 = 30 TO 34.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - NOVEMBER, 1978.
C                   CURRENT VERSION - DECEMBER, 1989.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
C     ==================================================================
C
      IF (L1.LT.30 .OR. L1.GT.34) GO TO 50
C
      L = L1 - 29
      GO TO ( 1,2,3,4,5), L
C
   1  GO TO (101,102,103,104,105,106,107,108,109,110,
     1       111,112,113,114,115,116,117,118,119,120,
     2       121,122,123,124,125,126,127,128,129,130,
     3       131,132,133,134,135,136,137,138,139), L2
   2  GO TO (201,202,203,204,205,206,207,208,209), L2
   3  GO TO (301,302,303,304,305,306, 50,308,309,310,
     1       311,312,313,314,315,316,317,318), L2
   4  GO TO (401,402), L2
   5  GO TO (501,502,503,504,505,506,507,508,509,510,
     1       511,512,513,514), L2
C
C     ..................................................................
C
  50  CALL ERROR (1)
      RETURN
C
C     ..................................................................
C
C     L1 = 30.
C
 101  WRITE (IPRINT,4001)
      RETURN
C
 102  WRITE (IPRINT,4002)
      RETURN
C
 103  WRITE (IPRINT,4003)
      RETURN
C
 104  WRITE (IPRINT,4004)
      RETURN
C
 105  WRITE (IPRINT,4005)
      RETURN
C
 106  WRITE (IPRINT,4006)
      RETURN
C
 107  WRITE (IPRINT,4007)
      RETURN
C
 108  WRITE (IPRINT,4008)
      RETURN
C
 109  WRITE (IPRINT,4009)
      RETURN
C
 110  WRITE (IPRINT,4010)
      RETURN
C
 111  WRITE (IPRINT,4011)
      RETURN
C
 112  WRITE (IPRINT,4012)
      RETURN
C
 113  WRITE (IPRINT,4013)
      RETURN
C
 114  WRITE (IPRINT,4014)
      RETURN
C
 115  WRITE (IPRINT,4015)
      RETURN
C
 116  WRITE (IPRINT,4016)
      RETURN
C
 117  WRITE (IPRINT,4017)
      RETURN
C
 118  WRITE (IPRINT,4018)
      RETURN
C
 119  WRITE (IPRINT,4019)
      RETURN
C
 120  WRITE (IPRINT,4020)
      RETURN
C
 121  WRITE (IPRINT,4021)
      RETURN
C
 122  WRITE (IPRINT,4022)
      RETURN
C
 123  WRITE (IPRINT,4023)
      RETURN
C
 124  WRITE (IPRINT,4024)
      RETURN
C
 125  WRITE (IPRINT,4025)
      RETURN
C
 126  WRITE (IPRINT,4026)
      RETURN
C
 127  WRITE (IPRINT,4027)
      RETURN
C
 128  WRITE (IPRINT,4028)
      RETURN
C
 129  WRITE (IPRINT,4029)
      RETURN
C
 130  WRITE (IPRINT,4030)
      RETURN
C
 131  WRITE (IPRINT,4031)
      RETURN
C
 132  WRITE (IPRINT,4032)
      RETURN
C
 133  WRITE (IPRINT,4033)
      RETURN
C
 134  WRITE (IPRINT,4034)
      RETURN
C
 135  WRITE (IPRINT,4035)
      RETURN
C
 136  WRITE (IPRINT,4036)
      RETURN
C
 137  WRITE (IPRINT,4037)
      RETURN
C
 138  WRITE (IPRINT,4038)
      RETURN
C
 139  WRITE (IPRINT,4039)
      RETURN
C
C     ..................................................................
C
C     L1 = 31.
C
 201  WRITE (IPRINT,4101)
      RETURN
C
 202  WRITE (IPRINT,4102)
      RETURN
C
 203  WRITE (IPRINT,4103)
      RETURN
C
 204  WRITE (IPRINT,4104)
      RETURN
C
 205  WRITE (IPRINT,4105)
      RETURN
C
 206  WRITE (IPRINT,4106)
      RETURN
C
 207  WRITE (IPRINT,4107)
      RETURN
C
 208  WRITE (IPRINT,4108)
      RETURN
C
 209  WRITE (IPRINT,4109)
      RETURN
C
C     ..................................................................
C
C     L1 = 32.
C
 301  WRITE (IPRINT,4201)
      RETURN
C
 302  WRITE (IPRINT,4202)
      RETURN
C
 303  WRITE (IPRINT,4203)
      RETURN
C
 304  WRITE (IPRINT,4204)
      RETURN
C
 305  WRITE (IPRINT,4205)
      RETURN
C
 306  WRITE (IPRINT,4206)
      RETURN
C
 308  WRITE (IPRINT,4208)
      RETURN
C
 309  WRITE (IPRINT,4209)
      RETURN
C
 310  WRITE (IPRINT,4210)
      RETURN
C
 311  WRITE (IPRINT,4211)
      RETURN
C
 312  WRITE (IPRINT,4212)
      RETURN
C
 313  WRITE (IPRINT,4213)
      RETURN
C
 314  WRITE (IPRINT,4214)
      RETURN
C
 315  WRITE (IPRINT,4215)
      RETURN
C
 316  WRITE (IPRINT,4216)
      RETURN
C
 317  WRITE (IPRINT,4217)
      RETURN
C
 318  WRITE (IPRINT,4218)
      RETURN
C
C     ..................................................................
C
C     L1 = 33.
C
 401  WRITE (IPRINT,4301)
      RETURN
C
 402  WRITE (IPRINT,4302)
      RETURN
C
C     ..................................................................
C
C     L1 = 34.
C
 501  WRITE (IPRINT,4401)
      RETURN
C
 502  WRITE (IPRINT,4402)
      RETURN
C
 503  WRITE (IPRINT,4403)
      RETURN
C
 504  WRITE (IPRINT,4404)
      RETURN
C
 505  WRITE (IPRINT,4405)
      RETURN
C
 506  WRITE (IPRINT,4406)
      RETURN
C
 507  WRITE (IPRINT,4407)
      RETURN
C
 508  WRITE (IPRINT,4408)
      RETURN
C
 509  WRITE (IPRINT,4409)
      RETURN
C
 510  WRITE (IPRINT,4410)
      RETURN
C
 511  WRITE (IPRINT,4411)
      RETURN
C
 512  WRITE (IPRINT,4412)
      RETURN
C
 513  WRITE (IPRINT,4413)
      RETURN
C
 514  WRITE (IPRINT,4414)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
4001  FORMAT (1X,31HBJZERO OF (E) PUT IN COLUMN (C))
4002  FORMAT (1X,30HBJONE OF (E) PUT IN COLUMN (C))
4003  FORMAT (1X,31HBYZERO OF (E) PUT IN COLUMN (C))
4004  FORMAT (1X,30HBYONE OF (E) PUT IN COLUMN (C))
4005  FORMAT (1X,31HBIZERO OF (E) PUT IN COLUMN (C))
4006  FORMAT (1X,30HBIONE OF (E) PUT IN COLUMN (C))
4007  FORMAT (1X,31HBKZERO OF (E) PUT IN COLUMN (C))
4008  FORMAT (1X,30HBKONE OF (E) PUT IN COLUMN (C))
4009  FORMAT (1X,32HEXIZERO OF (E) PUT IN COLUMN (C))
4010  FORMAT (1X,31HEXIONE OF (E) PUT IN COLUMN (C))
4011  FORMAT (1X,32HEXKZERO OF (E) PUT IN COLUMN (C))
4012  FORMAT (1X,31HEXKONE OF (E) PUT IN COLUMN (C))
4013  FORMAT (1X,69HKBIZERO OF (E) PUT REAL PART IN COL (C) AND IMAGINAR
     1Y PART IN COL (C))
4014  FORMAT (1X,66HKBIONE OF (E) PUT REAL PART OF RESULT IN (C) IMAGINA
     1RY PART IN (C))
4015  FORMAT (1X,69HKBKZERO OF (E) PUT REAL PART IN COL (C) AND IMAGINAR
     1Y PART IN COL (C))
4016  FORMAT (1X,68HKBKONE OF (E) PUT REAL PART IN COL (C) AND IMAGINARY
     1 PART IN COL (C))
4017  FORMAT (1X,70HKEXIZERO OF (E) PUT REAL PART IN COL (C) AND IMAGINA
     1RY PART IN COL (C))
4018  FORMAT (1X,69HKEXIONE OF (E) PUT REAL PART IN COL (C) AND IMAGINAR
     1Y PART IN COL (C))
4019  FORMAT (1X,70HKEXKZERO OF (E) PUT REAL PART IN COL (C) AND IMAGINA
     1RY PART IN COL (C))
4020  FORMAT (1X,69HKEXKONE OF (E) PUT REAL PART IN COL (C) AND IMAGINAR
     1Y PART IN COL (C))
4021  FORMAT (1X,67HCIZERO OF R EQUAL TO (E), A EQUAL TO (E), PUT X IN (
     1C) AND Y IN (C))
4022  FORMAT (1X,66HCIONE OF R EQUAL TO (E), A EQUAL TO (E), PUT X IN (C
     1) AND Y IN (C))
4023  FORMAT (1X,66HCKZERO OF R EQUAL TO (E) A EQUAL TO (E), PUT X IN (C
     1) AND Y IN (C))
4024  FORMAT (1X,66HCKONE OF R EQUAL TO (E), A EQUAL TO (E), PUT X IN (C
     1) AND Y IN (C))
4025  FORMAT (1X,68HCEIZERO OF R EQUAL TO (E), A EQUAL TO (E), PUT X IN 
     1(C) AND Y IN (C))
4026  FORMAT (1X,67HCEIONE OF R EQUAL TO (E), A EQUAL TO (E), PUT X IN (
     1C) AND Y IN (C))
4027  FORMAT (1X,68HCEKZERO OF R EQUAL TO (E), A EQUAL TO (E), PUT X IN 
     1(C) AND Y IN (C))
4028  FORMAT (1X,67HCEKONE OF R EQUAL TO (E), A EQUAL TO (E), PUT X IN (
     1C) AND Y IN (C))
4029  FORMAT (1X,30HINTJO OF (E) PUT IN COLUMN (C))
4030  FORMAT (1X,41HELLIPTICAL FIRST OF (E) PUT IN COLUMN (C))
4031  FORMAT (1X,42HELLIPTICAL SECOND OF (E) PUT IN COLUMN (C))
4032  FORMAT (1X,71HBESJN OF (K) PUT IN COL (C) $ IF NRMAX EXCEEDS 99 ON
     1LY 1ST 100 COMPUTED)
4033  FORMAT (1X,69HZEROS BJZERO PUT IN (C) AND (C) $ IF NRMAX GT 1000 O
     1NLY 1000 COMPUTED)
4034  FORMAT (1X,68HZEROS BJONE PUT IN (C) AND (C) $ IF NRMAX GT 1000 ON
     1LY 1000 COMPUTED)
4035  FORMAT (1X,36HSTRUVE ZERO OF (E) PUT IN COLUMN (C))
4036  FORMAT (1X,35HSTRUVE ONE OF (E) PUT IN COLUMN (C))
4037  FORMAT (1X,71HHARMONIC ANALYSIS OF COL (C) FOR (N) ORDINATES, PUT 
     1COEFFICIENTS IN (C))
4038  FORMAT (1X,71HBESIN OF (K) PUT IN COL (C) $ IF NRMAX EXCEEDS 99 ON
     1LY 1ST 100 COMPUTED)
4039  FORMAT (1X,71HBESKN OF (K) PUT IN COL (C) $ IF NRMAX EXCEEDS 99 ON
     1LY 1ST 100 COMPUTED)
4101  FORMAT (1X,63HCTOF FOR CENTIGRADE (E) PUT FAHRENHEIT EQUIVALENT IN
     1 COLUMN (C))
4102  FORMAT (1X,62HFTOC FAHRENHEIT IS (E) PUT CENTIGRADE EQUIVALENT IN 
     1COLUMN (C))
4103  FORMAT (1X,35HATOMIC MASS TABLE PUT IN COLUMN (C))
4104  FORMAT (1X,69HMOLWT OF COMPOUNDS Z=(K), N=(K); Z=(K), N=(K); ... E
     1TC PUT IN COL (C))
4105  FORMAT (1X,71HEINSTEIN OF TEMPS (E), WAVE NUMBERS (E), PUT TABLE I
     1N (C) AND SUCC COLS/
     3        1X,71HEINSTEIN TEMPS (E), WAVE NOS (E), R=(K), PUT TABLE I
     3N (C) AND SUCC COLS)
4106  FORMAT (1X,70HPFTRANSLATIONAL TEMP IS (E) MOL WT (E) PUT TABLE IN 
     1(C) AND SUCC. COLS)
4107  FORMAT (1X,69HPFATOMIC TEMP (E) MOL WT (E) WAVE NOS (C) DEGENS (C)
     1, TABLE IN (C) ON)
4108  FORMAT (1X,71HPARTFUNCTION TEMP (E) WAVE NOS IN (C) DEGENS IN (C) 
     1PUT TABLE IN (C) ON)
4109  FORMAT (1X,71HBOLDISTRIBUTION FOR TEMP (E), WAVE NOS (C), DEGENS (
     1C), TABLE IN (C) ON)
4201  FORMAT (1X,71HCADD REAL (E) IMAG (E) TO REAL (E) IMAG (E) PUT REAL
     1 IN (C) IMAG IN (C))
4202  FORMAT (1X,70HCSUBTRACT REAL (E) IMAG (E) FROM REAL (E) IMAG (E), 
     1PUT IN (C) AND (C))
4203  FORMAT (1X,71HCMULTIPLY REAL (E) IMAG (E) X REAL (E) IMAG (E) REAL
     1 IN (C) IMAG IN (C))
4204  FORMAT (1X,71HCDIVIDE REAL (E) IMAG (E) BY REAL (E) IMAG (E), REAL
     1 IN (C) IMAG IN (C))
4205  FORMAT (1X,70HCRECTANGULAR FOR RHO = (E), THETA = (E) PUT X IN COL
     1 (C), Y IN COL (C))
4206  FORMAT (1X,64HCPOLAR FOR X = (E), Y = (E) PUT RHO IN COL (C), THET
     1A IN COL (C))
4208  FORMAT (1X,57HCALCOMP SPEED (N)  $ N=0 FOR SLOW MODE, N=1 FOR FAST
     1 MODE)
4209  FORMAT (1X,32HCALCOMP SIZE (N) HEIGHT OF PAPER/
     2        1X,45HCALCOMP SIZE (N) HEIGHT OF VERTICAL OR Y-AXIS/
     2        1X,60HCALCOMP SIZE (N) HEIGHT OF PAPER (K) HEIGHT OF VERTI
     2CAL AXIS/
     2        1X,71HCALCOMP SIZE (K) HEIGHT OF VERTICAL AXIS, (K) LENGTH
     2 OF HORIZONTAL AXIS/
     3        1X,70HCALCOMP SIZE (N) HEIGHT OF PAPER, (K) VERTICAL HEIGH
     3T, (K) HORIZ WIDTH)
4210  FORMAT (1X,70HCALCOMP PLOT (N) CURVES (E) OPTIONS COLUMNS (C),(C),
     1...,(C) VERSUS (C)/
     2        1X,71HCALCOMP PLOT (N),(E) VERTICAL SCALES (K) TO (K), (C)
     2,(C),...,(C) VS (C)/
     2        1X,67HCALCOMP PLOT (N),(E),(C),...,(C) VS (C) HOR. SCALES 
     2FROM (K) TO (K)/
     2        1X,68HCALCOMP PLOT (N),(E), VERTICAL (K),(K),(C) VS (C) HO
     2RIZONTAL (K),(K)/
     2        1X,63HCALCOMP PLOT (N) CURVES, (E) OPTIONS (C) VS (C), ...
     2,(C) VS (C)/
     2        1X,71HCALCOMP PLOT (N),(E), VERTICAL SCALE (K),(K), (C) VS
     2 (C),...,(C) VS (C)/
     2        1X,70HCALCOMP PLOT (N),(E), (C) VS (C)...(C) VS (C) HORIZO
     2NTAL SCALE (K),(K)/
     3        1X,70HCALCOMP PLOT (N),(E) VERT (K),(K), (C) VS (C)...(C) 
     3VS (C) HOR (K),(K))
4211  FORMAT (1X,17HCALCOMP SLOW MODE)
4212  FORMAT (1X,24HCALCOMP FAST OR ZIP MODE)
4213  FORMAT (1X,33HCALCOMP PAPER (N) HEIGHT OF PAPER)
4214  FORMAT (1X,54HCALCOMP AXIS (K) HEIGHT OF Y-AXIS, (K) WIDTH OF X-AX
     1IS)
4215  FORMAT (1X,23HCALCOMP TAPE ''L'' UNIT)
4216  FORMAT (1X,70HTEKTRONIX PLOT (N) CURVES (E) OPTIONS COLUMNS (C),(C
     1)...(C) VERSUS (C)/
     2        1X,70HTEKTRONIX PLOT (N),(E) VERTICAL SCALES (K) TO (K),(C
     2),(C)...(C) VS (C)/
     2        1X,69HTEKTRONIX PLOT (N),(E),(C),...,(C) VS (C) HOR. SCALE
     2S FROM (K) TO (K)/
     2        1X,70HTEKTRONIX PLOT (N),(E), VERTICAL (K),(K),(C) VS (C) 
     2HORIZONTAL (K),(K)/
     2        1X,64HTEKTRONIX PLOT (N) CURVES, (E) OPTIONS (C) VS (C),..
     2.,(C) VS (C)/
     2        1X,70HTEKTRONIX PLOT (N),(E), VERTICAL SCALE (K),(K),(C) V
     2S (C)...(C) VS (C)/
     2        1X,71HTEKTRONIX PLOT (N),(E),(C) VS (C)...(C) VS (C) HORIZ
     2ONTAL SCALE (K),(K)/
     3        1X,71HTEKTRONIX PLOT (N),(E) VERT (K),(K),(C) VS (C)...(C)
     3 VS (C) HOR (K),(K))
4217  FORMAT (1X,56HTEKTRONIX AXIS (K) HEIGHT OF Y-AXIS, (K) WIDTH OF X-
     1AXIS)
4218  FORMAT (1X,52HTEKTRONIX TERMINAL (N) TERMINAL MODEL, (N) BAUD RATE
     1 )
4301  FORMAT (1X,23HSTEM LEAF OF COLUMN (C)/
     2        1X,52HSTEM LEAF OF COLUMN (C) AND PUT SCRAWL IN COLUMN (C)
     2/
     2        1X,63HSTEM LEAF OF COL (C), SCRAWL IN (C) AND PUT DEPTH IN
     2 COLUMN (C)/
     2        1X,66HSTEM LEAF OF COLUMN (C) FOR PARAMETER VALUES (I), (J
     2), (K) AND (L)/
     2        1X,68HSTEM LEAF OF COL (C) FOR (I), (J), (K) AND (L) PUT S
     2CRAWL IN COL (C)/
     3        1X,70HSTEM LEAF OF COL (C) FOR (I), (J), (K) (L) SCRAWL IN
     3 (C), DEPTH IN (C))
4302  FORMAT (1X,49HSSTEM LEAF OF COLUMN (C) PUT SCRAWL IN COLUMN (C)/
     2        1X,64HSSTEM LEAF OF COL (C) PUT SCRAWL IN COL (C) AND DEPT
     2H IN COL (C)/
     2        1X,67HSSTEM LEAF COL (C) FOR (I), (J), (K) AND (L), PUT SC
     2RAWL IN COL (C)/
     3        1X,71HSSTEM LEAF OF (C) FOR (I), (J), (K) AND (L), SCRAWL 
     3IN (C) DEPTH IN (C))
4401  FORMAT (1X, 7HENGLISH)
4402  FORMAT (1X, 8HFRANCAIS)
4403  FORMAT (1X, 7HDEUTSCH)
4404  FORMAT (1X, 7HESPANOL)
4405  FORMAT (1X, 8HITALIANO)
4406  FORMAT (1X, 5HNORSK)
4407  FORMAT (1X, 5HDANSK)
4408  FORMAT (1X, 8HJAPANESE)
4409  FORMAT (1X, 8HYUGOSLAV)
4410  FORMAT (1X, 9HPORTUGESE)
4411  FORMAT (1X,10HNEDERLANDS)
4412  FORMAT (1X, 7HSVENSKA)
4413  FORMAT (1X, 7HSLOVENE)
4414  FORMAT (1X,10HVOCABULARY)
C
C     ==================================================================
C
      END
*DESC9
      SUBROUTINE DESC9
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  DESC9 V 7.00 12/ 7/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE DESCRIBE INSTRUCTION FOR ...
C        L1 = 35 TO 49.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - NOVEMBER, 1978.
C                   CURRENT VERSION - DECEMBER, 1989.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW
C
C     ==================================================================
C
      IF (L1.LT.35 .OR. L1.GT.49) GO TO 50
C
      L = L1 - 34
      GO TO (  1, 50, 50, 50, 50, 50, 50, 50, 50, 50,
     1       201,203,205,207,208), L
C
   1  GO TO (101,102,103,104,105,106,107,108,109,110,
     1       111,112,113,114,115,116,117,118,119,120,
     2       121,122,123,124,125,126,127,128), L2
C
C     ..................................................................
C
  50  CALL ERROR (1)
      RETURN
C
C     ..................................................................
C
C     L1 = 35.
C
 101  WRITE (IPRINT,4501)
      RETURN
C
 102  WRITE (IPRINT,4502)
      RETURN
C
 103  WRITE (IPRINT,4503)
      RETURN
C
 104  WRITE (IPRINT,4504)
      RETURN
C
 105  WRITE (IPRINT,4505)
      RETURN
C
 106  WRITE (IPRINT,4506)
      RETURN
C
 107  WRITE (IPRINT,4507)
      RETURN
C
 108  WRITE (IPRINT,4508)
      RETURN
C
 109  WRITE (IPRINT,4509)
      RETURN
C
 110  WRITE (IPRINT,4510)
      RETURN
C
 111  WRITE (IPRINT,4511)
      RETURN
C
 112  WRITE (IPRINT,4512)
      RETURN
C
 113  WRITE (IPRINT,4513)
      RETURN
C
 114  WRITE (IPRINT,4514)
      RETURN
C
 115  WRITE (IPRINT,4515)
      RETURN
C
 116  WRITE (IPRINT,4516)
      RETURN
C
 117  WRITE (IPRINT,4517)
      RETURN
C
 118  WRITE (IPRINT,4518)
      RETURN
C
 119  WRITE (IPRINT,4519)
      RETURN
C
 120  WRITE (IPRINT,4520)
      RETURN
C
 121  WRITE (IPRINT,4521)
      RETURN
C
 122  WRITE (IPRINT,4522)
      RETURN
C
 123  WRITE (IPRINT,4523)
      RETURN
C
 124  WRITE (IPRINT,4524)
      RETURN
C
 125  WRITE (IPRINT,4525)
      RETURN
C
 126  WRITE (IPRINT,4526)
      RETURN
C
 127  WRITE (IPRINT,4527)
      RETURN
C
 128  WRITE (IPRINT,4528)
      RETURN
C
C     ..................................................................
C
C     L1 = 45.
C
 201  WRITE (IPRINT,5501)
      RETURN
C
C     ..................................................................
C
C     L1 = 46.
C
 203  WRITE (IPRINT,5601)
      RETURN
C
C     ..................................................................
C
C     L1 = 47.
C
 205  WRITE (IPRINT,5701)
      RETURN
C
C     ..................................................................
C
C     L1 = 48.
C
 207  WRITE (IPRINT,5801)
      RETURN
C     L1 = 49.
C
 208  WRITE (IPRINT,5901)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
4501  FORMAT (1X,62HTABLE FREQUENCY (N) WAY WITH LEVELS IN COLS (C), (C)
     1, ..., (C)/
     3        1X,71HTABLE FREQUENCY (N) WAY, LEVELS (C), (C), ..., (C) S
     3TART STORING IN (C))
4502  FORMAT (1X,67HTABLE SUM (N) WAY, LEVELS IN COLS (C), ..., (C) FOR 
     1DATA IN COL (C)/
     3        1X,70HTABLE SUM (N) WAY, LEVELS (C),...,(C) DATA IN (C) ST
     3ART STORING IN (C))
4503  FORMAT (1X,69HTABLE AVERAGE (N) WAY, LEVELS IN COLS (C), (C), ...,
     1 (C), DATA IN (C)/
     3        1X,71HTABLE AVERAGE (N) WAY, LEVELS (C), ..., (C) DATA (C)
     3, START STORING (C))
4504  FORMAT (1X,68HTABLE STDDEV (N) WAY, LEVELS IN COLS (C),...,(C) FOR
     1 DATA IN COL (C)/
     3        1X,71HTABLE STDDEV (N) WAY, LEVELS (C),...,(C), DATA (C) S
     3TART STORING IN (C))
4505  FORMAT (1X,69HTABLE MINIMUM (N) WAY, LEVELS IN COLS (C), (C), ...,
     1 (C), DATA IN (C)/
     3        1X,71HTABLE MINIMUM (N) WAY, LEVELS (C), ..., (C) DATA (C)
     3, START STORING (C))
4506  FORMAT (1X,69HTABLE MAXIMUM (N) WAY, LEVELS IN COLS (C), (C), ...,
     1 (C), DATA IN (C)/
     3        1X,71HTABLE MAXIMUM (N) WAY, LEVELS (C), ..., (C) DATA (C)
     3, START STORING (C))
4507  FORMAT (1X,69HTABLE RANGE (N) WAY, LEVELS IN COLS (C), ..., (C) FO
     1R DATA IN COL (C)/
     3        1X,71HTABLE RANGE (N) WAY, LEVELS (C),...,(C), DATA (C), S
     3TART STORING IN (C))
4508  FORMAT (1X,71HTABLE MEDIAN (N) WAY, LEVELS IN COLS (C), (C), ..., 
     1(C) FOR DATA IN (C)/
     3        1X,70HTABLE MEDIAN (N) WAY, LEVELS (C),...,(C) DATA (C) ST
     3ART STORING IN (C))
4509  FORMAT (1X,63HTABLE PERCENTAGE (N) WAY WITH LEVELS IN COLS (C), (C
     1), ..., (C)/
     3        1X,69HTABLE PERCENTAGE (N) WAY, LEVELS IN (C),...,(C), STA
     3RT STORING IN (C))
4510  FORMAT (1X,63HTABLE PROPORTION (N) WAY WITH LEVELS IN COLS (C), (C
     1), ..., (C)/
     3        1X,69HTABLE PROPORTION (N) WAY, LEVELS IN (C),...,(C), STA
     3RT STORING IN (C))
4511  FORMAT (1X,55HTABLE RPERCENTAGE (N) WAY, LEVELS IN (C), (C), ..., 
     1(C)/
     3        1X,70HTABLE RPERCENTAGE (N) WAY, LEVELS IN (C),...,(C), ST
     3ART STORING IN (C))
4512  FORMAT (1X,55HTABLE CPERCENTAGE (N) WAY, LEVELS IN (C), (C), ..., 
     1(C)/
     3        1X,70HTABLE CPERCENTAGE (N) WAY, LEVELS IN (C),...,(C), ST
     3ART STORING IN (C))
4513  FORMAT (1X,55HTABLE RPROPORTION (N) WAY, LEVELS IN (C), (C), ..., 
     1(C)/
     3        1X,70HTABLE RPROPORTION (N) WAY, LEVELS IN (C),...,(C), ST
     3ART STORING IN (C))
4514  FORMAT (1X,55HTABLE CPROPORTION (N) WAY, LEVELS IN (C), (C), ..., 
     1(C)/
     3        1X,70HTABLE CPROPORTION (N) WAY, LEVELS IN (C),...,(C), ST
     3ART STORING IN (C))
4515  FORMAT (1X,63HNTABLE FREQUENCY (N) WAY WITH LEVELS IN COLS (C), (C
     1), ..., (C)/
     3        1X,71HNTABLE FREQUENCY (N) WAY, LEVELS IN (C), (C)...(C) S
     3TART STORING IN (C))
4516  FORMAT (1X,68HNTABLE SUM (N) WAY, LEVELS IN COLS (C), ..., (C) FOR
     1 DATA IN COL (C)/
     3        1X,71HNTABLE SUM (N) WAY, LEVELS (C),...,(C) DATA IN (C) S
     3TART STORING IN (C))
4517  FORMAT (1X,69HNTABLE AVERAGE (N) WAY, LEVELS IN COLS (C), (C)...(C
     1) FOR DATA IN (C)/
     3        1X,71HNTABLE AVERAGE (N) WAY, LEVELS (C)...(C) DATA (C), S
     3TART STORING IN (C))
4518  FORMAT (1X,70HNTABLE STDEV (N) WAY, LEVELS IN COLS (C), ..., (C) F
     1OR DATA IN COL (C)/
     3        1X,70HNTABLE, STDDEV (N) WAY, LEVELS (C)...(C) DATA (C) ST
     3ART STORING IN (C))
4519  FORMAT (1X,70HNTABLE MINIMUM (N) WAY, LEVELS IN (C), (C) ... (C) F
     1OR DATA IN COL (C)/
     3        1X,70HNTABLE MINIMUM (N) WAY, LEVELS (C)...(C) DATA (C) ST
     3ART STORING IN (C))
4520  FORMAT (1X,71HNTABLE MAXIMUM (N) WAY, LEVELS IN COLS (C),..., (C) 
     1FOR DATA IN COL (C)/
     3        1X,71HNTABLE MAXIMUM (N) WAY, LEVELS (C)...(C) DATA (C), S
     3TART STORING IN (C))
4521  FORMAT (1X,68HNTABLE RANGE (N) WAY, LEVELS IN COLS (C),...,(C) FOR
     1 DATA IN COL (C)/
     3        1X,70HNTABLE RANGE (N) WAY, LEVELS (C),...,(C)DATA (C), ST
     3ART STORING IN (C))
4522  FORMAT (1X,71HNTABLE MEDIAN (N) WAY, LEVELS IN COLS (C), ..., (C) 
     1FOR DATA IN COL (C)/
     3        1X,69HNTABLE MEDIAN (N) WAY, LEVELS (C)...(C) DATA (C) STA
     3RT STORING IN (C))
4523  FORMAT (1X,64HNTABLE PERCENTAGE (N) WAY WITH LEVELS IN COLS (C), (
     1C), ..., (C)/
     3        1X,71HNTABLE PERCENTAGE (N) WAY, LEVELS IN (C), ..., (C) S
     3TART STORING IN (C))
4524  FORMAT (1X,64HNTABLE PROPORTION (N) WAY WITH LEVELS IN COLS (C), (
     1C), ..., (C)/
     3        1X,71HNTABLE PROPORTION (N) WAY, LEVELS IN (C), ..., (C) S
     3TART STORING IN (C))
4525  FORMAT (1X,56HNTABLE RPERCENTAGE (N) WAY, LEVELS IN (C), (C), ...,
     1 (C)/
     3        1X,71HNTABLE RPERCENTAGE (N) WAY, LEVELS IN (C),...,(C), S
     3TART STORING IN (C))
4526  FORMAT (1X,56HNTABLE CPERCENTAGE (N) WAY, LEVELS IN (C), (C), ...,
     1 (C)/
     3        1X,71HNTABLE CPERCENTAGE (N) WAY, LEVELS IN (C),...,(C), S
     3TART STORING IN (C))
4527  FORMAT (1X,56HNTABLE RPROPORTION (N) WAY, LEVELS IN (C), (C), ...,
     1 (C)/
     3        1X,71HNTABLE RPROPORTION (N) WAY, LEVELS IN (C),...,(C), S
     3TART STORING IN (C))
4528  FORMAT (1X,56HNTABLE CPROPORTION (N) WAY, LEVELS IN (C), (C), ...,
     1 (C)/
     3        1X,71HNTABLE CPROPORTION (N) WAY, LEVELS IN (C),...,(C), S
     3TART STORING IN (C))
5501  FORMAT (1X,45HREAD TAPE ''L'' INTO COLUMNS (C), (C) ... (C)/
     3        1X,45HREAD UNIT ''L'' INTO COLUMNS (C), (C) ... (C)/
     1        1X,59HREAD TAPE ''L'' ''L'' FORMAT INTO COLUMNS (C), (C), 
     1... (C)/
     3        1X,59HREAD UNIT ''L'' ''L'' FORMAT INTO COLUMNS (C), (C), 
     3... (C))
5601  FORMAT (1X,64HCREAD TAPE ''L'' USING (N) RECORDS INTO COLUMNS (C),
     1 (C) ... (C)/
     3        1X,70HCREAD UNIT ''L'' USING (R) LOGICAL RECORDS INTO COLS
     3 (C), (C), ... (C)/
     3        1X,68HCREAD TAPE ''L'' ''L'' FORMAT USING (N) RECORDS, INT
     1O COLS (C)...(C)/
     3        1X,69HCREAD UNIT ''L'' ''L'' FORMAT USING (N) BLOCKS, INTO
     3 COLS (C) ... (C))
5701  FORMAT (1X,46HWRITE TAPE ''L'' FROM COLUMNS (C), (C) ... (C)/
     3        1X,46HWRITE UNIT ''L'' FROM COLUMNS (C), (C) ... (C)/
     3        1X,59HWRITE TAPE ''L'' ''L'' FORMAT FROM COLUMNS (C), (C) 
     1... (C)/
     3        1X,59HWRITE UNIT ''L'' ''L'' FORMAT FROM COLUMNS (C), (C) 
     3... (C))
5801  FORMAT (1X,30HSET TAPE ''L'' INTO COLUMN (C)/
     2        1X,50HSET TAPE ''L'' STARTING WITH ROW (R) OF COLUMN (C)/
     2        1X,30HSET UNIT ''L'' INTO COLUMN (C)/
     3        1X,50HSET UNIT ''L'' STARTING WITH ROW (R) OF COLUMN (C))
5901  FORMAT (1X,49HCSET TAPE ''L'' USING (N) RECORDS INTO COLUMN (C)/
     2        1X,60HCSET TAPE ''L'' USING (N) RECORDS INTO ROW (R) OF CO
     2LUMN (C)/
     2        1X,57HCSET UNIT ''L'' USING (R) LOGICAL RECORDS INTO COLUM
     2N (C)/
     3        1X,68HCSET UNIT ''L'' USING (R) LOGICAL RECORDS INTO ROW (
     3R) OF COLUMN (C))
C
C     ==================================================================
C
      END
*DETRNK
      SUBROUTINE DETRNK (A,NROW,N,DET,RANK)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. DETRNK V 7.00  2/20/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EVALUATE THE DETERMINANT AND RANK OF MATRIX A
C
C     INPUT ...
C
C     A, MATIX IN QUESTION
C     NROW IS THE DIMENSION OF A IN DIMENSION STATEMENT
C     N IS THE SIZE OF A
C
C     OUTPUT ...
C
C     DET, THE VALUE OF DETERMINANT OF A
C     RANK, RANK OF A
C
C     THE ORIGINAL VALUES OF A ARE DESTROYED.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - FEBRUARY, 1968.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
      COMMON /RMCONS/ RALOG, RER, REXP, RMIFY, RMXINT, RPIFY, RSD, RTRG
C
      REAL             A(NROW,NROW)
      REAL             DET, RANK
      REAL             PR, T, TOLNCE, VAL, X
C
C     ==================================================================
C
      NK   = N
      NN   = N - IONE
      VAL  = RONE
      TOLNCE = RTEN * RER
      DO 70 I=1,NN
        II = I + IONE
        IB = I
        DO 10 J=II,N
          IF (ABS(A(IB,I)).GE.ABS(A(J,I))) GO TO 10
          IB = J
  10    CONTINUE
        IF (ABS(A(IB,I)).GE.1.E-7) GO TO 20
C
        GO TO 40
  20    IF (IB.EQ.I) GO TO 40
        DO 30 J=I,N
          T = A(I,J)
          A(I,J) = A(IB,J)
          A(IB,J) = T
  30    CONTINUE
        VAL = -VAL
  40    DO 60 J=II,N
          X = FDIV (A(J,I),A(I,I),IND)
          DO 50 K=I,N
            A(J,K) = A(J,K) - X * A(I,K)
  50      CONTINUE
  60    CONTINUE
  70  CONTINUE
C
      DO 80 I=1,N
        IF (ABS(A(I,I)).GE.TOLNCE)  GO TO 80
        NK = NK - IONE
  80  CONTINUE
C
      RANK = NK
      IF (NK.NE.N) GO TO 100
      PR = RONE
      DO 90 I=1,N
        PR = PR * A(I,I)
  90  CONTINUE
C
      DET = VAL * PR
      RETURN
C
C     ..................................................................
C
 100  DET = RZERO
      RETURN
C
C     ==================================================================
C
      END
*DISPRO
      SUBROUTINE DISPRO
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. DISPRO V 7.00 2/20/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS PROGRAM UNIT DETERMINES WHICH PROBABILITY DISTRIBUTION
C        PROPERTY IS TO BE CALCULATED.
C
C          FOR PROPERTIES
C             1 AND  2     L1 = 55
C             3 AND  4     L1 = 56
C             5 AND  6     L1 = 57
C             7 AND  8     L1 = 58
C
C     L2 =  1 THRU 31 IF PROPERTY IS ODD   (I.E. 1, 3, 5, 7,  9)
C     L2 = 32 THRU 62 IF PROPERTY IS EVEN  (I.E. 2, 4, 6, 8, 10)
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -   AUGUST, 1973.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA ICA / 31 /
      DATA ICB / 55 /
C
C     ==================================================================
C
      IST = IZERO
      IF (L2.LE.ICA) GO TO 10
      L2  = L2 - ICA
      IST = IONE
  10  L1  = ITWO * (L1-ICB) + IST + IONE
C
      IF (L1.LE.IZERO .OR. L1.GE.9) GO TO 300
C
      GO TO (110,120,130,140,150,160,170,180), L1
C
C     (1)   DENSITY.
C
 110  CALL PRBPDF
      RETURN
C
C     (2)   CUMULATIVE.
C
 120  CALL PRBCDF
      RETURN
C
C     (3)   PERCENTILE.
C
 130  CALL PRBPPF
      RETURN
C
C     (4)   RANDOM.
C
 140  CALL PRBRAN
      RETURN
C
C     (5)   PLOT.
C
 150  CALL PRBPLT
      RETURN
C
C     (6)   ANALYSIS.
C
 160  CONTINUE
      GO TO 300
C
C     (7)   CONFIDENCE.
C
 170  CONTINUE
      GO TO 300
C
C     (8)   TOLERANCE.
C
 180  CONTINUE
      GO TO 300
C
C     ..................................................................
C
 300  CALL ERROR (1)
      RETURN
C
C     ==================================================================
C
      END
*DIXAB
      SUBROUTINE DIXAB (AA,BB,X,BETAX,BETAAB,PDF,CDF,IERR)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  DIXAB V 7.00 4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EVALUATE INCOMPLETE BETA FUNCTION CDF FOR PARAMETERS A AND B.
C
C     ALSO COMPUTED ARE ...
C
C        BETAX  = INTEGRAND X COMPLETE BETA FUNCTION.
C        BETAAB = COMPLETE BETA FUNCTION.
C        PDF    = INTEGRAND, OR PROBABILITY DENSITY FUNCTION.
C
C     IERR = FAULT INDICATOR.
C          = 0, IF NORMAL RETURN.
C          = 1, IF A OR B NOT INTEGER OR HALF-INTEGER.
C          = 2, IF A OR B EXCEED LARGEST FLOATING (HALF) INTEGER.
C          = 3, IF X IS LESS THAN ZERO.
C          = 4, IF X IS GREATER THAN ONE.
C          = 5, IF RESULT QUESTIONABLE (POOR CONVERGENCE).
C          = 6, IF A OR B NON-POSITIVE.
C
C     INPUT, ALL REAL, ...
C        A = FIRST PARAMETER.
C        B = SECOND PARAMETER.
C        X = VALUE OF VARIATE.
C
C     OUTPUT, ALL DOUBLE PRECISION (EXCEPT IERR), ...
C        BETAX   = INCOMPLETE BETAT DIVIDED BY COMPLETE BETA.
C        BETAAB = COMPLETE BETA FUNCTION.
C        PDF    = BETA PROBABILITY DENSITY FUNCTION.
C        CDF    = INCOMPLETE BETA FUNCTION (C.D.F.).
C        IERR   = FAULT INDICTATOR.
C
C     METHOD USED IS CONTINUED FRACTION IN FORWARD DIRECTION.
C        COMPUTE I(X,A,B),                  IF X LT A / (A+B).
C        COMPUTE I(X,A,B) = 1 - I(1-X,B,A), IF X GT A / (A+B).
C               WRITTEN BY IRENE STEGUN.
C
C               ADAPTED TO OMNITAB BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY, 
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - NOVEMBER, 1978.
C                   CURRENT VERSION -    APRIL, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /DMCONS/ DMAXDP, DMXINT, DSNCOS, DXEXP
      COMMON /DPICON/ DHLFPI, DPI, DSQRPI, D2BYSP
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /IMCONS/ IEXP, MMXINT, MXINT, NBC, NBM, NSBB
      COMMON /PRTCTS/ IPLACE, NCHTIT(4), NLENGT, NLSWT, NSIGD, NCWIDE
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             AA, BB, X
      REAL             A, B, RLI
      REAL             FDIV
C
      DOUBLE PRECISION BETAX, BETAAB, PDF, CDF
      DOUBLE PRECISION TDV(2), FNV(2), FDV(2)
      DOUBLE PRECISION AN, BLN, BN, BXLN, CA, CB, CX, DEL, DX
      DOUBLE PRECISION FC, FD, FIX, FIXALN, FIXLN, FN, FNM1, FNM2
      DOUBLE PRECISION GN, GNM1, GNM2, HA, HB, PREV, PT4, PT9, PWRLN
      DOUBLE PRECISION RE, REP, REPM, RIA, RIB
      DOUBLE PRECISION SCF, SCFT, SGF, SGN, SMIN, SUMA, TA, TB, TC, TD
      DOUBLE PRECISION TEMP, TEMPB, TEMPC, TN, TOLER
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DHLFPI, DPI, DSQRPI, D2BYSP
      DOUBLE PRECISION DMAXDP, DMXINT, DSNCOS, DXEXP
      DOUBLE PRECISION FDDIV, FDEXP, FDLOG
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA PT4   / 4.0D-1     /
      DATA PT9   / 9.0D-1     /
C
C     ==================================================================
C
      A      = AA
      B      = BB
      TOLER  = FDDIV (DTWO,DMXINT*DBLE (FLOAT (MXINT)),JIND)
      IERR   = IZERO
      ISP    = IZERO
      RLI    = RTEN ** (NSIGD - IONE)
      IP     = FDDIV (FDLOG(DMAXDP),FDLOG(DTWO),JIND)
      IPS    = IP - NSIGD
      IC     = IZERO
      SCF    = DTWO ** IPS
      SMIN   = FDDIV (DONE,SCF,JIND)
      TEMP   = B
      DX     = X
      SUMA   = DZERO
      SGN    = DZERO
      I      = IONE
      IF (A.LE.RZERO .OR. B.LE.RZERO) GO TO 10
      IF (A.GT.RLI .OR. B.GT.RLI) IERR = ITWO
      IF (IERR.EQ.ITWO) GO TO 15
      A = FDIV (AINT (RTWO*A),RTWO,JIND)
      IF (A.NE.AA) IERR = IONE
      B = FDIV (AINT (RTWO*B),RTWO,JIND)
      IF (B.NE.BB) IERR = IONE
      GO TO 20
  10  IERR   = 6
  15  CDF    = DZERO
      BETAAB = DZERO
      BETAX  = DZERO
      PDF    = DZERO
      RETURN
C
C     ..................................................................
C
  20  IF (X) 330,30,40
  30  CDF    = DZERO
      BETAX  = DZERO
      GO TO 170
C
  40  IF (DX-DONE) 60,50,340
  50  CDF    = DONE
      GO TO 170
C
  60  IF (X.LT.FDIV(A,A+B,JIND)) GO TO 70
      CX     = DONE - DX
      CA     = B
      CB     = A
      TEMPB  = DX
      SUMA   = DONE
      SGN    = - DONE
      GO TO 80
C
  70  CX     = DX
      CA     = A
      CB     = B
      TEMPB  = DONE - DX
      SUMA   = DZERO
      SGN    = DONE
  80  SGF    = - DONE
      TA     = CA
      TB     = CA + CB
      TC     = CA
      TD     = CA + DONE
      FNM2   = DZERO
      GNM2   = DONE
      FNM1   = DONE
      GNM1   = DONE
      PREV   = FDDIV (FNM1,GNM1,JIND)
      REPM   = DMAXDP
      REP    = DMAXDP
      BN     = DONE
  90  AN     = CX * SGF * FDDIV (TA,TC,JIND) * FDDIV (TB,TD,JIND)
      FN     = BN * FNM1 + AN * FNM2
      GN     = BN * GNM1 + AN * GNM2
      IF (GN) 100,160,100
 100  FC     = FDDIV (FN,GN,JIND)
      IF (PREV) 110,160,110
 110  RE     = DABS ( DONE-FDDIV(FC,PREV,JIND) )
      IF (RE.LT.TOLER) GO TO 180
      IF (RE.GT.REPM)  GO TO 120
      IC     = IC - IONE
      GO TO 130
C
 120  IC     = IC + IONE
 130  IF (IC.GT.IZERO) GO TO 160
      SCFT   = DTWO * DABS (AN)
      TEMP   = DMAX1 (DABS(FN),DABS(GN))
      IF (TEMP.LT.DONE) GO TO 140
      IF (SCFT.LT.FDDIV(DMAXDP,TEMP,JIND)) GO TO 150
      FN     = FDDIV (FN,SCF,JIND)
      GN     = FDDIV (GN,SCF,JIND)
      FNM1   = FDDIV (FNM1,SCF,JIND)
      GNM1   = FDDIV (GNM1,SCF,JIND)
      GO TO 150
C
 140  IF (SCFT.GT.FDDIV(SMIN,TEMP,JIND)) GO TO 150
      FN     = FN * SCF
      GN     = GN * SCF
      FNM1   = FNM1 * SCF
      GNM1   = GNM1 * SCF
 150  TA     = TD - TA
      TB     = TB + SGF * TD
      TC     = TC + DONE
      TD     = TD + DONE
      PREV   = FC
      SGF    = - SGF
      FNM2   = FNM1
      GNM2   = GNM1
      FNM1   = FN
      GNM1   = GN
      REPM   = REP
      REP    = RE
      GO TO 90
C
 160  IERR   = IFIVE
      FC     = - DONE
      GO TO 180
C
 170  CA     = A
      CB     = B
      PDF    = DZERO
      ISP    = IONE
 180  IA     = CA
      RIA    = IA
      HA     = CA - RIA
      IB     = CB
      RIB    = IB
      HB     = CB - RIB
      IPSM   = IZERO
      TEMP   = HA + HB
      TEMPC  = DONE
      IF (TEMP.LT.PT4) GO TO 190
      IF (TEMP.LT.PT9) GO TO 210
      GO TO 230
C
 190  INIP   = ITHRE
      DEL    = DONE
      FD     = CA + CB - DONE
      TN     = DONE
      IF (CB.GE.CA) GO TO 200
      FN     = CB - DONE
      TD     = CA
      GO TO 250
C
 200  FN     = CA - DONE
      TD     = CB
      GO TO 250
C
 210  INIP   = ITWO
      DEL    = DTWO
      FD     = ITWO * (IA+IB) - IONE
      TN     = DTWO
      IF (HA.NE.DZERO) GO TO 220
      FN     = DTWO * (CA-DONE)
      TD     = ITWO * IB + IONE
      GO TO 250
C
 220  FN     = DTWO * (CB-DONE)
      TD     = ITWO * IA + IONE
      GO TO 250
C
 230  INIP   = IONE
      DEL    = DTWO
      TEMPC  = TEMPC * DPI
      TDV(1) = DTWO
      TDV(2) = ITWO * IA + ITWO
      FNV(1) = ITWO * IA - IONE
      FNV(2) = ITWO * IB - IONE
      FDV(1) = ITWO * IA
      FDV(2) = ITWO * (IA+IB)
      I      = IONE
 240  TN     = DONE
      FN     = FNV(I)
      IF (FN.LT.DONE) GO TO 290
      TD     = TDV(I)
      FD     = FDV(I)
 250  TEMP   = FDDIV (TN,TD,JIND)
      IF (TEMP.GE.DONE) GO TO 260
      IF (TEMPC.GE.FDDIV(SMIN,TEMP,JIND)) GO TO 270
      TEMPC  = TEMPC * SCF
      IPSM   = IPSM - IPS
      GO TO 270
C
 260  IF (TEMPC.LE.FDDIV(DMAXDP,TEMP,JIND)) GO TO 270
      TEMPC  = FDDIV (TEMPC,SCF,JIND)
      IPSM   = IPSM + IPS
 270  TEMPC  = TEMPC * TEMP
      IF (TN.GE.FN) GO TO 280
      TN     = TN + DEL
      TD     = TD + DEL
      GO TO 250
C
 280  IF (INIP.GT.IONE) GO TO 300
 290  IF (I.GE.ITWO) GO TO 310
      I      = I + IONE
      GO TO 240
C
 300  IF (TD.GE.FD) GO TO 310
      TD     = FD
      FN     = DEL
      TN     = DEL
      GO TO 250
C
 310  TEMP   = IPSM
      BLN    = FDLOG (TEMPC) + TEMP * FDLOG (DTWO)
      BETAAB = FDEXP (BLN)
      IF (ISP.EQ.IONE) GO TO 320
      PWRLN  = CA * FDLOG (CX) + CB * FDLOG (TEMPB)
      TEMP   = PWRLN - BLN
      PDF    = FDEXP (TEMP)
      IF (FC.LE.DZERO) IERR = IFIVE
      FIXLN  = TEMP + FDLOG (FC) - FDLOG (CA)
      FIX    = FDEXP (FIXLN)
      CDF    = SUMA + SGN * FIX
      IF (CDF.EQ.DZERO) IERR = IFIVE
      FIXALN = FDLOG (CDF)
      BXLN   = FIXALN + BLN
      BETAX  = FDEXP (BXLN)
      RETURN
C
C     ..................................................................
C
 320  IF (X.NE.RONE) RETURN
      BETAX  = BETAAB
      RETURN
C
C     ..................................................................
C
 330  IERR   = ITHRE
      CDF    = DZERO
      GO TO 350
C
 340  IERR   = IFOUR
      CDF    = DONE
 350  PDF    = DZERO
      BETAX  = DZERO
      BETAAB = DZERO
      RETURN
C
C     ==================================================================
C
      END
*DSCRIB
      SUBROUTINE DSCRIB
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. DSCRIB V 7.00 12/ 7/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE DESCRIBE INSTRUCTION.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -  OCTOBER, 1978.
C                   CURRENT VERSION - DECEMBER, 1989.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ARRAYD/ ITB(14), ITP(9,2), NALPH(5), NL(25)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW         
      COMMON /VECDIM/ RSUM(172), VWXYZ(8), NAME(8)         
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA LBRK1   / 11 /
      DATA LBRK2   / 12 /
      DATA LBRK3   / 15 /
      DATA LBRK4   / 18 /
      DATA LBRK5   / 22 /
      DATA LBRK6   / 24 /
      DATA LBRK7   / 29 /
      DATA LBRK8   / 34 /
      DATA LBRK9   / 49 /
      DATA LBRK10  / 55 /
      DATA LBRK11  / 57 /
C
C     ==================================================================
C
      NAME(1) = NAME(3)
      NAME(2) = NAME(4)
      NAME(3) = NAME(5)
      NAME(4) = NAME(6)
      NAME(5) = NAME(7)
      NAME(6) = NAME(8)
      NAME(7) = IZERO
      NAME(8) = IZERO
C
      IF (NAME(1).EQ.NL(13) .AND. NAME(2).EQ.NL(14) .AND.
     1    NAME(3).EQ.NL( 7) .AND. NAME(4).EQ.NL( 8)) GO TO 200
C
      IF (NAME(1).EQ.NL( 1) .AND. NAME(2).EQ.NL( 2)) GO TO 201
      IF (NAME(1).EQ.NL( 5) .AND. NAME(2).EQ.NL( 6)) GO TO 202
      IF (NAME(1).EQ.NL( 9) .AND. NAME(2).EQ.NL(10)) GO TO 203
      IF (NAME(1).EQ.NL( 9) .AND. NAME(2).EQ.NL(17)) GO TO 207
      IF (NAME(1).EQ.NL( 9) .AND. NAME(2).EQ.NL(18)) GO TO 208
      IF (NAME(1).EQ.NL(11) .AND. NAME(2).EQ.NL(12)) GO TO 209
      IF (NAME(1).EQ.NL( 7) .AND. NAME(2).EQ.NL( 8)) GO TO 210
      IF (NAME(1).EQ.NL( 3) .AND. NAME(2).EQ.NL( 4)) GO TO 213
      IF (NAME(1).EQ.NL(15) .AND. NAME(2).EQ.NL(16)) GO TO 214
      IF (NAME(1).EQ.NL(20) .AND. NAME(2).EQ.NL(21)) GO TO 215
C
      CALL LOOKUP (LNAME)
C
C     ..................................................................
C
      IF (L1.GT.LBRK1) GO TO 20
        CALL DESC1
        RETURN
C
C     ..................................................................
C
  20  IF (L1.GT.LBRK2) GO TO 30
        CALL DESC2
        RETURN
C
C     ..................................................................
C
  30  IF (L1.GT.LBRK3) GO TO 40
        CALL DESC3
        RETURN
C
C     ..................................................................
C
  40  IF (L1.GT.LBRK4) GO TO 50
        CALL DESC4
        RETURN
C
C     ..................................................................
C
  50  IF (L1.GT.LBRK5) GO TO 60
        CALL DESC5
        RETURN
C
C     ..................................................................
C
  60  IF (L1.GT.LBRK6) GO TO 70
        CALL DESC6
        RETURN
C
C     ..................................................................
C
  70  IF (L1.GT.LBRK7) GO TO 80
        CALL DESC7
        RETURN
C
C     ..................................................................
C
  80  IF (L1.GT.LBRK8) GO TO 90
        CALL DESC8
        RETURN
C
C     ..................................................................
C
  90  IF (L1.GT.LBRK9) GO TO 100
        CALL DESC9
        RETURN
C
C     ..................................................................
C
 100  IF (L1.GT.LBRK10) GO TO 110
        CALL DESC10
        RETURN
C
C     ..................................................................
C
 110  IF (L1.GT.LBRK11) GO TO 120
        CALL DESC11
        RETURN
C
C     ..................................................................
C
 120  CALL ERROR (1)
      RETURN
C
C     ..................................................................
C
C     COMMAND EXECUTED BY PROGRAM UNIT LOOKUP.
C
C     PRINT NOTE
C
 200  WRITE (IPRINT,2313)
      RETURN
C
C     ..................................................................
C
C     COMMANDS EXECUTED BY PROGRAM UNIT OMNIT.
C
C     OMNITAB
C
 201  WRITE (IPRINT,1001)
      RETURN
C
C     STOP
C
 202  WRITE (IPRINT,1002)
      RETURN
C
C     TITLE
C
 203  IF (IARGS(1).EQ.IONE)  WRITE (IPRINT,1003)
      IF (IARGS(1).EQ.ITWO)  WRITE (IPRINT,1004)
      IF (IARGS(1).EQ.ITHRE) WRITE (IPRINT,1005)
      IF (IARGS(1).EQ.IFOUR) WRITE (IPRINT,1006)
      RETURN
C
C     TITLEY
C
 207  WRITE (IPRINT,1007)
      RETURN
C
C     TITLEX
C
 208  WRITE (IPRINT,1008)
      RETURN
C
C     FORMAT
C
 209  WRITE (IPRINT,1009)
      RETURN
C
C     NOTE
C
 210  IF (IARGS(1).EQ.IONE) WRITE (IPRINT,1010)
      IF (IARGS(1).EQ.ITWO) WRITE (IPRINT,1011)
      IF (IARGS(1).NE.IONE .AND. IARGS(1).NE.ITWO) WRITE (IPRINT,1012)
      RETURN
C
C     FINISH
C
 213  WRITE (IPRINT,1013)
      RETURN
C
C     HEAD
C
 214  WRITE (IPRINT,1014)
      RETURN
C
C     FILE
C
 215  WRITE (IPRINT,1015)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
1001  FORMAT (1X,67HOMNITAB   $ INFORMATION IS PRINTED AS TITLE AT THE T
     1OP OF EACH PAGE)
1002  FORMAT (1X,59HSTOP   $ THIS IS LAST STATEMENT OF LAST SET OF INSTR
     1UCTIONS)
1003  FORMAT (1X,66HTITLE1   $ NEXT 60 CHARACTERS PRINTED ON FIRST HALF 
     1OF SECOND LINE)
1004  FORMAT (1X,67HTITLE2   $ NEXT 60 CHARACTERS PRINTED ON SECOND HALF
     1 OF SECOND LINE)
1005  FORMAT (1X,65HTITLE3   $ NEXT 60 CHARACTERS PRINTED ON FIRST HALF 
     1OF THIRD LINE)
1006  FORMAT (1X,66HTITLE4   $ NEXT 60 CHARACTERS PRINTED ON SECOND HALF
     1 OF THIRD LINE)
1007  FORMAT (1X,71HTITLEY  $ 51 CHARACTERS AFTER 2ND SPACE ARE PRINTED 
     1ON VER AXIS OF PLOT)
1008  FORMAT (1X,71HTITLEX  $ 60 CHARACTERS AFTER 2ND SPACE ARE PRINTED 
     1ON HOR AXIS OF PLOT) 
1009  FORMAT (1X,70HFORMAT ''L'' (      )  $ PUT REGULAR FORTRAN FORMAT 
     1INSIDE PARENTHESES)
1010  FORMAT (1X,70HNOTE1   $ NEXT SIXTY CHARACTERS STORED FOR PRINTING 
     1FIRST HALF OF NOTE)
1011  FORMAT (1X,71HNOTE2   $ NEXT SIXTY CHARACTERS STORED FOR PRINTING 
     1SECOND HALF OF NOTE)
1012  FORMAT (1X,71HNOTE   $ INFORMATION IN HOLLERITH CARD COLS 7-80 IS 
     1PRINTED IMMEDIATELY)
1013  FORMAT (1X,41HFINISH STORING INSTRUCTIONS FOR LATER USE)
1014  FORMAT (1X,71HHEAD COL (C)/               $ 12 CHARACTERS AFTER / 
     1USED AS COL HEADING)
1015  FORMAT (1X,71HFILE FLENAME                $ NO DESCRIPTIVE TEXT AL
     1LOWED              )
2313  FORMAT (1X,69HPRINT NOTE  $ INFORMATION FROM NOTE1 AND NOTE2 IS PR
     1INTED IMMEDIATELY)
C     ==================================================================
C
      END
*DSUMAL
      SUBROUTINE DSUMAL (DX,NN,SUM)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. DSUMAL V 7.00  2/26/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     ALGORITHM DESCRIBED BY MALCOLM IN COM. OF ACM VOL. 14, NO. 11
C
C     SPECIAL ALGORITHM FOR SUMMING DOUBLE PRECISION NUMBERS.
C        (USE SUMMAL, IF NUMBERS ARE REAL.)
C
C     NN EQUALS       ZERO, CLEAR AREA TO PREPARE FOR NEW SUM.
C     NN EQUALS        ONE, OBTAIN FINAL SUM.
C     NN GREATER THAN ZERO, CLEAR, DO SUM ON NN TERMS AND GET FINAL SUM.
C     NN LESS THAN    ZERO, CONTINUE SUM FOR NEXT ABS(NN) TERMS,
C                              DO NOT GET FINAL SUM.
C
C     NSBB = NO. OF BITS TO REPRESENT MANTISSA IN MOST SIGNIFICANT PART
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - NOVEMBER, 1971.
C                   CURRENT VERSION - FEBRUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION DX(*)
C
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
C
      DOUBLE PRECISION             DX, SUM, SNEG, SPOS
C
      DATA RZERO /0.0/
C
C     ==================================================================
C
      IF(NN) 30,10,20
  10  SPOS = RZERO
      SNEG = RZERO
      RETURN
C
C     ..................................................................
C
  20  IF (NN.EQ.IONE) GO TO 50
      SPOS = RZERO
      SNEG = RZERO
C
  30  N = IABS (NN)
      DO 40 I=1,N
        IF (DX(I).LT.RZERO) SNEG = SNEG + DX(I)
        IF (DX(I).GE.RZERO) SPOS = SPOS + DX(I)
  40  CONTINUE
C
      IF (NN.LT.IZERO) RETURN
C
  50  SUM = SPOS + SNEG
      RETURN
C
C     ==================================================================
C
      END
*DUTCH
      SUBROUTINE DUTCH
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  DUTCH V 7.00  4/ 5/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     TRANSLATION GENEROUSLY PROVIDED BY JOHN MANDEL.   FEBRUARY 1972.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS
C                      A337 ADMINISTRATION BUILDING
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - SEPTEMBER 1975.
C                   CURRENT VERSION -     APRIL 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION L(282), LO(25), LD( 8), LW(35,2), LU(10), LCXX(6)
      DIMENSION LC( 8), LF(30), LP( 5),   LT(14), LCH(7)
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      CHARACTER LCH*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
C     (1)   ONE WORD COMMANDS IN IR(.).
C
      DATA L(  1) / 1354314727 /
      DATA L(  2) / 1331609724 /
      DATA L(  3) /   80211280 /
      DATA L(  4) /   80211280 /
      DATA L(  5) / 1025417069 /
      DATA L(  6) / 1619104032 /
      DATA L(  7) /  155413851 /
      DATA L(  8) /  155414040 /
      DATA L(  9) /  155414067 /
      DATA L( 10) /  155414769 /
      DATA L( 11) /  155414776 /
      DATA L( 12) /  155414777 /
      DATA L( 13) / 1138703981 /
      DATA L( 14) / 1318111703 /
      DATA L( 15) / 1323508897 /
      DATA L( 16) / 1323508897 /
      DATA L( 17) / 1369815179 /
      DATA L( 18) /  105401605 /
      DATA L( 19) / 1372113566 /
      DATA L( 20) / 1372113478 /
      DATA L( 21) / 1372113478 /
      DATA L( 22) /  112706900 /
      DATA L( 23) / 1354904157 /
      DATA L( 24) / 1347402423 /
      DATA L( 25) /  198010206 /
      DATA L( 26) /  198010395 /
      DATA L( 27) /  198010422 /
      DATA L( 28) / 1315515071 /
      DATA L( 29) / 1315515071 /
      DATA L( 30) /  200500000 /
      DATA L( 31) /  200505103 /
      DATA L( 32) /  200505832 /
      DATA L( 33) /  128411286 /
      DATA L( 34) / 1368001126 /
      DATA L( 35) /  525106673 /
      DATA L( 36) / 1352108748 /
      DATA L( 37) /  160006939 /
      DATA L( 38) /  161206939 /
      DATA L( 39) /  161207668 /
      DATA L( 40) /  161208397 /
      DATA L( 41) /  161214677 /
      DATA L( 42) /  171610341 /
      DATA L( 43) /  172704146 /
      DATA L( 44) /  174310341 /
      DATA L( 45) /  175404146 /
      DATA L( 46) /  177010341 /
      DATA L( 47) /  178104146 /
      DATA L( 48) /  187519553 /
      DATA L( 49) /  195303807 /
      DATA L( 50) /  214810341 /
      DATA L( 51) /  215904146 /
      DATA L( 52) /  260814727 /
      DATA L( 53) /  230008897 /
      DATA L( 54) /  233111318 /
      DATA L( 55) /  233119107 /
      DATA L( 56) /  233311318 /
      DATA L( 57) /  233319107 /
      DATA L( 58) /   91010459 /
      DATA L( 59) /  260512057 /
      DATA L( 60) /  239500000 /
      DATA L( 61) / 1619101111 /
      DATA L( 62) /  241811453 /
      DATA L( 63) /  244510341 /
      DATA L( 64) /  245604146 /
      DATA L( 65) /  249910341 /
      DATA L( 66) /  251004146 /
      DATA L( 67) /  278613478 /
      DATA L( 68) /  259603645 /
      DATA L( 69) / 1619105250 /
      DATA L( 70) /  260614729 /
      DATA L( 71) /  260614837 /
      DATA L( 72) /  261013269 /
      DATA L( 73) /  261100000 /
      DATA L( 74) /  261105103 /
      DATA L( 75) /  261105832 /
      DATA L( 76) /  261106959 /
      DATA L( 77) /  261205103 /
      DATA L( 78) /  261205292 /
      DATA L( 79) /  261205319 /
      DATA L( 80) /   77014619 /
      DATA L( 81) /  263111475 /
      DATA L( 82) /  263408784 /
      DATA L( 83) /  267802423 /
      DATA L( 84) /  269300000 /
      DATA L( 85) /  222015071 /
      DATA L( 86) /  233414108 /
      DATA L( 87) /  296813851 /
      DATA L( 88) /  160901058 /
      DATA L( 89) /  306304190 /
      DATA L( 90) / 1129800763 /
      DATA L( 91) /  307002682 /
      DATA L( 92) /  316504527 /
      DATA L( 93) /  317200000 /
      DATA L( 94) /  317204042 /
      DATA L( 95) /  306304023 /
      DATA L( 96) /  318103165 /
      DATA L( 97) /  306304023 /
      DATA L( 98) /  349908994 /
      DATA L( 99) /  380411921 /
      DATA L(100) /  390214396 /
      DATA L(101) /  390214722 /
      DATA L(102) / 1557216180 /
      DATA L(103) /  480014580 /
      DATA L(104) /  424009316 /
      DATA L(105) / 1619117029 /
      DATA L(106) / 1557216448 /
      DATA L(107) /  430211318 /
      DATA L(108) /  430219107 /
      DATA L(109) /  430411318 /
      DATA L(110) /  430419107 /
      DATA L(111) /  430900000 /
      DATA L(112) / 1133317021 /
      DATA L(113) /  430906959 /
      DATA L(114) /   77011710 /
      DATA L(115) / 1608414580 /
      DATA L(116) /  470317741 /
      DATA L(117) / 1129704136 /
      DATA L(118) / 1958016187 /
      DATA L(119) /  480013566 /
      DATA L(120) /  486102736 /
      DATA L(121) /  486508645 /
      DATA L(122) /  440913379 /
      DATA L(123) /  495308748 /
      DATA L(124) /  514309504 /
      DATA L(125) /  525204136 /
      DATA L(126) /  587709896 /
      DATA L(127) /  592814108 /
      DATA L(128) /  598509740 /
      DATA L(129) /  608013167 /
      DATA L(130) /  609414992 /
      DATA L(131) /  635410463 /
      DATA L(132) /  694305250 /
      DATA L(133) /  694305515 /
      DATA L(134) /  694305604 /
      DATA L(135) /  694308431 /
      DATA L(136) /  694308348 /
      DATA L(137) /  694310456 /
      DATA L(138) /  694213270 /
      DATA L(139) /  696111077 /
      DATA L(140) /  524603792 /
      DATA L(141) /  695904132 /
      DATA L(142) /  695904147 /
      DATA L(143) /  695907695 /
      DATA L(144) /  696104151 /
      DATA L(145) / 1139304205 /
      DATA L(146) /   91711317 /
      DATA L(147) /  710613275 /
      DATA L(148) /  808211318 /
      DATA L(149) /  808219107 /
      DATA L(150) /  808411318 /
      DATA L(151) /  808419107 /
      DATA L(152) /  817806980 /
      DATA L(153) /  817807268 /
      DATA L(154) /  817808438 /
      DATA L(155) /  817808726 /
      DATA L(156) /  877703969 /
      DATA L(157) /  878215462 /
      DATA L(158) /  879304637 /
      DATA L(159) /  889004027 /
      DATA L(160) /  889705651 /
      DATA L(161) /  223401068 /
      DATA L(162) /  915601053 /
      DATA L(163) /  916000000 /
      DATA L(164) /  916003645 /
      DATA L(165) /  916014828 /
      DATA L(166) /  989814727 /
      DATA L(167) / 1557208267 /
      DATA L(168) /  952800000 /
      DATA L(169) /  952806933 /
      DATA L(170) /  952809734 /
      DATA L(171) /  953611703 /
      DATA L(172) /  959400933 /
      DATA L(173) /  961606602 /
      DATA L(174) /  962105252 /
      DATA L(175) / 1005315179 /
      DATA L(176) /  972404043 /
      DATA L(177) /  973400000 /
      DATA L(178) /  973406933 /
      DATA L(179) /  973416191 /
      DATA L(180) /  979211318 /
      DATA L(181) /  980201605 /
      DATA L(182) /  982915179 /
      DATA L(183) / 1007613566 /
      DATA L(184) / 1007613478 /
      DATA L(185) / 1007613478 /
      DATA L(186) /  989403747 /
      DATA L(187) /  990014811 /
      DATA L(188) / 1619111989 /
      DATA L(189) /  990404157 /
      DATA L(190) /  982902423 /
      DATA L(191) /  999301054 /
      DATA L(192) /  951015071 /
      DATA L(193) /  951015071 /
      DATA L(194) / 1003501126 /
      DATA L(195) / 1003506602 /
      DATA L(196) / 1619109626 /
      DATA L(197) / 1619109626 /
      DATA L(198) / 1007608136 /
      DATA L(199) / 1007608371 /
      DATA L(200) /  987608748 /
      DATA L(201) / 1030309173 /
      DATA L(202) / 1034803902 /
      DATA L(203) / 1034804309 /
      DATA L(204) / 1043114406 /
      DATA L(205) / 1062909802 /
      DATA L(206) / 1065011475 /
      DATA L(207) / 1074701787 /
      DATA L(208) / 1078500000 /
      DATA L(209) / 1129514580 /
      DATA L(210) /  403403991 /
      DATA L(211) / 1142504023 /
      DATA L(212) / 1170915030 /
      DATA L(213) / 1170915108 /
      DATA L(214) / 1170914843 /
      DATA L(215) / 1181702336 /
      DATA L(216) / 1557216448 /
      DATA L(217) /  128411302 /
      DATA L(218) / 1506710731 /
      DATA L(219) /  559004752 /
      DATA L(220) / 1208118618 /
      DATA L(221) / 1216503494 /
      DATA L(222) / 1129411347 /
      DATA L(223) / 1216512087 /
      DATA L(224) /  950706391 /
      DATA L(225) / 1316305238 /
      DATA L(226) / 1316305619 /
      DATA L(227) / 1326007011 /
      DATA L(228) / 1326011048 /
      DATA L(229) / 1327011480 /
      DATA L(230) / 1557216448 /
      DATA L(231) / 1327308778 /
      DATA L(232) /  598514396 /
      DATA L(233) / 1327700986 /
      DATA L(234) / 1719014727 /
      DATA L(235) /   90911317 /
      DATA L(236) / 1337514269 /
      DATA L(237) / 1726816191 /
      DATA L(238) /   91101262 /
      DATA L(239) / 1686313613 /
      DATA L(240) / 1396804541 /
      DATA L(241) / 1396816155 /
      DATA L(242) / 1936408168 /
      DATA L(243) /  826719103 /
      DATA L(244) / 1920314729 /
      DATA L(245) / 1661708436 /
      DATA L(246) / 1679510639 /
      DATA L(247) / 1619108442 /
      DATA L(248) / 1409400000 /
      DATA L(249) / 1410800000 /
      DATA L(250) / 1410805103 /
      DATA L(251) / 1410805832 /
      DATA L(252) / 1410806959 /
      DATA L(253) / 1713316191 /
      DATA L(254) / 1137911467 /
      DATA L(255) / 1691608166 /
      DATA L(256) / 1427414733 /
      DATA L(257) / 1428414828 /
      DATA L(258) / 1721409437 /
      DATA L(259) / 1628613420 /
      DATA L(260) /  864103403 /
      DATA L(261) / 1730001278 /
      DATA L(262) / 1439214842 /
      DATA L(263) / 1439215024 /
      DATA L(264) / 1439503073 /
      DATA L(265) / 1733003802 /
      DATA L(266) /   91113268 /
      DATA L(267) /   91113268 /
      DATA L(268) / 1426909630 /
      DATA L(269) / 1460908883 /
      DATA L(270) / 1476900000 /
      DATA L(271) / 1477600000 /
      DATA L(272) / 1477700000 /
      DATA L(273) / 1462303645 /
      DATA L(274) / 1466903724 /
      DATA L(275) / 1473309734 /
      DATA L(276) / 1521612003 /
      DATA L(277) / 1520604254 /
      DATA L(278) / 1539803724 /
      DATA L(279) / 1569614580 /
      DATA L(280) / 1620914580 /
      DATA L(281) / 1648509429 /
      DATA L(282) / 1701414796 /
C
C     ..................................................................
C
C     (3)   COMMANDS EXECUTED BY OMNIT IN NL(.).
C
      DATA LO( 1), LO( 2), LO( 3), LO( 4), LO( 5), LO( 6), LO( 7) /
     1      11300,   7102,   1598,   6943,  14406,  12101,  10631 /
      DATA LO( 8), LO( 9), LO(10), LO(11), LO(12), LO(13), LO(14) /
     1       7110,  14843,   3969,   4797,   9505,   3423,   8321 /
      DATA LO(15), LO(16), LO(17), LO(18), LO(19) /
     1      11386,   2421,   3994,   3993,   9477 /
C
C     ..................................................................
C
C     (4)   RESET, PRINT, ETC. IN ID(.)
C
      DATA LD( 1), LD( 2) /  843611298,  342308321 /
      DATA LD( 3), LD( 4) / 1208314000,  890904023 /
      DATA LD( 5), LD( 6) /   90211441, 1324815617 /
      DATA LD( 7), LD( 8) /  960315617, 1033215617 /
C
C     ..................................................................
C
C     (5)   TWO-WORD COMMANDS IN IRD(.).
C
      DATA LW( 1,1), LW( 1,2) / 1045414580,  223401068 /
      DATA LW( 2,1), LW( 2,2) /  260512159,          0 /
      DATA LW( 3,1), LW( 3,2) / 1045415935,  178303627 /
      DATA LW( 4,1), LW( 4,2) /  947700000, 1814400000 /
      DATA LW( 5,1), LW( 5,2) /  947700000, 1749600000 /
      DATA LW( 6,1), LW( 6,2) /  947700000, 1754700000 /
      DATA LW( 7,1), LW( 7,2) /  947700000,   83700000 /
      DATA LW( 8,1), LW( 8,2) /  947700000,  294300000 /
      DATA LW( 9,1), LW( 9,2) /  947700000,  132300000 /
      DATA LW(10,1), LW(10,2) /  947700000, 1603800000 /
      DATA LW(11,1), LW(11,2) /  398107013,  379400000 /
      DATA LW(12,1), LW(12,2) /  398107013, 1520603645 /
      DATA LW(13,1), LW(13,2) /  178303627,          0 /
      DATA LW(14,1), LW(14,2) / 1440915908, 1078500000 /
      DATA LW(15,1), LW(15,2) / 1440915908,  379400000 /
      DATA LW(16,1), LW(16,2) / 1337514269,          0 /
      DATA LW(17,1), LW(17,2) / 1910711448,  175404146 /
      DATA LW(18,1), LW(18,2) / 1910711448,  174310341 /
      DATA LW(19,1), LW(19,2) /  515114364,  864103403 /
      DATA LW(20,1), LW(20,2) / 1439609477,  888404374 /
      DATA LW(21,1), LW(21,2) / 1438403996,  888404374 /
      DATA LW(22,1), LW(22,2) /  437400000, 1679513638 /
      DATA LW(23,1), LW(23,2) / 1429507101, 1200314580 /
      DATA LW(24,1), LW(24,2) / 1045203645, 1200314580 /
      DATA LW(25,1), LW(25,2) / 1045203645, 1065011475 /
      DATA LW(26,1), LW(26,2) / 1045203645,  263111475 /
      DATA LW(27,1), LW(27,2) / 1045203645, 1030309173 /
      DATA LW(28,1), LW(28,2) / 1389111993, 1703006318 /
      DATA LW(29,1), LW(29,2) / 1389111993, 1703006258 /
      DATA LW(30,1), LW(30,2) / 1472615081, 1200314580 /
      DATA LW(31,1), LW(31,2) / 1472615081,  138613851 /
      DATA LW(32,1), LW(32,2) / 1472615081, 1473309734 /
      DATA LW(33,1), LW(33,2) /  879404131, 1200314580 /
      DATA LW(34,1), LW(34,2) /  879404131,  138613851 /
      DATA LW(35,1), LW(35,2) /  879404131, 1473309734 /
C
C     ..................................................................
C
C     (6)   UNIT COMMANDS IN ITP(.).
C
      DATA LU( 1), LU( 2) / 1325802916,  267800837 /
      DATA LU( 3), LU( 4) / 1726214715, 1400600000 /
      DATA LU( 5), LU( 6) /  270514580,  402704629 /
      DATA LU( 7), LU( 8) / 1328006943, 1415711664 /
      DATA LU( 9), LU(10) /  148808548,          0 /
C
C     ..................................................................
C
C     (7)   CENSOR XX IN ICP(.).
C
      DATA LCXX(1), LCXX(2), LCXX(3), LCXX(4), LCXX(5), LCXX(6)/
     1        8883,    4104,    5238,    5643,    9288,   10341/
C
C     ..................................................................
C
C     (8)   CALCOMP IN ICL(.).
C
      DATA LC( 1), LC( 2) / 1462303645, 1428803753 /
      DATA LC( 3), LC( 4) / 1412003645, 1200314580 /
      DATA LC( 5), LC( 6) / 1419016767,  442014580 /
      DATA LC( 7), LC( 8) / 1170704131,  138613851 /
C
C     ..................................................................
C
C     (9)   DISTRIBUTIONS IN IDIST(.).
C
      DATA LF( 1), LF( 2) / 1062909516,  916010629 /
      DATA LF( 3), LF( 4) /  587104767, 1458000000 /
      DATA LF( 5), LF( 6) /  241214331,  514309504 /
      DATA LF( 7), LF( 8) /  437400000,  161300729 /
      DATA LF( 9), LF(10) / 1433810400,  124708886 /
      DATA LF(11), LF(12) /  223502428,  878801567 /
      DATA LF(13), LF(14) /  952809734, 1691102037 /
      DATA LF(15), LF(16) / 1317408892,  430911318 /
      DATA LF(17), LF(18) /  307512083,  916007094 /
      DATA LF(19), LF(20) / 1030700000, 1029006094 /
      DATA LF(21), LF(22) / 1029300000,  161110632 /
      DATA LF(23), LF(24) /  171511295, 1034801715 /
      DATA LF(25), LF(26) / 1207814379,  525309632 /
      DATA LF(27), LF(28) /  652304138,  317802678 /
      DATA LF(29), LF(30) / 1171002226, 1005614837 /
C
C     ..................................................................
C
C     (10)   PROPERTIES OF DISTRIBUTIONS IN IPROP(.).
C
      DATA LP( 1), LP( 2) /  306514114,  276715634 /
      DATA LP( 3), LP( 4) / 1181702336, 1316303334 /
      DATA LP( 5)         /  559004752             /
C
C     ..................................................................
C
C     (11)   TABLE MAKING IN ITB(.).
C
      DATA LT( 1), LT( 2) /  486512965, 1443100000 /
      DATA LT( 3), LT( 4) /  132813156, 1439503073 /
      DATA LT( 5), LT( 6) /  973406933,  952806933 /
      DATA LT( 7), LT( 8) / 1316305238,  961606602 /
      DATA LT( 9), LT(10) / 1181702336, 1216512087 /
      DATA LT(11), LT(12) / 1355913208,  262413208 /
      DATA LT(13), LT(14) / 1357211382,  263711382 /
C
C     ..................................................................
C
C     (13)   COLUMN IN ICOLHD(.).
C
      DATA LCH(1), LCH(2), LCH(3), LCH(4), LCH(5), LCH(6), LCH(7)/
     1        'K',    'O',    'L',    'O',    'M',    ' ',    ' '/
C
C     ==================================================================
C
      CALL MVELNG (L, LO, LD, LW, LU, LCXX, LC, LF, LP, LT, LCH)
C
      RETURN
C
C     ==================================================================
C
      END
*ELIPSE
      SUBROUTINE ELIPSE (AELP,BELP,CELP,FELP,XCNTER,YCNTER,NOPTS,X,Y) 
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. ELIPSE V 7.00  2/20/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     COMPUTE BOUNDARY OF ELLIPSE USING ALGORITHM 
C        DEVELOPED BY DAVID HOGBEN.
C
C           (A) FIND LARGEST VALUE OF X, WHERE DERIVATIVE IS INFINITE.
C           (B) USE UNIFORM INCREMENTS OF X FROM 0 TO A, WHERE
C                  A IS DETERMINED FROM (A).
C           (C) COMPUTE FOUR POINTS (X,Y), (X,-Y), (-X,Y) AND (-X,-Y),
C
C     EQUATION OF ELLIPSE IS A*X**2 + B*X*Y* + C*Y**2  + F = 0.
C
C     INPUT...
C
C       AELP   VALUE OF A FOR THE ELLIPSE EQUATION.
C       BELP   VALUE OF B FOR THE ELLIPSE EQUATION.
C       CELP   VALUE OF C FOR THE ELLIPSE EQUATION.
C       FELP   VALUE OF F FOR THE ELLIPSE EQUATION.
C       XCNTER CENTER PT. OF ORIGINAL ELLIPSE ON X AXIS.
C       YCNTER CENTER PT. OF ORIGINAL ELLIPSE ON Y AXIS.
C       NOPTS  NUMBER OF INTERVALS TO BE USED FROM 0 TO A.
C              NUMBER OF U AND V VALUES TO BE COMPUTED WILL BE 4*NOPTS.
C
C     OUTPUT ...
C
C       X      VALUES OF THE ELLIPSE ON THE X AXIS.
C       Y      VALUES OF THE ELLIPSE ON THE Y AXIS.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION - DECEMBER, 1977.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
C
      COMMON /CONSTS/ DEG, E, HALFPI, PI, RAD
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             X(*), Y(*)
      REAL             AELP, BELP, CELP, FELP, XCNTER, YCNTER
      REAL             AX2PF, BXNOW, C2, DTHETA, QAX2PF, XNOW
      REAL             XZERO, YTERM, YZERO
C
C     ==================================================================
C
      KPTS = NOPTS
      JSUBY1 = IONE 
      JSUBY2 = JSUBY1 + KPTS
      JSUBY3 = JSUBY2 + KPTS
      JSUBY4 = JSUBY3 + KPTS
      JSUBX1 = IONE 
      JSUBX2 = JSUBX1 + KPTS
      JSUBX3 = JSUBX2 + KPTS
      JSUBX4 = JSUBX3 + KPTS
      YTERM = CELP * (FDIV(RFOR*AELP*CELP,BELP**2,IND)-RONE)
      YZERO = FSQRT (FDIV(FELP,YTERM,IND))
      XZERO = FDIV (RTWO*CELP*YZERO,BELP,IND)
      IF (BELP.EQ.RZERO) XZERO = FSQRT ( FDIV (FELP,AELP,IND) )
      C2 = RTWO * CELP
      DTHETA = FLOAT (KPTS-IONE)
      DO 10 I=1,KPTS
        XNOW = FDIV (FLOAT(I-IONE)*XZERO,DTHETA,IND)
        BXNOW = BELP * XNOW
        AX2PF = RFOR * CELP * (AELP*XNOW**2-FELP) 
        QAX2PF = FSQRT (BXNOW**2-AX2PF) 
        X(JSUBX1) = XCNTER + XNOW
        X(JSUBX2) = XCNTER + XNOW
        X(JSUBX3) = XCNTER - XNOW
        X(JSUBX4) = XCNTER - XNOW
        Y(JSUBY1) = YCNTER + FDIV (-BXNOW+QAX2PF,C2,IND)
        Y(JSUBY2) = YCNTER + FDIV (-BXNOW-QAX2PF,C2,IND)
        Y(JSUBY3) = YCNTER + FDIV (BXNOW+QAX2PF,C2,IND)
        Y(JSUBY4) = YCNTER + FDIV (BXNOW-QAX2PF,C2,IND)
        JSUBX1 = JSUBX1 + IONE
        JSUBX2 = JSUBX2 + IONE
        JSUBX3 = JSUBX3 + IONE
        JSUBX4 = JSUBX4 + IONE
        JSUBY1 = JSUBY1 + IONE
        JSUBY2 = JSUBY2 + IONE
        JSUBY3 = JSUBY3 + IONE
        JSUBY4 = JSUBY4 + IONE
  10  CONTINUE
      RETURN
C
C     ==================================================================
C
      END 
*ENGLSH
      SUBROUTINE ENGLSH
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. ENGLSH V 7.00  4/ 5/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     TRANSLATION NOT NECESSARY.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS
C                      A337 ADMINISTRATION BUILDING
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION - SEPTEMBER 1975.
C                   CURRENT VERSION -     APRIL 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION L(282), LO(25), LD( 8), LW(35,2), LU(10), LCXX(6)
      DIMENSION LC( 8), LF(30), LP( 5),   LT(14), LCH(7)
C
C     ==================================================================
C
C                        ***   TYPE STATEMENTS   ***
C
      CHARACTER LCH*1
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
C     (1)   ONE WORD COMMANDS IN IR(.). 
C
      DATA L(  1) /   76002916 /
      DATA L(  2) /   77804132 /
      DATA L(  3) /   80200000 /
      DATA L(  4) /   80211280 /
      DATA L(  5) /   81315796 /
      DATA L(  6) /   82501058 /
      DATA L(  7) /   82513851 /
      DATA L(  8) /   82513959 /
      DATA L(  9) /   82514067 /
      DATA L( 10) /   82514580 /
      DATA L( 11) /   82514688 /
      DATA L( 12) /   82514796 /
      DATA L( 13) /   84100000 /
      DATA L( 14) /   84204631 /
      DATA L( 15) /   84616038 /
      DATA L( 16) /   84616285 /
      DATA L( 17) /   88201247 /
      DATA L( 18) /  105401605 /
      DATA L( 19) /  109516173 /
      DATA L( 20) /  110109288 /
      DATA L( 21) /  110109297 /
      DATA L( 22) /  112706900 /
      DATA L( 23) /  117911372 /
      DATA L( 24) /  121607079 /
      DATA L( 25) /  125110206 /
      DATA L( 26) /  125110314 /
      DATA L( 27) /  125110422 /
      DATA L( 28) /  126301458 /
      DATA L( 29) /  126302016 /
      DATA L( 30) /  127010206 /
      DATA L( 31) /  127010314 /
      DATA L( 32) /  127010422 /
      DATA L( 33) /  128409723 /
      DATA L( 34) /  128701126 /
      DATA L( 35) /  132813156 /
      DATA L( 36) /  143613527 /
      DATA L( 37) /  160006939 /
      DATA L( 38) /  161206939 /
      DATA L( 39) /  161207668 /
      DATA L( 40) /  161208397 /
      DATA L( 41) /  161214677 /
      DATA L( 42) /  171610341 /
      DATA L( 43) /  172704146 /
      DATA L( 44) /  174310341 /
      DATA L( 45) /  175404146 /
      DATA L( 46) /  177010341 /
      DATA L( 47) /  178104146 /
      DATA L( 48) /  187503178 /
      DATA L( 49) /  195303807 /
      DATA L( 50) /  214810341 /
      DATA L( 51) /  215904146 /
      DATA L( 52) /  221802916 /
      DATA L( 53) /  230416285 /
      DATA L( 54) /  233111318 /
      DATA L( 55) /  233119107 /
      DATA L( 56) /  233311318 /
      DATA L( 57) /  233319107 /
      DATA L( 58) /  233614274 /
      DATA L( 59) /  234004374 /
      DATA L( 60) /  239500000 /
      DATA L( 61) /  240410400 /
      DATA L( 62) /  241811453 /
      DATA L( 63) /  244510341 /
      DATA L( 64) /  245604146 /
      DATA L( 65) /  249910341 /
      DATA L( 66) /  251004146 /
      DATA L( 67) /  255909297 /
      DATA L( 68) /  259603645 /
      DATA L( 69) /  260511709 /
      DATA L( 70) /  260614729 /
      DATA L( 71) /  260614837 /
      DATA L( 72) /  261013269 /
      DATA L( 73) /  261100000 /
      DATA L( 74) /  261102916 /
      DATA L( 75) /  261105832 /
      DATA L( 76) /  261106959 /
      DATA L( 77) /  261200000 /
      DATA L( 78) /  261202916 /
      DATA L( 79) /  261205832 /
      DATA L( 80) /  261310746 /
      DATA L( 81) /  263111475 /
      DATA L( 82) /  263408793 /
      DATA L( 83) /  267802728 /
      DATA L( 84) /  269300000 /
      DATA L( 85) /  272102016 /
      DATA L( 86) /  274204374 /
      DATA L( 87) /  296813851 /
      DATA L( 88) /  305706944 /
      DATA L( 89) /  306304190 /
      DATA L( 90) /  306411480 /
      DATA L( 91) /  307002682 /
      DATA L( 92) /  316504527 /
      DATA L( 93) /  317200000 /
      DATA L( 94) /  317204042 /
      DATA L( 95) /  318100000 /
      DATA L( 96) /  318103165 /
      DATA L( 97) /  318106674 /
      DATA L( 98) /  349908994 /
      DATA L( 99) /  380411921 /
      DATA L(100) /  390214396 /
      DATA L(101) /  390214722 /
      DATA L(102) /  413213986 /
      DATA L(103) /  414911421 /
      DATA L(104) /  424009316 /
      DATA L(105) /  429605873 /
      DATA L(106) /  429802774 /
      DATA L(107) /  430211318 /
      DATA L(108) /  430219107 /
      DATA L(109) /  430411318 /
      DATA L(110) /  430419107 /
      DATA L(111) /  430900000 /
      DATA L(112) /  430901111 /
      DATA L(113) /  430906959 /
      DATA L(114) /  463700000 /
      DATA L(115) /  464103753 /
      DATA L(116) /  470317741 /
      DATA L(117) /  470711664 /
      DATA L(118) /  471301278 /
      DATA L(119) /  480013566 /
      DATA L(120) /  486102736 /
      DATA L(121) /  486512965 /
      DATA L(122) /  492902187 /
      DATA L(123) /  495308748 /
      DATA L(124) /  514309504 /
      DATA L(125) /  525204132 /
      DATA L(126) /  587709896 /
      DATA L(127) /  592814108 /
      DATA L(128) /  598509740 /
      DATA L(129) /  608013167 /
      DATA L(130) /  609414992 /
      DATA L(131) /  635410463 /
      DATA L(132) /  672812393 /
      DATA L(133) /  673003645 /
      DATA L(134) /  673014580 /
      DATA L(135) /  673503645 /
      DATA L(136) /  673514580 /
      DATA L(137) /  673703645 /
      DATA L(138) /  694213270 /
      DATA L(139) /  695804151 /
      DATA L(140) /  695903839 /
      DATA L(141) /  695904132 /
      DATA L(142) /  695904147 /
      DATA L(143) /  695907695 /
      DATA L(144) /  696104151 /
      DATA L(145) /  707915163 /
      DATA L(146) /  708908795 /
      DATA L(147) /  710613169 /
      DATA L(148) /  808211318 /
      DATA L(149) /  808219107 /
      DATA L(150) /  808411318 /
      DATA L(151) /  808419107 /
      DATA L(152) /  817806980 /
      DATA L(153) /  817807268 /
      DATA L(154) /  817808438 /
      DATA L(155) /  817808726 /
      DATA L(156) /  877703969 /
      DATA L(157) /  878215462 /
      DATA L(158) /  879304637 /
      DATA L(159) /  889004027 /
      DATA L(160) /  889705651 /
      DATA L(161) /  901014580 /
      DATA L(162) /  915601053 /
      DATA L(163) /  916000000 /
      DATA L(164) /  916003645 /
      DATA L(165) /  916014729 /
      DATA L(166) /  950802916 /
      DATA L(167) /  952402403 /
      DATA L(168) /  952800000 /
      DATA L(169) /  952806933 /
      DATA L(170) /  952809734 /
      DATA L(171) /  959004631 /
      DATA L(172) /  959400933 /
      DATA L(173) /  961606602 /
      DATA L(174) /  962105252 /
      DATA L(175) /  963001247 /
      DATA L(176) /  972404043 /
      DATA L(177) /  973400000 /
      DATA L(178) /  973406933 /
      DATA L(179) /  973416191 /
      DATA L(180) /  979211318 /
      DATA L(181) /  980201605 /
      DATA L(182) /  982915179 /
      DATA L(183) /  984316173 /
      DATA L(184) /  984909288 /
      DATA L(185) /  984909297 /
      DATA L(186) /  989417307 /
      DATA L(187) /  990014811 /
      DATA L(188) /  990403645 /
      DATA L(189) /  992711372 /
      DATA L(190) /  996407079 /
      DATA L(191) /  999301054 /
      DATA L(192) / 1001101458 /
      DATA L(193) / 1001102016 /
      DATA L(194) / 1003501126 /
      DATA L(195) / 1003506602 /
      DATA L(196) / 1005614580 /
      DATA L(197) / 1005614839 /
      DATA L(198) / 1007602304 /
      DATA L(199) / 1007602539 /
      DATA L(200) / 1018413527 /
      DATA L(201) / 1030309173 /
      DATA L(202) / 1034803902 /
      DATA L(203) / 1034804309 /
      DATA L(204) / 1043114406 /
      DATA L(205) / 1062909802 /
      DATA L(206) / 1065011475 /
      DATA L(207) / 1074701787 /
      DATA L(208) / 1078508748 /
      DATA L(209) / 1129514580 /
      DATA L(210) / 1131816819 /
      DATA L(211) / 1142504131 /
      DATA L(212) / 1170912165 /
      DATA L(213) / 1170914431 /
      DATA L(214) / 1170914763 /
      DATA L(215) / 1181702336 /
      DATA L(216) / 1181704797 /
      DATA L(217) / 1182714998 /
      DATA L(218) / 1184613163 /
      DATA L(219) / 1200314580 /
      DATA L(220) / 1208118396 /
      DATA L(221) / 1216503486 /
      DATA L(222) / 1216509902 /
      DATA L(223) / 1216512087 /
      DATA L(224) / 1315813986 /
      DATA L(225) / 1316305238 /
      DATA L(226) / 1316308532 /
      DATA L(227) / 1326007011 /
      DATA L(228) / 1326011048 /
      DATA L(229) / 1327011480 /
      DATA L(230) / 1327303692 /
      DATA L(231) / 1327308778 /
      DATA L(232) / 1327615003 /
      DATA L(233) / 1327700986 /
      DATA L(234) / 1349200000 /
      DATA L(235) / 1354810314 /
      DATA L(236) / 1355014431 /
      DATA L(237) / 1389413543 /
      DATA L(238) / 1393310206 /
      DATA L(239) / 1394713613 /
      DATA L(240) / 1396804541 /
      DATA L(241) / 1396816155 /
      DATA L(242) / 1398713211 /
      DATA L(243) / 1399803746 /
      DATA L(244) / 1400201216 /
      DATA L(245) / 1400600000 /
      DATA L(246) / 1402214580 /
      DATA L(247) / 1408213667 /
      DATA L(248) / 1409400000 /
      DATA L(249) / 1410800000 /
      DATA L(250) / 1410802916 /
      DATA L(251) / 1410805832 /
      DATA L(252) / 1410806959 /
      DATA L(253) / 1421813543 /
      DATA L(254) / 1426816173 /
      DATA L(255) / 1427004267 /
      DATA L(256) / 1427414580 /
      DATA L(257) / 1428402322 /
      DATA L(258) / 1429809429 /
      DATA L(259) / 1432814580 /
      DATA L(260) / 1433101220 /
      DATA L(261) / 1438401278 /
      DATA L(262) / 1439214842 /
      DATA L(263) / 1439215024 /
      DATA L(264) / 1439503073 /
      DATA L(265) / 1441411557 /
      DATA L(266) / 1442000000 /
      DATA L(267) / 1442015067 /
      DATA L(268) / 1443100000 /
      DATA L(269) / 1460908883 /
      DATA L(270) / 1462100000 /
      DATA L(271) / 1462102916 /
      DATA L(272) / 1462105832 /
      DATA L(273) / 1462303645 /
      DATA L(274) / 1466903724 /
      DATA L(275) / 1473309734 /
      DATA L(276) / 1521612003 /
      DATA L(277) / 1521616819 /
      DATA L(278) / 1539803724 /
      DATA L(279) / 1569614580 /
      DATA L(280) / 1620914580 /
      DATA L(281) / 1648509429 /
      DATA L(282) / 1701414796 /
C
C     ..................................................................
C
C     (3)   COMMANDS EXECUTED BY OMNIT IN NL(.).
C
      DATA LO( 1), LO( 2), LO( 3), LO( 4), LO( 5), LO( 6), LO( 7) /
     1      11300,   7102,   4631,   7082,  14406,  11664,  10631 /
      DATA LO( 8), LO( 9), LO(10), LO(11), LO(12), LO(13), LO(14) /
     1       3645,  14843,   8883,   4797,   9524,  12159,  10746 /
      DATA LO(15), LO(16), LO(17), LO(18), LO(19) /
     1       5968,   2916,   8908,   8907,   9477 /
C
C     ..................................................................
C
C     (4)   RESET, PRINT, ETC. IN ID(.) 
C
      DATA LD( 1), LD( 2) / 1327604185, 1215910746 /
      DATA LD( 3), LD( 4) / 1224502403, 1325802916 /
      DATA LD( 5), LD( 6) /   80106676,  117906959 /
      DATA LD( 7), LD( 8) /  992706959, 1065606959 /
C
C     ..................................................................
C
C     (5)   TWO-WORD COMMANDS IN IRD(.).
C
      DATA LW( 1,1), LW( 1,2) / 1061100000,  901014580 /
      DATA LW( 2,1), LW( 2,2) /  252613986, 1574100000 /
      DATA LW( 3,1), LW( 3,2) / 1036400000, 1169803645 /
      DATA LW( 4,1), LW( 4,2) /  947700000, 1814400000 /
      DATA LW( 5,1), LW( 5,2) /  947700000, 1749600000 /
      DATA LW( 6,1), LW( 6,2) /  947700000, 1754700000 /
      DATA LW( 7,1), LW( 7,2) /  947700000,   83700000 /
      DATA LW( 8,1), LW( 8,2) /  947700000,  294300000 /
      DATA LW( 9,1), LW( 9,2) /  947700000,  132300000 /
      DATA LW(10,1), LW(10,2) /  947700000, 1603800000 /
      DATA LW(11,1), LW(11,2) /  398107013,  463514391 /
      DATA LW(12,1), LW(12,2) /  398107013, 1398911317 /
      DATA LW(13,1), LW(13,2) / 1169803645, 1200314580 /
      DATA LW(14,1), LW(14,2) / 1440915908, 1910710935 /
      DATA LW(15,1), LW(15,2) / 1440915908, 1131800000 /
      DATA LW(16,1), LW(16,2) / 1355000000, 1443100000 /
      DATA LW(17,1), LW(17,2) / 1910711448,  175404146 /
      DATA LW(18,1), LW(18,2) / 1910711448,  174310341 /
      DATA LW(19,1), LW(19,2) /  515114364, 1296103403 /
      DATA LW(20,1), LW(20,2) / 1439609477,  888404374 /
      DATA LW(21,1), LW(21,2) / 1438403996,  888404374 /
      DATA LW(22,1), LW(22,2) /  437400000, 1216501487 /
      DATA LW(23,1), LW(23,2) / 1429507101, 1200314580 /
      DATA LW(24,1), LW(24,2) / 1045203645, 1200314580 /
      DATA LW(25,1), LW(25,2) / 1045203645, 1065011475 /
      DATA LW(26,1), LW(26,2) / 1045203645,  263111475 /
      DATA LW(27,1), LW(27,2) / 1045203645, 1030309173 /
      DATA LW(28,1), LW(28,2) / 1389111993, 1703006318 /
      DATA LW(29,1), LW(29,2) / 1389111993, 1703006258 /
      DATA LW(30,1), LW(30,2) / 1472615081, 1200314580 /
      DATA LW(31,1), LW(31,2) / 1472615081,  138613851 /
      DATA LW(32,1), LW(32,2) / 1472615081, 1473309734 /
      DATA LW(33,1), LW(33,2) /  879404131, 1200314580 /
      DATA LW(34,1), LW(34,2) /  879404131,  138613851 /
      DATA LW(35,1), LW(35,2) /  879404131, 1473309734 /
C
C     ..................................................................
C
C     (6)   UNIT COMMANDS IN ITP(.).
C
      DATA LU( 1), LU( 2) / 1325802916,  267800837 /
      DATA LU( 3), LU( 4) / 1726214715, 1400600000 /
      DATA LU( 5), LU( 6) /  270514580,  402704629 /
      DATA LU( 7), LU( 8) / 1328006943, 1415711664 /
      DATA LU( 9), LU(10) /  148808548,          0 /
C
C     ..................................................................
C
C     (7)   CENSOR XX IN ICP(.).
C
      DATA LCXX(1), LCXX(2), LCXX(3), LCXX(4), LCXX(5), LCXX(6)/
     1        8883,    4104,    5238,    5643,    9288,   10341/
C
C     ..................................................................
C
C     (8)   CALCOMP IN ICL(.).
C
      DATA LC( 1), LC( 2) / 1462303645, 1428803753 /
      DATA LC( 3), LC( 4) / 1412003645, 1200314580 /
      DATA LC( 5), LC( 6) / 1419016767,  442014580 /
      DATA LC( 7), LC( 8) / 1170704131,  138613851 /
C
C     ..................................................................
C
C     (9)   DISTRIBUTIONS IN IDIST(.).
C
      DATA LF( 1), LF( 2) / 1062909516,  916010629 /
      DATA LF( 3), LF( 4) /  587104767, 1458000000 /
      DATA LF( 5), LF( 6) /  241214331,  514309504 /
      DATA LF( 7), LF( 8) /  437400000,  161300729 /
      DATA LF( 9), LF(10) / 1433810400, 1569604797 /
      DATA LF(11), LF(12) /  223502428,  878801567 /
      DATA LF(13), LF(14) /  431313270, 1691102037 /
      DATA LF(15), LF(16) / 1170904200,  430911318 /
      DATA LF(17), LF(18) /  307512083,  916007094 /
      DATA LF(19), LF(20) / 1030700000, 1029006094 /
      DATA LF(21), LF(22) / 1029300000,  161110632 /
      DATA LF(23), LF(24) /  171511295, 1034801715 /
      DATA LF(25), LF(26) / 1207814379,  525309632 /
      DATA LF(27), LF(28) /  652304138,  317802678 /
      DATA LF(29), LF(30) / 1171002226, 1005614837 /
C
C     ..................................................................
C
C     (10)   PROPERTIES OF DISTRIBUTIONS IN IPROP(.).
C
      DATA LP( 1), LP( 2) /  306514114,  276715634 /
      DATA LP( 3), LP( 4) / 1181702336, 1316303334 /
      DATA LP( 5)         / 1200314580             /
C
C     ..................................................................
C
C     (11)   TABLE MAKING IN ITB(.).
C
      DATA LT( 1), LT( 2) /  486512965, 1443100000 /
      DATA LT( 3), LT( 4) /  132813156, 1439503073 /
      DATA LT( 5), LT( 6) /  973406933,  952806933 /
      DATA LT( 7), LT( 8) / 1316305238,  961606602 /
      DATA LT( 9), LT(10) / 1181702336, 1216512087 /
      DATA LT(11), LT(12) / 1355913208,  262413208 /
      DATA LT(13), LT(14) / 1357211382,  263711382 /
C
C     ..................................................................
C
C     (13)   COLUMN IN ICOLHD(.).
C
      DATA LCH(1), LCH(2), LCH(3), LCH(4), LCH(5), LCH(6), LCH(7)/
     1        'C',    'O',    'L',    'U',    'M',    'N',    ' '/
C
C     ==================================================================
C
      CALL MVELNG (L, LO, LD, LW, LU, LCXX, LC, LF, LP, LT, LCH)
C
      RETURN
C
C     ==================================================================
C
      END 
*ERASE
      SUBROUTINE ERASE
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  ERASE V 7.00  2/20/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     ERASE COLUMNS (C), (C), (C), ETC.
C
C     IF NO COLUMNS ARE SPECIFIED, THE ENTIRE WORKSHEET IS ERASED
C        AND NRMAX IS RESET TO ZERO.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ERRMES/ ISE, KMES, LLIST, MESS(15), MNOE, NERR, NRM, NROLD
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
C     ==================================================================
C
      IF (NARGS.EQ.IZERO) GO TO 30
      IF (NRMAX.GT.IZERO) GO TO 10
        CALL ERROR (9)
        RETURN
  10  CALL CHKCOL
      IF (NERROR.NE.IZERO) RETURN
C
      DO 20 I=1,NARGS
        CALL VECTOR (RZERO,IARGS(I))
  20  CONTINUE
      RETURN
C
C     CLEAR ENTIRE DIMENSIONED WORKSHEET.
C
  30  IF (NERROR.NE.IZERO) RETURN
      NROLD = NRMAX
      NRMAX = NROW * NCOL
      CALL VECTOR (RZERO,IONE)
      NRMAX = IZERO
      CALL ERROR (252)
      RETURN
C
C     ==================================================================
C
      END
*ERRINT
      SUBROUTINE ERRINT (X,ERF,ERFC)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. ERRINT V 7.00  2/20/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM TO COMPUTE ERROR FUNCTION.
C
C               WRITTEN BY IRENE STEGUN AND RUTH ZUCKER.
C               ADAPTED TO OMNITAB BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /DMCONS/ DMAXDP, DMXINT, DSNCOS, DXEXP
      COMMON /DPICON/ DHLFPI, DPI, DSQRPI, D2BYSP
      COMMON /IMCONS/ IEXP, MMXINT, MXINT, NBC, NBM, NSBB
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      DOUBLE PRECISION ERF, ERFC, X
      DOUBLE PRECISION AN, BN, CONS, C1, DN, F, FN, FNM1
      DOUBLE PRECISION FNM2, GN, GNM1, GNM2, P, PREV, RNBC, SCF
      DOUBLE PRECISION SUM, TN, TOLER, ULCF, WN, Y, YSQ
      DOUBLE PRECISION FDDIV, FDEXP
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DHLFPI, DPI, DSQRPI, D2BYSP
      DOUBLE PRECISION DMAXDP, DMXINT, DSNCOS, DXEXP
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA CONS / 0.83D0 /
C
C     ==================================================================
C
      RNBC = NBC
      TOLER = DTWO**(-NBM)
      IF (X) 20,10,20
  10  ERF = DZERO
      ERFC = DONE
      RETURN
  20  Y = DABS (X)
      YSQ = Y**2
      IF (Y-DONE) 50,50,30
  30  C1 = DTWO ** FDDIV (RNBC-DONE,DTWO,IND)
      ULCF = CONS * C1
      SCF = DTWO**(C1**2-RNBC)
      IF (Y-ULCF) 90,90,40
  40  ERF = DONE
      ERFC = RZERO
      GO TO 80
  50  SUM = RZERO
      DN = DONE
      TN = DONE
      P = DTWO * YSQ
  60  DN = DN + DTWO
      TN = P * FDDIV (TN,DN,IND)
      SUM = TN + SUM
      IF (TN-TOLER) 70,60,60
  70  ERF = (SUM+DONE) * D2BYSP * Y * FDEXP (-YSQ)
      ERFC = DONE - ERF
  80  IF (X.GE.RZERO) RETURN
      ERF = -ERF
      ERFC = DTWO - ERFC
      RETURN
C
C     ..................................................................
C
  90  FNM2 = RZERO
      GNM2 = DONE
      FNM1 = DTWO * Y
      GNM1 = DTWO * YSQ + DONE
      PREV = FDDIV (FNM1,GNM1,IND)
      WN   = DONE
      BN   = GNM1 + DFOR
 100  AN   = -WN * (WN+DONE)
      FN   = BN * FNM1 + AN * FNM2
      GN   = BN * GNM1 + AN * GNM2
      F    = FDDIV (FN,GN,IND)
      IF (DABS(DONE- FDDIV(F,PREV,IND))-TOLER) 150,150,110
 110  IF (PREV-F) 120,120,140
 120  IF (GN.LT.SCF) GO TO 130
      FN   = FDDIV (FN,SCF,IND)
      GN   = FDDIV (GN,SCF,IND)
      FNM1 = FDDIV (FNM1,SCF,IND)
      GNM1 = FDDIV (GNM1,SCF,IND)
 130  FNM2 = FNM1
      GNM2 = GNM1
      FNM1 = FN
      GNM1 = GN
      WN   = WN + DTWO
      BN = BN + DFOR
      PREV = F
      GO TO 100
 140  F = PREV
 150  ERFC = FDDIV (F*FDEXP(-YSQ)*D2BYSP,DTWO,IND)
      ERF  = DONE - ERFC
      GO TO 80
C
C     ==================================================================
C
      END
*ERROR
      SUBROUTINE ERROR (NBRERR)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  ERROR V 7.00 11/ 7/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     WRITE ERROR MESSAGE ON SCRATCH UNIT.
C        CALL PROGRAM UNIT TO PRINT ERROR MESSAGE, IF IN DEMAND MODE.
C
C        IF   1 .LE. I .LE. 100, FATAL ERROR
C        IF 101 .LE. I .LE. 200, ARITHMETIC FAULT
C        IF 201 .LE. I .LE. 300, INFORMATIVE DIAGNOSTIC
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ERRMES/ ISE, KMES, LLIST, MESS(15), MNOE, NERR, NRM, NROLD
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT
      COMMON /IOUNIT/ LPTAPE
      COMMON /REPMOD/ ARGTAB(100), COM(2000), INDEX(6,8), LEVEL, NSTMTH
      COMMON /SWITCH/ IHCNT, ILABEL, ISBFT, KRDKNT, NCRT, NDEMD, NLOCRM
      COMMON /TOPRNT/ IPRINT, ISIGD, LENGTH, LWC, LWIDE, NCW         
C
C     ==================================================================
C
      I      = NBRERR
      ISCRUN = ISCRT
      INFB   = IZERO
      INFC   = NROLD
      IF (I.EQ.IZERO) GO TO 70
C
      NERR   = NERR + IONE
      IF (I.GT.IHRD) GO TO 30
      NERROR = NERROR + IONE
      INFB   = LLIST
      INFA   = -IONE
      LLIST  = ITHRE
      IF (I.EQ.ITEN) INFA = NARGS
      IF (I.EQ.29)   INFA = NARGS + IONE
      IF (I.EQ.17 .OR. I.EQ.11 .OR. I.EQ.42) INFB = NCOL
      IF (I.EQ.16 .OR. I.EQ.17) INFA = NROW
      IF (I.EQ.34) INFA = NRMAX
      IF (I.EQ.34) INFB = NRM
      WRITE (ISCRUN,120) I, INFA, INFB, INFC
 10   IF (IPRINT .NE. NPRNT) THEN
        IHOLD = IPRINT
        IPRINT = NPRNT
        CALL ERRPRT
        IPRINT = IHOLD
      ELSE
        CALL ERRPRT
      ENDIF
      IF (LEVEL.NE.IZERO) CALL RNDOWN
C
C     FORCE OUT OF REPEAT MODE, IF FATAL ERROR.
C
      IF (I.LE.IHRD) LEVEL = IZERO
      WRITE (ISCRUN,130)
      RETURN
C
C     ..................................................................
C
  30  IF (NERR.LE.MNOE .OR. LLIST.NE.ITHRE) GO TO 40
      IF (ISE.NE.IZERO) RETURN
      ISE = IONE
      WRITE (ISCRUN,140) MNOE
      RETURN
C
C     ..................................................................
C
  40  IF (I.GT.200) GO TO 50
C
C     ARITHMETIC FAULTS, SET FLAGS.
C
      IARGMT = I - IHRD
      J = MIN0 (IARGMT,KMES)
      MESS(J) = MESS(J) + IONE
      RETURN
C
C     ..................................................................
C
C     INFORMATIVE DIAGNOSTICS.
C
  50  IF (MOD(LLIST,ITWO).EQ.IZERO) RETURN
      IF (LLIST.EQ.IZERO) RETURN
      INFA = IZERO
      IF (I.EQ.237) INFA = ISIGD
      IF (I.EQ.244) INFA = NARGS
      IF (I.EQ.252 .AND. NRMAX.EQ.NROLD) RETURN
      IF (I.EQ.252) INFA = NRMAX
      IF (I.EQ.254) INFA = IARGS(1)
      IF (I.EQ.255) INFA = LENGTH
      IF (I.EQ.256) INFA = NARGS
      IF (I.EQ.201) INFA = NROW
      IF (I.EQ.213 .OR. I.EQ.218 .OR. I.EQ.226 .OR. I.EQ.231
     1     .OR. I.EQ.243) INFA = NROW
      IF (I.EQ.213) INFC = NCOL
      IF (I.EQ.230 .OR. I.EQ.240 .OR. I.EQ.215) INFA = NRMAX
      IF (I.EQ.240) INFC = IARGS(1)
      IF (I.GE.245 .AND. I.LE.247) INFA = LWIDE
      IF (I.NE.257) GO TO 60
      INFA  = NRMAX
      INFB = NRM
  60  WRITE (ISCRUN,150) I, INFA, INFB, INFC
      GO TO 10
C
C     ..................................................................
C
C     INSTRUCTION EXECUTED, DUMP RESULTS.
C
  70  IF (LLIST.LT.ITWO .OR. LLIST.EQ.IFOUR) GO TO 100
      DO 90 K=1,KMES
        J = K
        IF (MESS(J).EQ.IZERO) GO TO 90
        WRITE (ISCRT,160)
        WRITE(ISCRT,170) J, MESS(J), IONE, INFC
        IF (NDEMD.EQ.IZERO) GO TO 80
        JPRINT = IPRINT
        IPRINT = NPRNT
        CALL ERRPRT
        IPRINT = JPRINT
  80    IF (LEVEL.NE.IZERO) CALL RNDOWN
        WRITE (ISCRT,160)
        MESS(J) = IZERO
  90  CONTINUE
C
      IF (LEVEL.NE.IZERO) RETURN
      ISE = IZERO
      NERR = IZERO
      RETURN
C
C     ..................................................................
C
 100  DO 110 K=1,KMES
        MESS(K) = IZERO
 110  CONTINUE
C
      ISE = IZERO
      NERR   = IZERO
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 120  FORMAT (3H***,4I5,61X)
 130  FORMAT (84X)
 140  FORMAT (  / 7X,1H*,I4,57H INFORMATIVE AND ARITH. DIAGNOSTICS HAVE 
     1BEEN ENCOUNTERED/7X,
     263H*  ANY SUCH ADDITIONAL DIAGNOSTICS FOR THIS COMMAND OR REPEAT  
     3 /7X,24H*  MODE ARE DISREGARDED.,39X)
 150  FORMAT (3H*  ,4I5,61X)
 160  FORMAT (70X)
 170  FORMAT (3H** ,4I5,7X)
C
C     ==================================================================
C
      END
*ERRPRT
      SUBROUTINE  ERRPRT
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/20/81. ERRPRT V 7.00  1/18/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     BACKSPACE ISCRT, REREAD IT AND PRINT THE PROPER ERROR MESSAGE.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IST(3)
C
      COMMON /ABCDEF/ LA(74)
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /IOUNIT/ INUNIT, IPUNCH, ISCRT, KBDOUT, LTAPE, MPRNT, NPRNT
      COMMON /IOUNIT/ LPTAPE
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
      CHARACTER  LA*1
      CHARACTER IST*1
C
C     ==================================================================
C
      ISCRUN = ISCRT
      BACKSPACE ISCRUN
      READ (ISCRUN,500) IST(1), IST(2), IST(3), I, INFA, INFB, INFC
      J = IZERO
      DO 10 KI=1,3
        IF (IST(KI).EQ.LA(41))  J = J + IONE
  10  CONTINUE
C
      IF (I.GT.300) GO TO 400
C
      GO TO (100,200,300),J
C
C     ..................................................................
C
C     PRINT INFORMATIVE DIAGNOSTIC MESSAGES.
C
 100  CALL INFERR (I,INFA,INFC)
      RETURN
C
C     ..................................................................
C
C     PRINT ARITHMETIC FAULT MESSAGES.
C
 200  CALL RTHERR (I,INFA)
      RETURN
C
C     ..................................................................
C
C     PRINT FATAL ERROR MESSAGES.
C
 300  CALL FTLERR (INFA,INFB,I)
      RETURN
C
C     ..................................................................
C
C     PRINT CALCOMP ERROR MESSAGES.
C
 400  CALL CALERR (I)
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 500  FORMAT (3A1,4I5)
C
C     ==================================================================
C
      END
*EVALOM
      SUBROUTINE EVALOM
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. EVALOM V 7.00 10/26/89. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     EXECUTE EVALUATE INSTRUCTION ...
C        EVALUATE  (EQUATION)
C
C     (A)  NO COMMENTS OR DESCRIPTIVE WORDS ARE PERMITTED.
C     (B)  INSTRUCTION CANNOT NOT BE USED IN THE REPEAT MODE.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -     MAY, 1978.
C                   CURRENT VERSION - OCTOBER, 1989.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***

      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /SCNCRD/ ARG, ARG2, KARG, KRDPOS, MODE
      COMMON /SCNLCD/ LENCRD, LKARD, KARD(83), KRDEND     
      COMMON /SCNCHR/ NEWCRD(80)
C
C     ==================================================================
C
C                    ***   TYPE STATEMENTS   ***
      CHARACTER NEWCRD*1
C
C     ==================================================================

C
C     ERROR CHECKING.
C
      IF (NRMAX.LE.IZERO) CALL ERROR ( 9)
      IF (NARGS.LE.IONE)  CALL ERROR (10)
      DO 10 I=1,NARGS
        IF (KIND(I).EQ.IONE) GO TO 10
        II = I
        CALL ADRESS (II,IARGS(I))
  10  CONTINUE
      IF (KIND(1).EQ.IONE) CALL ERROR (20)
      IF (NERROR.NE.IZERO) RETURN
C
C     ..................................................................
C
C     LOCATE BEGINNING OF EQUATION.
C
      I = ITHRE
      CALL NONBLA (I,ICHAR)
      CALL BLANK
C
C     CALL COMPIL TO EVALUATE EQUATION.
C
      I     = KRDPOS - ITWO
      ISTOP = KRDEND + IONE - I
      CALL COMPIL (NEWCRD(I),ISTOP,NRMAX)
      RETURN
C
C     ==================================================================
C
      END
*EXINVT
      SUBROUTINE EXINVT
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. EXINVT V 7.00  2/20/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     CALL APPROPRIATE PROGRAM UNIT WHICH CALLS PROGRAM UNIT INVERT.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -    MARCH, 1978.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
C
C     ==================================================================
C
      IF (L1.NE.16) GO TO 10
        CALL INVERT
        RETURN
C
C     ..................................................................
C
  10  IF (L1.NE.24) GO TO 20
        CALL CORREL
        RETURN
C
C     ..................................................................
C
  20    CALL MPROP
        RETURN
C
C     ==================================================================
C
      END
*EXPAND
      SUBROUTINE EXPAND (J,WHERE)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. EXPAND V 7.00  2/20/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS ROUTINE EXPANDS STORED COMMANDS FROM WHERE TO A USABLE
C         FORM IN ARGS, IARGS AND KIND.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /ARGMTS/ ARGS(100), IARGS(100), KIND(100), NARGS
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /INSTRN/ L1, L2, NCOL, NERROR, NRMAX, NROW
      COMMON /RCONST/ RFIVE, RFOR, RHALF, RONE, RTEN, RTHRE, RTWO, RZERO
C
      REAL             WHERE(*)
      REAL             T, Y
      REAL             SPCA
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA SPCA / 8192.0 /
C
      DATA  ICA / 8192   /
C
C     ==================================================================
C
      II = IZERO
      I = IZERO
      JJJ = J
C
C     CONVERT ONLY FIRST ARGUMENT, IF COMMAND IS INCREMENT OR RESTORE.
C
      IF (L1.NE.14) GO TO 10
      IF (L2.GE.6 .AND. L2.LE.8) JJJ = ITWO
  10  II = II + IONE
  20  I = I + IONE
      IF (I.GE.JJJ) RETURN
      T = WHERE(I)
      IF (T) 50,40,30
  30  KIND(II) = IZERO
      IARGS(II) = T - SPCA
      GO TO 10
  40  KIND(II) = IONE
      I = I + IONE
      ARGS(II) = WHERE(I)
      GO TO 10
  50  IF (T.EQ.(-RONE)) GO TO 60
      CALL XPND (WHERE(I),K,Y,KND)
      ARGS(II) = Y
      IF (K.LT.IZERO) RETURN
      KIND(II) = KND
      IF (KND.EQ.IZERO) IARGS(II) = ARGS(II)
      I = I + K
      GO TO 10
C
C     IF STORED VALUE = -1, THEN ARGS (INTEGER) ARE TO BE EXPANDED FROM
C        PREVIOUS ARG TO FOLLOWING WITH A MAXIMUM TOTAL OF 70.
C
  60  I = I + IONE
C
C     PICK UP NEXT ARG
C
      IU = WHERE(I)
      IF (KIND(II-1).NE.IZERO .OR. I.GE.J) GO TO 130
      IF (IU.LT.IZERO) GO TO 110
      IF (IU.EQ.IZERO) GO TO 130
      IU = IU - ICA
  70  K = IU - IARGS(II-1)
      NARGS = NARGS + IABS(K) - IONE
      IF (NARGS.GT.IHRD) GO TO 140
      IF (K.EQ.IZERO) GO TO 20
      IF (K.GT.IZERO) GO TO 80
      INC = -IONE
      K = -K
      GO TO 90
  80  INC = IONE
  90  DO 100 IT=1,K
        KIND(II) = IZERO
        IARGS(II) = IARGS(II-1) + INC
        II = II + IONE
 100  CONTINUE
      GO TO 20
C
C     EXPAND FORM IARG  ***  ''ARG''
C
 110  CALL XPND (WHERE(I),K,Y,KND)
      Y = ARGS(II)
      IF (K.LT.IZERO) RETURN
      I = I + K
      IF (KND.EQ.IZERO) GO TO 120
      CALL ERROR (20)
      RETURN
C
C     ..................................................................
C
 120  IU = ARGS(II)
      GO TO 70
 130  CALL ERROR (211)
      GO TO 10
 140  CALL ERROR (10)
      RETURN
C
C     ==================================================================
C
      END
*EXPINT
      SUBROUTINE EXPINT (RN,X,ENX,EXPENX,IERR)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. EXPINT V 7.00  2/20/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C
C     LANGUAGE.  AMERICAN NATIONAL STANDARD FORTRAN
C
C     DEFINITIONS.
C       EN(X)= INTEGRAL (EXP(-X*T)DT/(T**N)), FROM 1 TO INFINITY
C                 RN(=N), POSITIVE INTEGER
C                      X, REAL AND POSITIVE
C
C      SPECIAL CASES
C       EN(0)=INFINITY(=RINF, MAXIMUM MACHINE VALUE)    N .LE. 1
C       EN(0)= 1/N-1     N .GT. 1
C
C       E0(X)= EXP(-X)/X    X .GT. 1/RINF
C       E0(X)= INFINITY     X .LE. 1/RINF
C
C     USAGE.     CALL EXPINT (RN,X,ENX,EXPENX,IERR)
C
C      FORMAL PARAMETERS
C       RN  REAL OR DOUBLE PRECISION TYPE                  INPUT
C           FOR A POSITIVE INTEGER N
C       X                          (SAME TYPE AS RN)       INPUT
C       ENX= EN(X)                  (SAME TYPE AS X)      OUTPUT
C       EXPENX= EXP(X)*EN(X)        (SAME TYPE AS X)      OUTPUT
C       IERR INTEGER VARIABLE                             OUTPUT
C             NORMAL RETURN  IERR= 0
C             ERROR RETURN   IERR= 1, X AND/OR N NEGATIVE
C                        ENX=EXPENX=-INFINITY (IMPOSSIBLE VALUE)
C                            IERR= 2, N NON-INTEGER
C                        ENX=EXPENX=INFINITY
C
C      MODIFICATIONS
C       DOUBLE PRECISION UNIVAC 1108 RESULTS ARE OBTAINED IF AS
C          SET UP BELOW WHERE
C
C         NBM=ACCURACY DESIRED OR MAXIMUM NUMBER OF BINARY
C              DIGITS IN THE MANTISSA OF A FLOATING POINT NUMBER
C
C       WITH
C        (1) THE DOUBLE PRECISION TYPE STATEMENT
C        (2) THE MAXIMUM MACHINE VALUE AND THE MAXIMUM INTEGER
C            (=DMXINT) CONVERTIBLE TO A FLOATING POINT NUMBER
C            GIVEN AS DOUBLE PRECISION CONSTANTS
C        (3) DOUBLE PRECISION DECIMAL CONSTANTS
C        (4) DATA STATEMENT NBM=60 FOR THE CONTROL VARIABLE
C        (5) FUNCTION TYPE STATEMENTS - FDEXP, FDLOG.
C
C       SINGLE PRECISION UNIVAC 1108 RESULTS ARE OBTAINED BY
C          (1) DELETING THE DOUBLE PRECISION TYPE STATEMENT
C          (2) ADJUSTING MAXIMUM MACHINE VALUE
C          (3) CHANGING THE D'S TO E'S ON THE DATA CARDS FOR ALL
C              DECIMAL CONSTANTS
C          (4) SETTING NBM=27 IN THE CONTROL VARIABLE DATA
C          (5) CHANGING FUNCTION TYPE - EXP, ALOG.
C
C         FOR OTHER COMPUTERS THE APPROPRIATE VALUE OF NBM MUST
C         BE INSERTED AND ALL VALUES ADJUSTED ACCORDINGLY.
C
C       IF A PRECOMPUTED VALUE OF TOLER(=2**(-NBM)) IS
C          INCLUDED IN A DATA STATEMENT, COMPUTATION OF THE CONTROL
C          VARIABLE MAY BE OMITTED AND THE DATA STATEMENT FOR NBM
C          DELETED.
C
C       CAUTION - THE SUBROUTINE CANNOT READILY BE ADAPTED TO
C                 COMPUTE THE EXPONENTIAL INTEGRAL FOR A COMPLEX
C                 ARGUMENT AS THE CONTINUED FRACTION IS INVALID
C                 ALONG THE NEGATIVE REAL AXIS. IN ADDITION MANY
C                 OF THE COMPARISONS BECOME MEANINGLESS.
C
C     METHOD.
C      POWER SERIES,    X .LE. 1(=ULPS, UPPER LIMIT FOR POWER
C                                 SERIES)
C       ENX = SUM(TM),   M=0,1,2,...,K
C        TM =-((-X)**M/1*2*3...M)/(M-N+1=D)      M .NE. N-1
C           = PTERM/D
C        TM = PTERM*(LOG(X)-PSI(N))    M .EQ. N-1
C         PTERM(0)=-1
C         PTERM(M+1)= PTERM(M)*(-X)/(M+1)
C         PSI(N)= -EULER+1+1/2+ ... +1/(N-1)
C         IF R.E.(=ABS(TM/SUM)) .LE. TOLER (=2**(-NBM)),     M=K
C
C      CONTINUED FRACTION,    X .GT. 1
C       EN(X)=EXP(-X)*(1I/I(X+N)-   1*N I/I(X+N+2)-
C                                2*(N+1)I/I(X+N+4)-...
C       EN(X)=EXP(-X)*II(AM I/I BM)        M=1,2,...,K
C         AM(1)=1    AM(M)=-(M-1)*(N+M-2)
C         BM(M)=X+N+2*(M-1)
C
C       EN(X)=EXP(-X)*FM(K)/GM(K)=EXP(-X)*F(K)
C            IF R.E. (=ABS(1-PREV/F)) .LE. TOLER(=2**(-NBM)),M=K
C         F=FM/GM     PREV=FMM1/GMM1
C
C     RANGE.
C      FOR EXP(X)*EN(X)    (N+X) .LE. MAXIMUM MACHINE VALUE
C      FOR EN(X)   X=APPROXIMATELY 85.0, SINGLE PRECISION
C                                 704.0, DOUBLE PRECISION
C      BEYOND THIS RANGE EN(X)=0
C
C     ACCURACY.  THE NUMBER OF ACCURATE BINARY DIGITS IS ESSEN-
C                TIALLY THE LESSER OF
C                    NBM - SQUARE ROOT OF NBM
C                    NBM - I(NUMBER OF BINARY DIGITS REPRESENT-
C                          ING THE INTEGER PART OF X)
C                          (ACCURACY OF THE EXPONENTIAL ROUTINE)
C
C     PRECISION. VARIABLE - BY SETTING THE DESIRED NBM OR TOLER.
C
C     MAXIMUM TIME.              UNIVAC 1108-EXEC II
C        (SECONDS)                NBM=27    NBM=60
C                                  .0025     .015
C
C     STORAGE.   COMPILED BY THE UNIVAC 1108, EXEC 8/FORTRAN V,
C                THIS SUBROUTINE REQUIRES 478 WORDS OF STORAGE.
C
C               WRITTEN BY -
C                      IRENE STEGUN AND RUTH ZUCKER
C               ADAPTED TO OMNITAB BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      COMMON /DCONST/ DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      COMMON /DMCONS/ DMAXDP, DMXINT, DSNCOS, DXEXP
      COMMON /DPICON/ DHLFPI, DPI, DSQRPI, D2BYSP
      COMMON /DTCONS/ DALOG2, DEULER
      COMMON /ICONST/ IFIVE, IFOUR, IHRD, IONE, ITEN, ITHRE, ITWO, IZERO
      COMMON /IMCONS/ IEXP, MMXINT, MXINT, NBC, NBM, NSBB
C
      DOUBLE PRECISION ENX, EXPENX, RN
      DOUBLE PRECISION AM, ASUM, BM, D, EXPNX, F, FM, FMM1, FMM2
      DOUBLE PRECISION GM, GMM1, GMM2, ONPTFV, PREV, PSI, PTERM
      DOUBLE PRECISION RM, RNM1, RRN, SUM, T, TEMP
      DOUBLE PRECISION TM, TOLER, ULPS, X, XLOG
      DOUBLE PRECISION FDDIV, FDEXP, FDLOG
      DOUBLE PRECISION DEHT, DFOR, DHALF, DONE, DSIX, DTHRE, DTWO, DZERO
      DOUBLE PRECISION DHLFPI, DPI, DSQRPI, D2BYSP
      DOUBLE PRECISION DMAXDP, DMXINT, DSNCOS, DXEXP
      DOUBLE PRECISION DALOG2, DEULER
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA ONPTFV, ULPS / 1.5D0, 1.0D0 /
C
C     ==================================================================
C
      TOLER = DTWO ** (-NBM)
C
C         VALIDITY TEST FOR INPUT PARAMETERS
C            ERROR CONDITION, IMPOSSIBLE VALUES RETURNED.
C
C                NEGATIVE ZERO CHECKS.
C
      IERR = IZERO
      IF (RN .LT. DZERO) GO TO 30
      IF (RN .GT. DMXINT) GO TO 40
C
C                VALIDITY TEST FOR N INTEGER.
C
      N = RN
      RRN = N
      IF (RRN) 20,10,20
  10  IF ((RN-RRN)-TOLER) 40,40,20
  20  T = FDDIV (RRN,RN,IND)
      IF (IND.NE.IZERO) CALL ERROR (106)
      IF ((DONE-T).LE.TOLER) GO TO 40
      RRN = RRN + DONE
      T = FDDIV (RN,RRN,IND)
      IF (IND.NE.IZERO) CALL ERROR (106)
      IF ((DONE-T).LE.TOLER) GO TO 40
C
C              ERROR RETURN
C                 N NON-INTEGER.
C
      IERR   = ITWO
      ENX    = DMAXDP
      EXPENX = ENX
      RETURN
C
C     ..................................................................
C
C
  30  IF (-RN) 60,40,60
C
  40  IF (X) 50,80,70
  50  IF (-X) 60,80,60
C
C              ERROR RETURN
C                 X AND/OR N NEGATIVE.
C
  60  IERR   = IONE
      ENX    = -DMAXDP
      EXPENX = ENX
      RETURN
C
C     ..................................................................
C
C            FUNCTION TYPE STATEMENTS.
C
  70  EXPNX = FDEXP (-X)
      XLOG  = FDLOG (X)
C
      IF (RN .GE. DHALF) GO TO 110
C
C            SPECIAL CASES.
C
      T = FDDIV (DONE,DMAXDP,IND)
      IF (IND.NE.IZERO) CALL ERROR (106)
      IF (X .LE. T) GO TO 90
      EXPENX = FDDIV (DONE,X,IND)
      IF (IND.NE.IZERO) CALL ERROR (106)
      ENX = EXPNX * EXPENX
      RETURN
C
C     ..................................................................
C
C
  80  IF (RN .LT. ONPTFV) GO TO 90
      ENX = FDDIV (DONE,RN-DONE,IND)
      IF (IND.NE.IZERO) CALL ERROR (106)
      GO TO 100
C
  90  ENX    = DMAXDP
 100  EXPENX = ENX
      RETURN
C
C     ..................................................................
C
C
 110  IF (RN .GT. DMXINT) GO TO 120
      IF (X .LE. ULPS) GO TO 130
 120  IF (X .LE. (DMAXDP-RN)) GO TO 210
      EXPENX = DZERO
      ENX    = EXPENX
      RETURN
C
C     ..................................................................
C
C        METHOD --- POWER SERIES.
C
 130  RM    = DZERO
      PTERM = -DONE
      SUM   = DZERO
      PSI   = -DEULER
      D     = -(RN-DONE)
C
 140  IF (D .GE. DHALF) GO TO 160
        IF (-D .GE. DHALF) GO TO 150
C
C            COMPUTE TM FOR M .EQ. N-1.
C
        SUM = PTERM * (XLOG-PSI) + SUM
        GO TO 200
C
C            COMPUTE PSI(N).
C
 150    PSI = PSI + FDDIV (DONE,RM+DONE,IND)
      IF (IND.NE.IZERO) CALL ERROR (106)
C
C            COMPUTE TM FOR M .NE. N-1.
C
 160    TM = FDDIV (PTERM,D,IND)
      IF (IND.NE.IZERO) CALL ERROR (106)
        SUM = TM + SUM
C
C            TOLERANCE CHECK.
C
C                ZERO CHECKS.
C
        IF (SUM .LT. DZERO) GO TO 170
        ASUM = SUM
        GO TO 180
C
 170    ASUM = -SUM
 180    IF (ASUM) 190,200,190
 190    IF (TM .LT. DZERO) TM = -TM
      T = FDDIV (TM,ASUM,IND)
      IF (IND.NE.IZERO) CALL ERROR (106)
        IF ( T .GT. TOLER) GO TO 200
C
      ENX =  SUM
      EXPENX = FDDIV (ENX,EXPNX,IND)
      IF (IND.NE.IZERO) CALL ERROR (106)
      RETURN
C
C            ADDITIONAL TERMS.
C
 200    RM = RM + DONE
        D = D + DONE
      PTERM = -FDDIV (X*PTERM,RM,IND)
      IF (IND.NE.IZERO) CALL ERROR (106)
        GO TO 140
C
C        METHOD --- CONTINUED FRACTION.
C
 210  RM   = DONE
      FMM2 = DONE
      GMM2 = DZERO
      FMM1 = DZERO
      GMM1 = DONE
      PREV = FDDIV (FMM1,GMM1,IND)
      AM   = DONE
      BM   = X + RN
      RNM1 = RN - DONE
C
 220  FM = BM * FMM1 + AM * FMM2
      GM = BM * GMM1 + AM * GMM2
      F = FDDIV (FM,GM,IND)
      IF (IND.NE.IZERO) CALL ERROR (106)
C
C            TOLERANCE CHECK.
C
      TEMP = DONE - FDDIV (PREV,F,IND)
      IF (IND.NE.IZERO) CALL ERROR (106)
        IF (TEMP .GT. DZERO) GO TO 230
C
      EXPENX = PREV
      GO TO 240
C
 230    IF (TEMP .GT. TOLER) GO TO 250
C
      EXPENX = F
 240  ENX = EXPNX * EXPENX
      RETURN
C
C     ..................................................................
C
 250  T = FDDIV (DMAXDP,BM,IND)
      IF (IND.NE.IZERO) CALL ERROR (106)
      IF (GM.LT.T)  GO TO 260
C
C            SCALING.
C
C     BOTH FM AND GM MUST BE TESTED IF N=1 AND X .LT. .44.
C        SCALING SHOULD NOT BE DELETED AS THE VALUES OF FM AND GM
C        MAY OVERFLOW FOR SOME CHOICES OF THE PARAMETERS.
C
            FMM1 = FDDIV (FMM1,BM,IND)
      IF (IND.NE.IZERO) CALL ERROR (106)
            GMM1 = FDDIV (GMM1,BM,IND)
      IF (IND.NE.IZERO) CALL ERROR (106)
            FM   = FDDIV (FM,BM,IND)
      IF (IND.NE.IZERO) CALL ERROR (106)
            GM   = FDDIV (GM,BM,IND)
      IF (IND.NE.IZERO) CALL ERROR (106)
C
C            ADDITIONAL CONVERGENTS.
C
 260    AM   = -RM * (RNM1+RM)
        RM   = RM + DONE
        BM   = BM + DTWO
        FMM2 = FMM1
        GMM2 = GMM1
        FMM1 = FM
        GMM1 = GM
        PREV = F
        GO TO 220
C
C     ==================================================================
C
      END
