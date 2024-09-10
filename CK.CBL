0001  $SET ANS85 SIGN"ASCII" MODEL"HUGE" MF QUAL NATIVE"ASCII"
0002  $SET NOALTER ZEROSEQ NOCHECK
0003   IDENTIFICATION DIVISION.
0004   PROGRAM-ID. NEWCKBK.
0005   AUTHOR. ANTHONY PERRELLA.
0006   ENVIRONMENT DIVISION.
0007   CONFIGURATION SECTION.
0008   SOURCE-COMPUTER. INTEL-25MHZ-386.
0009   OBJECT-COMPUTER. INTEL-25MHZ-386.
0010   INPUT-OUTPUT SECTION.
0011   FILE-CONTROL.
0012       SELECT CKBKFILE
0013         ASSIGN TO DISK
0014         ORGANIZATION IS LINE SEQUENTIAL
0015         ACCESS MODE IS SEQUENTIAL
0016         FILE STATUS IS WS-CKBKFL-STAT.
0017   DATA DIVISION.
0018   FILE SECTION.
0019   FD  CKBKFILE
0020       LABEL RECORDS ARE STANDARD
0021       VALUE OF FILE-ID IS WS-FILE-NAME
0022       DATA RECORD IS CKBK-REC.
0023   01  CKBK-REC.
0024       02 CKBK-NUMB PIC X(10).
0025       02 CKBK-DATE.
0026          03 CKBK-YEAR PIC 99.
0027          03 CKBK-MONTH PIC 99.
0028          03 CKBK-DAY PIC 99.
0029       02 CKBK-MERCHANT PIC X(12).
0030       02 CKBK-ITEM.
0031          03 CKBK-ITEM1 PIC X(7).
0032          03 CKBK-ITEM2.
0033             04 FILLER PIC X.
0034             04 CKBK-ITEM-RATE.
0035                05 CKBK-RATE1 PIC 99.
0036                05 FILLER PIC X.
0037                05 CKBK-RATE2 PIC 99.
0038             04 FILLER PIC X.
0039             04 CKBK-DOLLARS PIC 9(4).
0040             04 FILLER PIC X(3).
0041             04 CKBK-IDCODE PIC X.
0042       02 CKBK-AMOUNT PIC S9(5)V99.
0043   WORKING-STORAGE SECTION.
0044   77  WS-CKBK-AMOUNT PIC S99999V99 VALUE ZEROS.
0045   77  WS-BOT-AMOUNT  PIC ZZZZ9.99CR VALUE ZERO.
0046  * 77  WS-FILE-NAME PIC X(12) VALUE "BARNETT.DAT ".
0047  *         The following counter is used to identify
0048  *         the index of the top row on the screen.
0049   77  CUR-TAB-ROW PIC 9(4) COMP VALUE 1.
0050  *         The following counter is used to identify
0051  *         the row of the cursor line
0052   77  CUR-CRT-ROW PIC 9(4) COMP VALUE 1.
0053  *         The following counter is used to remember
0054  *         the index of the last record in the table.
0055   77  LAST-TAB-ROW PIC 9(4) COMP.
0056   77  I            PIC 9(4) COMP VALUE ZEROS.
0057   77  TEMP         PIC 9(4) COMP VALUE ZEROS.
0058   77  THE-ROW      PIC 9(4) COMP.
0059   77  WS-REMARKS   PIC X(42) VALUE "TONY MESSAGE".
0060   77  WS-ONE       PIC 999  VALUE 001.
0061   77  WS-SPACES    PIC X(42) VALUE SPACES.
0062  ***********************************************************
0063  *         COLOR CODES
0064  ***********************************************************
0065   78  BLACK;         VALUE IS 0.
0066   78  BLUE;          VALUE IS 1.
0067   78  GREEN;         VALUE IS 2.
0068   78  CYAN;          VALUE IS 3.
0069   78  RED;           VALUE IS 4.
0070   78  MAGENTA;       VALUE IS 5.
0071   78  BROWN;         VALUE IS 6.
0072   78  WHITE;         VALUE IS 7.
0073   78  GRAY;          VALUE IS 8.
0074   78  GREY;          VALUE IS 8.
0075   78  LIGHT-BLUE;    VALUE IS 9.
0076   78  LIGHT-GREEN;   VALUE IS 10.
0077   78  LIGHT-CYAN;    VALUE IS 11.
0078   78  LIGHT-RED;     VALUE IS 12.
0079   78  LIGHT-MAGENTA; VALUE IS 13.
0080   78  YELLOW;        VALUE IS 14.
0081   78  BRIGHT-WHITE;  VALUE IS 15.
0082  ***********************************************************
0083  *
0084  *        The following value is the maximum number of
0085  *        records which may be stored in the
0086  *        tables.  This should be no larger than
0087  *        64k / 80 = 800  (which is excessive anyway).
0088  ***********************************************************
0089   78  MAX-TABLE-RECORDS;  VALUE IS 800.
0090  ***********************************************************
0091  *
0092  *        The following value is in the range 0 and up.
0093  *        It is added to the starting row to build
0094  *        the actual row on screen.
0095  ***********************************************************
0096   78  CRT-ROW-OFFSET;      VALUE IS  3.
0097  ***********************************************************
0098  *        This value determines how many records are displayed.
0099  ***********************************************************
0100   78  MAX-ROWS-DISPLAYED; VALUE IS 15.
0101  ***********************************************************
0102  *        The following constants represent
0103  *        the keystrokes that cause the ACCEPT
0104  *        statement to terminate.
0105  ***********************************************************
0106   78  KEY-UP-ARROW;   VALUE IS 1.
0107   78  KEY-DOWN-ARROW; VALUE IS 4.
0108   78  KEY-DOWN-PAGE;  VALUE IS 5.
0109   78  KEY-UP-PAGE;    VALUE IS 6.
0110   78  KEY-F1;         VALUE IS 3.
0111   78  KEY-F2;         VALUE IS 7.
0112   78  KEY-F3;         VALUE IS 8.
0113   78  KEY-F4;         VALUE IS 9.
0114   78  KEY-F5;         VALUE IS 10.
0115   78  KEY-F6;         VALUE IS 11.
0116   78  KEY-F10;        VALUE IS 2.
0117  ***********************************************************
0118  *     Dad's checkbook records.
0119  ***********************************************************
0120   01  WORK-AREAS.
0121       02  PREVIOUS-BALANCE PIC S9(5)V99 VALUE ZEROS.
0122       02  TEMP-BALANCE PIC S9(5)V99 VALUE ZEROS.
0123       02  TEMP-AMOUNT PIC S9(5)V99 VALUE ZEROS.
0124       02  BALANCE-AMOUNT PIC S9(5)V99 VALUE ZEROS.
0125       02  CRT-BALANCE PIC S9(5)V99 VALUES ZEROS.
0126       02  SAVE-NUMB PIC X(10) VALUE SPACES.
0127       02  SAVE-CALDATE PIC 9(6) VALUE ZEROS.
0128       02  FALSESW PIC X VALUE SPACE.
           02  RECORD-NUMB PIC 9(4) VALUE ZEROS.
0129   01  WS-CKBKFL-STAT.
0130       02  WS-INSTAT1 PIC X VALUE SPACES.
0131       02  WS-INSTAT2 PIC X VALUE SPACES.
0132   01  WS-FILE-NAME.
0133       02  WS-DRIVE-ID PIC XX VALUE "C:".
0134       02  WS-DIR-ID PIC X(7) VALUE "\CHECK\".
0135       02  WS-FILE-ID PIC X(12) VALUE "BARNETT.DAT ".
0136   01  CALDATE.
0137       02 CALDATE-DAY   PIC 99 VALUE ZEROS.
0138       02 CALDATE-MONTH PIC 99 VALUE ZEROS.
0139       02 CALDATE-YEAR  PIC 99 VALUE ZEROS.
0140  /
0141  ***********************************************************
0142  *        The following is used to identify the
0143  *        function keys that will be accepted by
0144  *        this program.
0145  ***********************************************************
0146   01  KEY-STATUS.
0147       05  KEY-FUNCTION PIC 99 COMP VALUE 0.
0148       05  KEY-PARAM.
0149           10  RETURN-BYTE PIC 99 COMP.
0150  * ARROW UP (1)
0151           10  FILLER PIC 99 COMP VALUE 2.
0152           10  FILLER PIC 99 COMP VALUE 0.
0153           10  FILLER PIC 99 COMP VALUE 72.
0154  * F10	 (2)
0155           10  FILLER PIC 99 COMP VALUE 2.
0156           10  FILLER PIC 99 COMP VALUE 0.
0157           10  FILLER PIC 99 COMP VALUE 68.
0158  * F1	 (3)
0159           10  FILLER PIC 99 COMP VALUE 2.
0160           10  FILLER PIC 99 COMP VALUE 0.
0161           10  FILLER PIC 99 COMP VALUE 59.
0162  * DOWN ARROW (4)
0163           10  FILLER PIC 99 COMP VALUE 2.
0164           10  FILLER PIC 99 COMP VALUE 0.
0165           10  FILLER PIC 99 COMP VALUE 80.
0166  * PAGE DOWN (5)
0167           10  FILLER PIC 99 COMP VALUE 2.
0168           10  FILLER PIC 99 COMP VALUE 0.
0169           10  FILLER PIC 99 COMP VALUE 81.
0170  * PAGE UP   (6)
0171           10  FILLER PIC 99 COMP VALUE 2.
0172           10  FILLER PIC 99 COMP VALUE 0.
0173           10  FILLER PIC 99 COMP VALUE 73.
0174  * F2	  (7)
0175           10  FILLER PIC 99 COMP VALUE 2.
0176           10  FILLER PIC 99 COMP VALUE 0.
0177           10  FILLER PIC 99 COMP VALUE 60.
0178  * F3	  (8)
0179           10  FILLER PIC 99 COMP VALUE 2.
0180           10  FILLER PIC 99 COMP VALUE 0.
0181           10  FILLER PIC 99 COMP VALUE 61.
0182  * F4	  (9)
0183           10  FILLER PIC 99 COMP VALUE 2.
0184           10  FILLER PIC 99 COMP VALUE 0.
0185           10  FILLER PIC 99 COMP VALUE 62.
0186  * F5	  (10)
0187           10  FILLER PIC 99 COMP VALUE 2.
0188           10  FILLER PIC 99 COMP VALUE 0.
0189           10  FILLER PIC 99 COMP VALUE 63.
0190  * F6	  (11)
0191           10  FILLER PIC 99 COMP VALUE 2.
0192           10  FILLER PIC 99 COMP VALUE 0.
0193           10  FILLER PIC 99 COMP VALUE 64.
0194  * END OF LIST
0195           10  FILLER PIC 99 COMP VALUE 0.
0196  ***********************************************************
0197  *        The following is used to check CRT-STATUS
0198  *        after an accept statement is used.
0199  *        The following is used as input to the
0200  *        SCROLL program to determine what direction
0201  *        to scroll, what starting row, and what
0202  *        ending row.
0203  ***********************************************************
0204   01  SCROLLING-INFO.
0205       05  SCROLL-DIR PIC S9(4) COMP.
0206       05  START-ROW  PIC 9(4)  COMP.
0207       05  END-ROW    PIC 9(4)  COMP.
0208  /
0209  ***********************************************************
0210  *         The following is a table used to hold each data
0211  *         record that will be edited.
0212  * KNOWN BUG:
0213  *    THE LAST FIELD IN THE RECORD MUST HAVE A VALUE
0214  * THAT COMPLETELY FILLS IT'S INDICATED PICTURE SIZE.
0215  * FOR EXAMPLE, A FIELD OF PIC XXX  MUST HAVE A VALUE
0216  * THREE CHARACTERS LONG (I.E. "   " OR EVEN "ABC").
0217  ***********************************************************
0218   01  TEXT-TABLE.
0219       02  TEXT-LINE OCCURS MAX-TABLE-RECORDS TIMES.
0220           05  TEXT-NUMB PIC X(10).
0221           05  FILLER PIC X VALUE "%".
0222           05  TEXT-DATE.
0223               10  TEXT-DAY PIC XX.
0224               10  TEXT-DASH1 PIC X.
0225               10  TEXT-MONTH PIC XX.
0226               10  TEXT-DASH2 PIC X.
0227               10  TEXT-YEAR PIC XX.
0228           05  FILLER PIC X VALUE "%".
0229           05  TEXT-MERCHANT PIC X(12).
0230           05  FILLER PIC X VALUE "%".
0231           05  TEXT-ITEM PIC X(22).
0232           05  FILLER PIC X VALUE "%".
0233           05  TEXT-AMOUNT PIC ZZZZZ.99CR.
0234           05  FILLER PIC X VALUE "%".
0236           05  TEXT-BALANCE PIC ZZZZZ.99DB VALUE ZEROS.
0238  /
0239  ***********************************************************
0240  *        The following section contains record descriptions
0241  *        for status lines and various prompts that appear
0242  *        during operation.
0243  ***********************************************************
0244   SCREEN SECTION.
0245  ***********************************************************
0246  *  The heading lines follow
0247  ***********************************************************
0248   01  HEADING-LINES.
0249       02  BLANK SCREEN.
0250       02  LINE 1 COL 31 VALUE "DAD'S CHECKBOOK"
0251           FOREGROUND-COLOR IS BLACK
0252           BACKGROUND-COLOR IS MAGENTA.
0253       02  LINE2 LINE 2.
0254           05  COL 1 VALUE "CHK-NUMBER"
0255               FOREGROUND-COLOR IS BLACK
0256               BACKGROUND-COLOR IS GREEN.
0257           05  COL 12 VALUE "DATE    "
0258               FOREGROUND-COLOR IS BLACK
0259               BACKGROUND-COLOR IS GREEN.
0260           05  COL 21 VALUE "MERCHANT    "
0261               FOREGROUND-COLOR IS BLACK
0262               BACKGROUND-COLOR IS GREEN.
0263           05  COL 34 VALUE "ITEM                  "
0264               FOREGROUND-COLOR IS BLACK
0265               BACKGROUND-COLOR IS GREEN.
0266           05  COL 57 VALUE "AMOUNT    "
0267               FOREGROUND-COLOR IS BLACK
0268               BACKGROUND-COLOR IS GREEN.
0269           05  COL 68 VALUE "BALANCE   "
0270               FOREGROUND-COLOR IS BLACK
0271               BACKGROUND-COLOR IS GREEN.
0272       02  LINE22.
0273           05  LINE 22 COL 1 VALUE " ***********READ MESSAGE---->"
0274               FOREGROUND-COLOR IS BLUE
0275               BACKGROUND-COLOR IS WHITE.
0276           05  COL 31 PIC X(42) USING WS-REMARKS
0277               FOREGROUND-COLOR IS CYAN
0278               BACKGROUND-COLOR IS RED.
0279       02  LINE23.
0280           05  LINE 23 COL 1 VALUE "*** GROUP LEFT OVER AMOUNT-> "
0281               FOREGROUND-COLOR IS WHITE
0282               BACKGROUND-COLOR IS BROWN.
0283           05  COL 31 PIC X(10) USING WS-BOT-AMOUNT
0284               FOREGROUND-COLOR IS BLACK
0285               BACKGROUND-COLOR IS CYAN.
0286       02  LINE24 LINE 24.
0287           05  VALUE "F1 - HELP   "
0288               FOREGROUND-COLOR IS BLACK
0289               BACKGROUND-COLOR IS GREEN.
0290           05  VALUE " ".
0291           05  VALUE "F2 - DELETE "
0292               FOREGROUND-COLOR IS BLACK
0293               BACKGROUND-COLOR IS GREEN.
0294           05  VALUE " ".
0295           05  VALUE "F3 - INSERT "
0296               FOREGROUND-COLOR IS BLACK
0297               BACKGROUND-COLOR IS GREEN.
0298           05  VALUE " ".
0299           05  VALUE "F4 - ADD    "
0300               FOREGROUND-COLOR IS BLACK
0301               BACKGROUND-COLOR IS GREEN.
0302           05  VALUE " ".
0303           05  VALUE "            "
0304               FOREGROUND-COLOR IS BLACK
0305               BACKGROUND-COLOR IS GREEN.
0306           05  VALUE " ".
0307           05  VALUE "F10- EXIT   "
0308               FOREGROUND-COLOR IS BLACK
0309               BACKGROUND-COLOR IS GREEN.
0310   01  BLANK-SCREEN.
0311           05  BLANK SCREEN.
0312   01  BLANK-LINE.
0313           05  BLANK LINE.
0314   01  STATUS-LINE.
0315       02  LINE25.
0316           05  LINE 25 VALUE "TABLE ROW:"
0317               BACKGROUND-COLOR IS BROWN.
0318           05  PIC 9(4) FROM CUR-TAB-ROW.
0319           05  VALUE " CRT ROW:"
0320               BACKGROUND-COLOR IS BROWN.
0321           05  PIC 9(4) FROM CUR-CRT-ROW.
0322           05  VALUE " LAST ROW:"
0323               BACKGROUND-COLOR IS BROWN.
0324           05  PIC 9(4) FROM LAST-TAB-ROW.
0322           05  VALUE " RECORD NO. "
0323               BACKGROUND-COLOR IS BLUE
                   HIGHLIGHT.
0324           05  PIC 9(4) FROM RECORD-NUMB
0323               BACKGROUND-COLOR IS BLUE
                   HIGHLIGHT.
0325   01  WS-LINE22-REMSPACE.
0326       02  LINE 22 COLUMN 38 PIC X(42) USING WS-SPACES.
0327  /
0328  ***********************************************************
0329  *   P R O C E D U R E   D I V I S I O N.
0330  ***********************************************************
0331   PROCEDURE DIVISION.
0332   BEGIN.
0333  *****************************************************
0334  * INITIALIZE THE TABLE OF TEXT RECORDS              *
0335  *****************************************************
0336       MOVE 1 TO I.
0337       MOVE ZERO TO LAST-TAB-ROW.
0338       PERFORM 020-NEWID-ROUTINE THRU 020A-EXIT.
0558       PERFORM 020B-READ-CKREC THRU 020B-EXIT.
0338       PERFORM 030-BALANCE-CHECK THRU 030-EXIT.
0340   005-INITIALIZATION.
0341       DISPLAY HEADING-LINES.
           PERFORM UPDATE-STATUS.
0343       MOVE 1 TO CUR-TAB-ROW.
0344       PERFORM DISPLAY-PAGE.
0345  ***********************************
0346  * Set-up the function key tables.
0347  *****************************************************
0348       CALL X"B0" USING KEY-FUNCTION, KEY-PARAM.
0349  /
0350  *****************************************************
0351  *          MAIN INPUT LOOP                          *
0352  *****************************************************
0353   MAIN-LOOP.
0354       PERFORM UPDATE-STATUS.
0355       COMPUTE THE-ROW = CUR-TAB-ROW + CUR-CRT-ROW - 1.
0356       COMPUTE I = CUR-CRT-ROW + CRT-ROW-OFFSET.
0357       ACCEPT TEXT-LINE (THE-ROW)
0358            WITH HIGHLIGHT
0359            FOREGROUND-COLOR WHITE
0360            BACKGROUND-COLOR BLUE
0361            AT LINE I COL 1.
0362  * Now, erase the highlight bar.
0363       DISPLAY TEXT-LINE (THE-ROW)
0364            AT LINE I COL 1.
0365  *****************************************************
0366  * CHECK FOR SPECIAL KEYS.
0367  *****************************************************
0368       EVALUATE RETURN-BYTE
0369       WHEN 0
0370           COMPUTE I = CUR-CRT-ROW + CUR-TAB-ROW - 1
0371           EVALUATE TRUE
0372           WHEN CUR-CRT-ROW = MAX-ROWS-DISPLAYED
0373             PERFORM SCROLL-UP
0374             ADD 1 TO CUR-TAB-ROW
0375           WHEN I LESS THAN LAST-TAB-ROW
0376             ADD 1 TO CUR-CRT-ROW
0377           END-EVALUATE
0378       WHEN KEY-DOWN-ARROW
0379           COMPUTE I = CUR-CRT-ROW + CUR-TAB-ROW - 1
0380           EVALUATE TRUE
0381           WHEN CUR-CRT-ROW = MAX-ROWS-DISPLAYED
0382             PERFORM SCROLL-UP
0383             ADD 1 TO CUR-TAB-ROW
0384           WHEN I LESS THAN LAST-TAB-ROW
0385             ADD 1 TO CUR-CRT-ROW
0386           END-EVALUATE
0387       WHEN KEY-UP-ARROW
0388           EVALUATE TRUE
0389           WHEN CUR-CRT-ROW = 1 AND CUR-TAB-ROW > 1
0390             PERFORM SCROLL-DOWN,
0391             SUBTRACT 1 FROM CUR-TAB-ROW
0392           WHEN CUR-CRT-ROW > 1
0393             SUBTRACT 1 FROM CUR-CRT-ROW
0394           END-EVALUATE
0395       WHEN KEY-F10
0396           DISPLAY BLANK-SCREEN
0397           DISPLAY "KEEP YOUR $20." AT LINE 23 COL 1
0398           STOP RUN
0399       WHEN KEY-F1
0400           DISPLAY "F1 PRESSED - NO HELP AVAILABLE YET"
0401           AT LINE 23 COL 1
0402       WHEN KEY-DOWN-PAGE
0403           COMPUTE I = LAST-TAB-ROW - MAX-ROWS-DISPLAYED
0404           IF CUR-TAB-ROW <= I THEN
0405             ADD MAX-ROWS-DISPLAYED TO CUR-TAB-ROW
0406             MOVE 1 TO CUR-CRT-ROW
0407           END-IF
0408           PERFORM DISPLAY-PAGE
0409       WHEN KEY-UP-PAGE
0410           COMPUTE THE-ROW = CUR-TAB-ROW + CUR-CRT-ROW - 1
0411           COMPUTE I = MAX-ROWS-DISPLAYED
0412           IF CUR-TAB-ROW > I
0413             SUBTRACT I FROM CUR-TAB-ROW
0414                    ELSE
0415             MOVE 1 TO CUR-TAB-ROW
0416           END-IF
0417           PERFORM DISPLAY-PAGE
0418       END-EVALUATE.
0419	   GO TO MAIN-LOOP.
0420  /
0421  *****************************************************
0422  *           S U B R O U T I N E S                   *
0423  *****************************************************
0424  ***********************************************************
0425  *  These two routines are used to scroll the scrolling
0426  *  region up or down.
0427  *  Inputs:
0428  *      CRT-ROW-OFFSET
0429  *      MAX-ROWS-DISPLAYED
0430  *  Uses:
0431  *      SCROLL-DIR
0432  *      START-ROW
0433  *      END-ROW
0434  ***********************************************************
0435   SCROLL-UP.
0436	   MOVE 1 TO SCROLL-DIR.
0437	   COMPUTE START-ROW = CRT-ROW-OFFSET + 1.
0438	   COMPUTE END-ROW   = START-ROW + MAX-ROWS-DISPLAYED - 1.
0439	   CALL "SCROLL" USING SCROLL-DIR, START-ROW, END-ROW.
0440   SCROLL-DOWN.
0441	   MOVE -1 TO SCROLL-DIR.
0442	   COMPUTE START-ROW = CRT-ROW-OFFSET + 1.
0443	   COMPUTE END-ROW   = START-ROW + MAX-ROWS-DISPLAYED - 1.
0444	   CALL "SCROLL" USING SCROLL-DIR, START-ROW, END-ROW.
0445  ***********************************************************
       UPDATE-STATUS.
           COMPUTE RECORD-NUMB = CUR-TAB-ROW + CUR-CRT-ROW - 1.
0342       DISPLAY STATUS-LINE.
0445  ***********************************************************
0446  * This routine will display the page beginning at
0447  * the CUR-TAB-ROW
0448  * Inputs:
0449  * CUR-TAB-ROW, CRT-ROW-OFFSET, MAX-ROWS-DISPLAYED, TEXT-LINE
0450  * LAST-TAB-ROW
0451  * Uses:
0452  *        I , TEMP, and THE-ROW.
0453  ***********************************************************
0454   DISPLAY-PAGE.
0455	   MOVE CUR-TAB-ROW TO THE-ROW.
0456       MOVE 1 TO I.
0457       COMPUTE TEMP = CRT-ROW-OFFSET + 1.
0458       PERFORM UNTIL (I > MAX-ROWS-DISPLAYED)
0459         OR (THE-ROW > LAST-TAB-ROW)
0460         DISPLAY TEXT-LINE (THE-ROW) AT LINE TEMP COL 1
0461         ADD 1 TO TEMP, THE-ROW, I
0462       END-PERFORM.
0463  * Now, blank out the remainder of the screen.
0464  *
0465       PERFORM UNTIL I > MAX-ROWS-DISPLAYED
0466           DISPLAY BLANK-LINE AT LINE TEMP COL 1
0467           ADD 1 TO TEMP, I
0468       END-PERFORM.
0469       DISPLAY STATUS-LINE.
0470   008A-CHECK-DATE.
0471       MOVE TEXT-YEAR (I) TO CALDATE-YEAR.
0472       MOVE TEXT-MONTH (I) TO CALDATE-MONTH.
0473       MOVE TEXT-DAY (I) TO CALDATE-DAY.
0474   008A1-BRAKE.
0475       IF CALDATE IS NOT NUMERIC
0476         GO TO 008A2-BADATE.
0477       IF CALDATE-YEAR IS LESS THAN 82
0478         GO TO 008A2-BADATE.
0479       IF CALDATE-MONTH IS GREATER THAN 12 OR LESS THAN 1
0480         GO TO 008A2-BADATE.
0481       IF CALDATE-DAY IS GREATER THAN 31 OR LESS THAN 1
0482         GO TO 008A2-BADATE.
0483  *     IF CALDATE < TEXT-DATE (I)
0484  *       GO TO 008A2-BADATE.
0485       GO TO 008A-EXIT.
0486   008A2-BADATE.
0487  *     MOVE SPACES TO TEXT-DATE (I).
0488       MOVE "***BAD DATE***" TO WS-REMARKS.
0489       DISPLAY LINE22.
0490   008A-EXIT.
0491       EXIT.
0492   008C-NUMB.
0493  *     MOVE SPACES TO CRT-NUMB.
0494  *     MOVE 03 TO COL.
0495  *     DISPLAY (LIN, COL), CRT-NUMB.
0496   008C-DATE.
0497  *     MOVE SPACES TO CRT-DATE.
0498  *     MOVE 14 TO COL.
0499  *     DISPLAY (LIN, COL), CRT-DATE.
0500   008C-ITEM.
0501  *     MOVE SPACES TO CRT-ITEM.
0502  *     MOVE 36 TO COL.
0503  *     DISPLAY (LIN, COL), CRT-ITEM.
0504   008C-EXIT.
0505       EXIT.
0506   008C-MESSAGE.
0507       DISPLAY LINE22.
0508       ACCEPT FALSESW.
0509       MOVE SPACES TO WS-REMARKS.
0510       DISPLAY LINE22.
0511  * 008C-EXIT.
0512   020-NEWID-ROUTINE.
0513       DISPLAY BLANK-SCREEN.
0514       MOVE SPACES TO WS-FILE-ID.
0515       DISPLAY "     *** FILE IDENTIFICATION ***".
0516       DISPLAY " ".
0517       DISPLAY "NOTE: DEFAULT <FILE ID> IS <BARNETT.DAT>".
0518       DISPLAY " OR ENTER <NFCU.DAT>".
0519       DISPLAY " ".
0520       DISPLAY "ENTER NEW <FILE-ID>".
0521       DISPLAY "        OR".
0522       DISPLAY " PRESS <RETURN> TO ACCEPT DEFAULT FILE.".
0523       ACCEPT WS-FILE-ID.
0524       IF WS-FILE-ID = SPACE
0525          MOVE "BARNETT.TST " TO WS-FILE-ID.
0526   020A-OPENDISK.
0527       OPEN INPUT CKBKFILE.
0528       IF WS-INSTAT1 < 2
0529         GO TO 020A-EXIT.
0530       DISPLAY BLANK-SCREEN.
0531       DISPLAY "FILE-ID-> ", WS-FILE-ID.
0532       MOVE SPACES TO WS-FILE-ID.
0533       DISPLAY "FILE NOT FOUND, ENTER NEW".
0534       DISPLAY " FILE-ID AND PRESS ENTER.".
0535       ACCEPT WS-FILE-ID.
0536       GO TO 020A-OPENDISK.
0537   020A-EXIT.
0538       EXIT.
0558   020B-READ-CKREC.
0559   020B1-READFILE.
0560       READ CKBKFILE AT END
0561         CLOSE CKBKFILE
0562         GO TO 020B-EXIT.
0563       MOVE CKBK-NUMB       TO TEXT-NUMB OF TEXT-LINE (I).
0564       MOVE CKBK-DAY        TO TEXT-DAY OF TEXT-LINE (I).
0565       MOVE "-"             TO TEXT-DASH1 OF TEXT-LINE (I).
0566       MOVE CKBK-MONTH      TO TEXT-MONTH OF TEXT-LINE (I).
0567       MOVE "-"             TO TEXT-DASH2 OF TEXT-LINE (I).
0568       MOVE CKBK-YEAR       TO TEXT-YEAR OF TEXT-LINE (I).
0569       MOVE CKBK-MERCHANT   TO TEXT-MERCHANT OF TEXT-LINE (I).
0570       MOVE CKBK-ITEM       TO TEXT-ITEM OF TEXT-LINE (I).
0571       MOVE CKBK-AMOUNT     TO TEXT-AMOUNT OF TEXT-LINE (I).
0572       ADD CKBK-AMOUNT      TO WS-CKBK-AMOUNT.
0573       MOVE WS-CKBK-AMOUNT  TO TEXT-BALANCE OF TEXT-LINE (I).
0574       ADD 1 TO I, LAST-TAB-ROW.
0575       GO TO 020B1-READFILE.
0576   020B-EXIT.
0577        EXIT.
0540   030-BALANCE-CHECK.
0541       MOVE 1 TO I.
0542       MOVE ZEROS TO SAVE-CALDATE, SAVE-NUMB.
0543       MOVE TEXT-AMOUNT (I) TO TEMP-AMOUNT.
0544       COMPUTE PREVIOUS-BALANCE = 0.
0545   030-BEGIN.
0546       IF TEXT-AMOUNT (I) = ZEROS
0547         GO TO 030-NEXT-REC.
0548       MOVE TEXT-DAY (I) TO CALDATE-DAY.
0549       MOVE TEXT-MONTH (I) TO CALDATE-MONTH.
0550       MOVE TEXT-YEAR (I) TO CALDATE-YEAR.
0551       IF TEXT-NUMB (I) = TO SAVE-NUMB AND
0552          CALDATE = SAVE-CALDATE
0553            MOVE PREVIOUS-BALANCE TO TEXT-BALANCE (I)
0554       ELSE
0555            MOVE TEXT-AMOUNT (I) TO TEMP-AMOUNT
0556            SUBTRACT TEMP-AMOUNT FROM PREVIOUS-BALANCE
0557                  GIVING TEMP-BALANCE
0558            MOVE TEMP-BALANCE TO TEXT-BALANCE (I)
0559            MOVE TEXT-BALANCE (I) TO PREVIOUS-BALANCE
0560            MOVE TEXT-NUMB (I) TO SAVE-NUMB
0561            MOVE CALDATE TO SAVE-CALDATE
0562       END-IF.
0563   030-NEXT-REC.
0564       ADD 1 TO I.
0565       IF I > LAST-TAB-ROW
0566         GO TO 030-EXIT.
0567       GO TO 030-BEGIN.
0568   030-EXIT.
0569       EXIT.
0570   040-WRITEFILE.
0571       MOVE 1 TO I.
0572       OPEN OUTPUT CKBKFILE.
0573   040-WRITE-REC.
0574       MOVE  TEXT-NUMB OF TEXT-LINE (I)      TO  CKBK-NUMB.
0575       MOVE  TEXT-DAY OF TEXT-LINE (I)       TO  CKBK-YEAR.
0576       MOVE  TEXT-MONTH OF TEXT-LINE (I)     TO  CKBK-MONTH.
0577       MOVE  TEXT-YEAR OF TEXT-LINE (I)      TO  CKBK-DAY.
0578       MOVE  TEXT-MERCHANT OF TEXT-LINE (I)  TO  CKBK-MERCHANT.
0579       MOVE  TEXT-ITEM OF TEXT-LINE (I)      TO  CKBK-ITEM.
0580       MOVE  TEXT-AMOUNT OF TEXT-LINE (I)    TO  CKBK-AMOUNT.
0581       WRITE CKBK-REC.
0582       ADD 1 TO I.
0583       IF I > LAST-TAB-ROW
0584         CLOSE CKBKFILE
0585         GO TO 040-EXIT.
0586       GO TO 040-WRITE-REC.
0587   040-EXIT.
0588        EXIT.
0589       END PROGRAM NEWCKBK.

