       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBEGIDX
       AUTHOR. Begüm Serra Sengül.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IDX-FILE   ASSIGN IDX-FILE
                             ORGANIZATION INDEXED
                             ACCESS MODE RANDOM
                             RECORD KEY IDX-KEY
                             STATUS IDX-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  IDX-FILE.
       01  IDX-REC.
           03 IDX-KEY.
              05 IDX-ID           PIC S9(05)   COMP-3.
              05 IDX-DVZ          PIC S9(03)   COMP.
           03 IDX-NAME            PIC X(30).
           03 IDX-DATE            PIC S9(7)    COMP-3.
           03 IDX-BALLANCE        PIC S9(15)   COMP-3.

       WORKING-STORAGE SECTION.
       01  WS-WORK-AREA.
           03 IDX-ST              PIC 9(2).
              88 IDX-SUCCESS            VALUE 00 97.
              88 IDX-NOTFND             VALUE 23.
           03 WS-PROCESS-AREA.
              05 WS-NAME          PIC X(15).
              05 WS-LNAME         PIC X(15).
              05 WS-INDEX-I       PIC 9(2).
              05 WS-INDEX-J       PIC 9(2).
              05 WS-UPDATED-NAME  PIC X(15).
              05 WS-UPDATED-LNAME PIC X(15).
              05 WS-FULLNAME      PIC X(30).
       LINKAGE SECTION.
       01  WS-SUB-AREA.
           05 WS-SUB-FUNC         PIC X(1).
           05 WS-SUB-ID           PIC 9(5).
           05 WS-SUB-DVZ          PIC 9(3).
           05 WS-SUB-RETC         PIC 9(2).
           05 WS-SUB-DSC          PIC X(30).
           05 WS-SUB-DATA         PIC X(60).

       PROCEDURE DIVISION USING WS-SUB-AREA.
       0000-MAIN.
           PERFORM H100-OPEN-FILES.
           PERFORM H200-PROCESS.
           PERFORM H999-PROGRAM-EXIT.
       0000-END. EXIT.

       H100-OPEN-FILES.
           OPEN I-O IDX-FILE.
           IF (NOT IDX-SUCCESS)
           MOVE IDX-ST TO WS-SUB-RETC
           MOVE 'UNABLE TO OPEN IDX FILE: ' TO WS-SUB-DSC
           PERFORM H999-PROGRAM-EXIT
           END-IF.
       H100-END. EXIT.

       H200-PROCESS.
           EVALUATE TRUE
             WHEN WS-SUB-FUNC = 'W'
                PERFORM H210-WRITE-DATA
             WHEN WS-SUB-FUNC = 'R'
                PERFORM H220-READ-DATA
             WHEN WS-SUB-FUNC = 'U'
                PERFORM H230-UPDATE-DATA
             WHEN WS-SUB-FUNC = 'D'
                PERFORM H240-DELETE-DATA
             WHEN OTHER
               MOVE 99 TO WS-SUB-RETC
               MOVE 'INVALID FUNCTION' TO WS-SUB-DSC
               PERFORM H999-PROGRAM-EXIT
           END-EVALUATE.
       H200-END. EXIT.

       H210-WRITE-DATA.
             MOVE WS-SUB-ID                              TO IDX-ID.
             MOVE WS-SUB-DVZ                             TO IDX-DVZ.
             MOVE 'BEGUM SERRA    SENGUL            '    TO IDX-NAME.
             MOVE 20001003                               TO IDX-DATE.
             MOVE 99999999                               TO IDX-BALLANCE.
             WRITE IDX-REC
                 INVALID KEY
                   MOVE 23 TO WS-SUB-RETC
                   MOVE 'UNABLE TO WRITE VSAM' TO WS-SUB-DSC
                   PERFORM H999-PROGRAM-EXIT
             END-WRITE.

             MOVE 'BEGUM SERRA    SENGUL            '    TO WS-SUB-DATA.
             MOVE 'OK'                                   TO WS-SUB-DSC.
       H210-END. EXIT.

       H220-READ-DATA.
           MOVE WS-SUB-ID TO IDX-ID.
           MOVE WS-SUB-DVZ TO IDX-DVZ.
           READ IDX-FILE KEY IS IDX-KEY
              INVALID KEY
                 MOVE IDX-ST TO WS-SUB-RETC
                 MOVE 'UNAVLE TO READ VSAM FILE' TO WS-SUB-DSC
                 PERFORM H999-PROGRAM-EXIT
           END-READ.
             MOVE IDX-NAME       TO WS-SUB-DATA.
             MOVE 'OK'           TO WS-SUB-DSC.
       H220-END. EXIT.

       H230-UPDATE-DATA.
           PERFORM H220-READ-DATA.
           PERFORM H231-PARSE-FNAME.
           PERFORM H232-UPDATE-FNAME.

           IF (WS-NAME = WS-UPDATED-NAME AND
                                            WS-LNAME = WS-UPDATED-LNAME)
              MOVE 'NOTHING TO UPDATE' TO WS-SUB-DSC
              MOVE IDX-NAME TO WS-SUB-DATA
              MOVE 23 TO WS-SUB-RETC
              PERFORM H999-PROGRAM-EXIT
           END-IF.
           STRING WS-UPDATED-NAME  WS-UPDATED-LNAME DELIMITED BY SIZE
                        INTO WS-FULLNAME.
           MOVE WS-FULLNAME TO IDX-NAME.
           REWRITE IDX-REC
              INVALID KEY
                MOVE IDX-ST TO WS-SUB-RETC
                MOVE 'UNABLE TO UPDATE VSAM FILE' TO WS-SUB-DSC
                PERFORM H999-PROGRAM-EXIT
           END-REWRITE.

       H231-PARSE-FNAME.
           MOVE IDX-NAME(1:15) TO WS-NAME.
           MOVE IDX-NAME(16:15) TO WS-LNAME.
       H231-END. EXIT.

       H232-UPDATE-FNAME.
           MOVE 1 TO WS-INDEX-I.
           MOVE 1 TO WS-INDEX-J.
           PERFORM UNTIL WS-INDEX-I > LENGTH OF WS-NAME
             IF WS-NAME(WS-INDEX-I:1) = SPACES
                ADD 1 TO WS-INDEX-I
             ELSE
                MOVE WS-NAME(WS-INDEX-I:1)
                                        TO WS-UPDATED-NAME(WS-INDEX-J:1)
                ADD 1 TO WS-INDEX-J
                ADD 1 TO WS-INDEX-I
             END-IF
           END-PERFORM.
           MOVE 1 TO WS-INDEX-I.
           MOVE 1 TO WS-INDEX-J.
           PERFORM UNTIL WS-INDEX-I > LENGTH OF WS-LNAME
             IF WS-LNAME(WS-INDEX-I:1) = SPACES
                ADD 1 TO WS-INDEX-I
             ELSE
                MOVE WS-LNAME(WS-INDEX-I:1)
                                       TO WS-UPDATED-LNAME(WS-INDEX-J:1)
                ADD 1 TO WS-INDEX-J
                ADD 1 TO WS-INDEX-I
             END-IF
           END-PERFORM.

       H230-END. EXIT.

       H240-DELETE-DATA.
           MOVE WS-SUB-ID TO IDX-ID.
           MOVE WS-SUB-DVZ TO IDX-DVZ.
           DELETE IDX-FILE
              INVALID KEY
                 MOVE IDX-ST TO WS-SUB-RETC
                 MOVE 'UNABLE TO DELETE VSAM' TO WS-SUB-DSC
                 PERFORM H999-PROGRAM-EXIT
           END-DELETE.
           MOVE 'OK' TO WS-SUB-DSC.
       H240-END. EXIT.

       H230-END. EXIT.
       H999-PROGRAM-EXIT.
           CLOSE IDX-FILE.
           EXIT PROGRAM.
       H999-END.
