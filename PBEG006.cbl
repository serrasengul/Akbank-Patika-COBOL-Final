       IDENTIFICATION DIVISION. 
       PROGRAM-ID. PBE006.
       AUTHOR.     Begum Serra Sengul.
       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT OUT-FILE  ASSIGN TO OUT-FILE
                            STATUS OUT-ST.
           SELECT INP-FILE  ASSIGN TO INP-FILE
                            STATUS INP-ST.
       DATA DIVISION. 
       FILE SECTION. 
       FD  OUT-FILE RECORDING MODE F.
       01  OUT-REC.
           03 OUT-FILLER-FNC      PIC X(7).
           03 OUT-FNC-TYPE        PIC X(10).
           03 OUT-FILLER-ID       PIC X(10).
           03 OUT-ID              PIC 9(5).
           03 OUT-DVZ             PIC 9(3).
           03 OUT-FILLER-RC       PIC X(10).
           03 OUT-RC              PIC 9(2).
           03 OUT-FILLER-DSC      PIC X(10).
           03 OUT-DSC             PIC X(30).
           03 OUT-FILLER-DATA     PIC X(10).
           03 OUT-DATA            PIC X(60).
       
       FD  INP-FILE RECORDING MODE F.
       01  INP-REC.
           03 INP-FUNC-TYPE       PIC X(1).
           03 INP-ID              PIC 9(5).
           03 INP-DVZ             PIC 9(3).
       WORKING-STORAGE SECTION. 
       01  WS-WORK-AREA.
           03 WS-PBEGIDX           PIC X(7) VALUE 'PBEGIDX'.
           03 OUT-ST               PIC P(2).
              88 OUT-SUCCESS                VALUE 00 97.
           03 INP-ST               PIC 9(2).
              88 INP-SUCCESS                VALUE 00 97.
              88 INP-EOF                    VALUE 10.
           03 WS-SUB-TYPE          PIC X(1).
              88 WS-SUB-TYPE-VALID          VALUE 'W' 'R' 'U' 'D'.
           03 WS-SUB-AREA.
              05 WS-SUB-FUNC       PIC X(1).
                 88 WS-FUNIS-WRITE          VALUE 'W'.
                 88 WS-FUNIS-READ           VALUE 'R'.
                 88 WS-FUNIS-UPDATE         VALUE 'U'.
                 88 WS-FUNIS-DELETE         VALUE 'D'.
               05 WS-SUB-ID        PIC 9(5).
               05 WS-SUB-DVZ       PIC 9(3).
               05 WS-SUB-RC        PIC 9(2).
               05 WS-SUB-DSC       PIC X(30).
               05 WS-SUB-DATA      PIC X(60).
              
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM H100-OPEN-FILES.
           PERFORM H200-PROCESS UNTIL INP-EOF.
           PERFORM H999-PROGRAM-EXIT.
       0000-END. EXIT.

       H100-OPEN-FILES.
           OPEN INPUT INP-FILE.
           OPEN OUTPUT OUT-FILE.
           IF (NOT OUT-SUCCESS)
           DISPLAY 'UNABLE TO OPEN OUTFILE: ' OUT-ST 
           MOVE OUT-ST TO RETURN-CODE 
           PERFORM H999-PROGRAM-EXIT
           END-IF.
           IF (NOT INP-SUCCESS)
           DISPLAY 'UNABLE TO READ INPFILE: ' INP-ST 
           MOVE INP-ST TO RETURN-CODE 
           PERFORM H999-PROGRAM-EXIT
           END-IF.     
       H100-END. EXIT.

       H200-PROCESS.
           READ INP-FILE 
              AT END SET INP-EOF TO TRUE 
           END-READ.
           IF INP-EOF 
              PERFORM H999-PROGRAM-EXIT
           END-IF.
           MOVE INP-FUNC-TYPE TO WS-SUB-TYPE.
           MOVE INP-ID TO WS-SUB-ID.
           MOVE INP-DVZ TO WS-SUB-DVZ.
           MOVE ZEROS TO WS-SUB-RC.
           MOVE SPACES TO WS-SUB-DSC.
           MOVE SPACES TO WS-SUB-DATA.
           EVALUATE WS-SUB-TYPE 
              WHEN 'R'
              SET WS-FUNIS-READ TO TRUE
              WHEN 'U'
              SET WS-FUNIS-UPDATE TO TRUE
              WHEN 'W'
              SET WS-FUNIS-WRITE TO TRUE
              WHEN 'D'
              SET WS-FUNIS-DELETE TO TRUE
              WHEN OTHER 
              MOVE 99 TO WS-SUB-RC 
              MOVE 'IVALID SUB-TYPE: ' TO WS-SUB-DSC 
              PERFORM H300-PROCESS-OUTPUT
              PERFORM H999-PROGRAM-EXIT
           END-EVALUATE.
           CALL WS-PBEGIDX USING WS-SUB-AREA.
           PERFORM H300-PROCESS-OUTPUT.
       H200-END. EXIT.

       H300-PROCESS-OUTPUT.
           MOVE WS-SUB-TYPE TO OUT-FNC-TYPE 
           MOVE WS-SUB-ID   TO OUT-ID.
           MOVE WS-SUB-DVZ  TO OUT-DVZ.
           MOVE WS-SUB-RC   TO OUT-RC .
           MOVE WS-SUB-DSC  TO OUT-DSC.
           IF (WS-SUB-DATA NOT = SPACES)
              MOVE WS-SUB-DATA TO OUT-DATA  
           ELSE 
              MOVE 'FUNCTION NOT SUPPERTED' TO OUT-DATA  
           END-IF.
           
           MOVE 'FUNC: '   TO OUT-FILLER-FNC.
           MOVE 'ID: '     TO OUT-FILLER-ID.
           MOVE 'RC: '     TO OUT-FILLER-RC.
           MOVE 'DESC: '   TO OUT-FILLER-DSC.
           MOVE 'DATA: '   TO OUT-FILLER-DATA.

           INSPECT OUT-REC REPLACING ALL LOW-VALUES BY SPACES.
           WRITE OUT-REC.
           IF (NOT OUT-SUCCESS)
              DISPLAY 'UANBLE TO WRITE OUTFILE: ' OUT-ST
              MOVE OUT-ST TO RETURN-CODE 
              PERFORM H999-PROGRAM-EXIT
           END-IF.
       H300-END. EXIT.

       H999-PROGRAM-EXIT.
           CLOSE OUT-FILE.
           CLOSE INP-FILE.
           DISPLAY 'PROGRAM EXIT WITH RETURN-CODE: ' WS-SUB-RC.
           STOP RUN.
       H999-END. EXIT.