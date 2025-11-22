       IDENTIFICATION DIVISION.
       PROGRAM-ID. GAME.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SAVE-FILE ASSIGN TO 'game.save'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.
           
           SELECT DIALOGUE-FILE ASSIGN TO 'dialogue.txt'
               ORGANISATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD SAVE-FILE
           RECORD CONTAINS 100 CHARACTERS
           DATA RECORD IS SAVE-RECORD.
       01 SAVE-RECORD              PIC X(100).

       FD DIALOGUE-FILE
           RECORD CONTAINS 500 CHARACTERS
           DATA RECORD IS DIALOGUE-RECORD.
       01 DIALOGUE-RECORD          PIC X(500).

       WORKING-STORAGE SECTION.
       01 WS-EOF-SAVE-FLAG         PIC X(1) VALUE 'N'.
           88 EOF-SAVE-REACHED              VALUE 'Y'.
       01 WS-SAVE-RECORD-COUNT     PIC 9(2) VALUE 0.

       01 WS-EOF-DIALOGUE-FLAG     PIC X(1) VALUE 'N'.
           88 EOF-DIALOGUE-REACHED          VALUE 'Y'.
       01 WS-DIALOGUE-RECORD-COUNT PIC 9(2) VALUE 0.

       01 WS-GAME-QUIT             PIC X(1) VALUE 'N'.
           88 GAME-QUIT                     VALUE 'Y'.

       01 USER-INPUT               PIC X(80).

       01 INPUT-VALID-FLAG         PIC X(1) VALUE 'N'.
           88 INPUT-VALID                   VALUE 'Y'
                                   WHEN SET TO FALSE IS 'N'.

      *We define a GAME-STATE AND MULTIPLE conditions TO determine the
      *current state of the game.
       01 GAME-STATE               PIC X(1) VALUE 'A'.
           88 MAIN-MENU            VALUE 'A'.
           88 EXPLORING            VALUE 'B'.
       
      *We define a TABLE that will hold world information and dialogue.
       01 WORLD-TABLE.
           02 DIALOGUE             PIC X(500) OCCURS 10 TIMES.

       01 PLAYER-DATA.
           02 PLAYER-HEALTH        PIC ZZ9.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM UNTIL GAME-QUIT
               PERFORM RECEIVE-USER-INPUT
           END-PERFORM.

           STOP RUN.

       INITIALIZE-WORLD-TABLE.
           OPEN INPUT DIALOGUE-FILE.
           
           PERFORM UNTIL EOF-DIALOGUE-REACHED
               READ DIALOGUE-FILE
                   AT END
                       SET EOF-DIALOGUE-REACHED TO TRUE
                   NOT AT END
                       MOVE DIALOGUE-RECORD TO 
                           DIALOGUE(WS-DIALOGUE-RECORD-COUNT)
                       ADD 1 TO WS-DIALOGUE-RECORD-COUNT
               END-READ
           END-PERFORM.

           CLOSE DIALOGUE-FILE.
       
       RECEIVE-USER-INPUT.
           IF MAIN-MENU
               PERFORM MAIN-MENU-ROUTINE
           ELSE IF EXPLORING
               PERFORM EXPLORING-ROUTINE
           END-IF.

       MAIN-MENU-ROUTINE.
           PERFORM INITIALIZE-WORLD-TABLE.

           DISPLAY FUNCTION TRIM(DIALOGUE(1)).

           PERFORM UNTIL INPUT-VALID
               DISPLAY "Welcome adventurer! Please select an option by "
                       "typing the number into the command line:"
               DISPLAY "1: Load Game"
               DISPLAY "2: New Game"

               ACCEPT USER-INPUT

               SET INPUT-VALID TO TRUE
               IF USER-INPUT = "1"
                   PERFORM LOAD-GAME-ROUTINE
               ELSE IF USER-INPUT = "2"
                   PERFORM NEW-GAME-ROUTINE
               ELSE
                   SET INPUT-VALID TO FALSE
                   DISPLAY "Invalid input!"
                   DISPLAY " "
               END-IF
           END-PERFORM.
       
       LOAD-GAME-ROUTINE.
           OPEN INPUT SAVE-FILE.

           PERFORM UNTIL EOF-SAVE-REACHED
               READ SAVE-FILE
                   AT END
                       SET EOF-SAVE-REACHED TO TRUE
                   NOT AT END
                       PERFORM LOAD-SAVE
               END-READ
           END-PERFORM.

           CLOSE SAVE-FILE.

           SET EXPLORING TO TRUE.

           DISPLAY " ".
       
       LOAD-SAVE.
      *    The first line is the player's health.
           IF WS-SAVE-RECORD-COUNT = 0
               MOVE SAVE-RECORD TO PLAYER-HEALTH
           END-IF.

           ADD 1 TO WS-SAVE-RECORD-COUNT.
       
       NEW-GAME-ROUTINE.
           DISPLAY "CREATING NEW GAME".

       EXPLORING-ROUTINE.
           DISPLAY "You wake up in a dimly lit room. You can see an "
                   "old wooden door. What do you want to do?".

           ACCEPT USER-INPUT.
