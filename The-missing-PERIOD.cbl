       IDENTIFICATION DIVISION.
       PROGRAM-ID. THE-MISSING-PERIOD.

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
           02 DIALOGUE             PIC X(500) OCCURS 100 TIMES.

       01 WS-STRING-POINTER        PIC 9(2) VALUE 1.
       
       01 AVAILABLE-ACTIONS.
           02 ACTION               PIC X(500) OCCURS 16 TIMES.
       01 CURRENT-ACTION-COUNTER   PIC 9(2) VALUE 1.
       01 CURRENT-ACTION-COLUMN    PIC 9(2) VALUE 1.
       01 ACTION-VALID-FLAG        PIC X(1) VALUE 'N'.
           88 ACTION-VALID                  VALUE 'Y'
                                   WHEN SET TO FALSE IS 'N'.

       01 CURRENT-DIALOGUE-INDEX   PIC 9(2) VALUE 1.

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
                           DIALOGUE(WS-DIALOGUE-RECORD-COUNT + 1)
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
      *        We check only the first character in the user input.
               IF USER-INPUT(1:1) = "1"
                   PERFORM LOAD-GAME-ROUTINE
               ELSE IF USER-INPUT(1:1) = "2"                            
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
           DISPLAY FUNCTION TRIM(DIALOGUE(CURRENT-DIALOGUE-INDEX)).

      *    We RESET all available actions and save the next available
      *    actions.    
           PERFORM RESET-AVAILABLE-ACTIONS.
           PERFORM INIT-AVAILABLE-ACTIONS.

           IF FUNCTION TRIM(ACTION(1)) NOT EQUAL "NONE"
               DISPLAY "------------------"
               DISPLAY "Available actions:"

               PERFORM DISPLAY-AVAILABLE-ACTIONS

               DISPLAY "Input: " WITH NO ADVANCING

               ACCEPT USER-INPUT

               DISPLAY " "

               PERFORM CHECK-ACTION-VALIDITY
           ELSE
               DISPLAY " "
               SET CURRENT-DIALOGUE-INDEX TO ACTION(2).
       
       RESET-AVAILABLE-ACTIONS.
           PERFORM UNTIL CURRENT-ACTION-COUNTER > 16
               MOVE " " TO ACTION(CURRENT-ACTION-COUNTER)
               ADD 1 TO CURRENT-ACTION-COUNTER
           END-PERFORM.

           MOVE 1 TO CURRENT-ACTION-COUNTER.
       
       INIT-AVAILABLE-ACTIONS.
           PERFORM UNTIL CURRENT-ACTION-COUNTER > 16
               UNSTRING DIALOGUE(CURRENT-DIALOGUE-INDEX + 1)
                   DELIMITED BY ";"
                   INTO ACTION(CURRENT-ACTION-COUNTER)
                   WITH POINTER WS-STRING-POINTER
               END-UNSTRING

               IF ACTION(CURRENT-ACTION-COUNTER) NOT EQUAL SPACES
                   ADD 1 TO CURRENT-ACTION-COUNTER
               ELSE
                   EXIT PERFORM
               END-IF
           END-PERFORM.

           MOVE 1 TO CURRENT-ACTION-COUNTER.
       
       DISPLAY-AVAILABLE-ACTIONS.
           PERFORM UNTIL CURRENT-ACTION-COUNTER > 16
               IF ACTION(CURRENT-ACTION-COUNTER) NOT EQUAL SPACES
                   DISPLAY FUNCTION TRIM(ACTION(CURRENT-ACTION-COUNTER))
                   ADD 2 TO CURRENT-ACTION-COUNTER
               ELSE
                   DISPLAY " "
                   EXIT PERFORM
               END-IF
           END-PERFORM.

           MOVE 1 TO CURRENT-ACTION-COUNTER.
       
       CHECK-ACTION-VALIDITY.
           PERFORM UNTIL CURRENT-ACTION-COUNTER > 16 OR ACTION-VALID
               IF USER-INPUT = ACTION(CURRENT-ACTION-COUNTER)
                   SET ACTION-VALID TO TRUE
               ELSE
                   ADD 2 TO CURRENT-ACTION-COUNTER
               END-IF
           END-PERFORM.

           IF ACTION-VALID
               SET CURRENT-DIALOGUE-INDEX TO
                   ACTION(CURRENT-ACTION-COUNTER + 1)
           ELSE
               DISPLAY "Invalid Input!"
               DISPLAY " ".
           
           SET ACTION-VALID TO FALSE.
           MOVE 1 TO CURRENT-ACTION-COUNTER.
