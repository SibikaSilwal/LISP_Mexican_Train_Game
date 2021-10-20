(defun message(aMessage)
  (princ aMessage)
  (terpri)
)

; *********************************************
; Helper function for input validation
; *********************************************
(defun validateInput(validInputs input)
  (cond ( (member input validInputs )
            input )
        ( t "z")
  )
)

; **********************************************************
; Source Code to toss a coin and determine the first player
; **********************************************************

; User calls a side of the coin
(defun callASide()
  (message "Please enter 'H' for head and 'T' for tail.")
  (let ( (call (validateInput '(H T) (read) )) )
    (cond ( (string= call "z" ) (callASide) )
          (t call))
  )
)

; tossing a coin
(defun tossACoin()
  (setf *random-state* (make-random-state t))
  (let ( (head 0) (tail 1) ( toss (random 2) ) ( call (callASide) ) )
    (cond ((string= call "H" )
              (cond ((= toss head) 1)
                    (t -1)))
          ((string= call "T" )
              (cond ((= toss tail) 1)
                    (t -1)))
          (t -1 ))
  )
)

; determining the first player in a game
(defun declareFirstPlayer(hScore cScore)
  (cond 
    ; if both players scores are equal toss coin
    ((= hScore cScore)
      (cond 
        ((= (tossACoin) 1)
         (message "You won the toss. You play first.") (princ "Press any key to start the game...") (read)
         (ext:run-shell-command "cls") 'H)
        (t 
          (message "You lost the toss. Computer plays first") (princ "Press any key to start the game...") (read)
          (ext:run-shell-command "cls") 'C )))
    ; otherwise return the player with lower score
    (t (cond ((< hScore cScore) 'h)
             (t 'c)))    
  )
)

; ********************************************************************
; DECK: Source Code to generate deck of tiles, shuffle, and deal them.
; ********************************************************************


(defun createDeck(i j)
  (cond ( (and (= i -1) (= j -1)) ())

        ((= j -1) (createDeck (- i 1) (+ j i)))

        (T (let ((tile (cons i (cons j ())))) 
            (cons tile (createDeck i (- j 1))) ))
  )
)

(defun shuffleDeck(deck)
  (cond ( (null deck) ())

        (T (setf *random-state* (make-random-state t))
          (let* ( (index (random (length deck)) ) )
            (cons (nth index deck) (shuffleDeck (removeTile index deck 0)))) )
  )
)

; help taken from: http://www.lee-mac.com/sublist.html
(defun dealDeck ( deck index len )
    (cond ( (null deck) ()) 
          ( (< 0  index) (dealDeck (cdr deck) (- index 1) len) )
          ( (null len) deck )
          ( (< 0  len) (cons (car deck) (dealDeck (cdr deck) index (- len 1))) )
    )
)

; ******************************************************************
; Engine: Source Code to get the engine tile given the round number
; ******************************************************************

; *********************************************************************
; Function Name: getEngine
; Purpose: to get the engine for the given round
; Parameters: round number and deck
;            
; Return Value: a list of the engine tile and the engine index in the deck
; Algorithm:
;           1) computes the engine tile and index from the round number
;           2) returns the list of engine tile and the engine index
; Assistance Received: none
; ********************************************************************* */
(defun getEngine(roundNum deck)
  (cond ( (= (mod roundNum 10) 0)
          (let* ( (tile 0) (engine (cons tile (cons tile ())) ) (engineIndex (getTileIndex engine (createDeck 9 9) 55 )) )
            (cons engine engineIndex)))
        (T
          (let* ( (tile (- 10 (mod roundNum 10))) (engine (cons tile (cons tile ()) ) ) (engineIndex (getTileIndex engine (createDeck 9 9) 55 )) )
            (cons engine engineIndex)))
  )
)

; *************************************************************
; TILE: Source Code for returning and removing tiles from deck
; *************************************************************


; *********************************************************************
; Function Name: removeTile
; Purpose: to remove a tile given an index and a deck
; Parameters: index of the tile being removed, deck, a counter
;            
; Return Value: the list of tiles (deck) after the nth item is removed
; Algorithm:
;           1) removed the required tile from the deck
;           2) returns the remaining deck
; Assistance Received: none
; ********************************************************************* */
(defun removeTile(index deck counter)
  (cond ((= counter index)
          (cdr deck))
        (T
          (cons (car deck) (removeTile index (cdr deck) (+ counter 1) ) )
        )
  )
)

; *********************************************************************
; Function Name: returnTile
; Purpose: to get tile given an index and a deck
; Parameters: index of the tile being returned, deck, a counter
;            
; Return Value: the required tile from the deck
; Algorithm:
;           1) call function recursively and increment counter until 
;               the index is found
;           2) return the tile on that index
;           3) if index < 0 returns (), if > deck length returns ()
; Assistance Received: none
; ********************************************************************* */
(defun returnTile(index deck counter)
  (cond ((= counter index)
          (list (car deck))) ; (car deck)
        ((< index 0)
          (message "Tile index cannot be less than 0")
          ())
        ((null deck)
          (message "Index too large, tile does not exist")
          ())
        (T
          (returnTile index (cdr deck) (+ counter 1) )
        )
  )
)

(defun isDoubleTile(tile)
  (cond 
    ((null tile) nil)
    ((= (car tile) (car (cdr tile))) t)
    (t nil)
  )
)

(defun getTileSum(tile)
  (+ (car tile) (car (cdr tile)))
)


; *************************************************************
; Helper Functions: to get index of given element from a list
; *************************************************************

; *********************************************************************
; Function Name: getIndexNames
; Purpose: to compute the index of a given trainName from the list of names
; Parameters: name whose index is being computed, list of names, size of list
;            
; Return Value: index of the name if found, -1 if name not present in list
; Algorithm:
;           1) search through the list until the name is found
;           2) if name found return the index
;           3) if name not found, return -1
; Assistance Received: none
; ********************************************************************* */
(defun getIndexNames(name names len)
  (cond ((equal (nth len names) name) len)
        ((= len 0) -1)
        (T (getIndexNames name names (- len 1)))
  )
)

; *********************************************************************
; Function Name: getTileIndex
; Purpose: to compute the index of a given trainName from the list of names
; Parameters: tile whose index is being computed, the deck, size of deck
;            
; Return Value: index of the tile if found, -1 if tile not present in deck
; Algorithm:
;           1) search through the tile until the tile is found
;           2) if tile found return the index
;           3) if tile not found, return -1
; Assistance Received: none
; ********************************************************************* */
(defun getTileIndex( tile deck len)
  (cond ((equal (nth len deck) tile) len)
        ((equal (nth len deck) (reverse tile)) len)
        ((= len 0) -1)
        (T (getTileIndex tile deck (- len 1)))
  )
)


; *********************************************************
; TRAIN: Source Code for TRAIN functions
; *********************************************************

(defun addTileToTrain(train tile)
  ;(print train)
  ;(message "tile added to train")
  (append train (list tile))
  ;(cons train tile)
)


(defun reverseTrain(train)
  (cond ((null train)
          ())
        (t 
          (append (reverseTrain (cdr train)) (cons (reverse (car train)) ()) ) 
        )
  )
)

(defun getLastTile(train engine)
  (cond ( (null train) engine )
        ( t (car (reverse train)) ) )
)

(defun returnTrain(index trains)
  (cond ( (= index 0)
          (car trains))
        ( (= index 1)
          (car (cdr trains)))
        ( t ;i.e index = 2
          (car (cdr (cdr trains))))
  )
  
)

; RETURNS TRAIN NAME FROM THE INDEX NUMBER PASSED AS PARAMETER
(defun getTrainName(index)
  (cond 
    ((= index 0) 'M-train)
    ((= index 1) 'H-train)
    ((= index 2) 'C-train)
    (t 'Invalid-train)
  )
)

(defun isOrphanTrain (train engine)
  (cond 
    ; if given train is empty its not an orphan train.
    ((= (length train) 0) nil)
    ((equal (isDoubleTile (getLastTile train engine)) t) t)
    (t nil)
  )
)

(defun getUpdatedTrains(trains playedTrain playedTrainIndex)
  (let* ((updatedTrains ()))
    ;(princ "played train index: ")
    ;(princ playedTrainIndex)
    ;(terpri)
    (cond ( (= playedTrainIndex 0)
            (append (append (append updatedTrains (list playedTrain)) (list (returnTrain 1 trains)) ) (list (returnTrain 2 trains)) )  )

          ( (= playedTrainIndex 1) 
            (append (append (append updatedTrains (list (returnTrain 0 trains))) (list playedTrain) ) (list (returnTrain 2 trains)) )  ) 

          ( t  
            (append (append (append updatedTrains (list (returnTrain 0 trains))) (list (returnTrain 1 trains)) ) (list playedTrain) )  )                            
    )
  )
)

; trainName = train's name whose eligibility is being checked, eligTrainNames: list of eligtrainNames
(defun isEligible(trainName eligTrainNames)
  (cond ( (= (getIndexNames trainName eligTrainNames (length eligTrainNames)) -1) () )
        ( t t)
  )
)



; ************************************************************
; Player Hand: Source Code for evaluating player hand tiles
; ************************************************************

;to determine if the player has a playable tile for a train, train = Train being checked
(defun hasPlayableTile(train playerHand engine)
  (cond 
        ; playerhand is empty means there are no playable tiles for the train
        ( (null playerHand) ()) 
        ; if the first element in playerhand is not validTile, call hasPlayableTile recursively for rest of playerHand
        ( (equal (validateTile (car playerHand) train engine) ()) (hasPlayableTile train (cdr playerHand) engine))
        ; otherwise return true, meaning the player had a playable tile for the train
        (t t)
  )
)

; check if user can play at all, or user has to draw from boneyard
(defun canPlay (trains eligTNames hand engine)
  (cond
    ;; if m train valid and player has playable tiles for the train
   ((equal 
    (and (not (= (getIndexNames 'm eligTNames (length eligTNames)) -1)) (equal (hasPlayableTile (returnTrain '0 trains) hand engine) t)) t) t)
   ((equal 
    (and (not (= (getIndexNames 'h eligTNames (length eligTNames)) -1)) (equal (hasPlayableTile (returnTrain '1 trains) hand engine) t)) t) t)
   ((equal 
    (and (not (= (getIndexNames 'c eligTNames (length eligTNames)) -1)) (equal (hasPlayableTile (returnTrain '3 trains) hand engine) t)) t) t)
   ; if none of the above conditions were true, player cannot play from its tiles in the hand
   (t nil)
  )
)

; *********************************************************
; Strategy: Source Code for computer player strategies
; *********************************************************

;returns the list of valid tiles for player hand for a chosen train
(defun validTilesList(train playerHand engine)
  (cond 
        ; playerhand is empty means there are no more valid tiles for the train
        ( (null playerHand) ()) 
        ; if the first element in playerhand is not validTile, call validTilesList recursively for rest of playerHand
        ( (equal (validateTile (car playerHand) train engine) ()) (validTilesList train (cdr playerHand) engine))
        ; otherwise concatenate the valid tile and call validTilesList for rest of the tiles in hand
        (t (cons (validateTile (car playerHand) train engine) (validTilesList train (cdr playerHand) engine)) )
  )
)

; check if player has a playable double tile
(defun findDoubleTile(validTiles)
  (cond
    ; if no double tile found return nil
    ((null validTiles) nil)
    ; if a double tile found, return that tile immediately
    ((equal (isDoubleTile (car validTiles)) t) (car validTiles))
    ; if double tile not found and validTiles is not yet empty, call findDoubleTile recursively with rest of ValidTiles
    (t (findDoubleTile (cdr validTiles)) )
  )
)

; to check if player can make an orphan train
(defun makeOrphanTrain(trains eligTNames hand engine M_elig H_elig C_elig)
  (cond
    ((= (length eligTNames) 0) -1) ; basecase, player can't make orphan train
    ((equal (car eligTNames) 'm) 
      (cond
        ; no double tile to play on m train 
        ( (equal (findDoubleTile (validTilesList (returnTrain 0 trains) hand engine)) nil)
          (makeOrphanTrain trains (cdr eligTNames) hand engine M_elig H_elig C_elig)
        )
        ; double tile found to play on m train
        (t 
         (cond
          ; can make an orphan train, because can play on h-train after removing the double tile
          ((equal (and H_elig (equal (hasPlayableTile (returnTrain 1 trains) 
                  (removeTile (getTileIndex (findDoubleTile (validTilesList (returnTrain 0 trains) hand engine)) 
                  hand (length hand)) hand 0) engine) t)) t) 
              ; returns mexican train index i.e. 0    
               '0) 

          ; can make an orphan train, because can play on c-train after removing the double tile
          ((equal (and C_elig (equal (hasPlayableTile (returnTrain 2 trains) 
                  (removeTile (getTileIndex (findDoubleTile (validTilesList (returnTrain 0 trains) hand engine)) 
                  hand (length hand)) hand 0) engine) t)) t) '0)
          (t (makeOrphanTrain trains (cdr eligTNames) hand engine M_elig H_elig C_elig))
         )
        )
      )
    )

    ((equal (car eligTNames) 'h) 
      (cond
        ; no double tile to play on h train 
        ( (equal (findDoubleTile (validTilesList (returnTrain 1 trains) hand engine)) nil)
          (makeOrphanTrain trains (cdr eligTNames) hand engine M_elig H_elig C_elig)
        )
        ; double tile found to play on h train
        (t 
         (cond
          ; can make an orphan train, because can play on m-train after removing the double tile
          ((equal (and M_elig (equal (hasPlayableTile (returnTrain 0 trains) 
                  (removeTile (getTileIndex (findDoubleTile (validTilesList (returnTrain 1 trains) hand engine)) 
                  hand (length hand)) hand 0) engine) t)) t) '1) 
          ; can make an orphan train, because can play on c-train after removing the double tile
          ((equal (and C_elig (equal (hasPlayableTile (returnTrain 2 trains) 
                  (removeTile (getTileIndex (findDoubleTile (validTilesList (returnTrain 1 trains) hand engine)) 
                  hand (length hand)) hand 0) engine) t)) t) '1)
          (t (makeOrphanTrain trains (cdr eligTNames) hand engine M_elig H_elig C_elig))
         )
        )
      )
    )

    ((equal (car eligTNames) 'c) 
      (cond
        ; no double tile to play on c train 
        ( (equal (findDoubleTile (validTilesList (returnTrain 2 trains) hand engine)) nil)
          (makeOrphanTrain trains (cdr eligTNames) hand engine M_elig H_elig C_elig)
        )
        ; double tile found to play on c train
        (t 
         (cond
          ; can make an orphan train, because can play on h-train after removing the double tile
          ((equal (and H_elig (equal (hasPlayableTile (returnTrain 1 trains) 
                  (removeTile (getTileIndex (findDoubleTile (validTilesList (returnTrain 2 trains) hand engine)) 
                  hand (length hand)) hand 0) engine) t)) t) '2) 
          ; can make an orphan train, because can play on c-train after removing the double tile
          ((equal (and M_elig (equal (hasPlayableTile (returnTrain 0 trains) 
                  (removeTile (getTileIndex (findDoubleTile (validTilesList (returnTrain 2 trains) hand engine)) 
                  hand (length hand)) hand 0) engine) t)) t) '2)
          (t (makeOrphanTrain trains (cdr eligTNames) hand engine M_elig H_elig C_elig))
         )
        )
      )
    )
  )
  
)

; returns the largest playable tile from the list of valid tiles
(defun findLargestTile (validTiles largestTile)
    (cond 
        ; playerhand is empty return the list of the largest tile and the tile index
        ( (null validTiles) largestTile ) 

        ; if the first element in validTile is larger, call findLargestTile recursively by replacing the previous largestTile
        ( (> (car (cdr (car validTiles))) (car (cdr largestTile)))
          (findLargestTile (cdr validTiles) (car validTiles))
        )
        ; otherwise call findLargestTile recursively without replacing the tile
        ( t (findLargestTile (cdr validTiles) largestTile) )
    )
)

; returns the index of the train that can play the largest tile pip
(defun findLargestTrain(playableList trains hand engine)
  ;(print "in find largest train")
  (let* ((playableTrainCount (getTrueCount playableList 0))
         (mTLrgstTileSum (getTileSum (findLargestTile (validTilesList (returnTrain 0 trains) hand engine) '(-1 -1))))
         (hTLrgstTileSum (getTileSum (findLargestTile (validTilesList (returnTrain 1 trains) hand engine) '(-1 -1))))
         (cTLrgstTileSum (getTileSum (findLargestTile (validTilesList (returnTrain 2 trains) hand engine) '(-1 -1))))
         (mTPlayable (car playableList))
         (hTPlayable (car (cdr playableList)))
         (cTPlayable (car (cdr (cdr playableList))))
        )
    
    (cond 
      ((= playableTrainCount 3)
       ; Returns the index of the train that can play the largest tile
       (getMax (cons mTLrgstTileSum (cons hTLrgstTileSum (cons cTLrgstTileSum ()))) -5 0 0)
      )
      ((= playableTrainCount 2)
        (cond 
          ((equal (and mTPlayable hTPlayable) t) (getMax (cons mTLrgstTileSum (cons hTLrgstTileSum ())) -5 0 0))
          ; getMax list construction (-1, hmax, cmax), because m-train is not available, put -1 for m-max
          ((equal (and hTPlayable cTPlayable) t) (getMax (cons -1 (cons hTLrgstTileSum (cons cTLrgstTileSum ()))) -5 0 0))
          ((equal (and mTPlayable cTPlayable) t) (getMax (cons mTLrgstTileSum (cons -1 (cons cTLrgstTileSum ()))) -5 0 0))
        )
      )
      (T
        (cond ((equal mTPlayable t) 0)
              ((equal hTPlayable t) 1)
              ((equal cTPlayable t) 2)
        )
      )
    )
  )
)

; *********************************************************
; Help Human functions.
; *********************************************************

(defun pickAtileHelpMode(hand pickedTrain pickedTrainIndex engine)
  (let* ( (doubleTile (findDoubleTile (validTilesList pickedTrain hand engine)))
          (largeTile (findLargestTile (validTilesList pickedTrain hand engine) '(-1 -1)))
          (tileIndex (getTileIndex largeTile hand (length hand))) ) 
    (cond 
      ; if double tile not found pick largeTile
      ((equal doubleTile nil) largeTile)

      ; if double tile found, pick double tile
      (t doubleTile)
    )
  )
)

(defun helpHuman(trains eligTNames hand engine)
  (let* ( ( canPlayHtrain (and (isEligible 'h eligTNames) (hasPlayableTile (returnTrain 1 trains) hand engine)) )
          ( canPlayMtrain (and (isEligible 'm eligTNames) (hasPlayableTile (returnTrain 0 trains) hand engine)) )
          ( canPlayCtrain (and (isEligible 'c eligTNames) (hasPlayableTile (returnTrain 2 trains) hand engine)) )
          ( playableList (cons canPlayMtrain (cons canPlayHtrain (cons canPlayCtrain ()))) )
          ( playableTrainCount (getTrueCount playableList 0) )
          ( canMakeOT (makeOrphanTrain trains eligTNames hand engine canPlayMtrain canPlayHtrain canPlayCtrain) )
          ( mtrainOrphan (isOrphanTrain (returnTrain 0 trains) engine) )
          ( htrainOrphan (isOrphanTrain (returnTrain 1 trains) engine) )
          ( ctrainOrphan (isOrphanTrain (returnTrain 2 trains) engine) )
          ( mtrainNotStarted (= (length (returnTrain 0 trains)) 0) )
        )

    (cond 
      ; CONDITIONS THAT WOULD BE POSSIBLE ONLY IF THERE ARE MORE THAN 1 PLAYABLE TRAINS
      ((> playableTrainCount 1) 
        (cond 
          ; human can make an orphan train
          ((equal (/= canMakeOT -1) t)
           (let* ((bestTile (pickAtileHelpMode hand (returnTrain canMakeOT trains) canMakeOT engine)))

            (cond ((= canMakeOT 0) (printHumanHelp bestTile "M-train" "you can make M-train an orphan train."))
                  ((= canMakeOT 1) (printHumanHelp bestTile "H-train" "you can make H-train an orphan train."))
                  ((= canMakeOT 2) (printHumanHelp bestTile "C-train" "C-train is marked and you can make C-train an orphan train."))
            ) 
           )
          )

          ; if m-train is an orphan train, pick another train
          ((equal mtrainOrphan t) 
            (let* ((bestTrain (findLargestTrain (cons nil (cons canPlayHtrain (cons canPlayCtrain ()))) trains hand engine))
                   (bestTile (pickAtileHelpMode hand (returnTrain bestTrain trains) bestTrain engine)))
              (printHumanHelp bestTile (getTrainName bestTrain) "this is the largest tile you can play without breaking the orphan train you created (M-train)")
            )
          )

          ; if h-train is an orphan train, pick another train
          ((equal htrainOrphan t) 
            (let* ((bestTrain (findLargestTrain (cons canPlayMtrain (cons nil (cons canPlayCtrain ()))) trains hand engine))
                   (bestTile (pickAtileHelpMode hand (returnTrain bestTrain trains) bestTrain engine)))
              (printHumanHelp bestTile (getTrainName bestTrain) "this is the largest tile you can play without breaking the orphan train you created (H-train)")
            )
          )

          ; if c-train is an orphan train, pick another train
          ((equal ctrainOrphan t) 
            (let* ((bestTrain (findLargestTrain (cons canPlayMtrain (cons canPlayHtrain (cons nil ()))) trains hand engine))
                   (bestTile (pickAtileHelpMode hand (returnTrain bestTrain trains) bestTrain engine)))
              (printHumanHelp bestTile (getTrainName bestTrain) "this is the largest tile you can play without breaking the orphan train you created (C-train)")
            )
          )

          ; if m-train is not yet started, suggest to start m-train
          ((equal mtrainNotStarted t)
           (printHumanHelp (pickAtileHelpMode hand (returnTrain 0 trains) 0 engine) "M-train" "M-train was not started yet")
          )

          ; if none of the trains are orphan, return the train that can play the largest tile
          (t 
            (let* ((bestTrain (findLargestTrain playableList trains hand engine))
                   (bestTile (pickAtileHelpMode hand (returnTrain bestTrain trains) bestTrain engine))
                   (trainName (getTrainName bestTrain)))
              (cond ((= bestTrain 2) (printHumanHelp bestTile trainName " C-train is marked and you can play the largest tile in C-train among all eligible trains"))
                    (t (printHumanHelp bestTile trainName "it is the largest tile that can be played among all eligible trains")))
            )
          )
        )
      )
      
      ((= playableTrainCount 0) (print " You do not have any playable tile. "))

      ; pick a train from the playable trains that can play the largest tile
      (t 
       (let* ((bestTrain (findLargestTrain playableList trains hand engine))
              (trainName (getTrainName bestTrain))
              (bestTile (pickAtileHelpMode hand (returnTrain bestTrain trains) bestTrain engine)))
            ;(print "in here............................")
          (cond 
            ((= bestTrain 2)
              ;(print "here here.................................")
              (cond 
                ((= (length eligTNames) 1)
                 (printHumanHelp bestTile trainName "C-trian is a must play orphan train"))
                (t (printHumanHelp bestTile trainName "C-train is marked and you can play the largest tile in C-train") ))
            )
            (t (printHumanHelp bestTile trainName "it is the largest tile that can be played among all eligible trains"))
          )
       )
      )
    )
  )
)

(defun printHumanHelp (tile trainName message)
  (terpri)
  (princ "I suggest you play tile ")
  (princ tile)
  (princ " on ")
  (princ trainName)
  (princ " because ")
  (princ message)
  (terpri)
)

; *********************************************************
; Helper functions.
; *********************************************************

; returns the # of 't's from a list of true and nil
(defun getTrueCount(aList counter)
  (cond 
    ; base case
    ((null aList) counter)
    ((equal (car aList) t) (getTrueCount (cdr aList) (+ counter 1)))
    (t (getTrueCount (cdr aList) counter))
  )
)

; get the largest number index from the list, maxNum = -5, maxIndex = 0, counter= 0
(defun getMax (aList maxNum maxIndex counter)
  (cond 
        ; aList is empty return the max # index
        ( (null aList) maxIndex ) 

        ; if the first element in aList is larger, call getMax recursively by replacing the previous maxIndex
        ( (> (car aList) maxNum)
          (getMax (cdr aList) (car aList) counter (+ counter 1))
        )
        ; otherwise call getMax recursively without replacing the maxIndex
        ( t (getMax (cdr aList) maxNum maxIndex (+ counter 1)) )
  )
)


; *********************************************************
; Source Code for gathering data before player makes a move
; *********************************************************

; hasToPlayODList first item for human second for computer
(defun playerHasToPlayOD(hasToPlayODList player)
  (cond
    ; player is human
    ((equal player 'h)
      (cond 
        ; human has to play orphan train
        ((equal (car hasToPlayODList) t) t)
        ; human does not have to play orphan train
        (t nil)
      )
    )
    (T ; player is computer
      (cond 
        ; computer has to play orphan train
        ((equal (car (cdr hasToPlayODList)) t) t)
        ; computer does not have to play orphan train
        (t nil)
      )
    )
  )
)

; in markers list first item is human marker second item is computer's marker
(defun getEligibleTrains(markers player hasToPlayODList trains engine)
  (cond 
    ((equal player 'h)
      (cond 
            ((equal (and (playerHasToPlayOD hasToPlayODList 'h) (> (length (getOrphanTrains trains engine 0 ())) 0) ) t) 
             (getOrphanTrains trains engine 0 ()))
            ((equal (car (cdr markers)) t) '(m h c))
            (t '(m h))
      )
    )
    (T
      (cond 
            ((equal (and (playerHasToPlayOD hasToPlayODList 'c) (> (length (getOrphanTrains trains engine 0 ())) 0) ) t)
             (getOrphanTrains trains engine 0 ()))
            ((equal (car markers) t) '(m h c))
            (t '(m c))
      )
    )
  )
)

(defun getOrphanTrains(trains engine counter orphanTrains)

  (cond
    ((= counter 3) orphanTrains)
    ((and (= counter 0) (equal (isOrphanTrain (returnTrain 1 trains) engine) t) )
     (getOrphanTrains trains engine (+ counter 1) 
     (append orphanTrains '(h))) )
     
    ((and (= counter 1) (equal (isOrphanTrain (returnTrain 0 trains) engine) t)) 
     (getOrphanTrains trains engine (+ counter 1) 
     (append orphanTrains '(m))) )

    ((and (= counter 2) (equal (isOrphanTrain (returnTrain 2 trains) engine) t)) 
     (getOrphanTrains trains engine (+ counter 1) 
     (append orphanTrains '(c))) )

    (t (getOrphanTrains trains engine (+ counter 1) orphanTrains) )
  )
)

; *********************************************************
; Source Code for picking train and tile for Human.
; *********************************************************

;;eligTrains has the list of all three trains in the order M H C
(defun pickAtrain(eligTrains eligTrainNames)
  (terpri)
  (princ "These are your valid trains for the turn: ")
  (princ eligTrainNames)
  (terpri)
  (message "Please pick from eligible trains to place a tile on.")
  (let* ( (pickedTrain (validateInput eligTrainNames (read) )) )
    (cond ( (string= pickedTrain "z" )
            (pickAtrain eligTrains eligTrainNames) )
          (t ;; human chose to pick a valid train, now return the train
            (cond ((string= pickedTrain "M")
                    (message "picked train: M train")
                    ;(car eligTrains) ;;returning index instead of the train itself
                    '0)
                  ((string= pickedTrain "H")
                    (message "picked train: H train") 
                    '1)
                  (t (message "picked train: C train")
                    '2
                  ) ;; this is c train
            )
          )
    )
  )
)

;returns the picked-validated tile and the index of the tile in the player's hand
(defun pickAtile(humanHand pickedTrain engine)
  (message "Please enter the number corresponding to the tile from your hand")
  (let* ( (index (read)) (pickedTile (returnTile index humanHand 0)))
    (cond ((or (null pickedTile) (equal pickedTile '(nil) ) ) 
            (pickAtile humanHand pickedTrain engine)) 
          (t 
            (let* ( (checkedTile (validateTile (car pickedTile) pickedTrain engine)) )
              (cond ( (null checkedTile)
                      (message "Picked tile pips is not equal to the train's last tile pip.")
                      (pickAtile humanHand pickedTrain engine))

                    (t (cons checkedTile (cons index ()))))
            )
          )
    )
  ) 
)


; *********************************************************
; Source Code for picking train and tile for Computer.
; *********************************************************

; function to make computer pick a train from the list of available trains, with priority level: h, m c 
(defun pickAtrainComputer (trains eligTNames hand engine)
  (ext:run-shell-command "cls")
  (terpri)
  (princ "--------------------------------------------------------------------------------------------------------------------------")
  (terpri)
  (let* ( ( canPlayHtrain (and (isEligible 'h eligTNames) (hasPlayableTile (returnTrain 1 trains) hand engine)) )
          ( canPlayMtrain (and (isEligible 'm eligTNames) (hasPlayableTile (returnTrain 0 trains) hand engine)) )
          ( canPlayCtrain (and (isEligible 'c eligTNames) (hasPlayableTile (returnTrain 2 trains) hand engine)) )
          ( playableList (cons canPlayMtrain (cons canPlayHtrain (cons canPlayCtrain ()))) )
          ( playableTrainCount (getTrueCount playableList 0) )
          ( canMakeOT (makeOrphanTrain trains eligTNames hand engine canPlayMtrain canPlayHtrain canPlayCtrain) )
          ( mtrainOrphan (isOrphanTrain (returnTrain 0 trains) engine) )
          ( htrainOrphan (isOrphanTrain (returnTrain 1 trains) engine) )
          ( ctrainOrphan (isOrphanTrain (returnTrain 2 trains) engine) )
          (mtrainNotStarted (= (length (returnTrain 0 trains)) 0))
        )
    
    (terpri)

    (cond 
      ; CONDITIONS THAT WOULD BE POSSIBLE ONLY IF THERE ARE MORE THAN 1 PLAYABLE TRAINS
      ((> playableTrainCount 1) 
        (cond 
          ; if can make an orphan train, return the train index for that
          ((equal (/= canMakeOT -1) t)           
           (princ "Computer could make an orphan train by playing a double tile therefore,") 
           canMakeOT)

          ; if m-train is an orphan train, pick another train
          ((equal mtrainOrphan t) 
           (princ "To not break the orphan Mexican Train that computer created,")
           (findLargestTrain (cons nil (cons canPlayHtrain (cons canPlayCtrain ()))) trains hand engine))

          ; if h-train is an orphan train, pick another train
          ((equal htrainOrphan t) 
           (princ "To not break the orphan human train that computer created ")
           (findLargestTrain (cons canPlayMtrain (cons nil (cons canPlayCtrain ()))) trains hand engine))

          ; if c-train is an orphan train, pick another train
          ((equal ctrainOrphan t) 
           (princ "To not break the orphan computer train that computer created ")
           (findLargestTrain (cons canPlayMtrain (cons canPlayHtrain (cons nil ()))) trains hand engine))
          
          ; if m-train is not yet started
          ((equal mtrainNotStarted t) 
           (princ "Since M-train was not started yet") 0)

          ; if none of the trains are orphan, return the train that can play the largest tile
          (t 
            (let* ((pickedTrainIndex (findLargestTrain playableList trains hand engine)))
              (cond ((= pickedTrainIndex 1) (princ "H-train had a marker therefore, ") pickedTrainIndex)
                    (t pickedTrainIndex))
            )
          )
        )
      )
      
      ; pick a train from the playable trains that can play the largest tile
      (t 
       (let* ((pickedTrainIndex (findLargestTrain playableList trains hand engine))
              (trainName (getTrainName pickedTrainIndex)))
          (cond ((= pickedTrainIndex 1)
                (cond ((= (length eligTNames) 1) (princ "H-train was a must play orphan train therefore,") pickedTrainIndex)
                      (t (princ "H-train had a marker therefore, ") pickedTrainIndex))
                )
                (t pickedTrainIndex)
          )
       )
      )

       ;(princ "Computer could play the largest tile from its hand ")
       ;(findLargestTrain playableList trains hand engine))
    )
  )
)

; picks the largest playable tile in the picked train and returns the tile, tile index in computer hand 
(defun pickAtileComputer(hand pickedTrain pickedTrainIndex engine)
  (let* ( (doubleTile (findDoubleTile (validTilesList pickedTrain hand engine)))
          (largeTile (findLargestTile (validTilesList pickedTrain hand engine) '(-1 -1)))
          (tileIndex (getTileIndex largeTile hand (length hand))) )
  
    (cond 
      ; if double tile not found pick largeTile
      ((equal doubleTile nil) 
       (printPlayerStrategy pickedTrainIndex largeTile " it was the largest playable tile.")
       (cons largeTile (cons tileIndex ())))

      ; if double tile found, pick double tile
      (t
       (printPlayerStrategy pickedTrainIndex doubleTile " it was the double playable tile.") 
       (cons doubleTile (cons (getTileIndex doubleTile hand (length hand)) ())))
    )
  )
)

(defun printPlayerStrategy(trainIndex tile message)
  (princ " Computer chose to play ") (princ tile) (princ " on ")
  (princ (getTrainName trainIndex)) (princ " as ")
  (princ message) (terpri)
  (princ "-----------------------------------------------------------------------------------------------------------------------------")
  (terpri)
)

; validates the picked tile before adding to the train, if invalid returns ()
(defun validateTile(tile train engine)
  (let* ((tileRightPips (car (cdr tile))) (tileLeftPips (car tile)) (trainLastPips (car (cdr (getLastTile train engine)))) )
    (cond ( (= tileLeftPips trainLastPips) tile)
          ( (= tileRightPips trainLastPips) (reverse tile))
          ( t () )
    )
  )
)
 
; *********************************************************
; Source Code for updating data after player makes a move
; *********************************************************


(defun updateMarker (playedTrainIndex player markers)
  (cond 
    ((equal player 'h)
      (cond 
        ; if human played on h-train,no marker on htrain but there will be no change to c-marker
        ((= playedTrainIndex '1) (cons nil (cdr markers)))
        ; if human did not play on its train, return the marker as it was before
        (t markers)))
    (T
      (cond 
        ; if computer played on c-train,no marker on ctrain but there will be no change to h-marker
        ((= playedTrainIndex '2) (cons (car markers) nil ))
        ; if computer did not play on its train, return the marker as it was before
        (t markers)
      )))
)

; after playing a tile updates whether the player has to play on the orphan double train next
(defun updateHasToPlayOD(playedTile currentPlayer)
  (cond
    ((equal currentPlayer 'h)
      (cond 
        ((equal (isDoubleTile playedTile) t) '(nil t))
        ;current player didn't play any tile, then both players play ODs (if any) in their coming turns
        ((equal playedTile ()) '(t t)) 
        (t '(t t))))
    (t 
      (cond 
        ((equal (isDoubleTile playedTile) t) '(t nil))
        ;current player didn't play any tile, then both players play ODs (if any) in their coming turns
        ((equal playedTile ()) '(t t))
        (t '(t t))))
  )
)

(defun getNextPlayer(playedTile currentPlayer)
  (cond 
    ( (equal currentPlayer 'h)
      ; if human player played a double tile next player is human
      (cond ((equal (isDoubleTile playedTile) t) 'h)
            ; if human did not play double tile next player is computer
            (t 'c)))
    (t 
      ; if computer player played a double tile next player is computer
      (cond ((equal (isDoubleTile playedTile) t) 'c)
          ; else next player is human
            (t 'h)))
  )
)

; *********************************************************
; Source Code for making a move / playing a turn
; *********************************************************

(defun drawMove(trains eligTrainNames humanHand computerHand engine boneyard player markers gameScores roundNum)
  ; pick tile from boneyard and add to player's hand
  (cond 
    ; human is playing
    ( (equal player 'h)
      (message "You do not have a playable tile. Please press 'B' to draw from boneyard.")
      (cond ((equal (validateInput '(b) (read)) "z") (drawMove trains eligTrainNames humanHand computerHand engine boneyard player markers gameScores roundNum))
            (t )) ;continue
      ;(read)
      ; check if canPlay the first tile on boneyard, which is the tile being drawn
      (cond ((equal (canPlay trains eligTrainNames (list (car boneyard)) engine) t)
              (princ "Drawn Tile: ") (princ (car boneyard)) (terpri)
              (let* ((drawnTile (car boneyard))
                    (updatedBoneyard (removeTile 0 boneyard 0))  
                    (pickedTrainIndex (pickAtrain trains eligTrainNames))
                    (pickedTrain (returnTrain pickedTrainIndex trains)) 
                    (pickedTile (validateTile drawnTile pickedTrain engine))
                    (playedTrain (addTileToTrain pickedTrain pickedTile))
                    (updatedTrains (getUpdatedTrains trains playedTrain pickedTrainIndex))
                    (updatedMarkers (updateMarker pickedTrainIndex 'h markers))
                    (nextPlayer (getNextPlayer pickedTile 'h))
                    (hasToPlayODList (updateHasToPlayOD pickedTile player)) ;player = current player
                    (eligTrains (getEligibleTrains updatedMarkers nextPlayer hasToPlayODList updatedTrains engine))
                  ) 
                (ext:run-shell-command "cls")
                (playTurn updatedTrains eligTrains humanHand computerHand updatedBoneyard engine nextPlayer updatedMarkers gameScores roundNum)
              )
            )
            ; add marker to human train
            (t            
              (let* ( (drawnTile (car boneyard))
                      (updatedBoneyard (removeTile 0 boneyard 0))
                      (updatedHumanHand (addTileToTrain humanHand drawnTile)) ; adding drawntile to humanhand not train
                      (hasToPlayODList (updateHasToPlayOD () player))
                      (updatedMarkers (cons t (cdr markers)))
                      (eligTrains (getEligibleTrains updatedMarkers 'c hasToPlayODList trains engine))
                    )
                (ext:run-shell-command "cls")
                (message "-----------------------------------------------------------------------------------------------------")
                (princ ">> Drawn Tile: ") (princ drawnTile)
                (message " Drawn Tile was not playable therefore, turn was passed to computer.")
                (message "-----------------------------------------------------------------------------------------------------")
                (playTurn trains eligTrains updatedHumanHand computerHand updatedBoneyard engine 'C updatedMarkers gameScores roundNum)
              )              
            )
      )
    )
    ; computer is playing
    (t 
      (cond ((equal (canPlay trains eligTrainNames (list (car boneyard)) engine) t)
              (let* ((drawnTile (car boneyard))
                    (updatedBoneyard (removeTile 0 boneyard 0))  
                    (pickedTrainIndex (pickAtrainComputer trains eligTrainNames (list (car boneyard)) engine))
                    (pickedTrain (returnTrain pickedTrainIndex trains)) 
                    (pickedTile (validateTile drawnTile pickedTrain engine))
                    (playedTrain (addTileToTrain pickedTrain pickedTile))
                    (updatedTrains (getUpdatedTrains trains playedTrain pickedTrainIndex))
                    (updatedMarkers (updateMarker pickedTrainIndex 'c markers))
                    (nextPlayer (getNextPlayer pickedTile 'c))
                    (hasToPlayODList (updateHasToPlayOD pickedTile player)) ;player = current player
                    (eligTrains (getEligibleTrains updatedMarkers nextPlayer hasToPlayODList updatedTrains engine))
                  ) 
                (ext:run-shell-command "cls")
                (message ">> Computer has no playable tile. Thus, is drawing tile from boneyard.")
                (printPlayerStrategy pickedTrainIndex pickedTile " it was the valid drawn tile.") 
                (playTurn updatedTrains eligTrains humanHand computerHand updatedBoneyard engine nextPlayer updatedMarkers gameScores roundNum)
              )
            )
            ; add marker to computer train
            (t
              ;(message "Drawn Tile was not playable.")
              (let* ( (drawnTile (car boneyard))
                      (updatedBoneyard (removeTile 0 boneyard 0))
                      (updatedComputerHand (addTileToTrain computerHand drawnTile)) ; adding drawntile to computerhand not train
                      (hasToPlayODList (updateHasToPlayOD () player))
                      (updatedMarkers (cons (car markers) (cons t ())))
                      (eligTrains (getEligibleTrains updatedMarkers 'h hasToPlayODList trains engine))
                    )
                (ext:run-shell-command "cls")
                (message "--------------------------------------------------------------------------------------------------------")
                (message ">> Computer has no playable tile. Thus, is drawing tile from boneyard.")
                (princ ">> Drawn Tile: ") (princ drawnTile)
                (message " Computer could not play any tile because drawn tile was not playble.")
                (message "--------------------------------------------------------------------------------------------------------")
                (playTurn trains eligTrains humanHand updatedComputerHand updatedBoneyard engine 'H updatedMarkers gameScores roundNum)
              )              
            )
      )
    )
  )
)


(defun makeMove(trains eligTrainNames humanHand computerHand engine boneyard player markers gameScores roundNum)
  
  (cond 
    ; human is playing
    ( (equal player 'h)
      (terpri) (message "<< Player: Human >>") (message "------------------------------")
      (let* ((userInput (displayMenu player)))
        (cond ((equal userInput 'h) (helpHuman trains eligTrainNames humanHand engine))
              ((equal userInput 'S) (message ">> Please enter file name to save the game.")
               (saveGame trains humanHand computerHand engine boneyard player markers gameScores roundNum) (exit))
              ((equal userInput 'Q) (declareWinner (computeScore humanhand (car gameScores)) (computeScore computerHand (car (cdr gameScores)))) (exit))
              (t )); continue game
      )
      
      (cond 
        ; check if canPlay, if not call drawMove
        ((equal (canPlay trains eligTrainNames humanHand engine) nil) 
          (drawMove trains eligTrainNames humanHand computerHand engine boneyard 'h markers gameScores roundNum))
        (t 
          (let* ( (pickedTrainIndex (pickAtrain trains eligTrainNames))
              (pickedTrain (returnTrain pickedTrainIndex trains)) 
              (pickedTilePair (pickAtile humanHand pickedTrain engine)) 
              (pickedTile (car pickedTilePair))
              (pickedTileIndex (car (cdr pickedTilePair)))
              (playedTrain (addTileToTrain pickedTrain pickedTile))
              (updatedTrains (getUpdatedTrains trains playedTrain pickedTrainIndex))
              (updatedHumanHand (removeTile pickedTileIndex humanHand 0))
              (updatedMarkers (updateMarker pickedTrainIndex 'h markers))
              (nextPlayer (getNextPlayer pickedTile 'h))
              (hasToPlayODList (updateHasToPlayOD pickedTile player)) ;player = current player
              (eligTrains (getEligibleTrains updatedMarkers nextPlayer hasToPlayODList updatedTrains engine))
            )
            (ext:run-shell-command "cls")
            (playTurn updatedTrains eligTrains updatedHumanHand computerHand boneyard engine nextPlayer updatedMarkers gameScores roundNum)
          )
        )
      )
      
    )
    ; computer is playing
    (t 
      (terpri) (message "<< Player: Computer >>") (message "------------------------------")
      (let* ((userInput (displayMenu player)))
        (cond ((equal userInput 'Q) (declareWinner (computeScore humanhand (car gameScores)) (computeScore computerHand (car (cdr gameScores)))) (exit))
              ((equal userInput 'S) (message ">> Please enter file name to save the game.")
               (saveGame trains humanHand computerHand engine boneyard player markers gameScores roundNum) (exit))
              (t )); continue game
      )
      ;(displayMenu player)
      (princ "eligible trains: ")
      (princ eligTrainNames)
      (terpri)

      (cond
        ; check if computer can play
        ((equal (canPlay trains eligTrainNames computerHand engine) nil) 
          (message ">> Computer has to pick from boneyard.")
          (drawMove trains eligTrainNames humanHand computerHand engine boneyard 'c markers gameScores roundNum))
        (T
          (let*
            ( (pickedTrainIndex (pickAtrainComputer trains eligTrainNames computerHand engine))
              (pickedTrain (returnTrain pickedTrainIndex trains)) 
              (pickedTilePair (pickAtileComputer computerHand pickedTrain pickedTrainIndex engine)) 
              (pickedTile (car pickedTilePair))
              (pickedTileIndex (car (cdr pickedTilePair)))
              (playedTrain (addTileToTrain pickedTrain pickedTile))
              (updatedTrains (getUpdatedTrains trains playedTrain pickedTrainIndex))
              (updatedComputerHand (removeTile pickedTileIndex computerHand 0))
              (updatedMarkers (updateMarker pickedTrainIndex 'c markers))
              (nextPlayer (getNextPlayer pickedTile 'c))
              (hasToPlayODList (updateHasToPlayOD pickedTile player)) ;player = current player
              (eligTrains (getEligibleTrains updatedMarkers nextPlayer hasToPlayODList updatedTrains engine))
            )
            (playTurn updatedTrains eligTrains humanHand updatedComputerHand boneyard engine nextPlayer updatedMarkers gameScores roundNum)
          )
        )
      )     
    )
  )
)

; all trains in order (m h c)
(defun playTurn(eligTrains eligTrainNames humanHand computerHand boneyard engine player markers gameScores roundNum)
  
  (let* ( (mTrain (returnTrain '0 eligTrains))
          (hTrain (returnTrain '1 eligTrains))
          (cTrain (returnTrain '2 eligTrains))
        )
    ;(ext:run-shell-command "cls")
    (printScene cTrain hTrain mTrain engine humanHand computerHand boneyard markers gameScores roundNum)
    (terpri) 
  )
  (cond 
    ((null humanHand) (terpri)
     (message "------------------------------------------------------------------------------")
     (message ">> Human hand is empty. Therefore, the round has ended.")
     (printScores humanHand computerHand)
     (message "------------------------------------------------------------------------------")
     (playNextRound (computeScore humanhand (car gameScores)) (computeScore computerHand (car (cdr gameScores))) roundNum))

    ((null computerHand) (terpri)
     (message "------------------------------------------------------------------------------")
     (message ">> Computer hand is empty. Therefore, the round has ended")
     (printScores humanHand computerHand)
     (message "------------------------------------------------------------------------------")
     (playNextRound (computeScore humanhand (car gameScores)) (computeScore computerHand (car (cdr gameScores))) roundNum))
    
    ; if both trains have marker and boneyard is empty
    ((equal (and (car markers) (car (cdr markers)) (null boneyard)) t) (terpri)
     (message "-----------------------------------------------------------------------------------------------")
     (message ">> Both players have passed their turns and boneyard is empty. Therefore, the round has ended")
     (printScores humanHand computerHand)
     (message "-----------------------------------------------------------------------------------------------")
     (playNextRound (computeScore humanhand (car gameScores)) (computeScore computerHand (car (cdr gameScores))) roundNum))

    (t 
      (makeMove eligTrains eligTrainNames humanHand computerHand engine boneyard player markers gameScores roundNum)
    )
  )
)

; *********************************************************
; Source Code for printing game board
; *********************************************************

; prints scores on the screen after a round is over
(defun printScores(humanHand computerHand)
  (message "Round Score are as follow. ")
  (princ "Human Score: ")
  (princ (computeScore humanHand 0))
  (princ " Computer Score: ")
  (princ (computeScore computerHand 0))
  (terpri)
)

(defun printHand(hand counter)
  (cond ((null hand) (terpri))
        (t  (princ counter)
            (princ "->")
            (princ (car hand))
            (princ " , ")
            (printHand (cdr hand) (+ counter 1))
        )
  )
)

(defun printTrain(train counter)
  (cond ((null train) ) ; do nothing
        (t  ;(princ "-[") (princ (car (car train))) (princ " ") (princ (car (cdr train))) (princ "]-")
            (princ "-") (princ (car train)) (princ "-")
            (printTrain (cdr train) (+ counter 1))
        )
  )
)

(defun printScene(ctrain htrain mtrain engine hhand chand boneyard markers gameScores roundNum)
  (princ "Round #: ") (princ roundNum) (printSpace 10)
  (princ "Boneyard (") (princ (length boneyard)) (princ ") -> ") (princ boneyard) (terpri)
  
  (printSpace 25) (princ "Round Score: ") (printSpace 25) (princ "Game Score: ") (terpri)

  (princ "Human Score: ") (printSpace 16) (princ (computeScore hhand 0)) (printSpace 35) (princ (car gameScores)) (terpri)
  (princ "Computer Score") (printSpace 15) (princ (computeScore chand 0)) (printSpace 35) (princ (car (cdr gameScores))) (terpri) (terpri)

  (princ "Human Hand :") (princ (length hhand)) (terpri)
  (printHand hhand 0) (terpri)

  (princ "Computer Hand :") (princ (length chand)) (terpri)
  (printHand chand 0) (terpri) (terpri) (terpri)
  

  (let* ((spaceCount1 (cond ((equal (car (cdr markers)) t) 5) (t 2)))
         (spaceCount2 (cond ((equal (car (cdr markers)) t) 4) (t 1))))

    ;printing engine vertically
    (printSpace (+(*(length ctrain) 7) spaceCount1)) (princ (car engine)) (terpri)

    (cond ((equal (car (cdr markers)) t) (princ "-M-"))
        (t ) ;do nothing
    )

    (cond ((null ctrain) (princ ""))
          (t  (printTrain (reverseTrain ctrain) 0)))
          ;(t (princ (reverseTrain ctrain))))
    ;(princ (reverseTrain ctrain))
    
    (princ "--")
    (princ "|")
    (princ "--")
    
    (cond ((null htrain) (princ ""))
          (t (printTrain htrain 0)))
    ;(princ htrain)
    (cond ((equal (car markers) t) (princ "-M-"))
          (t ) ;do nothing
    )
    (terpri)
    ;printing engine vertically
    (printSpace (+(*(length ctrain) 7) spaceCount1)) (princ (car engine)) (terpri)

    (printSpace (+(*(length ctrain) 7) spaceCount1))
    (princ "|") (terpri)

    (printSpace (+(*(length ctrain) 6) spaceCount2))
    (cond ((null mtrain) (princ ""))
        (t (printTrain mtrain 0))) 
  )
  
  ;(princ mtrain)

)

(defun printSpace(spaceCount)
  (cond ((= spaceCount 0) (princ ""))
        (t (princ " ") (printSpace (- spaceCount 1))))
)

(defun displayMenu(player)
  (message "Press 'S' to save the game.")
  (message "Press 'Q' to quit the game.")
  (cond 
    ((equal player 'h)    
     (message "Press 'H' to receive help.")
     (message "Press 'M' to continue making move.")
     (let* ((userInput (validateInput '(S Q H M) (read))))
      (cond ((equal userInput "z") (displayMenu player))
           (t userInput))) )

    ((equal player 'c)
     (message "Press 'M' to let computer make move.")
     (let* ((userInput (validateInput '(S Q H M) (read))))
      (cond ((equal userInput "z") (displayMenu player))
           (t userInput))) )
  )
)

; *********************************************************
; Source Code for computing scores and declaring winner
; *********************************************************

; scoreSum is initially 0 when the function is called for round 1, else scoreSum is the game score
(defun computeScore(hand scoreSum)
  (cond ((null hand) scoreSum)
        (t (computeScore (cdr hand) (+ (getTileSum (car hand)) scoreSum)) ) )
)

(defun declareWinner(hScore cScore)
  (message "Final Scores are as follow. ")
  (princ ">> Human Score: ") (princ hScore) (princ " >> Computer Score: ") (princ cScore)
  (terpri)(terpri)
  (cond ((< hScore cScore) (message "        ------ Congratulations you won the game !!------"))
        ((= hScore cScore) (message "        ------ Nobody won the game. The final scores were draw !!------"))
        (t (message "         ------ Computer won the game !! You lost the game :( ------") (terpri))
  )
)

(defun playNextRound(hScore cScore roundNum)
  (message "Would you like to play next round? Please press 'Y' for yes and 'N' for no.")
  (let* ((humanInput (read)))
    (cond 
      ((equal humanInput 'Y) (ext:run-shell-command "cls") (startRound (+ roundNum 1) hScore cScore))
      ((equal humanInput 'N) (declareWinner hScore cScore))
      ((equal humanInput "z") (playNextRound hScore cScore roundNum))
      (t (playNextRound hScore cScore roundNum)))
  )
)


; *********************************************************
; File I/O: Functions for reading and writing to file
; *********************************************************

(defun returnItem(index aList counter)
  (cond ((= counter index)
          (list (car aList))) ; (car deck)
        ;((< index 0)
        ;  (message "Tile index cannot be less than 0")
        ;  ())
        ((null aList)
          (message "File does not contain any item")
          ())
        (T
          (returnItem index (cdr aList) (+ counter 1) )
        )
  )
)

; check if trains read from files have marker, if yes, remove marker and return just train
(defun getTrain(aList)
  (cond 
    ; return cTrain without marker
    ((equal (car aList) 'M) (cdr aList))

    ;return hTrain without marker
    ((equal (car (returnItem (- (length aList) 1) aList 0)) 'M) (removeTile (- (length aList) 1) aList 0))
    
    ;if no marker return the train as it is
    (t aList)
  )
)

(defun getTrainWithMarker(train marker trainName engine)
  (cond ((equal trainName 'C)
          (cond ((equal marker t) (append '(M) (append (reverseTrain train) (list engine))))
                (t (append (reverseTrain train) (list engine)))))
        (T (cond ((equal marker t) (append (list engine)(append train '(M))))
                 (t (append (list engine) train))))
  )
)
; return t if train has marker and nil otherwise
(defun getMarker(train)
  (cond 
    ; return t if train has marker at the beginning
    ((equal (car train) 'M) t)

    ;return t if train has marker at the end
    ((equal (car (returnItem (- (length train) 1) train 0)) 'M) t)
    
    ;if no marker return nil
    (t nil)
  )
)

(defun getPlayerFromFile(player)
  (cond 
    ((equal player 'human) 'h)
    (t 'c))
)

(defun getPlayerForFile(player)
  (cond 
    ((equal player 'h) 'human)
    (t 'computer))
)
;(removeTile index deck counter
(defun readFromFile()
  (message "Please enter a file name to load the game from.") (princ ">>")
  (let* ((fileName (read))
        (in (open fileName :if-does-not-exist nil))
        (gameList (read in nil))
        (roundNum (car (returnItem 0 gameList 0)))
        (computerScore (car (returnItem 1 gameList 0)))
        (computerHand (car (returnItem 2 gameList 0)))
        (cTrainWithEngine (getTrain (car (returnItem 3 gameList 0))) )
        (cTrain (reverseTrain (removeTile (- (length cTrainWithEngine) 1) cTrainWithEngine 0))) ; remove engine tile from ctrain
        (humanScore (car (returnItem 4 gameList 0)))
        (humanHand (car (returnItem 5 gameList 0)))
        (hTrainWithEngine (car (returnItem 6 gameList 0)))
        (hTrain (cdr (getTrain (car (returnItem 6 gameList 0))))) ; cdr because first tile is engine tile
        (mTrain (car (returnItem 7 gameList 0)))
        (boneyard (car (returnItem 8 gameList 0)))
        (nextPlayer (getPlayerFromFile (car (returnItem 9 gameList 0))))
        (engine (car hTrainWithEngine))
        (markers (cons (getMarker hTrainWithEngine) (cons (getMarker (car (returnItem 3 gameList 0))) ())) )
        (hasToPlayOD '(t t))
        (trains (cons mTrain (cons hTrain (cons cTrain ()))))
        )
   
   (close in)
    (ext:run-shell-command "cls")
    (terpri)
    (terpri)
    ;call playturn
    (playTurn trains
              (getEligibleTrains markers nextPlayer hasToPlayOD trains engine)
              humanHand
              computerHand
              boneyard
              engine
              nextPlayer 
              markers
              (cons humanScore (cons computerScore ()))
              roundNum
    ) 
  )
)

;; Write to file
(defun saveGame(trains humanHand computerHand engine boneyard player markers gameScores roundNum)
  (let* ((mtrain (returnTrain 0 trains))
         (htrain (getTrainWithMarker (returnTrain 1 trains) (car markers) 'H engine))
         (ctrain (getTrainWithMarker (returnTrain 2 trains) (car (cdr markers)) 'C engine))
         )
    (with-open-file (stream (read) :direction :output)
      (format stream "~A~%" "(") (terpri stream)
      (format stream "~A~%" roundNum) (terpri stream)
      (format stream "~A~%" (car (cdr gameScores))) (terpri stream)
      (format stream "~A~%" computerHand) (terpri stream)
      (format stream "~A~%" ctrain) (terpri stream)
      (format stream "~A~%" (car gameScores)) (terpri stream)
      (format stream "~A~%" humanHand) (terpri stream)
      (format stream "~A~%" htrain) (terpri stream)
      (format stream "~A~%" mtrain) (terpri stream)
      (format stream "~A~%" boneyard) (terpri stream)
      (format stream "~A~%" (getPlayerForFile player)) (terpri stream)
      (format stream "~A~%" ")") (terpri stream)
    )
    (print "Your game was successfully saved.")
  )
  
)

; *********************************************************
; Functions for starting a game and a round
; *********************************************************

; scoreHuman, scoreComputer are the game scores of the players
(defun startRound(roundNum scoreHuman scoreComputer)
  (let* ( (player (declareFirstPlayer scoreHuman scoreComputer))
          (engine (getEngine roundNum (createDeck 9 9) )) 
          (deck (shuffleDeck  (removeTile (cdr engine) (createDeck 9 9) 0)))  
          (humanHand (dealDeck deck 0 16) )
          (computerHand (dealDeck deck 16 16) )
          (boneyard (dealDeck deck 32 22) ) 
          (hTrain ())
          (cTrain ())
          (mTrain ())
          (markers '(nil nil))
          (hasToPlayOD '(t t))
        )
  
    (playTurn '(() () ()) ;eligible trains all three nil for now
               (getEligibleTrains markers player hasToPlayOD '(() () ()) (car engine))
               humanHand
               computerHand
               boneyard
               (car engine)
               player ;assuming the first player is human
               markers
               (cons scoreHuman (cons scoreComputer ()))
               roundNum ) 
  )
)

(defun startGame()
  (message "Press 'L' to load game from a file and 'C' to start a fresh game.")
  (let* ((userInput (validateInput '(L C) (read)) ))
    ;(princ userInput)
    (cond 
      ; user entered invalid input
      ((equal userInput "z") (startGame))
      ((equal userInput 'L) (readFromFile))
      ((equal userInput 'C)  (startRound 1 0 0))
    )
  )
)


;; starting game from here

(message "    .........Welcome to Mexican Train.......... ") 
(terpri)
(startGame)