; *********************************************
; Helper function for input validation
; *********************************************

; *********************************************************************
; Function Name: validateInput
; Purpose: to check if the user's input is valid
; Parameters: a list of validInputs, and user input
;            
; Return Value: userinput if valid, "z" if not
; Algorithm:
;           1) checks if the user input belongs to the valid Inputs list
;           2) If yes, returns the input, if not returns "z" 
; Assistance Received: none
; ********************************************************************* */
(defun validateInput(validInputs input)
  (cond ( (member input validInputs )
            input )
        ( t "z")
  )
)

; **********************************************************
; Source Code to toss a coin and determine the first player
; **********************************************************

; *********************************************************************
; Function Name: callASide
; Purpose: to make user call a side for coin toss
; Parameters: none
;            
; Return Value: the side called (h ot t)
; Algorithm:
;           1) asks user to choose between h and t
;           2) calls the function recursively until user enter a vlaid input
;           3) returns the letter chosen by user
; Assistance Received: none
; ********************************************************************* */
; User calls a side of the coin
(defun callASide()
  (message "Please enter 'H' for head and 'T' for tail.")
  (let ( (call (validateInput '(H T) (read) )) )
    (cond ( (string= call "z" ) (callASide) )
          (t call))
  )
)

; *********************************************************************
; Function Name: tossACoin
; Purpose: to get coin toss result after user calls a side
; Parameters: none
;            
; Return Value: 1 if user won the toss -1 otherwise
; Algorithm:
;           1) assigns 0 as head, 1 as tail, generates a random number between
;               0 and 1, stores user call in a call variable
;           2) returns 1 if the random num = user's call otherwise returns -1
; Assistance Received: none
; ********************************************************************* */
; tossing a coin
(defun tossACoin()
  (let* ( (seed (make-random-state t)) (head 0) (tail 1) ( toss (random 2 seed) ) ( call (callASide) ) )
    (cond ((string= call "H" )
              (cond ((= toss head) 1)
                    (t -1)))
          ((string= call "T" )
              (cond ((= toss tail) 1)
                    (t -1)))
          (t -1 ))
  )
)

; *********************************************************************
; Function Name: declareFirstPlayer
; Purpose: determining the first player in the game, in each round
; Parameters: human and computer scores
;            
; Return Value: the first player (i.e. h for human and c for computer)
; Algorithm:
;           1) if the scores are equal, first player is the one who wins the toss
;           2) else, first player is the one who has lower score
; Assistance Received: none
; ********************************************************************* */
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


; *********************************************************************
; Function Name: createDeck
; Purpose: to create a deck of 55 unique tiles
; Parameters: the highest tile numbers, in this case 9 and 9
;            
; Return Value: a list of 55 tiles
; Algorithm:
;           1) uses recursion to generate nested loop result
;           2) base case: returns empty list when the i and j values are both -1
;           3) call function recursively by subtracting 1 from j until it is -1
;               and then subtract 1 from i until both i and j are -1
;           4) construct a list of tile (i j) in the process, each time the 
;               default condition is run
; Assistance Received: none
; ********************************************************************* */
(defun createDeck(i j)
  (cond ( (and (= i -1) (= j -1)) ())

        ((= j -1) (createDeck (- i 1) (+ j i)))

        (T (let ((tile (cons i (cons j ())))) 
            (cons tile (createDeck i (- j 1))) ))
  )
)

; *********************************************************************
; Function Name: shuffleDeck
; Purpose: to shuffle the deck of 55 tiles
; Parameters: the list of tiles (deck)
;            
; Return Value: shuffled deck
; Algorithm:
;           1) call the function recursively until all the tiles are shuffled
;           2) generate a random number (index) between 0 to deck length
;           3) construct a list of the tile at index and the rest of the deck
;              calling the function recursively
; Assistance Received: none
; ********************************************************************* */
(defun shuffleDeck(deck)
  (cond ( (null deck) ())
        (T (let* ((seed (make-random-state t)) (index (random (length deck) seed) ))
            (cons (nth index deck) (shuffleDeck (removeTile index deck 0)))) )
  )
)

; *********************************************************************
; Function Name: dealDeck
; Purpose: to get a sublist of a deck for dealing purpose
; Parameters: deck, start index to deal from, number of items to deal
;            
; Return Value: a sublist of the deck (list of tiles)
; Algorithm:
;           1) basecase: if deck is empty return empty list
;           2) if index is greater than 0, call dealDeck recursively
;              subtracting the index and taking rest of the deck (this 
;              is being done so that we reach the start index element on the actual deck)
;           3) when index<0, construct the sublist with the first element of the deck
;              until the length drop to 0
; Assistance Received: http://www.lee-mac.com/sublist.html
; ********************************************************************* */
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
;           2) if roundNum is exactly divisible by 10, engine tile: (0,0)
;           3) else engine tile is 10 - (roundNum mod 10)
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
  (cond ((= counter index) (cdr deck))
        (T (cons (car deck) (removeTile index (cdr deck) (+ counter 1))))
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

; *********************************************************************
; Function Name: isDoubleTile
; Purpose: to determine if a tile is double tile
; Parameters: the tile being examined
;            
; Return Value: t or nil
; Algorithm:
;           1) check if the left pip and right pip of the tile are equal
;           2) if yes return t if no return nil
; Assistance Received: none
; ********************************************************************* */
(defun isDoubleTile(tile)
  (cond 
    ((null tile) nil)
    ((= (car tile) (car (cdr tile))) t)
    (t nil)
  )
)

; *********************************************************************
; Function Name: getTileSum
; Purpose: to return the sum of tile's left pips and right pips
; Parameters: tile
;            
; Return Value: sum of left and right pips
; Algorithm:
;           1) add the left pip of tile and right pip of tile 
; Assistance Received: none
; ********************************************************************* */
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
; Purpose: to compute the index of a given tile from the list of tiles
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

; *********************************************************************
; Function Name: addTileToTrain
; Purpose: to add a tile to a train
; Parameters: train and tile being added to the train
;            
; Return Value: train (list of tile) with the new tile appended at the end
; Algorithm:
;           1) append function to append the list of given tile to the train
; Assistance Received: none
; ********************************************************************* */
(defun addTileToTrain(train tile)
  (append train (list tile))
)

; *********************************************************************
; Function Name: reverseTrain
; Purpose: to reverse a train entirely (reversing the tile inside it as well)
; Parameters: train (list of tiles)
;            
; Return Value: completely reversed train
; Algorithm:
;           1) append the reverse of rest of train to the front of the train's first
;              reversed tile until the train is null
;           2) base case: train has no items left to reverse
; Assistance Received: none
; ********************************************************************* */
(defun reverseTrain(train)
  (cond ((null train) ())
        (t (append (reverseTrain (cdr train)) (cons (reverse (car train)) ()) ))
  )
)


; *********************************************************************
; Function Name: getLastTile
; Purpose: to get the last tile of a train
; Parameters: train and engine tile
;            
; Return Value: last tile of train or engine tile if train is empty
; Algorithm:
;           1) check if train is empty, if yes return engine tile
;           2) if not return the last tile
; Assistance Received: none
; ********************************************************************* */
(defun getLastTile(train engine)
  (cond ( (null train) engine )
        ( t (car (reverse train)) ) )
)

; *********************************************************************
; Function Name: returnTrain
; Purpose: return the desired train from the list of trains
; Parameters: index of desired train, list of trains
;            
; Return Value: train
; Algorithm:
;           1) trains are always in the list: Mexican, Human, Computer
;           2) return the train evaluting the index and the order above
; Assistance Received: none
; ********************************************************************* */
(defun returnTrain(index trains)
  (cond ( (= index 0) (car trains))
        ( (= index 1) (car (cdr trains)))
        ( t (car (cdr (cdr trains))))
  )
)

; *********************************************************************
; Function Name: getTrainName
; Purpose: to return the train name for the given index
; Parameters: index
;            
; Return Value: TRAIN NAME
; Algorithm:
;           1) evalute the index (0: Mexican, 1: Human, 2: Computer)
;           2) return the train name for the index
; Assistance Received: none
; ********************************************************************* */
(defun getTrainName(index)
  (cond 
    ((= index 0) 'M-train)
    ((= index 1) 'H-train)
    ((= index 2) 'C-train)
    (t 'Invalid-train)
  )
)

; *********************************************************************
; Function Name: isOrphanTrain
; Purpose: determine of a train is an orphan train
; Parameters: train and engine tile
;            
; Return Value: t if orphan nil otherwise
; Algorithm:
;           1) if train is empty it is not orphan
;           2) if the last tile of the train is a double tile, return t
;           3) otherwise false
; Assistance Received: none
; ********************************************************************* */
(defun isOrphanTrain (train engine)
  (cond 
    ; if given train is empty its not an orphan train.
    ((= (length train) 0) nil)
    ((equal (isDoubleTile (getLastTile train engine)) t) t)
    (t nil)
  )
)

; *********************************************************************
; Function Name: getUpdatedTrains
; Purpose: to update the lists of trains, after a tile is added to a train
; Parameters: lists of trains, the played train, and the played train index
;            
; Return Value: list of trains with the played train replaced with the old train
; Algorithm:
;           1) if playedTrainIndex is 0 replace the 0th train in the trains list with 
;               the played train 
;           2) to do so, return a new list of trains by appending the played train in place
;              and the other two trains in place
; Assistance Received: none
; ********************************************************************* */
(defun getUpdatedTrains(trains playedTrain playedTrainIndex)
  (let* ((updatedTrains ()))
    (cond ( (= playedTrainIndex 0)
            (append (append (append updatedTrains (list playedTrain)) (list (returnTrain 1 trains)) ) (list (returnTrain 2 trains)) )  )

          ( (= playedTrainIndex 1) 
            (append (append (append updatedTrains (list (returnTrain 0 trains))) (list playedTrain) ) (list (returnTrain 2 trains)) )  ) 

          ( t  
            (append (append (append updatedTrains (list (returnTrain 0 trains))) (list (returnTrain 1 trains)) ) (list playedTrain) )  )                            
    )
  )
)

; *********************************************************************
; Function Name: isEligible
; Purpose: to check if a train name is listed in eligible train names list
; Parameters: trainName being evaluated, list of train names
;            
; Return Value: t or nil
; Algorithm:
;           1) get the index of the trainName in the eligTrainNames list
;           2) if index = -1 return nil else return t
; Assistance Received: none
; ********************************************************************* */
; trainName = train's name whose eligibility is being checked, eligTrainNames: list of eligtrainNames
(defun isEligible(trainName eligTrainNames)
  (cond ( (= (getIndexNames trainName eligTrainNames (length eligTrainNames)) -1) () )
        ( t t)
  )
)



; ************************************************************
; Player Hand: Source Code for evaluating player hand tiles
; ************************************************************


; *********************************************************************
; Function Name: hasPlayableTile
; Purpose: to determine if the player has a playable tile for a train
; Parameters: Train being checked 
;            
; Return Value: t if player has playable tile for te train, nil otherwise
; Algorithm:
;           1)base case: playerhand is empty means there are no playable tiles for the train
;           2) if the first element in playerhand is not validTile, call 
;              hasPlayableTile recursively for rest of playerHand
;           3) otherwise return true, meaning the player has a playable tile for the train
; Assistance Received: none
; ********************************************************************* */
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

; *********************************************************************
; Function Name: canPlay
; Purpose: to check if user can play at all, or user has to draw from boneyard
; Parameters: trains, eligible train names, player hand, engine tile
;            
; Return Value: t or nil
; Algorithm:
;           1) check for each of the three trains if it is eligible for the player
;               for the turn and has a playable tile
;           2) if any of the train is eligible and has playable tile return t immediately
;           3) if none of the trains are both eligible and hasplayable tile return nil
; Assistance Received: none
; ********************************************************************* */
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

; *********************************************************************
; Function Name: validTilesList
; Purpose: to return the list of valid tiles in player hand for a chosen train
; Parameters: selected train, player hand, engine tile
;            
; Return Value: list of valid tiles
; Algorithm:
;           1)basecase: playerhand is empty means there are no more valid tiles for the train 
;           2)if first tile in playerhand not valid, call validTilesList recursively for rest of playerHand 
;           3)otherwise concatenate the valid tile and call validTilesList for rest of the tiles in hand 
; Assistance Received: none
; ********************************************************************* */
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

; *********************************************************************
; Function Name: findDoubleTile
; Purpose: to check if player has a playable double tile in the validtiles list
; Parameters: validTiles list
;            
; Return Value: nil or the double tile
; Algorithm:
;           1) base case: list is empty, return nil
;           2) if a double tile found, return that tile immediately
;           3) call function recursively for rest of the tiles until a double tile
;              is found or list is null
; Assistance Received: none
; ********************************************************************* */
; 
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

; *********************************************************************
; Function Name: makeOrphanTrain
; Purpose: to check if a player can make an orphan train during its turn
; Parameters: lists of all 3 trains, elig train names, hand, engine, eligibility 
;           truth values for three trains
;            
; Return Value: index of the train that can be oprhan, if none found -1
; Algorithm:
;           1) for all eligible trains,(one at a time) check if the valid tiles list
;               have a double tile
;           2) if yes, remove the double tile from the hand, and check if the player
;             still has a playable tile in another train after the valid tile is removed
;           3) if yes, return the initial train's index since it can be made orphan
;           4) repeat same process for all three trains, if none found return -1 
; Assistance Received: none
; ********************************************************************* */
; to check if player can make an orphan train
(defun makeOrphanTrain(trains eligTNames hand engine M_elig H_elig C_elig)
  (cond
    ((= (length eligTNames) 0) -1) ; basecase, player can't make orphan train
    ((equal (car eligTNames) 'm) 
      (cond
        ; no double tile to play on m train 
        ( (equal (findDoubleTile (validTilesList (returnTrain 0 trains) hand engine)) nil)
          (makeOrphanTrain trains (cdr eligTNames) hand engine M_elig H_elig C_elig))
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
          (t (makeOrphanTrain trains (cdr eligTNames) hand engine M_elig H_elig C_elig))))
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
          (t (makeOrphanTrain trains (cdr eligTNames) hand engine M_elig H_elig C_elig))))
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
          (t (makeOrphanTrain trains (cdr eligTNames) hand engine M_elig H_elig C_elig)))) )
    )
  )
)

; *********************************************************************
; Function Name: findLargestTile
; Purpose: to find the largest playable tile from the list of valid tiles
; Parameters: validTiles list, (-1 -1) tile for comparison purpose
;            
; Return Value: the largest tile
; Algorithm:
;           1) basecase: validTiles list is empty, return the largest tile found so far
;           2) if the first element in validTile is larger, call findLargestTile 
;               recursively by replacing the previous largestTile
;           3) otherwise call findLargestTile recursively without replacing the largest tile
; Assistance Received: none
; ********************************************************************* */
(defun findLargestTile (validTiles largestTile)
    (cond 
        ; playerhand is empty return the list of the largest tile
        ( (null validTiles) largestTile ) 

        ; if the first element in validTile is larger, call findLargestTile recursively by replacing the previous largestTile
        ( (> (car (cdr (car validTiles))) (car (cdr largestTile)))
          (findLargestTile (cdr validTiles) (car validTiles))
        )
        ; otherwise call findLargestTile recursively without replacing the tile
        ( t (findLargestTile (cdr validTiles) largestTile) )
    )
)

; *********************************************************************
; Function Name: findLargestTrain
; Purpose: to Return the index of the train that can play the largest tile
; Parameters: playablelists of boolean values, lists of 3 trains, player's hand, engine
;            
; Return Value: the index of the train 
; Algorithm:
;           1) for all the playable trains, compares the sum of the largest tiles in each train 
;           2) uses getMax function to do so which returns the index of the max number in a list
;           3) returns the index returned by get max as a train that can play the lasrgest tile
; Assistance Received: none
; ********************************************************************* */
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
       (getMax (cons mTLrgstTileSum (cons hTLrgstTileSum (cons cTLrgstTileSum ()))) -5 0 0))
      ((= playableTrainCount 2)
        (cond 
          ((equal (and mTPlayable hTPlayable) t) (getMax (cons mTLrgstTileSum (cons hTLrgstTileSum ())) -5 0 0))
          ; getMax list construction (-1, hmax, cmax), because m-train is not available, put -1 for m-max
          ((equal (and hTPlayable cTPlayable) t) (getMax (cons -1 (cons hTLrgstTileSum (cons cTLrgstTileSum ()))) -5 0 0))
          ((equal (and mTPlayable cTPlayable) t) (getMax (cons mTLrgstTileSum (cons -1 (cons cTLrgstTileSum ()))) -5 0 0))
        ))
      (T (cond ((equal mTPlayable t) 0)
              ((equal hTPlayable t) 1)
              ((equal cTPlayable t) 2)))
    )
  )
)

; *********************************************************
; Help Human functions.
; *********************************************************

; *********************************************************************
; Function Name: pickAtileHelpMode
; Purpose: to let computer pick a tile for human for help mode
; Parameters: human hand, picked train list, picked train index, engine
;            
; Return Value: the picked tile
; Algorithm:
;           1) find the largest or double playable tile for the train
;           2) returns double tile if found otherwise returns largest tile
; Assistance Received: none
; ********************************************************************* */
(defun pickAtileHelpMode(hand pickedTrain pickedTrainIndex engine)
  (let* ( (doubleTile (findDoubleTile (validTilesList pickedTrain hand engine)))
          (largeTile (findLargestTile (validTilesList pickedTrain hand engine) '(-1 -1)))
          (tileIndex (getTileIndex largeTile hand (length hand))) ) 
    (cond 
      ; if double tile not found pick largeTile
      ((equal doubleTile nil) largeTile)

      ; if double tile found, pick double tile
      (t doubleTile))
  )
)

; *********************************************************************
; Function Name: helpHuman
; Purpose: to help human by printing the best possible move for the turn
; Parameters: trains list, elig train names list, human hand, engine
;            
; Return Value: call to printHumanHelp with best tile and train
; Algorithm:
;           1) used computer strategy function to find the best moves
;           2) if possible to make orphan train gives highest priority to it
;           3) suggest not to break a human made orphan train if any
;           4) Suggests to play the largest tile in a turn
; Assistance Received: none
; ********************************************************************* */
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
                  ((= canMakeOT 2) (printHumanHelp bestTile "C-train" "C-train is marked and you can make C-train an orphan train."))) 
           ))

          ; if m-train is an orphan train, pick another train
          ((equal mtrainOrphan t) 
            (let* ((bestTrain (findLargestTrain (cons nil (cons canPlayHtrain (cons canPlayCtrain ()))) trains hand engine))
                   (bestTile (pickAtileHelpMode hand (returnTrain bestTrain trains) bestTrain engine)))
              (printHumanHelp bestTile (getTrainName bestTrain) "this is the largest tile you can play without breaking the orphan train you created (M-train)")
            ))

          ; if h-train is an orphan train, pick another train
          ((equal htrainOrphan t) 
            (let* ((bestTrain (findLargestTrain (cons canPlayMtrain (cons nil (cons canPlayCtrain ()))) trains hand engine))
                   (bestTile (pickAtileHelpMode hand (returnTrain bestTrain trains) bestTrain engine)))
              (printHumanHelp bestTile (getTrainName bestTrain) "this is the largest tile you can play without breaking the orphan train you created (H-train)")
            ))

          ; if c-train is an orphan train, pick another train
          ((equal ctrainOrphan t) 
            (let* ((bestTrain (findLargestTrain (cons canPlayMtrain (cons canPlayHtrain (cons nil ()))) trains hand engine))
                   (bestTile (pickAtileHelpMode hand (returnTrain bestTrain trains) bestTrain engine)))
              (printHumanHelp bestTile (getTrainName bestTrain) "this is the largest tile you can play without breaking the orphan train you created (C-train)")
            ))

          ; if m-train is not yet started, suggest to start m-train
          ((equal mtrainNotStarted t)
           (printHumanHelp (pickAtileHelpMode hand (returnTrain 0 trains) 0 engine) "M-train" "M-train was not started yet"))

          ; if none of the trains are orphan, return the train that can play the largest tile
          (t 
            (let* ((bestTrain (findLargestTrain playableList trains hand engine))
                   (bestTile (pickAtileHelpMode hand (returnTrain bestTrain trains) bestTrain engine))
                   (trainName (getTrainName bestTrain)))
              (cond ((= bestTrain 2) (printHumanHelp bestTile trainName " C-train is marked and you can play the largest tile in C-train among all eligible trains"))
                    (t (printHumanHelp bestTile trainName "it is the largest tile that can be played among all eligible trains")))))
        )
      )
      
      ((= playableTrainCount 0) (print " You do not have any playable tile. "))

      ; pick a train from the playable trains that can play the largest tile
      (t 
       (let* ((bestTrain (findLargestTrain playableList trains hand engine))
              (trainName (getTrainName bestTrain))
              (bestTile (pickAtileHelpMode hand (returnTrain bestTrain trains) bestTrain engine)))
          (cond 
            ((= bestTrain 2)
              (cond ((= (length eligTNames) 1) (printHumanHelp bestTile trainName "C-trian is a must play orphan train"))
                    (t (printHumanHelp bestTile trainName "C-train is marked and you can play the largest tile in C-train") ))
            )
            (t (printHumanHelp bestTile trainName "it is the largest tile that can be played among all eligible trains"))
          )))
    )
  )
)

; *********************************************************************
; Function Name: 
; Purpose: 
; Parameters: 
;            
; Return Value: 
; Algorithm:
;           1) 
;           2) 
; Assistance Received: none
; ********************************************************************* */
(defun printHumanHelp (tile trainName message)
  (terpri)
  (princ "I suggest you play tile ") (princ tile) (princ " on ")
  (princ trainName) (princ " because ") (princ message)
  (terpri)
)

; *********************************************************
; Helper functions.
; *********************************************************

; *********************************************************************
; Function Name: getTrueCount
; Purpose: to return the # of 't's from a list of true and nil
; Parameters: list of t and nils, a counter starting from 0 initially
;            
; Return Value: the # of 't's from a list of true and nil
; Algorithm:
;           1) if list is empty return the counter value
;           2) if not, check is first item if list is t, if yes increament
;              counter and call function recursively
;           3) if not just call function recursively without increamenting counter
; Assistance Received: none
; ********************************************************************* */
(defun getTrueCount(aList counter)
  (cond 
    ; base case
    ((null aList) counter)
    ((equal (car aList) t) (getTrueCount (cdr aList) (+ counter 1)))
    (t (getTrueCount (cdr aList) counter))
  )
)

; *********************************************************************
; Function Name: getMax
; Purpose: to get the index of the largest number in a list
; Parameters: list of #s, maxNum = -5, maxIndex = 0, counter= 0
;            
; Return Value: index of largest #
; Algorithm:
;           1) if aList is empty return the maxIndex
;           2) if the first element in aList is larger, call getMax recursively 
;              by replacing the previous maxIndex
;           3) otherwise call getMax recursively without replacing the maxIndex
; Assistance Received: none
; ********************************************************************* */ 
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

; *********************************************************************
; Function Name: playerHasToPlayOD
; Purpose: to determine is the player has to play against an orphan train in its turn
; Parameters: hasToPlayODList (first item for human second for computer (t t)), player
;            
; Return Value: t or nil
; Algorithm:
;           1) if player is human, checks if the first item of hasToPlayODList is t
;           2) if player is computer, checks if the second item of hasToPlayODList is t
;           3) if yes returns t otherwise returns nil
; Assistance Received: none
; ********************************************************************* */
(defun playerHasToPlayOD(hasToPlayODList player)
  (cond
    ; player is human
    ((equal player 'h)
      (cond 
        ; human has to play orphan train
        ((equal (car hasToPlayODList) t) t)
        ; human does not have to play orphan train
        (t nil)))

    (T ; player is computer
      (cond 
        ; computer has to play orphan train
        ((equal (car (cdr hasToPlayODList)) t) t)
        ; computer does not have to play orphan train
        (t nil)))
  )
)

; *********************************************************************
; Function Name: getEligibleTrains
; Purpose: to return the eligible trains names for a player in its turn
; Parameters: markers, player, hasToPlayODList, trains, engine
;            
; Return Value: list of names of eligible trains
; Algorithm:
;           1) check if there is an orphan train that player has to play against, 
;              if yes return the name of the train
;           2) check if opponent train has marker if yes, return m, h,c as eligible
;           3) if not return player's personal train and mexican train as eligible
; Assistance Received: none
; ********************************************************************* */
(defun getEligibleTrains(markers player hasToPlayODList trains engine)
  ; in markers list first item is human marker second item is computer's marker
  (cond 
    ((equal player 'h)
      (cond 
        ;check if there is an orphan train that human has to play against, if yes return the name of the train
        ((equal (and (playerHasToPlayOD hasToPlayODList 'h) (> (length (getOrphanTrains trains engine 0 ())) 0)) t) 
          (getOrphanTrains trains engine 0 ()))
        ;check if c-train has marker if yes, return m,h, and c as eligible
        ((equal (car (cdr markers)) t) '(m h c))
        ;otherwise only m, h eligible for human player
        (t '(m h))))

    (T (cond 
          ((equal (and (playerHasToPlayOD hasToPlayODList 'c) (> (length (getOrphanTrains trains engine 0 ())) 0) ) t)
            (getOrphanTrains trains engine 0 ()))
          ((equal (car markers) t) '(m h c))
          (t '(m c))))
  )
)

; *********************************************************************
; Function Name: getOrphanTrains
; Purpose: to get a list of orphan train names, in a turn if any 
; Parameters: list of trains, engine, a counter, orphantrains list initially ()
;            
; Return Value: list of orphantrains name
; Algorithm:
;           1) if getOrphanTrains called for the first time check if human train is orphan
;           2) when called for the second time check if mexican train is orphan
;           3) when called the third time check if computer train is orphan
;           4) in each call append the train's name to orphanTrains list if it is orphan
;           5) if getOrphanTrains was called for the 4th time, return orphan trains list
; Assistance Received: none
; ********************************************************************* */
(defun getOrphanTrains(trains engine counter orphanTrains)

  (cond
    ; if getOrphanTrains was called for the 4th time, return orphan trains
    ((= counter 3) orphanTrains)

    ; when called the first time check if human train is orphan
    ((and (= counter 0) (equal (isOrphanTrain (returnTrain 1 trains) engine) t) )
     (getOrphanTrains trains engine (+ counter 1) (append orphanTrains '(h))) )

    ; when called the second time check if mexican train is orphan 
    ((and (= counter 1) (equal (isOrphanTrain (returnTrain 0 trains) engine) t)) 
     (getOrphanTrains trains engine (+ counter 1) (append orphanTrains '(m))) )

    ; when called the third time check if computer train is orphan
    ((and (= counter 2) (equal (isOrphanTrain (returnTrain 2 trains) engine) t)) 
     (getOrphanTrains trains engine (+ counter 1) (append orphanTrains '(c))) )
    
    ;if a train was not orphan do not append the train name to orphanTrains
    (t (getOrphanTrains trains engine (+ counter 1) orphanTrains) )
  )
)

; *********************************************************
; Source Code for picking train and tile for Human.
; *********************************************************

; *********************************************************************
; Function Name: pickAtrain
; Purpose: to allow human player to pick a train from eligible trains
; Parameters: list of all 3 trains, list of eligTrainNames 
;            
; Return Value: index of the picked train
; Algorithm:
;           1) ask for user input and check if the input is valid, i.e
;              human picked one of the eligible trains
;           2) if valid, return the index of the train picked
; Assistance Received: none
; ********************************************************************* */
(defun pickAtrain(trains eligTrainNames hand engine)
  (terpri) (princ "These are your valid trains for the turn: ") (princ eligTrainNames) (terpri)
  (message "Please pick from eligible trains to place a tile on.")

  (let* ( (pickedTrain (validateInput eligTrainNames (read) )) )
          
    (cond 
      ;; human did not enter a valid letter for the train
      ((string= pickedTrain "z" ) (pickAtrain trains eligTrainNames  hand engine))
          
      ;; human chose to pick a valid train, now return the train
      (t (cond 
          ((string= pickedTrain "M") 
            (cond ((equal (hasPlayableTile (returnTrain 0 trains) hand engine) nil)
                   (message "M-train is valid but you do not have any playable tile for M-train.") 
                   (pickAtrain trains eligTrainNames hand engine))
                  (t (message "picked train: M train") '0) ))
                          
          ((string= pickedTrain "H")
            (cond ((equal (hasPlayableTile (returnTrain 1 trains) hand engine) nil)
                   (message "H-train is valid but you do not have any playable tile for H-train.") 
                   (pickAtrain trains eligTrainNames hand engine))
                  (t (message "picked train: H train") '1) ))
          
          
          (t (cond ((equal (hasPlayableTile (returnTrain 2 trains) hand engine) nil)
                   (message "C-train is valid but you do not have any playable tile for C-train.") 
                   (pickAtrain trains eligTrainNames hand engine))
                  (t (message "picked train: C train") '2) )))
      ))
  )
)

; *********************************************************************
; Function Name: pickAtile
; Purpose: to let human player pick a tile to place on the picked train
; Parameters: humanHand, pickedTrain, engine
;            
; Return Value: the picked-validated tile and the index of the tile in the player's hand
; Algorithm:
;           1) take user input and validate
;           2) check if the tile picked is playable on the picked train
;           3) if yes return the tile and its index, if not ask user to pick tile again
; Assistance Received: none
; ********************************************************************* */
(defun pickAtile(humanHand pickedTrain engine)
  (message "Please enter the number corresponding to the tile from your hand")

  (let* ((index (read)) (pickedTile (returnTile index humanHand 0)))
    (cond ((or (null pickedTile) (equal pickedTile '(nil))) 
            (pickAtile humanHand pickedTrain engine))

          (t (let* ((checkedTile (validateTile (car pickedTile) pickedTrain engine)))
              (cond 
                ((null checkedTile)
                  (message "Picked tile pips is not equal to the train's last tile pip.")
                  (pickAtile humanHand pickedTrain engine))

                (t (cons checkedTile (cons index ()))))))
    )
  ) 
)


; *********************************************************
; Source Code for picking train and tile for Computer.
; *********************************************************

; *********************************************************************
; Function Name: pickAtrainComputer
; Purpose: to make computer pick the best train to play on
; Parameters: trains list, eligTNames list, computer hand, engine
;            
; Return Value: index of the picked train
; Algorithm:
;           1) check for playable train count and apply conditions accordingly
;           2) if possible to make orphan train gives highest priority to it
;           3) picks train such that computer made orphan train if any is not broken
;           4) picks the train that can play the largest tile in a turn
; Assistance Received: none
; ********************************************************************* */ 
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
                    (t pickedTrainIndex))))
        )
      )
      
      ; pick a train from the playable trains that can play the largest tile
      (t 
       (let* ((pickedTrainIndex (findLargestTrain playableList trains hand engine))
              (trainName (getTrainName pickedTrainIndex)))
          (cond ((= pickedTrainIndex 1)
                (cond ((= (length eligTNames) 1) (princ "H-train was a must play orphan train therefore,") pickedTrainIndex)
                      (t (princ "H-train had a marker therefore, ") pickedTrainIndex)))
                (t pickedTrainIndex))))
    )
  )
)

; *********************************************************************
; Function Name: pickAtileComputer
; Purpose: to let computer pick the best tile
; Parameters: computer hand, pickedTrain, pickedTrainIndex, engine
;            
; Return Value: list of picked tile and its index
; Algorithm:
;           1) looks for double tile playable for picked train, if found returns it
;           2) if not returns the largest playable tile for the train
; Assistance Received: none
; ********************************************************************* */
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
       (cons doubleTile (cons (getTileIndex doubleTile hand (length hand)) ()))))
  )
)

; *********************************************************************
; Function Name: printPlayerStrategy
; Purpose: to print computer player strategy
; Parameters: picked trainIndex, picked tile, message
;            
; Return Value: message printed
; Algorithm:
;           1) constructs and prints the computer player strategy
; Assistance Received: none
; ********************************************************************* */
(defun printPlayerStrategy(trainIndex tile message)
  (princ " Computer chose to play ") (princ tile) (princ " on ")
  (princ (getTrainName trainIndex)) (princ " as ")
  (princ message) (terpri)
  (princ "-----------------------------------------------------------------------------------------------------------------------------")
  (terpri)
)

; *********************************************************************
; Function Name: validateTile
; Purpose: to validate the picked tile before adding to the train
; Parameters: tile chosen to play on a train, the train, engine
;            
; Return Value: validated tile if tile valid, if invalid returns ()
; Algorithm:
;           1) checks if the tiles right or left pip is equal to train's
;              last tile right pips
;           2) if yes returns the tile, by versing it if needed
;           3) if not, returns nil
; Assistance Received: none
; ********************************************************************* */
(defun validateTile(tile train engine)
  (let* ((tileRightPips (car (cdr tile))) (tileLeftPips (car tile)) (trainLastPips (car (cdr (getLastTile train engine)))) )
    (cond ( (= tileLeftPips trainLastPips) tile)
          ( (= tileRightPips trainLastPips) (reverse tile))
          ( t () ))
  )
)
 
; *********************************************************
; Source Code for updating data after player makes a move
; *********************************************************

; *********************************************************************
; Function Name: updateMarker
; Purpose: to update the marker list after each player's turn
; Parameters: playedTrainIndex, player, markers list (eg. (t nil) )
;            
; Return Value: updated markers list
; Algorithm:
;           1) if player played on its own train, update its marker to nil
;           2) if player did not play on its train, leave the marker as it is
;           3) leave the opponents marker as it is
; Assistance Received: none
; ********************************************************************* */
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

; *********************************************************************
; Function Name: updateHasToPlayOD
; Purpose: updates whether the player has to play on the orphan double train next
; Parameters: playedTile, currentPlayer
;            
; Return Value: HasToPlayOD list
; Algorithm:
;           1)if  player played a double tile, player does not have to play the OD
;             next, but the opponent has to so update to nil for player and t for opponent 
;           2) if current player didn't play a double tile, both players have to play OD
;               if any
; Assistance Received: none
; ********************************************************************* */
(defun updateHasToPlayOD(playedTile currentPlayer)
  (cond
    ((equal currentPlayer 'h)
      (cond
        ; player played a double tile, player does not have to play the orphan train next 
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

; *********************************************************************
; Function Name: getNextPlayer
; Purpose: to get the mext player after a turn
; Parameters: playedTile, currentPlayer
;            
; Return Value: next player name (h or c)
; Algorithm:
;           1) if current player played a double tile, next player is current player
;           2) else next player is the next player
; Assistance Received: none
; ********************************************************************* */
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

; *********************************************************************
; Function Name: drawMove
; Purpose: to let players make a move by drawing from boneyard
; Parameters: trains, eligTrainNames, humanHand, computerHand, engine, boneyard, 
;             player, markers, gameScores, roundNum
;            
; Return Value: calls playTurn for next turn
; Algorithm:
;           1) if player is human, ask to press b to draw tile
;           2) after tile is drawn, ask players to pick a train to play the drawn tile on
;           3) if players could not play the drawn tile, add them to their hands, and
;              place marker on their train
;           4) if player could play the drawn tile, update all the data after placing tile on train
;           5) pass turn to next player(could be the same player) and call playTurn
; Assistance Received: none
; ********************************************************************* */
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
                    (pickedTrainIndex (pickAtrain trains eligTrainNames (addTileToTrain humanHand drawnTile) engine))
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

; *********************************************************************
; Function Name: makeMove
; Purpose: to let players make a move in their turn
; Parameters: trains, eligTrainNames, humanHand, computerHand, engine, boneyard, 
;             player, markers, gameScores, roundNum
;            
; Return Value: 
; Algorithm:
;           1) if player is human, ask for user inputs according to the display menu
;           2) check if players have playable tile, if not call draw move
;           2) if drawing not needed, ask players to pick a train
;           3) ask players to pick a tile for the train
;           4) add tile to train, update all the data after placing tile on train
;           5) pass turn to next player(could be the same player) and call playTurn
; Assistance Received: none
; ********************************************************************* */
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
          (let* ( (pickedTrainIndex (pickAtrain trains eligTrainNames humanHand engine))
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

      (cond
        ; check if computer can play
        ((equal (canPlay trains eligTrainNames computerHand engine) nil) 
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

; *********************************************************************
; Function Name: playTurn
; Purpose: to allow players play their turns one after another
; Parameters: trains, eligTrainNames, humanHand, computerHand, engine, boneyard, 
;             player, markers, gameScores, roundNum
;            
; Return Value: 
; Algorithm:
;           1) print updated game board
;           2) check if the round has ended
;           3) if not ask next player to make their move
; Assistance Received: none
; ********************************************************************* */
(defun playTurn(trains eligTrainNames humanHand computerHand boneyard engine player markers gameScores roundNum)
  
  (let* ( (mTrain (returnTrain '0 trains))
          (hTrain (returnTrain '1 trains))
          (cTrain (returnTrain '2 trains))
        )

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
      (makeMove trains eligTrainNames humanHand computerHand engine boneyard player markers gameScores roundNum)
    )
  )
)

; *********************************************************
; Source Code for printing game board
; *********************************************************

; *********************************************************************
; Function Name: printScores
; Purpose: to print round scores after a round ends
; Parameters: humanHand, computerHand
;            
; Return Value: print
; Algorithm:
;           1) computes score by calling computeScore function
;           2) prints the scores
; Assistance Received: none
; ********************************************************************* */
; prints scores on the screen after a round is over
(defun printScores(humanHand computerHand)
  (message "Round Score are as follow. ")
  (princ "Human Score: ")
  (princ (computeScore humanHand 0))
  (princ " Computer Score: ")
  (princ (computeScore computerHand 0))
  (terpri)
)

; *********************************************************************
; Function Name: printHand
; Purpose: to print players hands with their indices in game board
; Parameters: player hand, counter (counter is the current tile index)
;            
; Return Value: print
; Algorithm:
;           1) if player hand null, print an end line
;           2) else keep printing the first tile of the player's hand 
;              and call printHand recursively for rest of the tiles
; Assistance Received: none
; ********************************************************************* */
(defun printHand(hand counter)
  (cond ((null hand) (terpri))
        (t  (princ counter)
            (princ "->")
            (princ (car hand))
            (princ " , ")
            (printHand (cdr hand) (+ counter 1)))
  )
)

; *********************************************************************
; Function Name: printTrain
; Purpose: to print train on game board
; Parameters: train , counter
;            
; Return Value: print
; Algorithm:
;           1) if train null, do nothing
;           2) else keep printing the first tile of the train 
;              and call printtrain recursively for rest of the tiles
; Assistance Received: none
; ********************************************************************* */
(defun printTrain(train counter)
  (cond ((null train) ) ; do nothing
        (t  (princ "-") (princ (car train)) (princ "-")
            (printTrain (cdr train) (+ counter 1)))
  )
)

; *********************************************************************
; Function Name: printScene
; Purpose: to print everything in order on gameboard
; Parameters: ctrain htrain mtrain engine hhand chand boneyard markers gameScores roundNum
;            
; Return Value: print
; Algorithm:
;           1) print everything on game board by managing the spacing and end of lines
; Assistance Received: none
; ********************************************************************* */
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
)

; *********************************************************************
; Function Name: printSpace
; Purpose: print the # of spaces as given in the parameter
; Parameters: # of spaces to print
;            
; Return Value: print
; Algorithm:
;           1) print "" if space count =0
;           2) else recursively call printSpace, print " ", and decreament the spaceCount by 1
; Assistance Received: none
; ********************************************************************* */
(defun printSpace(spaceCount)
  (cond ((= spaceCount 0) (princ ""))
        (t (princ " ") (printSpace (- spaceCount 1))))
)

; *********************************************************************
; Function Name: displayMenu
; Purpose: to display turn menu
; Parameters: player
;            
; Return Value: userinput
; Algorithm:
;           1) validate uder input
;           2) display menu items according to the player
; Assistance Received: none
; ********************************************************************* */
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

; *********************************************************************
; Function Name: message
; Purpose: to print a message
; Parameters: string to print
;            
; Return Value: blank line
; Algorithm:
;           1) prints the string passed as a parameter
; Assistance Received: none
; ********************************************************************* */
(defun message(aMessage)
  (princ aMessage)
  (terpri)
)


; *********************************************************
; Source Code for computing scores and declaring winner
; *********************************************************

; *********************************************************************
; Function Name: computeScore
; Purpose: to compute a players score
; Parameters: player hand, game score
;            
; Return Value: player score
; Algorithm:
;           1) if hand is empty return the score sum
;           2) call computeScore recursively for rest hand and add the firs tiles pip each time
; Assistance Received: none
; ********************************************************************* */
; scoreSum is initially 0 when the function is called for round 1, else scoreSum is the game score
(defun computeScore(hand scoreSum)
  (cond ((null hand) scoreSum)
        (t (computeScore (cdr hand) (+ (getTileSum (car hand)) scoreSum)) ) )
)

; *********************************************************************
; Function Name: declareWinner
; Purpose: to declare Winner
; Parameters: humand and computer scores
;            
; Return Value: winner message
; Algorithm:
;           1) if scores are draw declare nobody as winner
;           2) else declare player with lower score as winner
; Assistance Received: none
; ********************************************************************* */
(defun declareWinner(hScore cScore)
  (message "Final Scores are as follow. ")
  (princ ">> Human Score: ") (princ hScore) (princ " >> Computer Score: ") (princ cScore)
  (terpri)(terpri)
  (cond ((< hScore cScore) (message "        ------ Congratulations you won the game !!------"))
        ((= hScore cScore) (message "        ------ Nobody won the game. The final scores were draw !!------"))
        (t (message "         ------ Computer won the game !! You lost the game :( ------") (terpri))
  )
)

; *********************************************************************
; Function Name: playNextRound
; Purpose: to ask human if they want to play next round
; Parameters: hScore cScore roundNum
;            
; Return Value: 
; Algorithm:
;           1) if yes, start next round with round# = rounNum + 1
;           2) else declare winner and quit
; Assistance Received: none
; ********************************************************************* */
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

; *********************************************************************
; Function Name: returnItem
; Purpose: to return the item located at the index from a list
; Parameters: index, the list, counter
;            
; Return Value: item of a list
; Algorithm:
;           1) check if the counter = index (counter starts at 1)
;           2) if yes, return the first item of the list
;           3) if not, call function recursively until counter = index,
;              increament the counter when doing so
; Assistance Received: none
; ********************************************************************* */
(defun returnItem(index aList counter)
  (cond ((= counter index)
          (list (car aList)))
        ((null aList)
          (message "File does not contain any item")
          ())
        (T
          (returnItem index (cdr aList) (+ counter 1) )
        )
  )
)

; *********************************************************************
; Function Name: getTrain
; Purpose: to get the train from the file without marker
; Parameters: to return the list of tiles of a train read from the file
;            
; Return Value: train
; Algorithm:
;           1) check if trains read from files have marker, 
;           2) if yes, remove marker and return just train
;           3) if not return train as it is
; Assistance Received: none
; ********************************************************************* */
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

; *********************************************************************
; Function Name: getTrainWithMarker
; Purpose: to create a list of train with marker to write to file
; Parameters: train marker trainName engine
;            
; Return Value: train list with marker
; Algorithm:
;           1) check if player train has marker, if yes append M and engine 
;              to start of computer train, or end of human train
;           2) if no marker append the engine tile to the train and return
; Assistance Received: none
; ********************************************************************* */
(defun getTrainWithMarker(train marker trainName engine)
  (cond ((equal trainName 'C)
          (cond ((equal marker t) (append '(M) (append (reverseTrain train) (list engine))))
                (t (append (reverseTrain train) (list engine)))))
        (T (cond ((equal marker t) (append (list engine)(append train '(M))))
                 (t (append (list engine) train))))
  )
)

; *********************************************************************
; Function Name: getMarker
; Purpose: to determine if train has marker for train read from file
; Parameters: a train
;            
; Return Value: t if has marker, nil otherwise
; Algorithm:
;           1) check if the train has marker at the start or the end. if yes
;               return t
;           2) if not return nil
; Assistance Received: none
; ********************************************************************* */
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

; *********************************************************************
; Function Name: getPlayerFromFile
; Purpose: to determine the next player as read from file
; Parameters: player
;            
; Return Value: h or c
; Algorithm:
;           1) check if player = "human" if yes return h
;           2) if no return c
; Assistance Received: none
; ********************************************************************* */
(defun getPlayerFromFile(player)
  (cond 
    ((equal player 'human) 'h)
    (t 'c))
)

; *********************************************************************
; Function Name: getPlayerForFile
; Purpose: get player to write to file
; Parameters: player
;            
; Return Value: human or computer
; Algorithm:
;           1) check if player is human is yes return human
;           2) if no return computer
; Assistance Received: none
; ********************************************************************* */
(defun getPlayerForFile(player)
  (cond 
    ((equal player 'h) 'human)
    (t 'computer))
)

; *********************************************************************
; Function Name: readFromFile
; Purpose: to read and store the game data from file, and initiate playTurn
; Parameters: none
;            
; Return Value: calls playTurn function
; Algorithm:
;           1) reads all the data from file stores them in variable in proper format
;           2) starts the player's turn by passing all parameters to playTurn function
; Assistance Received: none
; ********************************************************************* */
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

; *********************************************************************
; Function Name: saveGame
; Purpose: to save the current game to file
; Parameters: trains humanHand computerHand engine boneyard player markers gameScores roundNum
;            
; Return Value: print statement
; Algorithm:
;           1) format all the required information as necessary
;           2) write the lists and/or atoms to file
; Assistance Received: none
; ********************************************************************* */
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

; *********************************************************************
; Function Name: startRound
; Purpose: to start a new round
; Parameters: roundNum, gamescore Human, gamescore Computer
;            
; Return Value: calls playTurn
; Algorithm:
;           1) determine the first player
;           2) get engine for the round
;           3) shuffle and deal deck
;           4) initiate all trains as nil
;           5) other lists like markers and hastoPlayOD to default values
;           6) call playTurn for the player
; Assistance Received: none
; ********************************************************************* */
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
  
    (playTurn '(() () ()) 
               (getEligibleTrains markers player hasToPlayOD '(() () ()) (car engine))
               humanHand
               computerHand
               boneyard
               (car engine)
               player 
               markers
               (cons scoreHuman (cons scoreComputer ()))
               roundNum ) 
  )
)

; *********************************************************************
; Function Name: startGame 
; Purpose: to start a new game
; Parameters: none
;            
; Return Value: function call
; Algorithm:
;           1) Validate user input for the two options load or continue game
;           2) call appropriate functions as per the user's choice
; Assistance Received: none
; ********************************************************************* */
(defun startGame()
  (message "Press 'L' to load game from a file and 'C' to start a fresh game.")
  (let* ((userInput (validateInput '(L C) (read)) ))
  
    (cond 
      ; user entered invalid input
      ((equal userInput "z") (startGame))
      ; user chose to load a game
      ((equal userInput 'L) (readFromFile))
      ; user chose to continue the new game
      ((equal userInput 'C)  (startRound 1 0 0))
    )
  )
)


;; starting game from here

(message "    .........Welcome to Mexican Train..........    ") 
(terpri)
(startGame)