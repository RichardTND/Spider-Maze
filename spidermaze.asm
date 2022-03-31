//-------------------------
//Spider Maze
//A Scene World Programming
//tutorial game - Full version
//by Richard Bayliss
//-------------------------

//Zeropage variables

.var ZeroPage = $02  //ZeroPage 1 - Player/Gem collision
.var ZeroPage2 = $08 //ZeroPage 2 - Player/Wall collision

//Variables set as movement directions for the player
//and spiders

.var up = 1
.var down = 2
.var left = 3
.var right = 4


//Chars which the player must pick up or avoid

.var GemChar = 70 //Char value for character (GEM)
.var BulbChar = 87 //Char value which gives the player invicibility
.var SkullChar = 91 //Char value for deadly skull (Kills you instantly) 
.var HeartChar = 95 //Char value for awaring extra lives 

//Wall (2X2 char) - Corresponds to player, and spiders

.var WallTopLeft = 66 //Char value for top left wall
.var WallTopRight = 67 //Char value for top right wall
.var WallBottomLeft = 68  //Char value for bottom left wall
.var WallBottomRight = 69 //Char value for bottom right wall

//Set up some variables for storing multiple screen
//position co-ordinates for the sprite/background
//based collision for the spiders (Based on the example
//on the TND web site). The variables are based on LOW
//and HIGH byte of the starting screen row.

.var Spider1ScreenLoStore = $10
.var Spider1ScreenHiStore = $11
.var Spider2ScreenLoStore = $18
.var Spider2ScreenHiStore = $19
.var Spider3ScreenLoStore = $20
.var Spider3ScreenHiStore = $21
.var Spider4ScreenLoStore = $28
.var Spider4ScreenHiStore = $29

//Characters that represent the web pointers (Disguised
//in order to see which characters each part of the web
//are (See test_level_graphics.ctm charset).

.var Web_Up_Down_Left = 78  //The char values are used
.var Web_Up_Down_Right = 79 //in order to change the
.var Web_Any_Direction = 80 //possible direction of 
.var Web_Down_Left = 81		//each spider, every time
.var Web_Up_Left = 82		//it has approached the web
.var Web_Down_Right = 83	//char at the top left corner
.var Web_Up_Right = 84		//of the screen. 
.var Web_Up_Left_Right = 85 
.var Web_Down_Left_Right = 86

//Some constants set for various hardware/music routines

.const SCRN = $0400		 //Set address of screen RAM
.const COLR = $d800 	 //Set address of colour RAM
.const MusicInit = $0f00 //Set address to init music
.const MusicPlay = $0f03 //Set address to play music
.const SFXInit = $0f06	 //Set address to init sound effects

//Some additional variables for setting the music type


.var TitleMusic = 0 //Value set for TITLE MUSIC
.var WellDoneMusic = 1 //Value set for GET READY jingle
.var GameOverMusic = 2 //Value set for GAME OVER jingle
.var GameMusic = 3 //Value set for IN GAME music

BasicUpstart2(start) //Setup and call SYS addr, which
					 //executes the main game start address
					 //where the label, start has been 
					 //positioned!

	*=$0900 "DISK ACCESS CODE"
	.import source "diskaccess.asm"

//This imports the standard C64 program format
//(GoatTracker)
	*=$0f00 "MUSIC DATA"
	
	.import c64 "bin/music.prg" 
		
//Import game sprites - Raw binary format
//(SpritePad V2.0)
	*=$2000 "GAME SPRITES"
	.import binary "bin/sprites.bin"
	
//Import graphics charset - Raw binary format
//(Charpad V2.0)
	*=$2800 "CHARSET"
	.import binary "bin/gamecharset.bin"

	
//Import screen colour data - Raw binary format
	*=$3400 "ATTRIBUTES"
colourdata:	
	.import binary "bin/colordata.bin"
	
//=================================================
	*=$4000 "GAME CODE"
	
//Kill all existing interrupts and then
//setup the game screen.

start:
	sei
	lda #$36
	sta $01
	lda $02a6 //System detect 
	sta system
	lda #251
	sta 808 //Avoid run/restore
	jsr LoadHiScores //Hi-score loader subroutine
Restart:	
	jsr KillAllInterrupts
	jmp TitleScreenCode
GameStartSetup:
	sei
	jsr KillAllInterrupts
	lda #$5a
	sta ObjPos
	sta PlayerDefaultPosX
	lda #$a0
	sta ObjPos+1
	sta PlayerDefaultPosY
		
	lda #$33	//Set chars for lives, and levels
	sta Lives	//by using the digits.
	lda #$30
	sta Level	
	lda #$31	
	sta Level+1	
	ldx #$00
	stx LevelDrawPointer
	
//Clear score

	ldx #$00
ClearScore:
	lda #$30
	sta Score,x
	inx
	cpx #$06
	bne ClearScore
	
	ldx #$00
ZeroSpiderProperties: //Zero all spider properties
	lda #$00
	sta SpiderPropertiesStart,x
	inx
	cpx #SpiderPropertiesEnd-SpiderPropertiesStart
	bne ZeroSpiderProperties
	
//Setup + initialise IRQ interrupts
	
	
	
	lda #<IRQ //Low-byte of game IRQ
	sta $0314
	lda #>IRQ //Hi-byte of game IRQ
	sta $0315
	lda #$2e 
	sta $d012 //Store to hardware raster position
	lda #$7f  //Enable CIA interrupt
	sta $dc0d
	sta $dd0d
	lda #$1b //Screen on
	sta $d011
	lda #$01 //ACK IRQ raster interrupt
	sta $d01a
	
	
	cli		 //Clear IRQ flag for the main body of the
			 //game code.
			
//Complete the game initialise process			

InitialiseLevelProperties:		
	
//Read the map data and draw the data 
//to the default screen RAM $0400. Basically
//read the low/hi byte values for each of the 16 
//bytes from each low/hibyte table and then store to
//the self-mod code inside DrawNextLevel
	
	lda #0
	sta ColourDelay //Init colour properties
	sta ColourPointer
	
	ldx LevelDrawPointer
	lda LevelTableLo1,x
	sta levelpart1+1
	lda LevelTableHi1,x
	sta levelpart1+2
	lda LevelTableLo2,x
	sta levelpart2+1
	lda LevelTableHi2,x
	sta levelpart2+2
	lda LevelTableLo3,x
	sta levelpart3+1
	lda LevelTableHi3,x
	sta levelpart3+2
	lda LevelTableLo4,x
	sta levelpart4+1
	lda LevelTableHi4,x
	sta levelpart4+2
	lda IntervalTable1,x
	sta Spider1Interval
	lda IntervalTable2,x
	sta Spider2Interval
	lda IntervalTable3,x
	sta Spider3Interval
	lda IntervalTable4,x
	sta Spider4Interval
	inx
	cpx #LevelTableLo1End-LevelTableLo1
	beq GameComplete
	inc LevelDrawPointer
	
	jmp DrawNewLevel
		
//The game is completed. Reset the level to the very		
//first stage.		

GameComplete:		
	lda #$0b
	sta $d011
	lda #$36
	sta Level+1 //Avoid 17 as level
//Display well done message		
	lda #0
	sta $d015
	lda #TitleMusic
	jsr MusicInit
	ldx #$00	
DrawWellDone:		
	lda EndText,x	
	sta SCRN+400,x	
	lda #$00	
	sta COLR+400,x	
	inx	
	cpx #EndTextEnd-EndText	
	bne DrawWellDone	
	lda #0	
	sta FireButton	
	
	lda Score
	sec
	lda HiScore+5
	sbc Score+5
	lda HiScore+4
	sbc Score+4
	lda HiScore+3
	sbc Score+3
	lda HiScore+2
	sbc Score+2 
	lda HiScore+1
	sbc Score+1
	lda HiScore
	sbc Score
	bpl NoHiScoreEnd
	
	ldx #$00
MakeNewHiScoreEnd:
	lda Score,x
	sta HiScore,x
	inx
	cpx #$06
	bne MakeNewHiScoreEnd
	
NoHiScoreEnd:
	lda #$1b
	sta $d011
	jsr MaskPanel
WaitEnd:
	jsr SyncMain
	jsr ColourWash
	lda $dc00
	lsr
	lsr
	lsr
	lsr
	lsr
	bit FireButton
	ror FireButton
	bmi WaitEnd
	bvc WaitEnd
	jmp CheckHiScoreTable		//Check high score position
		
DrawNewLevel:	
	lda #$0b
	sta $d011
	
	
	
	ldx #$00
DrawScreen:	

levelpart1:	
	lda map,x	
	sta SCRN,x	
levelpart2:
	lda map+$100,x	
	sta SCRN+$100,x	
levelpart3:		
	lda map+$200,x	
	sta SCRN+$200,x	
levelpart4:	
	lda map+$2e8,x 	
	sta SCRN+$2e8,x	
	inx	
	bne DrawScreen	
		
//Now read the current screen, and select from	
//the attributes table the colour to paint the	
//chars with.	
		
	ldx #$00	
PaintScreen:	
	ldy SCRN,x	
	lda colourdata,y	
	sta COLR,x	
	ldy SCRN+$100,x 	
	lda colourdata,y	
	sta COLR+$100,x 	
	ldy SCRN+$200,x	
	lda colourdata,y	
	sta COLR+$200,x 	
	ldy SCRN+$2e8,x 	
	lda colourdata,y	
	sta COLR+$2e8,x	
	inx	
	bne PaintScreen	
	jsr MaskPanel //Mask panel once
	
	lda #$00
	sta UpStop				
	sta DownStop
	sta LeftStop
	sta RightStop
	sta PlayerMoveTime
	sta PlayerAlreadyMoving
	sta PlayerDirection
	
	//Zero a few of the spider properties
	lda #0
	sta Spider1AlreadyMoving
	sta Spider2AlreadyMoving
	sta Spider3AlreadyMoving
	sta Spider4AlreadyMoving
	sta Spider1MoveTime
	sta Spider2MoveTime
	sta Spider3MoveTime
	sta Spider4MoveTime
	sta Spider1Dir
	sta Spider2Dir
	sta Spider3Dir
	sta Spider4Dir
	sta Spider1HangTime
	sta Spider2HangTime
	sta Spider3HangTime
	sta Spider4HangTime
	
//Set hardware attributes for the 	
//charset mode, screen memory position	
//and sprites, etc.	
	
	lda #$03 //Switch to BANK #3	
	sta $dd00	
		
	lda #$18 //Multicolour mode on
	sta $d016
	
	lda #$1a //Charset is placed here	
	sta $d018	
	
	lda #$00 //Black border and background
	sta $d020
	sta $d021
	
	lda #$0c //Screen multicolour 1
	sta $d022
	
	lda #$0f //Screen multicolour 2
	sta $d023
	
	lda #$07
	sta $d027
	
	lda #%00011111
	sta $d015 //Enable 5 sprites only
	
	lda #$00 //All sprites hires
	sta $d01c
	
	lda #$80
	sta $07f8 //Sprite type set
	
	lda #$84  //4 Spiders
	sta $07f9
	lda #$85
	sta $07fa
	lda #$86
	sta $07fb
	lda #$85
	sta $07fc
	 
	lda #$0d
	sta $d028 //Light green spider
	lda #$0e
	sta $d029 //Light blue spider
	lda #$0a
	sta $d02a //Pink spider
	lda #$03
	sta $d02b //Cyan spider
	
	
	// X+Y starting positin for spider 1
	lda #$12
	sta ObjPos+2
	lda #$40
	sta ObjPos+3
	sta $d003
	
	// X+Y starting position for spider 2
	lda #$9a
	sta ObjPos+4
	lda #$40
	sta ObjPos+5
	sta $d005
	
	//X+Y starting position for spider 3
	lda #$12
	sta ObjPos+6
	
	lda #$c0
	sta ObjPos+7
	sta $d007
	
	//X+Y starting position for spider 4
	lda #$9a
	sta ObjPos+8
	lda #$c0
	sta ObjPos+9
	sta $d009
	
	//Reset value of shield for player before it gets
	//vulnerable to the spiders
	
	lda #200	//Shield value
	sta ShieldTime
	
	lda #$00
	sta PlayerKilledBySpider //Player is not killed by spider
				
			
	lda PlayerDefaultPosX
	sta ObjPos
	lda PlayerDefaultPosY
	sta ObjPos+1
	sta $d001
	lda #$1b			//Restore screen
	sta $d011
	lda #GameMusic //Initialise music player for in game music
	jsr MusicInit
//==================================================
	
//The main body of the game loop	
	
GameLoop:	
	lda $dc01			//read joystick port 1 + keyboard
	lsr					//controls. Check for CONTROL key
	lsr
	lsr
	bcs GameNotPaused
	jmp GamePaused
	
//The game hasn't been paused, allow the game loop	
//to continue.	
	
GameNotPaused:
	jsr SyncMain			//Synchronise timer
	jsr ExpandSpriteArea	//Expand the sprite area
	jsr PlayerControl		//Control the player 
	jsr SpiderControl		//Control the spiders via macro
	jsr CheckShield			//Check shield timer before collision test
	jsr MaskPanel			//Mask score+lives panel
	jmp GameLoop			//Repeat game loop 
	
//The key 'CONTROL' is pressed. Check for spacebar	
//in order to unpause the game.	
	
GamePaused:
	lda $dc00 	//Read firebutton
	lsr
	lsr
	lsr
	lsr
	lsr
	bit FireButton
	ror FireButton
	bmi ReadSpace
	bvs ReadSpace
	lda #0
	sta FireButton
	jmp GameLoop
ReadSpace:
	lda $dc01	//Read SPACEBAR key.
	lsr
	lsr
	lsr
	lsr
	bcs NotQuitKey
	jmp Restart
NotQuitKey:	
	lsr
	bcs GamePaused
	jmp GameLoop
	
	

//==================================================
	
//Main IRQ interrupt - Single IRQ  

//==================================================
	
IRQ:
	inc $d019		//Increment IRQ register
	lda #$fa		//Set lower raster position
	sta $d012
	lda #$01		//Set synchronized timer
	sta SyncTimer //Increment timer with IRQ
	jsr SystemDetect //Music speed PAL or NTSC?
	jmp $ea7e
	
//==================================================	
	
//PAL/NTSC detection for playing the music	
	
SystemDetect:	
	lda system			//Which system are we using?
	cmp #$01			//0 = NTSC, 1 = PAL
	beq PAL				//PAL detected
	inc NTSCTIMER		//NTSC is used, set the timer
	lda NTSCTIMER		//until it reaches 6
	cmp #$06			
	beq ResetNTSCTimer	//then reset the timer
	
PAL:	
	jsr MusicPlay		//Otherwise just play the music	
	rts	
ResetNTSCTimer:			//NTSC Timer is reset	
	lda #0	
	sta NTSCTIMER
	rts		
	
//==================================================	
	
//Code SyncMain - Synchronize timer outside	
//of the IRQ interrupt	
		
SyncMain:				
	lda #0			//Grab raster pointer 
	sta SyncTimer	//then compare to itself to
	cmp SyncTimer	//get a synchronized timer
	beq *-3			//delay for any looping code
	rts
	
//==================================================	
	
//Subroutine that expands the X-Position of
//sprites and stores custom sprite pointers
//to hardware X/Y sprite position
	
ExpandSpriteArea:
	ldx #$00	  
ExpandArea:
	lda ObjPos+1,x //Fetch selfmod Y pos value
	sta $d001,x    //Store to hardware sprite Y
	lda ObjPos,x   //Fetch selfmod X pos value
	asl			   //multiply the field size by 2
	ror $d010	   //and rotate to right.
	sta $d000,x	   //Store to hardware sprite X
	inx 		   //Increment X/Y position twice
	inx			  
	cpx #16		   //16 positions (Sprite 0XY - Sprite 7XY)
	bne ExpandArea
	rts

//================================================
	
//Player control (Player movement). This
//code uses player alignment movement therefore
//every time the player moves. It should not
//stop unless aligned according to character
	
PlayerControl:
	lda PlayerKilledBySpider  //Is the player killed by a spider?
	cmp #$01				  
	bne PlayerIsAlive		  //No...
	jmp AnimatePlayerDeath	  //Yes, the player is killed
PlayerIsAlive:
	jsr AnimatePlayerAlive	  //Player is alive, animate it
	
	jsr PlayerSpriteToBGRTest //Call player sprite/BGR collision for gems
	lda PlayerAlreadyMoving   //Check if the player is already moving
	cmp #1					  //PlayerAlreadMoving = ON (0=OFF, 1 = ON)
	bne AllowJoystickTest	  //If switched off, allow joystick control
	jmp DoMovementCheck		  //Go to movement check routine.
AllowJoystickTest:
	jmp AllowJoystick
	
//Animate the player's death sequence, then remove
//amount of lives by one. If no of lives = 0 then the
//game is well and truly over.

AnimatePlayerDeath:
	lda AnimDelay3	//Grab animation delay for death
	cmp #$03		//until delay is long enough
	beq DeathAnimContinue	//Then continue death anim
	inc AnimDelay3	//Increment animation delay
	rts
DeathAnimContinue:
	lda #$00		//Reset animation delay
	sta AnimDelay3
	ldx AnimPointer3 //Read loop animation pointer
	lda PlayerDeadFrame,x //Read table for player death animation
	sta $07f8			  //Store it to hardware sprite0 type (Player)
	inx					  //Move to next frame
	cpx #PlayerDeadFrameEnd-PlayerDeadFrame //Until reached last frame from table
	beq DeathAnimFinished
	inc AnimPointer3	  //Skip to next frame
	rts
DeathAnimFinished:
	ldx #$00			  //Reset animation pointer
	stx AnimPointer3
	
LoseLife:	dec Lives			  //Deduct one of the player's lives
	lda Lives			  //Read amount of lives
	cmp #$30			  //Digit 0 in lives counter?
	beq GameOver		  //Yes call game over
	
//Not all lives are lost. The player is allowed to spawn	
	
	lda PlayerDefaultPosX //Restore position of player
	sta ObjPos		 	  //X to the central of the maze
	lda PlayerDefaultPosY //Restore position of player
	sta ObjPos+1		  //Y to the central of the maze
	ldx #$00			  //Reset animation pointer for
	lda #$00			  //the player
	stx AnimPointer1	  //Animation pointer reset
	sta AnimDelay1		  //same for delay
	sta PlayerKilledBySpider //New player is NOT killed by a spider
	sta PlayerAlreadyMoving //The player is not ready to move, until joystick read
	sta PlayerMoveTime		//Reset the player's movement time
	lda #200				//And the shield
	sta ShieldTime
	jsr ResetAllDirections  //Player is ready to move
	rts
	
//All lives have been lost. The game is over.	
//display the GAME OVER message	
GameOver:
	lda #0
	sta ColourPointer
	lda #GameOverMusic //Initialise GAME OVER jingle
	jsr MusicInit
	jsr ClearMap

			
	lda #0
	sta $d015 //Remove all sprites
	
	//Check if the player has a brand new score.
	//if the score is higher than the hi score. It
	//should become the new hi score.
	
	lda Score
	sec
	lda HiScore+5
	sbc Score+5
	lda HiScore+4
	sbc Score+4
	lda HiScore+3
	sbc Score+3
	lda HiScore+2
	sbc Score+2
	lda HiScore+1
	sbc Score+1
	lda HiScore
	sbc Score
	bpl NoHiScore
	//Hi score is found!
	ldx #$00
MarkNewHiScore:
	lda Score,x
	sta HiScore,x
	inx
	cpx #$06
	bne MarkNewHiScore
NoHiScore:
	ldx #$00
	
WriteGameOverText:	
	lda #$00	
	sta COLR+400,x	
	lda GameOverText,x	
	sta SCRN+400,x	
	inx	
	cpx #GameOverTextEnd-GameOverText
	bne WriteGameOverText
	lda #$00
	sta FireButton
	
	jsr MaskPanel //Mask panel once more
	
GameOverLoop:

	jsr SyncMain
	jsr ColourWash
	lda $dc00		//Wait for fire button
	lsr				//to be pressed and released
	lsr
	lsr
	lsr
	lsr
	bit FireButton
	ror FireButton
	bmi GameOverLoop
	bvc GameOverLoop
	jmp CheckHiScoreTable		//Check high score position
		
//Animate the player alive
AnimatePlayerAlive:	
	lda AnimDelay1
	cmp #$05
	beq AnimAliveOkay
	inc AnimDelay1
	rts
AnimAliveOkay:
	lda #$00
	sta AnimDelay1
	ldx AnimPointer1
	lda PlayerAliveFrame,x
	sta $07f8
	inx
	cpx #PlayerAliveFrameEnd-PlayerAliveFrame
	beq ResetPlayerAliveAnim
	inc AnimPointer1
	rts
ResetPlayerAliveAnim:
	
	ldx #$00
	stx AnimPointer1
	rts
	
//Now animate those fiendish spiders

AnimateSpiders:
	lda AnimDelay2
	cmp #4
	beq SpiderAnimOK
	inc AnimDelay2
	rts
SpiderAnimOK:
	lda #$00
	sta AnimDelay2
	ldx AnimPointer2
	lda SpiderFrame1,x
	sta $07f9
	lda SpiderFrame2,x
	sta $07fa
	lda SpiderFrame3,x
	sta $07fb
	lda SpiderFrame4,x
	sta $07fc
	inx
	cpx #SpiderFrame1End-SpiderFrame1
	beq ResetSpiderAnim
	inc AnimPointer2
	rts
ResetSpiderAnim:
	ldx #$00
	stx AnimPointer2
	rts
	
	
AllowJoystick: 	
	//Test the player/sprite BGR - The sprite before activating
	//joystick control. Basically we want to see whether or 
	//not the player has a wall surround it. Assuming the player
	//has already stopped. The wall should prevent the direction
	//which the player moves.
	
	jsr PlayerSpriteToBGRTest2
	
JUp:
	lda #$01  //Wait for UP on joystick port 2
	bit $dc00 
	bne JDown //Up not read, move to next joy read routine
	lda UpStop //Player has to stop where a wall is above it
	cmp #1		
	bne NotStopUp
	jmp NoJ	   //Exit joystick control
NotStopUp:	
	jsr CheckMoveUp //Test if player can move up
	jsr ResetAllDirections //Player is ready to move - reset direction collision
	jmp AllowPlayerMove	   //then allow the player to move until aligned
	
JDown:
	lda #$02 	//Wait for down on Joystick port 2
	bit $dc00 
	bne JLeft 	//DOWN not read, move to next joy read routine
	lda DownStop //Player has to stop where a wall is right under it
	cmp #1
	bne NotStopDown
	jmp NoJ		//Exit joy control, player cannot move
NotStopDown:
	jsr CheckMoveDown	//Test if player can move down
	jsr ResetAllDirections  //Player is ready to move - reset direction collision
	jmp AllowPlayerMove //then allow the player to move within alignment
	
JLeft:
	lda #$04	//Wait for LEFT on Joystick port 2
	bit $dc00	
	bne JRight	//LEFT not read, move to next joy read routine
	lda LeftStop //Player has to stop where a wall is to the left of it
	cmp #1
	bne NotStopLeft
	jmp NoJ		//Exit joy control, player cannot move
NotStopLeft:
	jsr CheckMoveLeft
	jsr ResetAllDirections
	jmp AllowPlayerMove
	
JRight:
	lda #$08	//Finally wait for RIGHT on Joystick port 2
	bit $dc00
	bne NoJ		//RIGHT not read, player has stopped
	lda RightStop //Player has to stop where a wall is to the right of it
	cmp #1
	bne NotStopRight
	jmp NoJ		  //Exit joy control, player cannot move
NotStopRight:
	jsr CheckMoveRight
	jsr ResetAllDirections
	jmp AllowPlayerMove
	
NoJ: //Player alignment/movement
	rts
		
//Player movement setup. Force the PlayerAlreadyMoving trigger	
//to switch on. So that the joystick control is set to ignore	
//control when the player is in motion.	
	
AllowPlayerMove:
	lda #1
	sta PlayerAlreadyMoving
	jsr ResetAllDirections
	rts
	
//Every time a new movement is made, a subroutine
//to reset all of the directions must be set. That
//is of course until the player has stopped or the
//joystick control has already changed.

ResetAllDirections:
	lda #0			//0 = Off, 1 = On
	sta UpStop
	sta DownStop
	sta LeftStop
	sta RightStop
	rts
	
//As we check the direction of which the
//player should move. We should also check
//to see whether or not the player is already
//moving.
	
DoMovementCheck:
	
	lda PlayerDirection	//Has chosen direction been
	cmp #up				//set as up?
	bne PlayerNotMoveUp //No. Check for down 
	jsr AlignPlayerUp	//Yes. Align movement up
	jmp TestPlayerMoveTime //Set movement time for player
	
PlayerNotMoveUp:
	cmp #down			//What about down?
	bne PlayerNotMoveDown //No. Check for left
	jsr AlignPlayerDown   //Yes, Align movement down
	jmp TestPlayerMoveTime //Set movement time for player
	
PlayerNotMoveDown:
	cmp #left			//How about left?	
	bne PlayerNotMoveLeft //No. Check for right
	jsr AlignPlayerLeft	//Yes, align movement left
	jmp TestPlayerMoveTime //Set movement time for player
	
PlayerNotMoveLeft:
	cmp #right			//Finally can the player move right?
	bne PlayerNotMoveRight //No. Terminate subroutine
	jsr AlignPlayerRight   //Yes, align movement right
	jmp TestPlayerMoveTime //Set movement time for player
	
PlayerNotMoveRight:
	//Terminate movement check subroutine until called again
	
	rts
	
//Time the movement according to the custom set	
//value of the pointer (PlayerMoveStop)

TestPlayerMoveTime:	
	inc PlayerMoveTime	//Increment byte from pointer
	lda PlayerMoveTime	//Read pointer and
	cmp PlayerMoveStop	//check if reached custom value (PlayerMoveStop)
	beq PlayerNowAligned //Yes it has, call subroutine to stop player 	
	rts					//No terminate subroutine until called again
		
PlayerNowAligned:
	lda #0				//Reset the player's movement timer
	sta PlayerMoveTime
	sta PlayerAlreadyMoving //And stop current movement subroutine
	rts
	
//Player movement operations. Unlike some of the games	
//I wrote in the past. The movement subroutine doesn't require	
//any stop value if positioned to a certain area. The background	
//surrounds the player. So should it stop and the player tries	
//to hit a brick when stopped. It cannot move that direction.	
	
//Move player upwards	
	
AlignPlayerUp:
	lda ObjPos+1	//Read current sprite Y position of player
	sec				//subtract the position
	sbc #2			//by 2 and then
	sta ObjPos+1	//store new sprite position of player
	rts

//Move player downwards

AlignPlayerDown:
	lda ObjPos+1	//Read current sprite Y position of player	
	clc				//add the position
	adc #2			//by 2 and then
	sta ObjPos+1	//store new sprite position of the player
	rts

//Move player to the left
	
AlignPlayerLeft:
	lda ObjPos		//Read current sprite X position of player
	sec				//subtract the position
	sbc #1			//by 1 and then
	sta ObjPos		//store new sprite position of the player
	rts
	
//Move player to the right	
	
AlignPlayerRight:
	lda ObjPos		//Read current sprite X position of player
	clc				//add the position
	adc #1			//by 1 and then
	sta ObjPos		//store new sprite position of the player
	rts
	
//Check which direction to set values for movement.	

//Check if the player can move up if so, set
//the player direction, reset the pointer PlayerMoveTime
//and set value of PlayerMoveStop, which when moving
//the player should stop and align to the 2x2 tiles	
	
CheckMoveUp:
	
	lda PlayerAlreadyMoving //Is the player already moving?
	cmp #1					//yes
	beq SkipUp				//Terminate subroutine until called again
	
	//Set up the value for which the player
	//should move
	
	lda #up		//Direction UP to be set and
	sta PlayerDirection //stored to the pointer PlayerDirection
	lda #0		//Reset the player movement time
	sta PlayerMoveTime
	lda #8		//Also set the stopping value for the player to
	sta PlayerMoveStop //stop moving.
	rts
	
SkipUp:
	rts
	
//Check if the player can move down if so, set
//the player direction, reset the pointer PlayerMoveTime
//and set value of PlayerMoveStop, which when moving
//the player should stop and align to the 2x2 tiles

CheckMoveDown:
	lda PlayerAlreadyMoving //(See check move up)
	cmp #1
	beq SkipDown
	lda #down 	//Direction DOWN to be set 
	sta PlayerDirection //stored to the pointer PlayerDirection
	lda #0		//Reset the player movement time
	sta PlayerMoveTime
	lda #8		//Also set the stopping value for the player to
	sta PlayerMoveStop //stop moving
	rts
	
SkipDown:
	rts
	

//Check if the player can move left if so, set
//the player direction, reset the pointer PlayerMoveTime
//and set value of PlayerMoveStop, which when moving
//the player should stop and align to the 2x2 tiles	
	
CheckMoveLeft:
	lda PlayerAlreadyMoving 
	cmp #1
	beq SkipLeft
	lda #left	//Direction LEFT to be set
	sta PlayerDirection //stored to the pointer PlayerDirection
	lda #0		//Reset the player movement time
	sta PlayerMoveTime	
	lda #8		//Also because of sprite X being expanded, set 10
	sta PlayerMoveStop //to the player stop value
	rts
SkipLeft:
	rts
	

//Check if the player can move right if so, set
//the player direction, reset the pointer PlayerMoveTime
//and set value of PlayerMoveStop, which when moving
//the player should stop and align to the 2x2 tiles	
	
CheckMoveRight:
	lda PlayerAlreadyMoving
	cmp #1
	beq SkipRight
	lda #right	//Direction RIGHT to be set
	sta PlayerDirection	//stored to the pointer PlayerDirection
	lda #0		//Reset the player movement time
	sta PlayerMoveTime

	lda #8	//Set value of 8 to pointer PlayerMoveStop
	sta PlayerMoveStop
	rts
SkipRight:
	rts
	
	
//==================================================	
	
//Setup sprite / character collision (exclusive 	
//for the player only.

PlayerSpriteToBGRTest:	
	lda $d000 	//Read hardware sprite X
	sta PlayerX //Store to pointer PlayerX
	lda $d010   //Read hardware sprite XMSB
	sta PlayerXMSB //Store to pointer PlayerXMSB
	lda $d001	//Read hardware sprite sprite Y
	sta PlayerY //Store to pointer PlayerY
	jsr MapPlayerToChar //Map the player sprite to
						//the GEM charset.
	
	rts
	
//Calculate and map the player position to 
//the screen character position

MapPlayerToChar:
	lda PlayerX	//Read X position of the player
	sec
	sbc #$10 	//Read Central area of sprite 
	sta ZeroPage //Store to the screen ZeroPage COLUMN
	
	lda PlayerXMSB //Read MSB position of the player
	sbc #$00
	lsr
	lda ZeroPage  //Read first zero page of ZeroPage
	ror			  //Perform 16bit calculation
	lsr			  //(Division)
	lsr
	sta ZeroPage+3 //Store to selfmod zeropage for X position
	
	lda PlayerY	  //Read central Y position of player sprite	
	sec
	sbc #$2a	  //Y-Co-ordinates
	lsr			  //Perform 16-bit calculation
	lsr			  //Division
	lsr
	sta ZeroPage+4 //Store to selfmod ROW
	
	lda #<SCRN		//Read Low byte of screen ($0400)
	sta ZeroPage+1 //Store as screen read Low 
	lda #>SCRN		//Read Hi byte of screen ($0400)
	sta ZeroPage+2	//Store as screen read Hi
	
	ldx ZeroPage+4	//Read current row
	beq CheckCharType //Then perform char check
NextRow:
	lda ZeroPage+1 	//Read low byte of stored screen area
	clc				
	adc #$28		//Move 40 chars down
	sta ZeroPage+1  //Store to next ROW
	lda ZeroPage+2  //Read hi byte of stored screen area
	adc #$00
	sta ZeroPage+2	//Store to next COLUMN
	dex				
	bne NextRow
	
	//Calculation complete. Simply check the type of
	//character the player hits:
	
CheckCharType:	
	ldy ZeroPage+3
	lda (ZeroPage+1),y
	cmp #GemChar		//Gem char detected
	bne NotGemChar		//No, nothing collected
	jmp CollectGem		//Gem has been collected
NotGemChar:
	cmp #BulbChar		//Light bulb detected?
	bne NotBulbChar		//No, nothing collected
	jmp CollectBulb		//Collect bulb
NotBulbChar:
	cmp #SkullChar		//Does the player hit a skull?
	bne NotSkullChar	//No, no contact
	jmp KillPlayerInstantly //Otherwise kill player instantly
NotSkullChar:
	cmp #HeartChar		//Heart char detected?
	bne NotHeartChar	//No heart char detected
	jmp AwardExtraLife	//Otherwise award extra life
NotHeartChar:			//No more chars to check end
	rts					//subroutine.
	
	//The player picks up a gem. Turn all 4 
	//chars into a plain spacebar character
	//also award points per gem picked up
CollectGem:	
	jsr RemoveCharsetObject
	
	//Now award 200 points per gem collected 
 
	jsr DoScore
	jsr DoScore
	jsr CheckForGems
	jsr PlaySFX_CHOMP
	rts
	
//The player picks up a bulb. Award it 100 points and	
//also make the player invulnerable	

CollectBulb:	
	jsr RemoveCharsetObject	//Do remove charset subr.	
	jsr DoScore				//Add 100 pointers
	lda #200				//Reset player shield
	sta ShieldTime
	jsr PlaySFX_SHIELD
	rts
	
//The player crashes into a deadly skull object. Kill	
//the player instantly, even if the player's invulnerable.	

KillPlayerInstantly:	
	lda #0	
	sta ShieldTime	
	sta AnimPointer3
	sta AnimDelay3
	jmp PlayerIsDead
	rts	
		
//The player collects a heart, award the player an		
//extra life, unless the lives counter = 9 already.		

AwardExtraLife:		
	jsr RemoveCharsetObject		
	lda Lives		
	cmp #$39		
	beq SkipLives		
	inc Lives		
SkipLives:
	jsr PlaySFX_EXTRA_LIFE
	jsr DoScore
	rts		
	
//Object has been picked up - Remove the collectable	
	
RemoveCharsetObject:	

	lda #$20	//Value for spacebar char
	sta (ZeroPage+1),y //Store to char position
	iny 		//Move to next char column
	sta (ZeroPage+1),y //Store to char position
	tya			//Transfer current char row position to A
	clc			//calculate it to
	adc #$28	//move to the next row
	tay			//Transfer Y to new char row position
	lda #$20	//Spacebar char again
	sta (ZeroPage+1),y //Store new position
	dey			//Move back 1 char column
	sta (ZeroPage+1),y //Store new position	
	rts	
		
DoScore:
	inc Score+3	//increase digit on 4th character
	ldx #$04	//Read 4 chars
ScoreAdd:
	lda Score,x //Check score digit position
	cmp #$3a	//is character past '9' char?
	bne ScoreOK	//No, score is OK
	lda #$30	//Else reset current char to '0'
	sta Score,x
	inc Score-1,x //Then increment the character before it
ScoreOK:
	dex			  //Loop the score adding subroutine
	bne ScoreAdd
	rts
	
//Now check if any gems still exist on screen

CheckForGems:
	ldx #$00			//Loop start
ScreenReadLoop:
	lda SCRN,x			//Check screen position $0400-$04ff
	cmp #GemChar		//Gem char exist?
	beq LevelNotComplete	//Yes, level is not complete
	lda SCRN+$100,x		//No, check screen position $0500-$05ff
	cmp #GemChar		//Gem char exist?
	beq LevelNotComplete //Yes, level is not complete
	lda SCRN+$200,x		//No, check screen position $0600-$06ff
	cmp #GemChar		//Gem char exist?
	beq LevelNotComplete//Yes, level is not complete
	lda SCRN+$2e8,x		//No, check screen position $06e8-$07e8
	cmp #GemChar		//Gem char exist?
	beq LevelNotComplete //Yes, level is not complete yet
	inx 				//Continue loop until 256 bytes read
	bne ScreenReadLoop	
	jmp LevelComplete	//Then level is complete
LevelNotComplete:
	rts					//Let game continue how it is
	
//Level is complete. Process a well done screen, then	
//await fire button before the player can move on to the next level of the game.	
	
LevelComplete:
	
	jsr MaskPanel //Update score panel once more
	lda #0
	sta ColourPointer
	lda #WellDoneMusic
	jsr MusicInit
	jsr ClearMap //Clear the game map
	ldx #$00
PutWellDone:
	lda #$00
	sta COLR+400,x
	lda LevelCompleteText,x
	sta SCRN+400,x
	inx
	cpx #LevelCompleteTextEnd-LevelCompleteText
	bne PutWellDone
	
	lda #0
	sta FireButton
	sta $d015
WaitExitWellDone:
	jsr SyncMain
	jsr ColourWash
	lda $dc00
	lsr
	lsr
	lsr
	lsr
	lsr
	bit FireButton
	ror FireButton
	bmi WaitExitWellDone
	bvc WaitExitWellDone
	inc Level+1
	lda Level+1
	cmp #$3a //Is level char OVER 9?
	bne LevelCounterOK
	lda #$30
	sta Level+1
	inc Level
LevelCounterOK:
	jmp InitialiseLevelProperties
	
	 
//=======================================================================
//Self-Mod sprite/background collision - player to wall
//Similar to the code above, but some self-mod X-Y pointers 
//are set to calculate the area in which the walls may lie. 
//THIS ROUTINE SHOULD ONLY CORRESPOND AFTER THE PLAYER HAS
//STOPPED MOVING!
//=======================================================================

//Calculate and map the player position to 
//the screen character position

PlayerSpriteToBGRTest2:

	lda $d000		//Grab X position of player from hardware
	sta PlayerX		//Store to a temp pointer as we did with the GEM collision
	lda $d010		//Grab XMSB of of player from hardware
	sta PlayerXMSB	//Store to a temp pointer as we did with GEM collision
	lda $d001		//Grab Y position of player from hardware
	sta PlayerY		//Store to a temp pointer as we did with GEM collision
	lda #$10	    //X = Central area of sprite 
	sta PlayerSMX+1 //Store to self-mod X position
	lda #$32
	sta PlayerSMY+1 //Y = Top area of sprite

	lda #<UpStop	//Set UpStop in order to disable sprite
	sta StopSM+1	//position movement for that specific direction
	lda #>UpStop    //Those are set as low/hi bytes 
	sta StopSM+2 
	jsr MapPlayerToChar2 //Call second sprite/background collision
	
	lda #$10		//X = central area of the sprite
	sta PlayerSMX+1
	lda #$20		//Y = bottom area of the sprite
	sta PlayerSMY+1
	
	lda #<DownStop  //Set DownStop in order to disable sprite
	sta StopSM+1	//position movement for that specific direction
	lda #>DownStop  //Those are set as low/hi bytes   
	sta StopSM+2
	jsr MapPlayerToChar2 //Call collision subroutine
	
	lda #$18		//X = Left of the sprite 
	sta PlayerSMX+1
	lda #$2b		//Y = Central area of the sprite
	sta PlayerSMY+1 
	
	lda #<LeftStop  //Set LeftStop in order to disable sprite
	sta StopSM+1
	lda #>LeftStop  
	sta StopSM+2
	jsr MapPlayerToChar2 //Call collision subroutine
	
	lda #$04			//X = Right of the sprite
	sta PlayerSMX+1
	lda #$2b			//Y = Central area of the sprite
	sta PlayerSMY+1
	
	lda #<RightStop
	sta StopSM+1
	lda #>RightStop
	sta StopSM+2
	
	jsr MapPlayerToChar2
	rts
	
MapPlayerToChar2:
	lda PlayerX	//Read X position of the player
	sec
PlayerSMX:	
	sbc #$10 	//Read Central area of sprite 
	sta ZeroPage2 //Store to the screen ZeroPage COLUMN
	
	lda PlayerXMSB //Read MSB position of the player
	sbc #$00
	sta $fa
	lsr
	lda ZeroPage2  //Read first zero page of ZeroPage
	ror			  //Perform 16bit calculation
	lsr			  //(Division)
	lsr
	sta ZeroPage2+3 //Store to selfmod zeropage for X position
	
	lda PlayerY	  //Read Y position of player	
	sec
PlayerSMY:	
	sbc #$2a	  //Y-Co-ordinates
	lsr			  //Perform 16-bit calculation
	lsr			  //Division
	lsr
	sta ZeroPage2+4 //Store to selfmod ROW
	
	lda #<SCRN		//Read Low byte of screen ($0400)
	sta ZeroPage2+1 //Store as screen read Low 
	lda #>SCRN		//Read Hi byte of screen ($0400)
	sta ZeroPage2+2	//Store as screen read Hi
	
	ldx ZeroPage2+4	//Read current row
	beq CheckCharType2 //Then perform char check
NextRow2:
	lda ZeroPage2+1 	//Read low byte of stored screen area
	clc				
	adc #$28		//Move 40 chars down
	sta ZeroPage2+1  //Store to next ROW
	lda ZeroPage2+2  //Read hi byte of stored screen area
	adc #$00
	sta ZeroPage2+2	//Store to next COLUMN
	dex				
	bne NextRow2
	
	//Calculation complete. Simply check the type of
	//character the player hits:
	
CheckCharType2:	
	ldy ZeroPage2+3
	lda (ZeroPage2+1),y
	cmp #WallTopLeft	//Char representing top left wall
	bne NotHitWall1
	jmp SetStopDirection //Set the stop direction for the player
NotHitWall1:	
	cmp #WallTopRight 	//Char representing top right wall
	bne NotHitWall2
	jmp SetStopDirection //Set the stop direction for the player
NotHitWall2:	
	cmp #WallBottomLeft //Char representing the bottom left of the wall
	bne NotHitWall3
	jmp SetStopDirection //Set the stop direction for the player
NotHitWall3:	
	cmp #WallBottomRight //Char representing the bottom right of the wall	
	bne NotHitWall4	
	jmp SetStopDirection	
NotHitWall4:			 //No walls hit. Leave it
	rts
	
SetStopDirection:		 //A wall surrounds the player at a certain	
						 //direction. Disable that particular direction	
						 //by forcing a stop value.	
	lda #1				 //Stop mode = 1						
StopSM:											
	sta UpStop			 //Selfmod properties			
	rts			
				
	

//==================================================
//Move spiders ... Until they reach a specific  
//character set that should change their direction 						
//==================================================						


SpiderControl:
	jsr AnimateSpiders //Animate all of the spiders
	jsr ControlSpider1 //Subroutine to control spider 1
	jsr ControlSpider2 //Subroutine to control spider 2
	jsr ControlSpider3 //Subroutine to control spider 3
	jsr ControlSpider4 //Subroutine to control spider 4
	rts
	
//Macro defined control for each spider. Based on
//direction, aligned movement and also sprite/background
//collision

//Macro is defined as:
//SpiderAlreadyMoving - A Check pointer to test spider properties 
//SpiderMoveTime	  - Spider movement time before aligned to the next 2x2 block 
//SpiderDir 		  - Direction of any spider
//SpiderX			  - Corresponds to X position of spider object
//SpiderY  			  - Corresponds to Y position of spider object
//SpiderScreenLoStore - Low byte stored zero page for spider (Sprite/Background collision)
//SpiderScreenHiStore - Hi byte stored zero page for spider (Sprite/Background collision)

.macro controlspider(HangTime,IntervalValue,SpiderAlreadyMoving, SpiderDir, SpiderMoveTime, SpiderX, SpiderY, SpiderScreenLoStore, SpiderScreenHiStore) { 

	
controlspiderloop:
	lda HangTime //Check delay of spider movement
	cmp IntervalValue //Has value expired?
	beq carryon	//Yes, reset HangTime and continue animation
	inc HangTime
	rts
carryon:
	lda #0
	sta HangTime
	lda SpiderAlreadyMoving	//Is spider allowed to move?
	cmp #1						//YES
	beq spidercanmove			//check movement direction
	jmp spidertobgrcoll 		//else stop spider moving, check collision
	
spidercanmove:	
	lda SpiderDir	//Read direction of selected spider
	cmp #up			//Is the direction read to be up?
	bne spidernotup //No, check if the direction is down
	jmp shiftspiderup //else shift spider up.
spidernotup:
	cmp #down		//Is the direction red to be down?
	bne spidernotdown //no check if the direction is left
	jmp shiftspiderdown //else shift spider down.
spidernotdown:
	cmp #left		//Is the direction read to be left? 
	bne spidernotleft //No, check if the direction is right
	jmp shiftspiderleft //else shift spider left.
spidernotleft:
	cmp #right		//Is the direction read to be right?
	bne spidernotright //No, there must be a fault 
	jmp shiftspiderright //else shift spider right
spidernotright: //detect error in code and stop
	rts
	
//Move spider upwards then jump to the	
//alignment movement test	

shiftspiderup:	
	lda SpiderY		//Grab current Y position of spider
	sec				//subtract the current position 
	sbc #2			//by 2
	sta SpiderY		//store new position.
	jmp testalignment //Check if spider has aligned	
		
//Move spider downwards then jump to the		
//alignment movement test		
		
shiftspiderdown:	
	lda SpiderY		//Grab current Y position of spider
	clc				//add the value of 2 to the
	adc #2			//current position, then
	sta SpiderY		//store it.
	jmp testalignment //Check if spider has aligned	

//Move spider left then jump to the alignment movement
//test
		
shiftspiderleft:	
	lda SpiderX		//Grab current X position of spider
	sec				//subrtract the value of 1 to the
	sbc #1			//current position, then
	sta SpiderX		//store it.
	jmp testalignment //Check if spider has aligned
	
//Move spider right then setup the alignment movement 
//test 

shiftspiderright: 
	lda SpiderX		//Grab current X position of spider
	clc				//add the value of 1 to the 
	adc #1			//current position, then 
	sta SpiderX		//store it as a new position.
	
//Test sprite movement alignment. After 8 pixels of 
//movement, force the pointer SpiderAlreadyMoving to 
//stop. So that earlier code can check for the sprite 
//to background collision more accurately 

testalignment: 
	inc SpiderMoveTime	//Increment the value of the timer
	lda SpiderMoveTime //Read the timer, until the value
	cmp #8				//of 8 (Alignment value) has been read
	beq movementexpired //then assume movement time has expired
	rts
	
//Movement time has expired. Reset the clockm, and
//stop the spider moving until asked to by the code
//that controls it.

movementexpired:
	lda #0
	sta SpiderAlreadyMoving //Stop spider moving
	sta SpiderMoveTime		 //Reset spider movement time
	rts
	 	
//=====================================================	
//Spider to background collision (All linked inside the		
//macro in order to get a more accurate movement change		
//setup.		
//=====================================================

spidertobgrcoll:		

	lda SpiderY //Read Y position of spider		
	sec			 //work out the HEIGHT of the collision		
	sbc #$32	 //area around the whole sprite		
	lsr			 		
	lsr		
	lsr		
	tay			//Transfer area to Y for screen co-ords
	lda ScreenLo,y //Copy low byte of screen row table 
	sta SpiderScreenLoStore //Store to lo-byte pointer
	lda ScreenHi,y //Copy hi byte of screen row table
	sta SpiderScreenHiStore //Store to hi-byte pointer
	lda SpiderX  //Read X position of spider
	sec			 //calculate the WIDTH of the collision
	sbc #$08     //area across the whole sprite
	lsr
	lsr
	tay			 //Transfer area to Y for screen co-ords
	ldx #$03	 //Unrolled loop for self modifying code
	sty selfmod+1 //Store to self modifting loop +1 byte
bgrcolloop:	 
	
	//Now check which character the spider has touched
	
	lda (SpiderScreenLoStore),y
	cmp #Web_Up_Down_Left //Direction changed char hit?
	bne notwebupdownleft //No char hit in range of spider
	jmp changeupdownleft  //Yes, target spider's direction
notwebupdownleft:
	cmp #Web_Up_Down_Right //Next char hit?
	bne notwebupdownright //No char hit in range of spider
	jmp changeupdownright //Yes, target spider's direction
notwebupdownright:
	cmp #Web_Any_Direction	//Next char hit?
	bne notwebanydirection //No char hit in range of spider
	jmp changeanydirection //Yes, target spider's direction
notwebanydirection:
	cmp #Web_Down_Left		//Next char hit?
	bne notwebdownleft //No char hit in range of spider
	jmp changedownleft //Yes, target spider's direction
notwebdownleft:
	cmp #Web_Up_Left		//Next char hit?
	bne notwebupleft   //No char hit in range of spider
	jmp changeupleft   //Yes, target spider's direction
notwebupleft:
	cmp #Web_Down_Right	//Next char hit?
	bne notwebdownright //No char hit in range of spider
	jmp changedownright //Yes, target spider's direction
notwebdownright:	
	cmp #Web_Up_Right	//Next char hit?
	bne notwebupright	//No char hit in range of spider
	jmp changeupright   //Yes, target spider's direction
	
notwebupright:
	cmp #Web_Up_Left_Right //Next char hit?
	bne notwebupleftright //No char hit in range of spider
	jmp changeupleftright //Yes, target spider's direction
		
notwebupleftright:	
	cmp #Web_Down_Left_Right //Next char hit?	
	bne notwebdownleftright //No char hit in range of spider	
	jmp changedownleftright //Yes, target spider's direction	
	jmp selfmod			//Not found, do selfmod code
	
notwebdownleftright:	//Cannot read any more char types
	
//The self-modyfying loop. Process screen char rows	
//according to the low+hi byte values of the two	
//tables set.	

selfmod:	
	ldy #$00	
	lda SpiderScreenLoStore //Grab current row	
	clc						 //then move on to the next
	adc #$28				 //row (40 chars).
	sta SpiderScreenLoStore //Store new row
	bcc skipmod				 //Read value too low, skip mode
	inc SpiderScreenHiStore //Increment table value
skipmod:
	dex
	bne bgrcolloop2
	lda #1					 //Allow continue movement
	sta SpiderAlreadyMoving
	rts
bgrcolloop2:
	jmp bgrcolloop

//==================================================	
//Spider direction setup:	
//The spider has landed on a specific web character	
//that should force it to change direction.	
//==================================================	
	
//The spider lands on a char that should shift it 
//Up, Down or Left. Call randomizer to set new 
//direction for spider, but also check if an invalid
//value has also been detected. If it has, keep changing
//the value until a spider has a correct direction to 
//move around the maze.

//Spider can only go up, down or left
changeupdownleft:
	jsr RandomizeTimer //Call Randomize timer subr.
	lda RandTemp	   //Grab value of randtemp
	cmp #right		   //RIGHT detected?
	beq changeupdownleft //Yes, loop until right is not stored
	sta SpiderDir		//Store direction of spider
	jmp restorespiderprops //Restore spider properties
	
//Spider can only go up, down or right
changeupdownright:
	jsr RandomizeTimer	//Call Randomize time subr.
	lda RandTemp		//Grab value of RandTemp
	cmp #left			//LEFT detected?
	beq changeupdownright //Yes, loop until valid direction stored
	sta SpiderDir		//Then store the new direction
	jmp restorespiderprops //Restore spider properties
	
//Spider can move any direction	
changeanydirection:
	jsr RandomizeTimer	//Call RandomizeTimer subr.
	lda RandTemp		//Grab value of RandTemp
	sta SpiderDir		//No direction to detect, just store it to SpiderDir
	jmp restorespiderprops //Restore spider properties
	
//Spider can only move down or left	
changedownleft:					
	jsr RandomizeTimer	//Call RandomizeTimer subr.	
	lda RandTemp		//Grab value of RandTemp
	cmp #up				//UP detected?
	beq changedownleft  //YES, search next direction
	cmp #right			//Right detected?
	beq changedownleft	//YES, search next direction
	sta SpiderDir		//Else store the new direction
	jmp restorespiderprops //Restore spider properties
	
//Spider can only move up or left
changeupleft:
	jsr RandomizeTimer	//Call RandomizeTimer subr.
	lda RandTemp		//Grab value of RandTemp
	cmp #down			//DOWN detected?
	beq changeupleft	//YES, search for next direction
	cmp #right			//RIGHT detected?
	beq changeupleft	//YES, search for next direction
	sta SpiderDir		//Else store the new direction
	jmp restorespiderprops //Restore spider properties
	
//Spider can only move down or right
changedownright:
	jsr RandomizeTimer  //Call RandomizeTimer subr.
	lda RandTemp		//Grab value of RandTemp
	cmp #up				//UP detected?
	beq changedownright //YES, search for next direction
	cmp #left			//LEFT detected?
	beq changedownright //YES, search for next direction
	sta SpiderDir		//Else store the new direction
	jmp restorespiderprops //Restore spider properties
	
//Spider can only move up or right
changeupright:
 	jsr RandomizeTimer	//Call RandomizeTimer subr.
	lda RandTemp		//Grab value of RandTemp
	cmp #down			//DOWN detected?
	beq changeupright	//YES, search for next direction
	cmp #left			//LEFT detected?
	beq changeupright   //YES, search for next direction
	sta SpiderDir		//Else store the new direction
	jmp restorespiderprops //Restore spider properties
	
//Spider can only move up, left or right	
changeupleftright:	
	jsr RandomizeTimer //Call RandomizeTimer subr.	
	lda RandTemp	   //Grab value of RandTemp	
	cmp #down		   //Has DOWN been detected?	
	beq changeupleftright //YES, search for next direction	
	sta SpiderDir		//Else store the new direction
	jmp restorespiderprops //Restore spider properties
	
//Spider can only move down, left or right
changedownleftright:
	jsr RandomizeTimer //Call randomize timer subr.
	lda RandTemp		//Grab value of RandTemp
	cmp #up				//Has UP been detected?
	beq changedownleftright //YES, search for next direction
	sta SpiderDir		//Else store the new direction
	jmp restorespiderprops //Restore spider properties
	
//New directions have been set, allow spider properties
//to be enabled. 
restorespiderprops:
	lda #0
	sta SpiderMoveTime
	lda #1
	sta SpiderAlreadyMoving
	rts

}	//End of macro
//=========================================================
//Spider control subroutines (Linked to the above macro)
//The commands in brackets represent the pointers that are called
//to control each spider, according to the value of the macro called

//Spider1HangTime = The pointer that sets the delay of each spider movement,
//					depending on the level you are on.
//Spider1Interval = The value of the delayed time set for the spider to move
//Spider1AlreadyMoving is linked to macro pointer SpiderAlreadyMoving
//Spider1Dir is linked to maacro pointer SpiderDir
//Spider1MoveTime is linked to macro pointer SpiderMoveTime
//ObjPos+2 is linked to macro pointer SpiderX
//ObjPos+3 is linked to macro pointer SpiderY
//Spider1ScreenLoStore is linked to SpiderScreenLoStore
//Spider1ScreenHiStore is linked to SpiderScreenHiStore

//Subroutine to control spider 1 via macro
ControlSpider1:
	controlspider(Spider1HangTime,Spider1Interval,Spider1AlreadyMoving, Spider1Dir, Spider1MoveTime, ObjPos+2, ObjPos+3, Spider1ScreenLoStore, Spider1ScreenHiStore)

//Subroutine to control spider 2 via macro
ControlSpider2:
	controlspider(Spider2HangTime,Spider2Interval,Spider2AlreadyMoving, Spider2Dir, Spider2MoveTime, ObjPos+4, ObjPos+5, Spider2ScreenLoStore, Spider2ScreenHiStore)
	
//Subroutine to control spider 3 via macro
ControlSpider3:
	controlspider(Spider3HangTime,Spider3Interval,Spider3AlreadyMoving, Spider3Dir, Spider3MoveTime, ObjPos+6, ObjPos+7, Spider3ScreenLoStore, Spider3ScreenHiStore)
	
//Subroutine to control spider 4 via macro
ControlSpider4:
	controlspider(Spider4HangTime,Spider4Interval,Spider4AlreadyMoving, Spider4Dir, Spider4MoveTime, ObjPos+8, ObjPos+9, Spider4ScreenLoStore, Spider4ScreenHiStore)
//=========================================================	
	
	
//=======================================================	
//Randomize timer, and store new value to a temp pointer	
//(which then gets read to the spider's direction pointer	
//outside this subroutine	
//=======================================================
RandomizeTimer:
	ldx RandCounter		//Read pointer 
	lda RandDirTable,x  //Read all bytes from table
	sta RandTemp		//Store to temp byte
	inx					//Increment pointer RandCounter
	cpx #RandDirTableEnd-RandDirTable //Check until end of table reached
	beq ResetTable		//Then reset it
	inc RandCounter		//Increment pointer RandCounter
	rts
ResetTable:
	ldx #$00			//Reset the pointer RandCounter
	stx RandCounter		//so table read starts again.
	rts
	
//--------------------------------------------------------	
//Player shield properties. If the shield time = 0	
//then the player's shield should be disabled, and	
//the player is then vulnerable to the spiders.	
//--------------------------------------------------------	
CheckShield:	
	lda PlayerKilledBySpider	//Prevent existing collision
	cmp #$01					//check if player already dead
	beq SkipChecker
	lda ShieldTime				//Check if shield is ZERO
	cmp #$00	
	beq Vulnerable				//Shield is Zero. Player is vulnerable
	dec ShieldTime				//Decrement the shield time
	ldx ShieldPointer			//Read shield pointer loop 
	lda ShieldColour,x			//read colour table (for better flashing)
	sta $d027					//Store to the sprite colour
	inx	
	cpx #ShieldColourEnd-ShieldColour //Wait until we have reached	
	beq ResetShieldPointer			  //the end of the table before reset
	inc ShieldPointer	
	jmp SkipChecker	
ResetShieldPointer:	
	ldx #0	
	stx ShieldPointer	
SkipChecker:	
	rts	
	
Vulnerable:		//The shield is out, so now check the
				//sprite/sprite collision using software
				//values of the player (and spiders)
	
	lda #7			
	sta $d027			
	lda ObjPos		//Grab X position of player object
	sec				//Calculate the starting X
	sbc #$06		//position of the sprite collision area 
	sta SprSprColl	//Store to the next collision pointer
	clc				//then calculate the end X
	adc #$0c		//position of the collision area X
	sta SprSprColl+1 //Grab Y position of player object
	lda ObjPos+1	//Calculate the starting Y
	sec				//position of the sprite collision area
	sbc #$0c		
	sta SprSprColl+2 //Store to the next collision pointer
	clc				//Calculate the end position of the
	adc #$18		//Y sprite collision.
	sta SprSprColl+3 //Store to the next collision pointer
	
	ldy #$00		 //Generate a loop to check collision
CheckSprSpr:		 //(Are all spiders in range of player
	lda ObjPos+2,y	 //collision area?) - Grab Spider X position
	cmp SprSprColl	 //Check range of first pointer
	bcc NotDeadYet   //No collision
	cmp SprSprColl+1 //Check range of second pointer
	bcs NotDeadYet	 //No collision
	lda ObjPos+3,y	 //Grab Spider Y position
	cmp SprSprColl+2 //Check range of third pointer
	bcc NotDeadYet	 //No collision
	cmp SprSprColl+3 //Check range of forth pointer
	bcs NotDeadYet	 //No collision
	jmp PlayerIsDead //A collision has been detected, kill the player
NotDeadYet:
	iny				 //No collision, so increment the value of the
	iny				 //Sprite X and Sprite Y values for each spider until
	cpy #$08		 //All 4 spiders have been read
	bne CheckSprSpr
	rts				 //End of code
	
	
//The player dies:
PlayerIsDead:
	
	lda #$01
	sta PlayerKilledBySpider
	jsr PlaySFX_DEAD
	rts
	
//SFX Pointers	

PlaySFX_DEAD:	
	lda #<SFX_DEAD	
	ldy #>SFX_DEAD	
	ldx #14	
	jsr SFXInit	
	rts	
		
PlaySFX_CHOMP:	
	lda #<SFX_CHOMP	
	ldy #>SFX_CHOMP	
	ldx #14	
	jsr SFXInit	
	rts	
		
PlaySFX_SHIELD:	
	lda #<SFX_SHIELD	
	ldy #>SFX_SHIELD	
	ldx #14	
	jsr SFXInit	
	rts	
		
PlaySFX_EXTRA_LIFE:	
	lda #<SFX_EXTRA_LIFE	
	ldy #>SFX_EXTRA_LIFE	
	ldx #14	
	jsr SFXInit	
	rts	

		
	
//========================================================	
//Scoring and lives... mask the score panel	
//over the second but last screen row, in order to get	
//an updated score or amount of lives	
//========================================================	

MaskPanel:	
	ldx #$00	
DrawScorePanel:	
	lda ScorePanel,x	
	sta SCRN+920,x	
	inx	
	cpx #ScorePanelEnd-ScorePanel	
	bne DrawScorePanel	
	rts	

//Screen clear map

ClearMap:
	ldx #$00
ClearFill:
	lda #$20
	sta $0400,x
	sta $0500,x
	sta $0600,x
	sta $06e8-120,x
	inx
	bne ClearFill
	rts
//========================================================
//INTERRUPTS
//Subroutine that kills all existing 	
//interrupts and disables sprites	
//========================================================		
KillAllInterrupts:	
	sei	
	ldx #$31	//Force IRQ to be disabled	
	ldy #$ea	
	stx $0314	
	sty $0315	
	lda #$81	//Time out the CIA to disable
	sta $dc0d	//any existing interrupts
	sta $dd0d	
	lda #$00	//Switch the interrupt delay
	sta $d019	//off.
	sta $d01a	
	sta $d015	//Switch off all the sprites
	
	
	ldx #$00	//Switch off all channels and	
ClearSID:		//waveforms of the SID CHIP
	lda #$00	
	sta $d400,x	
	inx	
	cpx #$18	
	bne ClearSID	
	cli	
	rts	
		
//POINTERS		

LevelDrawPointer: .byte 0 //Draw current level
FireButton: .byte 0 //Fire bounce test
system: .byte 0		//PAL or NTSC pointer!		
NTSCTIMER: .byte 0	//NTSC timer (if detected) for playing music	
SyncTimer: .byte 0  //Game sync timer
PlayerDirection: .byte 0 //Pointer to set direction for player
PlayerAlreadyMoving: .byte 0 //Pointer/Switch to test if player is
							//already moving.
PlayerMoveTime: .byte 0 //Timer to move player
PlayerMoveStop: .byte 0 //Value of timer to stop moving
PlayerKilledBySpider: .byte 0 //Value of player killed by spider
PlayerDefaultPosX: .byte 0 //Value of player starting position X
PlayerDefaultPosY: .byte 0 //Value of player starting position Y
ShieldTime:		.byte 0 //Shield time, until vulnerable
ShieldPointer:  .byte 0


RandTemp: 		.byte $01
RandCounter:	.byte $55,$da

//Pointers to indicate stop position for
//player

UpStop: .byte 0
DownStop: .byte 0
LeftStop: .byte 0
RightStop: .byte 0


//The next set of pointers are the hardware					
//pointers for reading sprite/background 					
//collision. Basically if a sprite in range
//approaches a collision character.
					
PlayerX: 		.byte 0	//$D000 
PlayerXMSB: 	.byte 0	//$D010
PlayerY: 		.byte 0	//$D001

//Where spiders are moving around the maze, these are to
//based on timers, before restarting their own route again 


SpiderPropertiesStart:
Spider1AlreadyMoving: .byte 0 //Pointer for spider movement check
Spider1MoveTime: .byte 0	  //Timed movement for alignment
Spider1Dir: .byte 0			  //Directional pointer
Spider1HangTime: .byte 0
Spider1Interval: .byte 0
Spider2AlreadyMoving: .byte 0 //<--- Same for other spider properties
Spider2MoveTime: .byte 0
Spider2HangTime: .byte 0
Spider2Interval: .byte 0
Spider2Dir: .byte 0
Spider3AlreadyMoving: .byte 0
Spider3MoveTime: .byte 0
Spider3Dir: .byte 0
Spider3HangTime: .byte 0
Spider3Interval: .byte 0
Spider4AlreadyMoving: .byte 0
Spider4MoveTime: .byte 0
Spider4Dir: .byte 0
Spider4HangTime: .byte 0
Spider4Interval: .byte 0
SpiderPropertiesEnd:  

//general animation delays and pointers  
AnimDelay1:	  .byte 0 //Animation delay for player
AnimDelay2:	  .byte 0 //     "      "   for spider
AnimDelay3:	  .byte 0 //     "      "   for player death

AnimPointer1: .byte 0 //Animation pointer for player 
AnimPointer2: .byte 0 //     "      "   for spider
AnimPointer3: .byte 0 //     "      "   for player death
AnimPointer4: .byte 0  


ShieldColour: .byte $09,$08,$0a,$07,$01,$07,$0a,$08,$09
ShieldColourEnd:

//Sprite position X + Y table

ObjPos:		.byte $00,$00,$00,$00,$00,$00,$00,$00		
			.byte $00,$00,$00,$00,$00,$00,$00,$00		
				
//Sprite to sprite collision pointers for all 4 spiders				
//to player				

SprSprColl: 				
			.byte 0,0,0,0			

//Screen Lo+Hi byte tables

ScreenLo:	.byte <SCRN,<SCRN+40,<SCRN+80,<SCRN+120,<SCRN+160
			.byte <SCRN+200,<SCRN+240,<SCRN+280,<SCRN+320,<SCRN+360
			.byte <SCRN+400,<SCRN+440,<SCRN+480,<SCRN+520,<SCRN+560
			.byte <SCRN+600,<SCRN+640,<SCRN+680,<SCRN+720,<SCRN+760
			.byte <SCRN+800,<SCRN+840,<SCRN+880,<SCRN+920,<SCRN+960
			
ScreenHi:	.byte >SCRN,>SCRN+40,>SCRN+80,>SCRN+120,>SCRN+160
			.byte >SCRN+200,>SCRN+240,>SCRN+280,>SCRN+320,>SCRN+360
			.byte >SCRN+400,>SCRN+440,>SCRN+480,>SCRN+520,>SCRN+560
			.byte >SCRN+600,>SCRN+640,>SCRN+680,>SCRN+720,>SCRN+760
			.byte >SCRN+800,>SCRN+840,>SCRN+880,>SCRN+920,>SCRN+960
			
//Random direction for moving the spiders			
RandDirTable:
	.byte 1,2,3,4,1,3,4,1,2,4,1,3,1,2,4,1			
	.byte 3,1,4,1,4,3,1,4,2,1,1,4,3,1,2,4			
	.byte 4,3,1,2,2,1,4,3,1,2,3,2,1,4,2,4			
	.byte 1,3,2,4,2,3,1,2,4,3,4,1,2,4,1,2			
	.byte 2,1,4,3,4,1,2,4,2,1,4,2,3,1,4,3			
	.byte 4,2,3,1,2,3,2,1,3,2,3,4,1,2,4,1			
	.byte 4,1,2,4,1,2,4,2,1,4,2,4,1,4,2,1			
	.byte 3,1,2,4,1,2,3,1,4,2,3,1,2,4,3,4		
RandDirTableEnd:		

//Some tables for spider speed according to level

IntervalTable1: .byte $01,$01,$01,$01
				.byte $01,$01,$01,$01
				.byte $01,$01,$01,$01
				.byte $00,$00,$00,$00
IntervalTable1End:				

IntervalTable2: .byte $01,$01,$01,$01
				.byte $00,$00,$00,$00
				.byte $01,$01,$01,$01
				.byte $00,$00,$00,$00
				
IntervalTable3:	.byte $01,$01,$01,$01
				.byte $01,$01,$01,$01
				.byte $00,$00,$00,$00
				.byte $00,$00,$00,$00
				
IntervalTable4: .byte $01,$01,$01,$01
				.byte $01,$01,$01,$01
				.byte $01,$01,$01,$01
				.byte $00,$00,$00,$00
				
//Sprite animation tables				
				
//Player alive animation
PlayerAliveFrame:				
	.byte $80,$81,$82,$83,$82,$81				
PlayerAliveFrameEnd:				

//Spider animation				
SpiderFrame1:				
	.byte $84,$85,$86,$85		
			
SpiderFrame1End:		
SpiderFrame2:		
	.byte $85,$84,$85,$86			
SpiderFrame3:			
	.byte $86,$85,$84,$85			
SpiderFrame4:			
	.byte $85,$86,$85,$84			
				
//Player death animation				
PlayerDeadFrame:				
	.byte $87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f,$90,$8f,$90,$8f				
	.byte $90,$90,$90,$90,$90,$90,$90,$90,$90,$90,$90				
PlayerDeadFrameEnd:				

//Main score panel

ScorePanel:				
	.text "score: "
Score: .text "000000   *:"
Lives: .text "3    £:"
Level: .text "01   hi: "
HiScore: .text "072200"    				
ScorePanelEnd:			

//Level Complete Text

LevelCompleteText:
	.text "        wow, that was a-maze-ing        "
	.text "                                        "
	.text "  you have managed to clear this level  "
	.text "                                        " 
	.text "  the spiders are not very happy though "
LevelCompleteTextEnd:

//Game Over Text
GameOverText:
	.text "           bad luck matey !!!           "
	.text "                                        "
	.text "   the spiders were too smart for you   "
	.text "                                        "
	.text "      so, it's game over i'm afraid     "
GameOverTextEnd:	
	
EndText:
	.text "             congratulations            "
	.text "                                        "
	.text "   you have managed to escape the maze  "
	.text "    and you have completed your quest   "
	.text "    and you live happily ever after!    "          
EndTextEnd:
 
HallOfFameText:
	.text "   congratulations, a brilliant score   "
	.text "                                        "
	.text "you are one of the top five gem gobblers"
	.text "                                        "
	.text "        please sign in your name        "
HallOfFameTextEnd:
Name:
	.text "         "

//In game sound effects tables made from GoatTracker 

SFX_CHOMP:
               .byte $0e,$ee,$00,$C0,$81,$C3,$C4,$CC,$C7,$CC,$00
        
SFX_EXTRA_LIFE:
		 .byte $0E,$EE,$88,$B0,$41,$B0,$B4,$B4,$B7,$B7,$BC,$BC,$C0,$C0,$BC,$BC
         .byte $B7,$B7,$B4,$B4,$B0,$B0,$A0,$10,$00
         
SFX_DEAD: 
		.byte $0E,$EE,$88,$BC,$41,$BB,$BC,$BB,$BA,$BB,$BA,$BB,$BA,$B9,$BA,$B9
        .byte $B8,$B9,$B8,$B7,$B8,$B6,$B7,$B6,$B5,$B6,$B5,$B4,$B5,$B4,$B3,$B4
        .byte $B3,$B2,$B3,$B2,$B1,$B0,$90,$10,$00
        
SFX_SHIELD:
		.byte $0E,$EE,$88,$B0,$41,$B0,$B2,$B4,$B6,$B8,$BA,$BA,$BC,$BC,$BE,$BE
        .byte $90,$11,$00

		*=$6000
		
//Some more tables, this time representing the level 
//map position for 4 different areas in the code.

//First 256 byte Screen areas 
LevelTableLo1:
	.byte <level1map,<level2map,<level3map,<level4map
	.byte <level5map,<level6map,<level7map,<level8map
	.byte <level9map,<level10map,<level11map,<level12map
	.byte <level13map,<level14map,<level15map,<level16map,0
LevelTableLo1End:
	
LevelTableHi1:
	.byte >level1map,>level2map,>level3map,>level4map
	.byte >level5map,>level6map,>level7map,>level8map
	.byte >level9map,>level10map,>level11map,>level12map
	.byte >level13map,>level14map,>level15map,>level16map,0
	
	//The next 256 byte screen areas 
 
LevelTableLo2:
	.byte <level1map+$100,<level2map+$100,<level3map+$100,<level4map+$100
	.byte <level5map+$100,<level6map+$100,<level7map+$100,<level8map+$100
	.byte <level9map+$100,<level10map+$100,<level11map+$100,<level12map+$100
	.byte <level13map+$100,<level14map+$100,<level15map+$100,<level16map+$100,0
	
LevelTableHi2:
	.byte >level1map+$100,>level2map+$100,>level3map+$100,>level4map+$100
	.byte >level5map+$100,>level6map+$100,>level7map+$100,>level8map+$100
	.byte >level9map+$100,>level10map+$100,>level11map+$100,>level12map+$100
	.byte >level13map+$100,>level14map+$100,>level15map+$100,>level16map+$100,0
	
	//The next 256 byte screen areas after the previous table
	
LevelTableLo3:
	.byte <level1map+$200,<level2map+$200,<level3map+$200,<level4map+$200
	.byte <level5map+$200,<level6map+$200,<level7map+$200,<level8map+$200
	.byte <level9map+$200,<level10map+$200,<level11map+$200,<level12map+$200
	.byte <level13map+$200,<level14map+$200,<level15map+$200,<level16map+$200,0
	
LevelTableHi3:
	.byte >level1map+$200,>level2map+$200,>level3map+$200,>level4map+$200
	.byte >level5map+$200,>level6map+$200,>level7map+$200,>level8map+$200
	.byte >level9map+$200,>level10map+$200,>level11map+$200,>level12map+$200
	.byte >level13map+$200,>level14map+$200,>level15map+$200,>level16map+$200,0
	
	//Finally the last remaining bytes before area $03e7
	
LevelTableLo4:
	.byte <level1map+$2e8,<level2map+$2e8,<level3map+$2e8,<level4map+$2e8
	.byte <level5map+$2e8,<level6map+$2e8,<level7map+$2e8,<level8map+$2e8
	.byte <level9map+$2e8,<level10map+$2e8,<level11map+$2e8,<level12map+$2e8
	.byte <level13map+$2e8,<level14map+$2e8,<level15map+$2e8,<level16map+$2e8,0
	
LevelTableHi4:
	.byte >level1map+$2e8,>level2map+$2e8,>level3map+$2e8,>level4map+$2e8
	.byte >level5map+$2e8,>level6map+$2e8,>level7map+$2e8,>level8map+$2e8
	.byte >level9map+$2e8,>level10map+$2e8,>level11map+$2e8,>level12map+$2e8
	.byte >level13map+$2e8,>level14map+$2e8,>level15map+$2e8,>level16map+$2e8,0
	


//Game code is fully complete, so now import the level data
//in to C64 memory.

//Import screen / map data for the game 
//each map has been created as a separate 
//file in Charpad V2.0. It is also possible 
//to replace/add your own designed levels, but 
//make sure the arrows (directions) for spiders 
//do not lead to illegal places. Afterall we don't 
//want spiders jumping outside the screen or through 
//walls. :) 

		*=$7000 "LEVEL MAP DATA"	
map:	
level1map:
	.import binary "bin/level1map.bin"
level2map:
	.import binary "bin/level2map.bin"
level3map:
	.import binary "bin/level3map.bin"	
level4map:
	.import binary "bin/level4map.bin"
level5map:
	.import binary "bin/level5map.bin"
level6map:
	.import binary "bin/level6map.bin"
level7map:
	.import binary "bin/level7map.bin"
level8map:
	.import binary "bin/level8map.bin"
level9map:
	.import binary "bin/level9map.bin"
level10map:
	.import binary "bin/level10map.bin"
level11map:
	.import binary "bin/level11map.bin"
level12map:
	.import binary "bin/level12map.bin"
level13map:
	.import binary "bin/level13map.bin"
level14map:
	.import binary "bin/level14map.bin"
level15map:
	.import binary "bin/level15map.bin"
level16map:
	.import binary "bin/level16map.bin"
	
//Title screen text	

TitleScreenText:		
	.text "       (c)2018 the new dimension        "
	.text "   developed for scene world magazine   "
	.text "                                        "
	.text "programming, graphics, sound & music by:"
	.text "            richard  bayliss            "
	.text "                                        "
	.text "           press fire to play           "
TitleScreenTextEnd:	
	
HiScoreTableText:
	.text "       today's best gem gobblers        "
	.text "                                        "
HiScoreTableStart:	
	.text "          1. "
name1: .text"richard b "
hiscore1: .text "072200           "
	.text "          2. "
name2: .text"tnd       "
hiscore2: .text "061800           "
	.text "          3. "
name3: .text "games     "
hiscore3: .text "053400           "
	.text "          4. "
name4: .text "scene     "
hiscore4: .text "042600           "
	.text "          5. "
name5: .text "world     "
hiscore5: .text "018000           "
HiScoreTableEnd:
HiScoreTableTextEnd:
	
ScrollText:
	.text " ... welcome to - spider maze - ...   a "
	.text "fun little gobble 'em up style maze "
	.text "game with a slight twist ...   "
	.text "programming by richard bayliss in kickassembler ...   "
	.text "graphics and music also by richard ...   (c)2018 the new dimension ...   "
	.text "brought to you by scene world magazine ...   your ques"
	.text "t is to go around 16 different maze room"
	.text "s and eat up the "
	.byte 60
	.text " diamonds ...   once you"
	.text " have cleared one room, you will move on"	
	.text " to the next room ...   there are other o"
	.text "bjects which you can pick up on the way "
	.text "...   "
	.byte 61
	.text " light bulbs will give you a tempor"
	.text "ary shield for a while ...   "
	.byte 42
	.text " hearts will"
	.text " award you extra lives ...   avoid the d"
	.text "eadly "
	.byte 62 
	.text " skulls (found on later levels) ..."
	.text "   also watch out for the deadly "
	.byte 63 
	.text " spiders ...   "
	.text " they will be moving from web to web in random directions .."
	.text ".   "
	.text "there are 16 levels in total to complete, but "
	.text "your quest will not be that easy, believe me :) ...   control the player with joystick por"
	.text "t 2 ...    during play, press control to pause the game, spacebar or firebutton will resume play from pause mode ...   pressing back arrow will abort the game ...   press fire to play and have fun  ...       "
	.text "                                        "
	.byte 0
	
//Some colour pointers (for colour flashing) We have	
//7 rows for the front end's credits, high score text,	
//level complete, name entry, game complete and game over text.	

ColourDelay:
	.byte $00
ColourPointer:
	.byte $00
	
ColourTable:	
	//.byte $09,$02,$08,$07,$01,$07,$08,$02,$09,$00	
	.byte $06,$04,$05,$03,$01,$03,$05,$04	
	//.byte $06,$0d,$0e,$03,$01,$03,$0e,$0d,$06,$00	
ColourTableEnd:	
	
//====================================================
//TITLE SCREEN PRESENTATION
//
//This code sets up all of the title screen settings.
//Displays a screen using a multi-irq and also displays
//the title screen logo, switches text and plays music
//in the background.
//=====================================================

TitleScreenCode:

//Before running the title screen
//check for cheat code (from hiscore)
//if cheat is discovered. Award the player infinite lives

	ldx #$00
CheckCheatCode:
	lda Name,x
	cmp CheatCode,x
	bne NoCheat
	inx
	cpx #$09
	bne CheckCheatCode
	lda #$2c
	sta LoseLife
NoCheat:
	lda #$00	//Black border and background
	sta $d020
	sta $d021
	sta XPOS    //Init soft scroll pointer
	lda #<ScrollText	//Initialise scroll message
	sta MessRead+1		//by setting low/hi byte starting
	lda #>ScrollText	//address of text/
	sta MessRead+2
	ldx #$00	//Clear entire screen - also setup
SetupLogoColours: 
	lda #$20		//Fill entire screen with SPACEBAR
	sta SCRN,x		//Character
	sta SCRN+$100,x
	sta SCRN+$200,x
	sta SCRN+$2e8,x
	lda ColRAMData,x //Pick out the logo colour data and
	sta $d800,x	     //store to colour RAM
	lda ColRAMData+$100,x //The same for the next 256 bytes
	sta $d900,x			  //of the next address.
	inx
	bne SetupLogoColours
	ldx #$00
blackarea:
	lda #$00
	sta COLR+400,x
	sta COLR+440,x
	sta COLR+480,x
	sta COLR+520,x
	sta COLR+560,x
	sta COLR+600,x
	sta COLR+640,x
	sta COLR+680,x
	sta COLR+720,x
	sta COLR+760,x
	inx
	cpx #40
	bne blackarea
	lda #$00				
	sta $d015 //No sprites allowed
	jsr DisplayCredits
	
	//Setup IRQ for the title screen
	
	lda #<TIRQ	//Low byte of the IRQ
	sta $0314	//Store to interrupt vector LOW
	lda #>TIRQ	//Hi byte of the IRQ
	sta $0315	//Store to interrupt vector HI
	lda #$7f	//CIA timer set
	sta $dc0d
	lda #$2e	//Starting raster position before 
	sta $d012	//engaging interrupt.
	lda #$1b	
	sta $d011	//Screen on
	lda #$01
	sta $d01a	//ACK IRQ interrupt request timer
	lda #0
	sta FireButton //Init firebutton press
	sta PAGEDELAY	//Initialise page delays
	sta PAGEDELAY+1 //and pages so the page flip
	lda #1
	sta PAGE		//operates more accurately
	lda #TitleMusic //Initialise title screen music 
	jsr MusicInit
	
	cli
	
	//Main loop of the title screen. It should perform a 
	//soft scroll at the bottom of the screen. Also flip
	//pages between the title screen credits, high score
	//table.
	
TitleLoop:
	jsr SyncMain	//Synchronize timer code
	jsr SoftScroll  //Perform soft scroll
	jsr PageFlipRoutine //Switch between title text and hi scores
	jsr ColourWash	//Colour washing over every still text
	jsr WashScroll	//Colour washing over the scroll text
	lda $dc00		//Wait for joystick port 2 
	lsr				//fire button to be pressed
	lsr
	lsr
	lsr
	lsr
	bit FireButton	//Prevent hold down of fire
	ror FireButton
	bmi TitleLoop
	bvc TitleLoop
	jmp GameStartSetup //Run main game.
	
//The main interrupt for the title screen
//A double interrupt is being used for it
//this time round.

//First interrupt - Set normal text char mode
//and smooth scrolling text.

TIRQ:	inc $d019
		lda #$2e //Top Raster end position
		sta $d012
		lda #$1b //Bitmap mode off - screen on
		sta $d011
		lda XPOS //Scrolltext mode - no multicolour
		sta $d016
		lda #$1a //1x1 charset from $2800
		sta $d018
		lda #$03 //Default VIC bank (3)
		sta $dd00
		lda #<TIRQ2 //Point to next IRQ vector
		sta $0314
		lda #>TIRQ2
		sta $0315
		lda #1
		sta SyncTimer
		jsr SystemDetect
		
		jmp $ea7e
		
//The second interrupt - Set the bitmap display
//so we can see the logo

TIRQ2:	inc $d019
		lda #$89 //End raster position before credits
		sta $d012
		lda #$3b //Bitmap mode switched on - screen on
		sta $d011
		lda #$18 //Multicolour mode (STILL) on
		sta $d016
		lda #$18 //Bitmap video RAM mode at $c400
		sta $d018
		lda #$00 //VIC BANK #$00 - Highest memory where BMP is
		sta $dd00
		lda #<TIRQ3 //Point to next (and final IRQ vector)
		sta $0314
		lda #>TIRQ3
		sta $0315
		jmp $ea7e
		
//The third and final interrupt - Display the credits text
//at the bottom of the screen.

TIRQ3:	inc $d019
		lda #$ea //End raster position before scroll
		sta $d012
		lda #$1b //Standard screen mode
		sta $d011
		lda #$08 //No multicolour charset 
		sta $d016 
		lda #$1a //text charset
		sta $d018
		lda #$03 //Default VIC Bank #3
		sta $dd00
		lda #<TIRQ	//Point back to the very first IRQ
		sta $0314
		lda #>TIRQ
		sta $0315
		jmp $ea7e
	
//Place the title screen text on to the screen
//Pick it line by line ...

DisplayCredits:
PlaceTitleText:
	ldx #$00
DoTitleText:
	lda TitleScreenText,x
	sta SCRN+520,x
	lda TitleScreenText+40,x
	sta SCRN+560,x
	lda TitleScreenText+80,x
	sta SCRN+600,x
	lda TitleScreenText+120,x
	sta SCRN+640,x
	lda TitleScreenText+160,x
	sta SCRN+680,x
	lda TitleScreenText+200,x
	sta SCRN+720,x
	lda TitleScreenText+240,x
	sta SCRN+760,x
	inx
	cpx #40
	bne DoTitleText
	rts
	
//Display high score table	

DisplayHiScoreTable:	
	ldx #$00	
DrawTable:	
	lda HiScoreTableText,x	
	sta SCRN+520,x	
	lda HiScoreTableText+40,x
	sta SCRN+560,x
	lda HiScoreTableText+80,x
	sta SCRN+600,x
	lda HiScoreTableText+120,x
	sta SCRN+640,x
	lda HiScoreTableText+160,x
	sta SCRN+680,x
	lda HiScoreTableText+200,x
	sta SCRN+720,x
	lda HiScoreTableText+240,x
	sta SCRN+760,x
	inx
	cpx #40
	bne DrawTable
	rts
	
//Main soft scroll routine - You should know	
//this already for Star Toast and Zap Zone!.	
	
SoftScroll:	
		
	lda XPOS	
	sec	
	sbc #2
	and #7	
	sta XPOS	
	bcs ExitSoftScroll	
	ldx #$00	
HardScroll:	
	lda SCRN+921,x	
	sta SCRN+920,x	
		
	inx	
	cpx #39	
	bne HardScroll
	
MessRead:
	lda ScrollText
	cmp #$00
	bne StoreMSG
	lda #<ScrollText
	sta MessRead+1
	lda #>ScrollText
	sta MessRead+2
	jmp MessRead
StoreMSG:
	sta SCRN+959
	inc MessRead+1
	bne ExitSoftScroll
	inc MessRead+2
ExitSoftScroll:
	rts

//Capture a char from the main colour
//washing subroutine and place it over
//the very last char of the scroll text.
//Then call a loop to wash the colour from
//the right to the left of the screen.
	
WashScroll:	
	lda COLR+440
	sta COLR+959	
		
	ldx #$00		
WashLoop:	
	lda COLR+921,x
	sta COLR+920,x
	inx
	cpx #$27
	bne WashLoop
	rts
	
//A simple paage flip routine. Controlled by a page delay	
//pointer (PAGEDELAY). Once it has reached 200, the second	
//byte in the delay is incremented. Once second byte = 2	
//the page flipping check is formed and new text is deplayed.

//PAGE 0 = credits
//PAGE 1 = hiscore table 
	
PageFlipRoutine:	
		
	lda PAGEDELAY	//Read page delay
	cmp #200		//Has delay reached 200?
	beq NEXT		//yes it has ... call NEXT
	inc PAGEDELAY	//else keep on looping
	rts	
NEXT:
	lda #0			//Reset the page delay
	sta PAGEDELAY
	lda PAGEDELAY+1	//Read the next byte of the page delay
	cmp #2			//Has second byte of delay reached 2?
	beq SwapPage	//Yes, we can swap pages.
	inc PAGEDELAY+1	//Otherwise increment the page delay again
	rts
	
	//Check which page to be called, then display 
	//the text for credits or high score table.
	
SwapPage:
	lda PAGE					//Read pointer PAGE				
	cmp #0 //Page 0 = Credits	//Is the page read to be 0?
	beq Credits					//Yes, display credits
	cmp #1						//Is the page read to be 1?
	beq HiScores				//Yes, display high score table.
	rts							//end!
	
//Call subroutine to display credits then force
//the page to 1 (So next time the page flips, it can
//display the high score table).
Credits:
	jsr DisplayCredits
	lda #1
	sta PAGE
	rts
	
//Call subroutine to display high score table then force
//the page to 0 (So next time the page flips, it can
//display the credits).

HiScores:
	jsr DisplayHiScoreTable
	lda #0
	sta PAGE
	rts

//Some pointers, inidicating the page flipping process
//and also the smooth scrolling subroutine
	
PAGEDELAY: .byte 0,0 //2 bytes for page delay pointer
PAGE:	   .byte 0	 //The actual page being read 
XPOS: .byte 0 //Smooth scroller position

//=====================================================
//HIGH SCORE CHECK + NAME ENTRY subroutine
//Checks whether all digits of the player's score is
//a high score or not. If it is then prompt the player
//to type in his/her name. If not, skip the code to
//jump straight back to the title screen
//=====================================================
CheckHiScoreTable:
	//For the hi-score music. Init in game music again
	lda #GameMusic
	jsr MusicInit

	//Clear the entire screen.
	ldx #$00
ClearScreenHi:
	lda #$20
	sta SCRN,x
	sta SCRN+$100,x
	sta SCRN+$200,x
	sta SCRN+$2e8,x
	inx
	bne ClearScreenHi
	
	//Generate macro for checking standard score to high
	//score, before setting the position which the
	//player is at

	//HiScoreTarget = Target for high score rank to check	
	//Rank = Jump address for setting up the new rank	
		
.macro CheckHiScore(HiScoreTarget,notrank,rank) {

	lda Score			//Read first digit of score
	sec					//calculate / compare
	lda HiScoreTarget+5	//Read 6th digit of hiscore
	sbc Score+5			//check if 6th digit of score is lower
	lda HiScoreTarget+4	//Read 5th digit of hiscore
	sbc Score+4			//check if 5th digit of score is lower
	lda HiScoreTarget+3	//Read 4th digit of hiscore
	sbc Score+3			//check if 4th digit of score is lower
	lda HiScoreTarget+2	//Read 3rd digit of hiscore
	sbc Score+2			//check if 3rd digit of score is lower
	lda HiScoreTarget+1 //Read 2nd digit of hiscore
	sbc Score+1			//check if 2nd digit of score is lower
	lda HiScoreTarget	//Read 1st digit of hiscore
	sbc Score			//check if 1st digit of score is lower
	bpl notrank			//score is too low. skip current rank
	jsr NameEntry		//ELSE call subroutine for name entry
	jmp rank			//then set the new rank for the player
	
}

	//Setup the called macro to produce the high score check
	//subroutine.
CheckForFirstPlace:	
	CheckHiScore(hiscore1,NotFirstPlace,MakeFirstPlace)
NotFirstPlace:
	CheckHiScore(hiscore2,NotSecondPlace,MakeSecondPlace)
NotSecondPlace:
	CheckHiScore(hiscore3,NotThirdPlace,MakeThirdPlace)
NotThirdPlace:
	CheckHiScore(hiscore4,NotForthPlace,MakeForthPlace)
NotForthPlace:
	CheckHiScore(hiscore5,NotFithPlace,MakeFithPlace)
NotFithPlace:
	jmp Restart //No hi score achieved, so jump to the title screen
	
	//Set the player's position to first place, by moving
	//all high scores to the correct position. Same for
	//names as well.
	
MakeFirstPlace:	
	ldx #$00
movescore1:
	lda hiscore4,x	
	sta hiscore5,x
	lda hiscore3,x
	sta hiscore4,x
	lda hiscore2,x
	sta hiscore3,x
	lda hiscore1,x
	sta hiscore2,x
	lda Score,x
	sta hiscore1,x
	inx
	cpx #6
	bne movescore1
	ldx #$00
movenames1:	
	lda name4,x
	sta name5,x
	lda name3,x
	sta name4,x
	lda name2,x
	sta name3,x
	lda name1,x
	sta name2,x
	lda Name,x
	sta name1,x
	inx
	cpx #9
	bne movenames1
	jmp SaveHiScore
	
//Set player's score to second place

MakeSecondPlace:
	ldx #$00
movescores2:
	lda hiscore4,x
	sta hiscore5,x
	lda hiscore3,x
	sta hiscore4,x
	lda hiscore2,x
	sta hiscore3,x
	lda Score,x
	sta hiscore2,x
	inx
	cpx #6
	bne movescores2
	ldx #$00
movenames2:
	lda name4,x
	sta name5,x
	lda name3,x
	sta name4,x
	lda name2,x
	sta name3,x
	lda Name,x
	sta name2,x
	inx
	cpx #9
	bne movenames2
	
	jmp SaveHiScore
	
	//Place player to third place
	
MakeThirdPlace:
	ldx #$00
movescores3:
	lda hiscore4,x
	sta hiscore5,x
	lda hiscore3,x
	sta hiscore4,x
	lda Score,x
	sta hiscore3,x
	inx
	cpx #6
	bne movescores3
	ldx #$00
movenames3:
	lda name4,x
	sta name5,x
	lda name3,x
	sta name4,x
	lda Name,x
	sta name3,x
	inx
	cpx #9
	bne movenames3
	
	jmp SaveHiScore
	
	//Place player to forth place
	
MakeForthPlace:
	ldx #$00
movescores4:
	lda hiscore4,x
	sta hiscore5,x
	lda Score,x
	sta hiscore4,x
	inx
	cpx #6
	bne movescores4
	ldx #$00
movenames4:
	lda name4,x
	sta name5,x
	lda Name,x
	sta name4,x
	inx
	cpx #9
	bne movenames4
	
	jmp SaveHiScore
	//Finally place player to last place
	
MakeFithPlace:
	
	ldx #$00
movescores5:
	lda Score,x
	sta hiscore5,x
	inx
	cpx #6
	bne movescores5
	ldx #$00
movenames5:
	lda Name,x
	sta name5,x
	inx
	cpx #9
	bne movenames5
	
	jmp SaveHiScore
	
	
//Hi score table name entry subroutine. Display	
//a well done text, initialise the position of	
//the keyboard to be used - KERNAL should be	
//enabled, since JSR $FFE4 is to be used for	
//this game.	

NameEntry:	
	lda #0
	sta ColourPointer
	lda #$08	//Disable multicolour mode.
	sta $d016
		
	ldx #$00
PlaceHiAchievedMSG:
	lda HallOfFameText,x
	sta SCRN+400,x
	lda #$00
	sta COLR+400,x
	inx
	cpx #HallOfFameTextEnd-HallOfFameText
	bne PlaceHiAchievedMSG
	
	//Create a 9 digit dotted line for the name
	//to go above
	ldx #$00
DrawDottedLine:
	lda #$2e
	sta SCRN+735,x
	lda #$0e
	sta COLR+735,x
	lda #$20
	sta Name,x //Blank char for name
	inx
	cpx #9
	bne DrawDottedLine
	
	lda #0
	sta FireButton
	lda #<Name
	sta CharPos+1
	lda #>Name
	sta CharPos+2
	lda #0
	sta JoyDelay
	
//Joystick control name entry routine
	lda #$01	//Character A starts in
	sta Char
	
JoyReadLoop:
	
	jsr SyncMain	//Synchronize timer
	jsr ColourWash
	jsr DrawName	//Draw player's name
	inc JoyDelay
	lda JoyDelay
	cmp #$07
	beq JoystickIsOk
	inc JoyDelay //Delay a little
	jmp JoyReadLoop
	
JoystickIsOk:
	lda #0
	sta JoyDelay	//Reset delay
	lda $dc00
	lsr	//Check UP
	bcs CheckJDown
	jmp MoveCharUp
CheckJDown:
	lsr //CheckJDown
	bcs CheckJFire
	jmp MoveCharDown
CheckJFire:
	lda $dc00
	lsr
	lsr
	lsr
	lsr
	lsr
	bit FireButton
	ror FireButton
	bmi JoyReadLoop
	bvc JoyReadLoop
	jmp CheckCharacter
	
MoveCharUp:
	inc Char
	lda Char
	cmp #$21 //Char after spacebar not allowed
	beq CHAROVER
	jmp JoyReadLoop
CHAROVER:
	lda #$01
	sta Char
	jmp JoyReadLoop
	
MoveCharDown:
	dec Char
	lda Char
	cmp #$00 //Char @ not allowed. Move to top
	beq CHARUNDER
	jmp JoyReadLoop
CHARUNDER:
	lda #$20	//Char spacebar next
	sta Char
	jmp JoyReadLoop
	
//The fire button has been pressed - Check whether or not
//the UP arrow or LEFT arrow key has been detected. UP ARROW = DEL
//LEFT ARROW = END.

CheckCharacter:
	lda #0
	sta FireButton
	
	lda Char
	cmp #$1e //Up arrow was found
	beq DeleteChar //Delete one character
	cmp #$1f //Back arrow was found
	beq FinishedChar //End the name entry subroutine
	inc CharPos+1
	lda CharPos+1
	cmp #<Name+9
	beq Expired
	jmp JoyReadLoop
Expired:
	jsr DrawName
	rts
	
//Delect character detected. Make current char space
//then delete the row once:

DeleteChar:
	lda CharPos+1
	cmp #<Name
	beq PositionDisallowed
	lda #$20
	sta Char
	jsr DrawName
	dec CharPos+1
	lda CharPos+1
	cmp #<Name-1
	beq PositionDisallowed
	lda #$1e
	sta Char
	jmp JoyReadLoop
PositionDisallowed:	
	lda #<Name	
	sta CharPos+1	
	jmp JoyReadLoop
	
FinishedChar:
	jsr DrawName
	ldx #$00
DeleteUnwantedChars:
	lda Name,x
	cmp #$1f
	beq EndSpotted
Next:
	lda Name,x
	inx
	cpx #$09
	bne DeleteUnwantedChars
	rts
EndSpotted:
	lda #$20
	sta Name,x
	jmp Next
		
//Draw player's name to screen	

DrawName:	
	
	ldx #$00	
DrawNameLoop:	
	lda Char	
CharPos:				
	sta Name			//Allocate self-mod char position
	lda Name,x			//for reading the name.
	sta SCRN+735-40,x	
	lda #$0d	
	sta COLR+735-40,x
	inx
	cpx #$09
	bne DrawNameLoop
	rts
	
//Colour wash subroutine. This part of the code sets	
//a delay for the text colour wash feature. Once the	
//delay has reached its target, the main washing	
//can take place.	
	
ColourWash:	
	lda ColourDelay		//Delay of colour rolling	
	cmp #2				//Value of delay before expiring
	beq WashDelayOK		//Wash delay has expired
	inc ColourDelay		//Else move 1 step forward with the delay
	rts	
WashDelayOK:	
	jsr PaintColourTable //Paint the colour table chars.	
	lda #0				 //Reset colour delay
	sta ColourDelay	
	ldx ColourPointer	 //Read the colour pointer in a loop
	lda ColourTable,x	 //in order to grab each byte from the
	sta COLR+400+39		 //colour table and paste it into 
	sta COLR+440		 //screen colour position.
	sta COLR+480+39	
	sta COLR+520	
	sta COLR+560+39	
	sta COLR+600	
	sta COLR+640+39	
	sta COLR+680
	sta COLR+720+39
	sta COLR+760
	inx	
	cpx #ColourTableEnd-ColourTable	//Have we reached the end of the table?
	beq WashReset					//Surely we have	
	inc ColourPointer				//We haven't move to the next byte of the colour table
	rts								//exit loop.	
WashReset: 
	ldx #$00						//Reset the colour pointer		
	stx ColourPointer
	rts
	
//This is where the colours are being rolled across	
//from one character to according to the direction	
//they should move.	
	
PaintColourTable:
	ldx #$00
DoPaintBackwards:
	lda COLR+401,x	//Grab one colour char, and then
	sta COLR+400,x	//pull back. So that the colours are
	lda COLR+481,x	//rolling from the very last character
	sta COLR+480,x	//all the way to the first character
	lda COLR+561,x	//while the last character is flashing.
	sta COLR+560,x
	lda COLR+641,x
	sta COLR+640,x
	lda COLR+721,x
	sta COLR+720,x
	inx
	cpx #40
	bne DoPaintBackwards
	
	ldx #$27
DoPaintForwards:
	lda COLR+439,x	//Grab one colour char, and then
	sta COLR+440,x	//push the colour of the characters
	lda COLR+519,x	//forward, starting from the very 
	sta COLR+520,x	//first character minus one, all
	lda COLR+599,x	//the way to the very last character
	sta COLR+600,x	//on the very right of the screen.
	lda COLR+679,x
	sta COLR+680,x
	lda COLR+759,x
	sta COLR+760,x
	dex
	bne DoPaintForwards
	rts
	

Char: .byte 0	  //Character to be read for name entry	
JoyDelay: .byte 0 //Delay for cycling characters 	
				  //while entering the name on the hi 	
				  //score table	
		
CheatCode:	
	.byte $14,$01,$12,$01,$0e,$14,$15,$0c,$01	
//=====================================================	
//The title screen logo	
//---------------------	
//The logo was made in facePainter and was saved in
//Koalapaint format. The bitmap is stripped into 3
//different files (You can use GangEnd to strip the
//picture should you wish to)
//
//They are VIDEO MEMORY, COLOUR MEMORY and BITMAP
//=====================================================
	
	
//Import title screen logo screen RAM data

*=$c400	"TITLE LOGO - VIDEO RAM DATA"
VidRAMData:
	.import c64 "bin/logo_vidmem.prg"

//Import title screen logo colour RAM data
*=$c800 "TITLE LOGO - COLOUR RAM DATA"
ColRAMData:
	.import c64 "bin/logo_colram.prg"
	
//Import title screen logo bitmap data
*=$e000	"TITLE LOGO - BITMAP"
Bitmap:	
	.import c64 "bin/logo_bitmap.prg"	
	