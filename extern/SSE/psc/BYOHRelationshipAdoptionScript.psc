Scriptname BYOHRelationshipAdoptionScript extends Quest Conditional
{Quest Script for RelationshipAdoption, responsible for handling all of the post-Adoption behaviors.}

;----------------------------------------------------------------------------------------------------
;STANDARD VARIABLES
;----------------------------------------------------------------------------------------------------
;RelationshipAdoption makes extensive use of Actor Values Variable06-07 for package conditionals and state-tracking. Values are:
;
;******************************
; Variable06 - Child State: Primarily used for package conditionals to override the child's standard schedule.
;   - -2 - Just Adopted, block packages until the child moves to their new home.
;   - -1 - Just Adopted, travel to home, then trigger the scheduler.
;   -  0 -  Normal
;   -  1 - Override Behaviors: Small overrides to the child's schedule that don't trump real Orders. Used to make the child feel more responsive.
;      - 1.0 - Child was recently adopted and sandboxes in their new house for an hour.
;	   - 1.1 - Child was given a weapon and spars with it for an hour.
;	   - 1.2 - Child was given a doll and plays with it for an hour.
;      - 1.3 - Child runs off crying and sandboxes in their room for an hour.
;   -  2 - Override Orders: Child ordered outside (Inside, 6am-9pm)
;   -  3 - Override Orders: Child ordered inside (Outside, any time)
;   -  4 - Override Orders: Child ordered to do chores (Anywhere, 6am-9pm)
;   -  5 - Override Orders: Child ordered to bed (Anywhere, 6pm-8am)
;   - 10 - Override Behavior: Child will "Never Speak to You Again" due to a hostile action. This lasts for 24h.
;******************************
; Variable07 - Forcegreet State: Determines which forcegreet to run when the player 'comes home' after an absence.
;   -  0 - [NOT INITIALIZED]
;   -  1 - DLG: Thanks for Adopting	  - First FG after Adoption, Dirty Move.
;   -  2 - DLG: Thanks and Chest	  - First FG after Adoption, Clean Move.
;   -  3 - DLG: Check the Chest       - First Clean Move after Adoption.
;   -  4 - Adopt a Pet				  - Player has an Animal Companion, child asks to keep it.
;   -  5 - Adopt a Critter			  - Child brings home a critter. 10% chance (plus other conditions).
;   -  6 - Name Calling 1             - Scene: Children call each other names, then play tag. 10% chance (plus other conditions).
;   -  7 - Name Calling 2             - Scene: Children call each other names, one runs off crying. 10% chance (plus other conditions).
;   -    -
;   - 10 - Ask for Allowance          - Generic, 25% Chance
;   - 11 - Give Gift to Player        - Generic, 25% Chance
;   - 12 - Ask for a Present		  - Generic, 25% Chance
;   - 13 - Suggest a Game			  - Generic, 25% Chance
;
;******************************
;----------------------------------------------------------------------------------------------------
;PROPERTIES & VARIABLES
;-------------------------------------

;Total number of children adopted.
int property numChildrenAdopted = 0 Auto Conditional Hidden

;Aliases for the family.
ReferenceAlias property Player Auto
ReferenceAlias property Spouse Auto
ReferenceAlias property Child1 Auto
ReferenceAlias property Child2 Auto

;Aliases on this quest representing the family's location. Copied from the Scheduler subquest, used by dialogue.
LocationAlias property CurrentHomeHouse Auto
LocationAlias property CurrentHomeExterior Auto

;Aliases on the Scheduler subquest representing the family's location. Used to copy them into this quest.
LocationAlias property SchedulerCurrentHomeHouse Auto
LocationAlias property SchedulerCurrentHomeExterior Auto

;Aliases on the Scheduler subquest representing scene markers.
ReferenceAlias property SchedulerSceneMarker1 Auto
ReferenceAlias property SchedulerSceneMarker2 Auto

;Associated quests.
Quest property BYOHRelationshipAdoptionScheduler Auto				;Scheduler subquest. Handles packaging for the kids and pets in their 'current' house.
Quest property RelationshipMarriageFIN Auto						;Player Marriage Quest. Handles the spouse.
Quest property BYOHRelationshipAdoptable Auto						;Pre-Adoption functions, behaviors, and house tracking.
Quest property BYOHRelationshipAdoptableOrphanage Auto			;Pre-Adoption Orphanage Manager.
Quest property BYOHRelationshipAdoptableUrchins Auto				;Pre-Adoption Urchin Manager.
Quest property BYOHRelationshipAdoptionCWSiegeHandler Auto		;Handles Civil War Siege override behavior.
Quest property BYOHRelationshipAdoptionNewAdoptionHandler Auto	;Handles New Adoption override behavior.
Quest property WIGamesTag Auto										;Game: Tag
Quest property WIGamesHideAndSeek Auto								;Game: Hide and Seek
Quest property WIKill05 Auto											;WI: Related actor has been killed. We need for force-shutdown this in some cases.
CCHouseQuestScript property CCHouse Auto						; CC House Quest

;Associated factions
Faction property BYOHRelationshipAdoptableFaction Auto
Faction property CurrentFollowerFaction Auto

;House Data
int property currentHome = 0 Auto Conditional Hidden	;Int representing which house the child is currently living in.
int property newHome = 0 Auto Conditional Hidden		;Int representing which house the child has been told to move to.
ObjectReference property HouseSolitudeMarker Auto		;Markers representing the center point of each house.
ObjectReference property HouseWindhelmMarker Auto
ObjectReference property HouseMarkarthMarker Auto
ObjectReference property HouseRiftenMarker Auto
ObjectReference property HouseWhiterunMarker Auto
ObjectReference property HouseFalkreathMarker Auto
ObjectReference property HouseHjaalmarchMarker Auto
ObjectReference property HousePaleMarker Auto
Location property SolitudeLocation Auto					;Locations corresponding to each home city/exterior.
Location property WindhelmLocation Auto
Location property MarkarthLocation Auto
Location property RiftenLocation Auto
Location property WhiterunLocation Auto
Location property FalkreathHouseLocation Auto
Location property HjaalmarchHouseLocation Auto
Location property PaleHouseLocation Auto
Location property SolitudeProudspireManorLocation Auto	;Locations corresponding to each home interior.
Location property WindhelmHjerimLocation Auto
Location property MarkarthVlindrelHallLocation Auto
Location property RiftenHoneysideLocation Auto
Location property WhiterunBreezehomeLocation Auto
Location property FalkreathHouseInteriorLocation Auto
Location property HjaalmarchHouseInteriorLocation Auto
Location property PaleHouseInteriorLocation Auto

;Newly-Adopted Child Handling
ReferenceAlias property NewAdoptionHandlerChild1 Auto				;Family Aliases on the NAH quest.
ReferenceAlias property NewAdoptionHandlerChild2 Auto
ReferenceAlias property NewAdoptionHandlerSpouse Auto
Keyword property BYOHAdoptionNewAdoptionEvent Auto				;Keyword for NAH Story Manager Event
bool child1NewlyAdopted = False										;Keep track of whether the children have been recently adopted.
bool child2NewlyAdopted = False										;   the system handles moving them a little differently to avoid delays, which felt strange.
float property lastChildAdoptedTimestamp Auto Conditional Hidden	;Timestamp of when we last adopted a child + 3 days. Used to conditionalize some dialogue.

;Moving System Properties
bool moveQueued = False														;Has the player (or a system) requested a move?
bool initialMoveDone = False													;Has the family moved before?
bool property FirstMoveWithSpouse = True Auto Hidden						;Is this the first time you're moving with your spouse?
bool property AllowSpouseToMove = True Auto Hidden							;Should RelationshipMarriageFIN's quest script allow the spouse to actually move?
bool property MovingTogglePackageOn = False Auto Conditional Hidden		;Should the children's temporary toggle package be turned on?
ObjectReference newHomeRef													;Where are we moving to?
bool schedulerHasFailed = False												;Did the Scheduler fail the last time we tried to move the children?

;CW Siege handler Properties
Quest property CWSiege Auto					;CW Siege quest.
LocationAlias property CWSiegeCity Auto		;Location under siege.

;Orders System Properties
float property OrderToConfirm = 0.0 Auto Hidden		;Holding variable for an order the player has requested, but not yet confirmed.

;Gift system Properties
Formlist property BYOHRelationshipAdoptionPlayerGiftChildMale Auto		;List of gifts that can be given to children.
Formlist property BYOHRelationshipAdoptionPlayerGiftChildFemale Auto
int property GiftReaction Auto Hidden Conditional			;Child's "emotional" reaction to the gift, based on its gold value.
int property GiftStoredValueChild1 Auto Hidden				;Value of 'consumed' gifts to be added to Child1's gold.
int property GiftStoredValueChild2 Auto Hidden				;Value of 'consumed' gifts to be added to Child2's gold.

;Games System Properties
Keyword property WIGamesTagStart Auto			;Keywords for WIGames Story Manager Events
Keyword property WIGamesHideAndSeekStart Auto
Faction property WINeverFillAliasesFaction Auto	;Adding children to this faction prohibits them from being added to WIChangeLocation games.

;Pet System Properties
ReferenceAlias property AnimalCompanion Auto										;The player's current Animal Companion, as tracked by the DialogueFollower quest.
ReferenceAlias property FamilyPet Auto												;The family's Pet: A designated Animal Companion who lives in the player's house.
ReferenceAlias property Scheduler_FamilyPet Auto									;Scheduler Quest copy of the pet.
ReferenceAlias property TransientPet Auto												;A temporary alias to hold pets that haven't been 'approved' for adoption yet. Needed for dialogue conditions.
Faction property BYOHRelationshipPotentialPetFaction Auto						;Faction, applied via the TransientPet alias, whose rank represents the pet's suitability for adoption.
Formlist property BYOHRelationshipAdoption_PetAllowedRacesList Auto				;Races of pets that any kid can adopt.
Formlist property BYOHRelationshipAdoption_PetDogsList Auto						;Races of pets that are also dogs.
float petRetryTime																	;Limits frequency of Pet-related Forcegreet events. If (GetCurrentGameTime < petRetryTime), they won't occur.

;Critter System Properties
ReferenceAlias property FamilyCritter Auto				;The family's Critter: A creature created for one of the children.
ReferenceAlias property Scheduler_FamilyCritter Auto		;Scheduler Quest copy of the critter.
ReferenceAlias property TransientCritter Auto				;A temporary alias to hold critters that haven't been 'approved' for adoption yet. Needed for dialogue conditions.
ReferenceAlias property Scheduler_TransientCritter Auto	;Scheduler Quest copy of the transient critter.
FormList property BYOHRelationshipAdoption_CrittersMale Auto		;The list of critters boys will bring home.
FormList property BYOHRelationshipAdoption_CrittersFemale Auto	;The list of critters girls will bring home.
float critterRetryTime									;Limits frequency of Critter Forcegreet events. If (GetCurrentGameTime < critterRetryTime), they won't occur.
Actor rejectedCritter										;The last critter the children brought home, if it has been rejected by the player.
ActorBase rejectedCritterBase							;Base Object for the last critter the children brought home, if it has been rejected by the player.
int critterChance = 10									;Base chance that a critter event will occur.


;'Welcome Home' Forcegreet System Properties
int property WelcomeHomeDelay = 1 Auto Hidden							;The minimum time that must elapse before a Welcome Home Forcegreet triggers (1 day).
bool property ForcegreetEventReady = False Auto Hidden Conditional	;Is a Welcome Home Event queued up?
float property playerLastSeen = 0.0 Auto Hidden Conditional			;Timestamp of when we last 'saw' the player.
bool ChildChestIntroduced = False										;Has one of the children mentioned the chest (enqueued Event 2 or 3)?
MiscObject property Gold001 Auto
int property WeArePoorCount = 0 Auto Hidden Conditional				;Tracking for 'poverty' counter.
FormList property BYOHRelationshipAdoptionGifts_Poor Auto			;Formlists of gifts the child can give.
FormList property BYOHRelationshipAdoptionGifts_Junk Auto
FormList property BYOHRelationshipAdoptionGifts_0000 Auto
FormList property BYOHRelationshipAdoptionGifts_0025 Auto
FormList property BYOHRelationshipAdoptionGifts_0050 Auto
FormList property BYOHRelationshipAdoptionGifts_0100 Auto
FormList property BYOHRelationshipAdoptionGifts_0250 Auto
FormList property BYOHRelationshipAdoptionGifts_0500 Auto
FormList property BYOHRelationshipAdoptionGifts_1000 Auto

;'Welcome Home' name-calling scenes and properties.
Scene property RelationshipAdoption_SceneNameCalling01 Auto				;Name-Calling Scene 1. Ends with the children playing tag.
Scene property RelationshipAdoption_SceneNameCalling02 Auto				;Name-Calling Scene 2. Ends with one child sulking off to their room.
Formlist property BYOHRelationshipAdoption_NameCallingWinnersList Auto		;A prioritized list of children who always 'win' the name-calling forcegreets.
bool NameCalling1Done															;Has Name Calling Scene 1 triggered before? If so, we won't use it again.
bool NameCalling2Done															;Has Name Calling Scene 2 triggered before? If so, we won't use it again.
bool NameCallingUsedLast														;Was the last forcegreet event a Name-Calling Scene? If so, don't do another one.
int nameCallingChance = 10													;Base chance that a name-calling event will occur.



;----------------------------------------------------------------------------------------------------
;NEW ADOPTIONS
;-------------

;Pass this function a child to adopt them.
Function AdoptChild(Actor childToAdopt)
; 	;Debug.Trace("Now Adopting: " + childToAdopt)
	
	;Block activation on the child until we get everything sorted out.
	childToAdopt.BlockActivation(True)
	
	;Remove the child from all prior factions, then reinstate their Crime Faction.
	Faction oldCrimeFaction = childToAdopt.GetCrimeFaction()
	childToAdopt.RemoveFromAllFactions()
	childToAdopt.SetCrimeFaction(oldCrimeFaction)
	
	;Remove the child from interfering World Interactions. This has to be brute-forced in many cases since
	;most of these have a huge number of aliases and no clean way to abort if and only if this specific actor is in them.
	;Stop any running games.
	StopGames()
	;Stop WIKill05 (Run Home & Mourn)
	if (WIKill05.IsRunning())
		WIKill05.Stop()
	EndIf
	
	;Determine where we're moving the child to. The Adoptable system passes this to us by storing it in the child's Variable07.
	int childDestination = ValidateMoveDestination(childToAdopt.GetAV("Variable07") as int)
	Location childDestinationLoc = TranslateHouseIntToLoc(childDestination)
	Location childCurrentLoc = childToAdopt.GetCurrentLocation()
; 	;Debug.Trace("Child to move to: " + childDestination + " " + childDestinationLoc)
	
	;Which child is this?
	ReferenceAlias ChildAlias
	if (Child1.GetReference() == None)
		ChildAlias = Child1
		child1NewlyAdopted = True
		numChildrenAdopted = 1
	ElseIf (Child2.GetReference() == None)
		ChildAlias = Child2
		child2NewlyAdopted = True
		numChildrenAdopted = 2
		;Stop RelationshipAdoptable from adopting anyone else.
		BYOHRelationshipAdoptable.SetStage(255)
	Else
; 		Debug.Trace("BYOHRelationshipAdoptionScript ERROR: REFERENCES ALREADY FILLED")
	EndIf
	
	;Ping the Adoptable-Orphanage quest so it updates if you've adopted one of its children.
	if (BYOHRelationshipAdoptableOrphanage.IsRunning())
		(BYOHRelationshipAdoptableOrphanage as BYOHRelationshipAdoptableOrphanageSc).CheckAdoptOrphanageChild(childToAdopt, numChildrenAdopted)
	EndIf
	
	;Ping the Adoptable-Urchin quest so it updates if you've adopted one of its children. 
	if (BYOHRelationshipAdoptableUrchins.IsRunning())
		(BYOHRelationshipAdoptableUrchins as BYOHRelationshipAdoptableUrchinScript).CheckAdoptUrchinChild(childToAdopt, numChildrenAdopted)
	EndIf

	;Add the child to this quest.
	ChildAlias.ForceRefTo(childToAdopt)
	
	;Clear out the child's Actor Variables.
	childToAdopt.SetActorValue("Variable06", 0)
	childToAdopt.SetActorValue("Variable07", 0)
	childToAdopt.SetActorValue("Variable08", 0)
	
	;Reset RelationshipRank to 0 to dodge some inappropriate dialogue.
	childToAdopt.SetRelationshipRank(Game.GetPlayer(), 0)
	
	;Decide what to do with the newly-adopted child.
	;The child's packages expect Variable06 to be -1 for the child to run home (same city), or -2 for the child to wait around (different city).
; 	;Debug.Trace("Running Behavior Setup")
; 	;Debug.Trace("Behavior setup: " + childDestination + " " + childDestinationLoc == childCurrentLoc + " " + childDestinationLoc.IsChild(childCurrentLoc))
	if (childDestination <= 5 && \
		(childDestinationLoc == childCurrentLoc || childDestinationLoc.IsChild(childCurrentLoc)))
		childToAdopt.SetActorValue("Variable06", -1)
	Else
		childToAdopt.SetActorValue("Variable06", -2)
	EndIf
	
	;Start the NewAdoptionHandler quest to get the child to execute that behavior.
; 	;Debug.Trace("STARTING: " + BYOHRelationshipAdoptionNewAdoptionHandler.IsRunning() + " " + BYOHAdoptionNewAdoptionEvent + " " + childToAdopt)
	if (!BYOHRelationshipAdoptionNewAdoptionHandler.IsRunning())
		BYOHAdoptionNewAdoptionEvent.SendStoryEvent(None, childToAdopt)
		float time = Utility.GetCurrentGameTime()
		While (!BYOHRelationshipAdoptionNewAdoptionHandler.IsRunning())
			if (Utility.GetCurrentGameTime() - time > 3)
; 				Debug.Trace("Warning: Taking longer than 3s to start the New Adoption Event Handler.")
			EndIf
			Utility.Wait(0.1)
		EndWhile
	Else
		;Add the child to the existing NewAdoptionHandler.
		NewAdoptionHandlerChild2.ForceRefTo(childToAdopt)
; 		;Debug.Trace("Two children adopted in quick succession. Adjusting Child 1 to match Child 2's destination.")
		Child1.GetActorRef().SetActorValue("Variable06", childToAdopt.GetActorValue("Variable06"))
	EndIf
	
	;Push existing spouse and children into the NAH's aliases so it can check their status.
	If (Child1.GetActorRef() != NewAdoptionHandlerChild1.GetActorRef() && Child1.GetActorRef() != NewAdoptionHandlerChild2.GetActorRef())
		NewAdoptionHandlerChild2.ForceRefTo(Child1.GetActorRef())
	EndIf
	If (Spouse.GetActorRef() != None)
		NewAdoptionHandlerSpouse.ForceRefTo(Spouse.GetActorRef())
	EndIf
	
	;Queue up a clean move to happen when the player leaves the area.	
	QueueMoveFamily(childDestination, True)
	
	;Adjust the adoption timestamp, setting it to the current time plus 3d.
	lastChildAdoptedTimestamp = Utility.GetCurrentGameTime() + 3
	
	;Update the child's faction rank in the Adoptable quest to make sure the correct dialogue is active.
	;This has to be run on both children, because adopting Child2 sometimes kicks Child1 from the rank.
	Child1.GetActorRef().SetFactionRank(BYOHRelationshipAdoptableFaction, 25)
	if (Child2.GetActorRef() != None)
		Child2.GetActorRef().SetFactionRank(BYOHRelationshipAdoptableFaction, 25)
	EndIf
	
	;EVP the child to see what they should do next.
	childToAdopt.EvaluatePackage()
	
	;Initialize the 'last seen' timestamp.
	playerLastSeen = Utility.GetCurrentGameTime()
	
	;Release activation block.
	childToAdopt.BlockActivation(False)
	
	;Award 'Proud Parent' Achievement for adopting a child.
	Game.AddAchievement(61)
	
	;Whew! :)
; 	Debug.Trace("Adoption successful.")
EndFunction



;----------------------------------------------------------------------------------------------------
;MOVING SYSTEM
;-------------

;Queue up the family to move at the next opportunity.
Function QueueMoveFamily(int destination, bool forceQueue = False)
	newHome = ValidateMoveDestination(destination)
	newHomeRef = TranslateHouseIntToObj(newHome)
	if (Spouse.GetActorRef() != None)
		FirstMoveWithSpouse = False	
	EndIf
	if (currentHome == newHome && !forceQueue)
		moveQueued = False
; 		;Debug.Trace("De-queuing move.")
	Else
		moveQueued = True
; 		;Debug.Trace("Queuing Move to: " + destination)
	EndIf
EndFunction


;Move the family to the queued location.
Function MoveFamily()
; 	Debug.Trace("Moving Family.")
	
	;Switch to a temporary package to prevent the Scheduler's packages from continuing to use obsolete data.
	MovingTogglePackageOn = True
	
	;Turn off the NewAdoptionHandler if it's running. MoveFamily can take it from here.
	if (BYOHRelationshipAdoptionNewAdoptionHandler.IsRunning())
; 		;Debug.Trace("New Adoption Handler was running. SHUT IT DOWN.")
		BYOHRelationshipAdoptionNewAdoptionHandler.Stop()
	EndIf
	
	;Stop any running critter events.
	QuashCritterEvents()

	;Move everyone to the new house.
	if (Spouse.GetActorRef() != None && !Spouse.GetActorRef().IsDead())
		if (!(Spouse.GetActorRef().IsInFaction(CurrentFollowerFaction) || Spouse.GetActorRef().GetCurrentLocation() == Game.GetPlayer().GetCurrentLocation()))
; 			;Debug.Trace("Moving spouse.")
			Spouse.GetActorRef().MoveTo(newHomeRef)
		EndIf
		Spouse.GetActorRef().EvaluatePackage()
		;Permit RelationshipMarriageFIN to switch the Spouse's packages now.
		AllowSpouseToMove = True
		(RelationshipMarriageFIN as RelationshipMarriageSpouseHouseScript).MoveSpouseAdoption(Spouse.GetActorRef(), newHome)
		AllowSpouseToMove = False
	EndIf
	if (Child1.GetActorRef().GetCurrentLocation() != Game.GetPlayer().GetCurrentLocation())
		Child1.GetActorRef().MoveTo(newHomeRef)
		Child1.GetActorRef().EvaluatePackage()
	EndIf
	if (Child2.GetActorRef() != None && Child2.GetActorRef().GetCurrentLocation() != Game.GetPlayer().GetCurrentLocation())
		Child2.GetActorRef().MoveTo(newHomeRef)
		Child2.GetActorRef().EvaluatePackage()
	EndIf
	if (FamilyPet.GetActorRef() != None && !FamilyPet.GetActorRef().IsInFaction(CurrentFollowerFaction) && !FamilyPet.GetActorRef().Is3DLoaded())
		FamilyPet.GetActorRef().MoveTo(newHomeRef)
		FamilyPet.GetActorRef().EvaluatePackage()
	EndIf
	if (FamilyCritter.GetActorRef() != None)
		FamilyCritter.GetActorRef().MoveTo(newHomeRef)
		FamilyCritter.GetActorRef().EvaluatePackage()
	EndIf
	
	;Shut down the Scheduler Quest, if it was running.
	BYOHRelationshipAdoptionScheduler.Stop()
	While (BYOHRelationshipAdoptionScheduler.IsRunning())
		Utility.Wait(0.5)
	EndWhile
	
	;Move everyone again, in case delays in package evalaution or the Scheduler shutdown have caused them to leave the cell, which happens sometimes.
	if (Spouse.GetActorRef() != None && !Spouse.GetActorRef().IsDead())
		if (!(Spouse.GetActorRef().IsInFaction(CurrentFollowerFaction) || Spouse.GetActorRef().GetCurrentLocation() == Game.GetPlayer().GetCurrentLocation()))
			Spouse.GetActorRef().MoveTo(newHomeRef)
		EndIf
	EndIf
	if (Child1.GetActorRef().GetCurrentLocation() != Game.GetPlayer().GetCurrentLocation())
		Child1.GetActorRef().MoveTo(newHomeRef)
	EndIf
	if (Child2.GetActorRef() != None && Child2.GetActorRef().GetCurrentLocation() != Game.GetPlayer().GetCurrentLocation())
		Child2.GetActorRef().MoveTo(newHomeRef)
	EndIf
	if (FamilyPet.GetActorRef() != None && !FamilyPet.GetActorRef().IsInFaction(CurrentFollowerFaction) && !FamilyPet.GetActorRef().Is3DLoaded())
		FamilyPet.GetActorRef().MoveTo(newHomeRef)
		FamilyPet.GetActorRef().EvaluatePackage()
	EndIf
	if (FamilyCritter.GetActorRef() != None)
		FamilyCritter.GetActorRef().MoveTo(newHomeRef)
		FamilyCritter.GetActorRef().EvaluatePackage()
	EndIf
	
	;Restart the Scheduler Quest in the new home.
	int schedulerFailCount = 0
	BYOHRelationshipAdoptionScheduler.SetStage(0)
	While (!BYOHRelationshipAdoptionScheduler.IsRunning() && schedulerFailCount < 10)
; 		Debug.Trace("Adoption: Waiting for Scheduler to start...")
		schedulerFailCount = schedulerFailCount + 1
		Utility.Wait(0.5)
		;The most common cause of Scheduler failures is Child1 leaving the house AGAIN. Move them back and retry.
		Child1.GetActorRef().MoveTo(newHomeRef)
		BYOHRelationshipAdoptionScheduler.SetStage(0)
	EndWhile
	if (!BYOHRelationshipAdoptionScheduler.IsRunning() && schedulerFailCount >= 10)
		;Uh-oh... this means the markers in the selected house are in a bad state (something probably didn't get enabled).
; 		Debug.Trace("THE ADOPTION SCHEDULER HAS FAILED TO START.")
		if (!schedulerHasFailed)
			;Try to move the children back to their previous home. That's better than leaving them in their new home and broken.
			schedulerHasFailed = True
			QueueMoveFamily(currentHome, True)
			MoveFamily()
			Return
		Else		
			;If the scheduler failed on our last move attempt as well, queue up an emergency move to somewhere. Hopefully somewhere else.
; 			Debug.Trace("THE ADOPTION SCHEDULER HAS FAILED TO START AGAIN. QUEUING EMERGENCY MOVE.")
			QueueMoveFamily(-1, False)
			Return
		EndIf
	EndIf
	
	;Update our copy of the Scheduler's Aliases.
	CurrentHomeHouse.ForceLocationTo(SchedulerCurrentHomeHouse.GetLocation())
	CurrentHomeExterior.ForceLocationTo(SchedulerCurrentHomeExterior.GetLocation())
	
	;Clear Variable06 on the children to cancel any prior orders and the Just-Adopted override behaviors.
	Child1.GetActorRef().SetActorValue("Variable06", 0)
	if (Child2.GetActorRef() != None)
		Child2.GetActorRef().SetActorValue("Variable06", 0)
	EndIf
	
	;Return to regularly-scheduled packaged behavior.
	MovingTogglePackageOn = False
	
	;Reevaluate all packages relative to the new home.
	if (Spouse.GetActorRef() != None)
		Spouse.GetActorRef().EvaluatePackage()
	EndIf
	Child1.GetActorRef().EvaluatePackage()
	if (Child2.GetActorRef() != None)
		Child2.GetActorRef().EvaluatePackage()
	EndIf
	if (FamilyPet.GetActorRef() != None)
		FamilyPet.GetActorRef().EvaluatePackage()
	EndIf
	if (FamilyCritter.GetActorRef() != None)
		FamilyCritter.GetActorRef().EvaluatePackage()
	EndIf
	
	;After the first clean move, and only the first clean move, fill the Child's Chest so it starts with something.
	if (!initialMoveDone)
		initialMoveDone = True
; 		;Debug.Trace("Now Refilling.")
		(BYOHRelationshipAdoptionScheduler as BYOHRelationshipAdoptionSc).OnUpdateGameTime()
	EndIf
	
	;Move completed successfully. Fixup variables.
	moveQueued = False
	currentHome = newHome	
	schedulerHasFailed = False
	
	;If a newly-adopted child has just moved in with us, decide which Forcegreet event they should play.
	if (child1NewlyAdopted && child2NewlyAdopted)
; 		;Debug.Trace("Clean Move Readies Forcegreet, both children newly adopted.")
		;Queue up Forcegreet Event #2 (Child 1 Thanks w/ Chest Intro)
		ReadyForcegreetEvent()
		;Neither child is now Newly Adopted.
		child1NewlyAdopted = False
		child2NewlyAdopted = False
		;Both children now sandbox at home for an hour.
		OrderToConfirm = 1.0
		IssueOrderWithDuration(Child1.GetActorRef(), 1)
		IssueOrderWithDuration(Child2.GetActorRef(), 1)
	ElseIf (child1NewlyAdopted)
; 		;Debug.Trace("Clean Move Readies Forcegreet, Child 1 Only newly adopted.")
		;Queue up Forcegreet Event #2 (Child 1 Thanks w/ Chest Intro)
		ReadyForcegreetEvent()
		;Child 1 is no longer Newly Adopted.
		child1NewlyAdopted = False	
		;Child 1 now sandboxes at home for an hour.
		OrderToConfirm = 1.0
		IssueOrderWithDuration(Child1.GetActorRef(), 1)
	ElseIf (child2NewlyAdopted)
; 		;Debug.Trace("Clean Move Readies Forcegreet, Child 2 Only newly adopted.")
		;Queue up Forcegreet Event #1 (Child 2 Thanks) or 2 (Child 2 Thanks w/ Chest Intro)
		ReadyForcegreetEvent()
		;Child 2 is no longer Newly Adopted.
		child2NewlyAdopted = False	
		;Child 2 now sandboxes at home for an hour.
		OrderToConfirm = 1.0
		IssueOrderWithDuration(Child2.GetActorRef(), 1)
	EndIf
	
; 	Debug.Trace("Move completed.")
EndFunction


;A 'dirty move' occurs in one very specific but unfortunately all-too-common case: the player adopts a child,
;sends them to their home in the same city, races home ahead of them, and expects the child to behave
;normally once they arrive.
;
;Since we can't teleport the family while the player is watching, we instead perform a 'dirty' move,
;one with no teleports. The spouse and any other children will begin walking home (which this system
;otherwise tries to avoid). We leave the clean move queued so it can run as soon as possible.
Function DirtyMoveFamily()
; 	;Debug.Trace("Performing a Dirty Move.")
	
	;Switch to a temporary package to prevent the Scheduler's packages from continuing to use obsolete data.
	;Then force package evaluation to make sure everyone picks up the new package.
	MovingTogglePackageOn = True
	if (Spouse.GetActorRef() != None)
		Spouse.GetActorRef().EvaluatePackage()
	EndIf
	Child1.GetActorRef().EvaluatePackage()
	if (Child2.GetActorRef() != None)
		Child2.GetActorRef().EvaluatePackage()
	EndIf
	if (FamilyPet.GetActorRef() != None)
		FamilyPet.GetActorRef().EvaluatePackage()
	EndIf
	
	;Turn off the NewAdoptionHandler if it's running. DirtyMoveFamily can take it from here.
	if (BYOHRelationshipAdoptionNewAdoptionHandler.IsRunning())
; 		;Debug.Trace("New Adoption Handler was running. SHUT IT DOWN.")
		BYOHRelationshipAdoptionNewAdoptionHandler.Stop()
	EndIf

	;Shut down the old Scheduler Quest, if it was running.
	BYOHRelationshipAdoptionScheduler.Stop()
	While (BYOHRelationshipAdoptionScheduler.IsRunning())
		Utility.Wait(0.5)
	EndWhile
	
	;If the player just adopted their second child, but their first child isn't at home, we have a problem:
	;the Scheduler can't start unless Child1 is in the house, and by defintion, the player is there, so we
	;can't warp them in. So pull some sleight-of-hand...
	if (Child1.GetActorRef().GetCurrentLocation() != TranslateHouseIntToInteriorLoc(newHome))
		if (Child2.GetActorRef() == None)
			;This is an error case: Child 1 is not home, and there isn't a Child 2. Teleport Child 1 just to get this to work.
; 			;Debug.Trace("Dirty move warps children.")
			Child1.GetActorRef().MoveTo(newHomeRef)
		Else
; 			;Debug.Trace("Dirty Move swaps children.")
			SwapChildren()
		EndIf
	EndIf
	
	;Restart the Scheduler Quest in the new home.
	int schedulerFailCount = 0
	BYOHRelationshipAdoptionScheduler.SetStage(0)
	While (!BYOHRelationshipAdoptionScheduler.IsRunning() && schedulerFailCount < 10)
; 		Debug.Trace("Adoption: Waiting for Scheduler to start...")
		schedulerFailCount = schedulerFailCount + 1
		Utility.Wait(0.5)
		BYOHRelationshipAdoptionScheduler.SetStage(0)
	EndWhile
	if (!BYOHRelationshipAdoptionScheduler.IsRunning() && schedulerFailCount >= 10)
		;Uh-oh... this means the markers in the selected house are in a bad state (something probably didn't get enabled).
; 		Debug.Trace("THE ADOPTION SCHEDULER HAS FAILED TO START.")
		if (!schedulerHasFailed)
			;Try to move the children back to their previous home. That's better than leaving them in their new home and broken.
			schedulerHasFailed = True
			QueueMoveFamily(currentHome, True)
			MoveFamily()
			Return
		Else		
			;If the scheduler failed on our last move attempt as well, queue up an emergency move to somewhere. Hopefully somewhere else.
; 			Debug.Trace("THE ADOPTION SCHEDULER HAS FAILED TO START AGAIN. QUEUING EMERGENCY MOVE.")
			QueueMoveFamily(-1, False)
			Return
		EndIf
	EndIf
	
	;Permit RelationshipMarriageFIN to switch the Spouse's packages to those for the new home.
	if (Spouse.GetActorRef() != None && !Spouse.GetActorRef().IsDead())
		AllowSpouseToMove = True
		(RelationshipMarriageFIN as RelationshipMarriageSpouseHouseScript).MoveSpouseAdoption(Spouse.GetActorRef(), newHome)
		AllowSpouseToMove = False
	EndIf
	
	;Update our copy of the Scheduler's Aliases.
	CurrentHomeHouse.ForceLocationTo(SchedulerCurrentHomeHouse.GetLocation())
	CurrentHomeExterior.ForceLocationTo(SchedulerCurrentHomeExterior.GetLocation())
	
	;Clear Variable06 on the children to cancel any prior orders and the Just-Adopted override behaviors.
	Child1.GetActorRef().SetActorValue("Variable06", 0)
	if (Child2.GetActorRef() != None)
		Child2.GetActorRef().SetActorValue("Variable06", 0)
	EndIf
	
	;Return to regularly-scheduled packaged behavior.
	MovingTogglePackageOn = False
	
	;Reevaluate all packages relative to the new home.
	if (Spouse.GetActorRef() != None)
		Spouse.GetActorRef().EvaluatePackage()
	EndIf
	Child1.GetActorRef().EvaluatePackage()
	if (Child2.GetActorRef() != None)
		Child2.GetActorRef().EvaluatePackage()
	EndIf
	if (FamilyPet.GetActorRef() != None)
		FamilyPet.GetActorRef().EvaluatePackage()
	EndIf
	
	;Dirty move completed successfully. Fixup variables.
	currentHome = newHome	
	schedulerHasFailed = False
	
	;If a newly-adopted child has just moved in with us, decide which Forcegreet event they should play.
	if (child1NewlyAdopted && child2NewlyAdopted)
; 		;Debug.Trace("Dirty Move Readies Forcegreet, both children newly adopted.")
		;Queue up Forcegreet Event #1 (Child 1 Thanks)
		ReadyForcegreetEvent()
		;Neither child is now Newly Adopted.
		child1NewlyAdopted = False
		child2NewlyAdopted = False
		;Both children now sandbox at home for an hour.
		OrderToConfirm = 1.0
		IssueOrderWithDuration(Child1.GetActorRef(), 1)
		IssueOrderWithDuration(Child2.GetActorRef(), 1)
	ElseIf (child1NewlyAdopted)
; 		;Debug.Trace("Dirty Move Readies Forcegreet, Child 1 Only newly adopted.")
		;Queue up Forcegreet Event #1 (Child 1 Thanks)
		ReadyForcegreetEvent()
		;Child 1 is no longer Newly Adopted.
		child1NewlyAdopted = False
		;Child 1 now sandboxes at home for an hour.
		OrderToConfirm = 1.0
		IssueOrderWithDuration(Child1.GetActorRef(), 1)
	ElseIf (child2NewlyAdopted)
; 		;Debug.Trace("Dirty Move Readies Forcegreet, Child 2 Only newly adopted.")
		;Queue up Forcegreet Event #1 (Child 2 Thanks)
		ReadyForcegreetEvent()
		;Child 2 is no longer Newly Adopted.
		child2NewlyAdopted = False
		;Child 2 now sandboxes at home for an hour.
		OrderToConfirm = 1.0
		IssueOrderWithDuration(Child2.GetActorRef(), 1)
	EndIf
	
; 	Debug.Trace("Dirty Move completed.")
EndFunction


;----------------------------------------------------------------------------------------------------
;GAMES SYSTEM
;------------

;Starts one of the WI Games. 1 = Tag, 2 = HideAndSeek
Function StartGame(int gameToPlay, Actor childToPlay)
	;Cleanup any previous games to make sure the new one starts sucessfully.
	StopGames()
	;Unblock games for this child, if they were blocked.
	UnblockGames(childToPlay)
	;Run the game.
	if gameToPlay == 1
		WIGamesTagStart.SendStoryEvent(CurrentHomeExterior.GetLocation(), Game.GetPlayer(), childToPlay)
	else
		WIGamesHideAndSeekStart.SendStoryEvent(CurrentHomeExterior.GetLocation(), Game.GetPlayer(), childToPlay)
	EndIf
EndFunction

;Stop any active WI Games.
Function StopGames()
	WIGamesTag.Stop()
	WIGamesHideAndSeek.Stop()
EndFunction

;Block the child from being added to any new WE Games.
;Used to avoid problems when they're in a scene or forcegreet we don't want them to be pulled out of.
Function BlockGames(Actor child)
	child.AddToFaction(WINeverFillAliasesFaction)
EndFunction

;Release the block, allowing children to be added to new WE Games.
Function UnblockGames(Actor child)
	child.RemoveFromFaction(WINeverFillAliasesFaction)
EndFunction


;----------------------------------------------------------------------------------------------------
;ORDERS SYSTEM
;-------------

;Player issues an order to their child.
;All orders received from dialogue have an standard duration of 3h.
Function IssueOrder(ObjectReference child)
	IssueOrderWithDuration(child, 3)
EndFunction

;Player issues an order to their child.
Function IssueOrderWithDuration(ObjectReference child, float duration)
; 	;Debug.Trace("Received order " + OrderToConfirm + " for " + child)
	
	;Remove the child from any running games.
	StopGames()
	
	;Check whether this order is safe to execute.
	;Really only an issue for the gift-driven orders, since we don't want both kids using the same marker at the same time.
	if ((OrderToConfirm == 1.1 || OrderToConfirm == 1.2) && \
		 (Child1.GetActorRef().GetAV("Variable06") == OrderToConfirm || \
	     (Child2.GetActorRef() != None && Child2.GetActorRef().GetAV("Variable06") == OrderToConfirm)))
		;If not safe, just void it.
; 		;Debug.Trace("Voided unsafe order " + OrderToConfirm)
		OrderToConfirm = 0
	Else
		if (child == Child1.GetActorRef())
			(Child1 as BYOHRelationshipAdoptionChildScript).IssueOrder(OrderToConfirm, duration)
		ElseIf (child == Child2.GetActorRef())
			(Child2 as BYOHRelationshipAdoptionChildScript).IssueOrder(OrderToConfirm, duration)
		Else
; 			Debug.Trace("This object is not a child: " + child)
		EndIf
	EndIf
	
	;For the 'Never Speak to You Again' order, void any outstanding Forcegreet Events.
	if (OrderToConfirm == 10)
; 		;Debug.Trace("Order was: Never Speak to You Again, so voiding Forcegreets.")
		ForcegreetEventReady = False
		Child1.GetActorRef().SetAV("Variable07", 0)
		if (Child2 != None)
			Child2.GetActorRef().SetAV("Variable07", 0)
		EndIf
	EndIf	
EndFunction

;Issue the 'Never Speak to You Again' order, which causes both kids to flee to their rooms and lasts for 24h.
Function IssueOrderNeverSpeakToYouAgain(ObjectReference child)
	OrderToConfirm = 10
	IssueOrderWithDuration(child, 24)
EndFunction


;----------------------------------------------------------------------------------------------------
;GIFT-GIVING SYSTEM
;------------------

;Display the gift menu, process the results, and return control to dialogue when done.
Function GiveChildGift(Actor child)
	;Clear the variable that stores the child's immediate response.
	GiftReaction = 0

	;Determine which gift list to use based on the child's gender.
	Formlist giftList
	if (child.GetActorBase().GetSex() == 0)
		giftList = BYOHRelationshipAdoptionPlayerGiftChildMale
	Else
		giftList = BYOHRelationshipAdoptionPlayerGiftChildFemale
	EndIf
	
	;Alert the child to pay attention to items being given to them.
	if (child == Child1.GetActorRef())
		(Child1 as BYOHRelationshipAdoptionChildScript).SetGiftState(True)
	Else
		(Child2 as BYOHRelationshipAdoptionChildScript).SetGiftState(True)
	EndIf
	
; 	;Debug.Trace("Now Giving Gifts...")
	
	;Player gives gifts to the child.
	;As the player gives gifts, BYOHRelationshipAdoptionChildScript's OnItemAdded event will process them and call the secondary functions below.
	bool gaveAnyGift = child.ShowGiftMenu(True, giftlist)
	
	;Exit gift-giving state on the child.
	if (child == Child1.GetActorRef())
		(Child1 as BYOHRelationshipAdoptionChildScript).SetGiftState(False)
	Else
		(Child2 as BYOHRelationshipAdoptionChildScript).SetGiftState(False)
	EndIf
	
; 	;Debug.Trace("Gift giving completed. Response is: Any:" + gaveAnyGift + ", Value:" + GiftReaction)
EndFunction


;Called by BYOHRelationshipAdoptionChildScript to store off the gold and emotional value of the gifts.
Function UpdateGiftValues(Actor child, int EmotionalValue, int RealValue)
; 	;Debug.Trace("Updating Gift Value: " + child + " " + EmotionalValue + " " + RealValue)
	;Regardless of which child this is, GiftReaction increments.
	GiftReaction = GiftReaction + EmotionalValue

	;Then store the value of the gift to subsequently be added to the child's gold supply.
	if (child == Child1.GetActorRef())
		GiftStoredValueChild1 = GiftStoredValueChild1 + RealValue
	Else
		GiftStoredValueChild2 = GiftStoredValueChild2 + RealValue
	EndIf
EndFunction



;----------------------------------------------------------------------------------------------------
;CHILD CHEST SYSTEM
;------------------

;When requested by the Scheduler, refill the child's chest. This is handled like a give player gift event, using similar formlists.
Function RefillChest(ObjectReference childChest)
; 	;Debug.Trace("RefillChest refills chest.")
	;Generate 1-3 instances of 3-5 items from the Junk list.
	int roll = Utility.RandomInt(3, 5)
	int random
; 	;Debug.Trace("Level 1 Items: " + roll)
	While (roll > 0)
		random = Utility.RandomInt(0, BYOHRelationshipAdoptionGifts_Junk.GetSize() - 1)
		childChest.AddItem(BYOHRelationshipAdoptionGifts_Junk.GetAt(random), Utility.RandomInt(1,3))
		roll = roll - 1
	EndWhile
	
	;Generate 1-3 items from the 0025 list.
	roll = Utility.RandomInt(1, 3)
; 	;Debug.Trace("Level 2 Items: " + roll)
	While (roll > 0)
		random = Utility.RandomInt(0, BYOHRelationshipAdoptionGifts_0025.GetSize() - 1)
		childChest.AddItem(BYOHRelationshipAdoptionGifts_0025.GetAt(random), 1)
		roll = roll - 1
	EndWhile
	
	;Generate one item from a higher-level list if the child's gold supports it, deducting accordingly.
	Actor child
	if (Child2.GetActorRef() == None)
		child = Child1.GetActorRef()
	ElseIf (Child1.GetActorRef().GetItemCount(Gold001) > Child2.GetActorRef().GetItemCount(Gold001))
		child = Child1.GetActorRef()
	Else
		child = Child2.GetActorRef()
	EndIf
	if (child.GetItemCount(Gold001) > 25)
; 		;Debug.Trace("Level 3 Item Added")
		Formlist list = PickGiftList(child.GetItemCount(Gold001))
		roll = Utility.RandomInt(0, list.GetSize())
		childChest.AddItem(list.GetAt(roll), 1)
		int deduction = PickDeduction(child.GetItemCount(Gold001))
		deduction = (deduction / 2) + Utility.RandomInt(0, deduction / 2)
; 		;Debug.Trace("Deducting: " + deduction)
		child.RemoveItem(Gold001, deduction)
	EndIf	
EndFunction



;----------------------------------------------------------------------------------------------------
;PET EVENTS
;----------
;Reminder: Pets are player follower animals (dogs), Critters are kid animals (hares, foxes, etc.).

;Adopt the pet under consideration into the family.
Function AdoptPet()
; 	;Debug.Trace("Adopting " + TransientPet.GetActorRef())
	Actor newPet = TransientPet.GetActorRef()
	TransientPet.Clear()
	FamilyPet.ForceRefTo(newPet)
	Scheduler_FamilyPet.ForceRefTo(newPet)
	FamilyPet.GetActorRef().EvaluatePackage()	
EndFunction

;Reject the pet under consideration, preventing further pet events for 15d.
Function RejectPet()
; 	;Debug.Trace("Rejecting " + TransientPet.GetActorRef())
	petRetryTime = petRetryTime + 15
EndFunction

;We 'ban' future pet and critter events by dramatically increasing the timer.
Function BanPet()
; 	;Debug.Trace("Now banning pets and critters")
	petRetryTime = petRetryTime + 100000
	critterRetryTime = critterRetryTime + 100000
EndFunction

;When a pet unloads, if it has been dismissed by the player (not at home, not in follower faction), warp it home.
Function PetUnloaded()
; 	;Debug.Trace("Pet detached.")
	if (FamilyPet.GetActorRef().GetCurrentLocation() != CurrentHomeHouse.GetLocation() && \
		Game.GetPlayer().GetCurrentLocation() != CurrentHomeHouse.GetLocation() && \
		!FamilyPet.GetActorRef().IsInFaction(CurrentFollowerFaction))
; 		;Debug.Trace("Moving pet home.")
		FamilyPet.GetActorRef().MoveTo(TranslateHouseIntToObj(currentHome))
	EndIf
EndFunction

;When a pet is killed:
; - Remove them from their Adoption aliases to allow pets to be adopted in the future.
; - If the player killed the pet, put the children into the "Never Speak to You Again" behavior.
Function PetDeath(Actor akKiller)
; 	;Debug.Trace("Pet has died.")
	FamilyPet.Clear()
	Scheduler_FamilyPet.Clear()
	if (akKiller == Game.GetPlayer())
; 		;Debug.Trace("Player killed pet, so putting children into 'Never Speak to You Again'.")
		IssueOrderNeverSpeakToYouAgain(Child1.GetActorRef())
		if (Child2.GetActorRef() != None)
			IssueOrderNeverSpeakToYouAgain(Child2.GetActorRef())
		EndIf
	EndIf
EndFunction



;----------------------------------------------------------------------------------------------------
;CRITTER EVENTS
;--------------
;Reminder: Pets are player follower animals (dogs), Critters are kid animals (hares, foxes, etc.).


;Spawn a new critter the child will ask to keep.
Function SetupCritter(Actor child)
; 	;Debug.Trace("Now setting up the critter.")
	
	;Pick a critter from the formlist. Anything but the last one we rejected will do.
	ActorBase critterBase = rejectedCritterBase
	While(critterBase == rejectedCritterBase)
		If (child.GetActorBase().GetSex() == 0)
			critterBase = 	BYOHRelationshipAdoption_CrittersMale.GetAt(Utility.RandomInt(0, BYOHRelationshipAdoption_CrittersMale.GetSize() - 1)) as ActorBase
		Else
			critterBase = 	BYOHRelationshipAdoption_CrittersFemale.GetAt(Utility.RandomInt(0, BYOHRelationshipAdoption_CrittersFemale.GetSize() - 1)) as ActorBase
		EndIf
	EndWhile
	
	;Create the critter and force it into aliases.
; 	;Debug.Trace("Spawning: " + critterBase)
	Actor newCritter = child.PlaceActorAtMe(critterBase)
	TransientCritter.ForceRefTo(newCritter)
	Scheduler_TransientCritter.ForceRefTo(newCritter)
	
	;Set AV06 on the critter to represent which child 'owns' it.
	if (Child1.GetActorRef() == child)
		newCritter.SetAV("Variable06", 1)
	Else
		newCritter.SetAV("Variable06", 2)
	EndIf
	
	;EVP Child and Critter, then move them to their scene locations.
	child.EvaluatePackage()
	child.MoveToPackageLocation()
	newCritter.EvaluatePackage()
	newCritter.MoveToPackageLocation()
EndFunction

;If a critter event is running, and we need to stop it, this function will do the job.
Function QuashCritterEvents()
; 	;Debug.Trace("Quashing Critter Events.")
	if (Child1.GetActorRef().GetAV("Variable07") == 5)
		Child1.GetActorRef().SetAV("Variable07", 0)
	EndIf
	if (Child2.GetActorRef() != None && Child2.GetActorRef().GetAV("Variable07") == 5)
		Child2.GetActorRef().SetAV("Variable07", 0)
	EndIf
	if (TransientCritter.GetActorRef() != None)
		RejectCritter()
	EndIf	
EndFunction


;Adopt the critter under consideration into the family.
Function AdoptCritter()
; 	;Debug.Trace("Adopting " + TransientCritter.GetActorRef())
	Actor newCritter = TransientCritter.GetActorRef()
	TransientCritter.Clear()
	Scheduler_TransientCritter.Clear()
	FamilyCritter.ForceRefTo(newCritter)
	Scheduler_FamilyCritter.ForceRefTo(newCritter)
	
	;EVP the critter.
	FamilyCritter.GetActorRef().EvaluatePackage()
EndFunction

;Reject the critter under consideration, preventing further critter events for 15d.
Function RejectCritter()
; 	;Debug.Trace("Rejecting " + TransientCritter.GetActorRef())
	rejectedCritter = TransientCritter.GetActorRef()
	rejectedCritterBase = TransientCritter.GetActorRef().GetActorBase()
	critterRetryTime = critterRetryTime + 15
	
	;Disable the critter. A bit ugly, since most of them don't fade.
	rejectedCritter.Disable(True)
	
; 	;Debug.Trace("Deleting rejected critter.")
	TransientCritter.Clear()
	Scheduler_TransientCritter.Clear()
	rejectedCritter.Disable()
	rejectedCritter.Delete()
	rejectedCritter = None
EndFunction

;We 'ban' future pet and critter events by dramatically increasing the timer.
Function BanCritters()
; 	;Debug.Trace("Now banning pets and critters")
	petRetryTime = petRetryTime + 100000
	critterRetryTime = critterRetryTime + 100000
	
	;A ban also rejects the current critter.
	RejectCritter()
EndFunction

;When a critter is killed:
; - Remove them from their Adoption aliases to allow critters to be adopted in the future.
; - Block new critter events for 15d.
; - If the player killed the pet, put the children into the "Never Speak to You Again" behavior.
Function CritterDeath(Actor akKiller)
; 	;Debug.Trace("Critter has died.")
	FamilyCritter.Clear()
	Scheduler_FamilyCritter.Clear()
	critterRetryTime = Utility.GetCurrentGameTime() + 15
	if (akKiller == Game.GetPlayer())
; 		;Debug.Trace("Player killed critter, so putting children into 'Never Speak to You Again'.")
		IssueOrderNeverSpeakToYouAgain(Child1.GetActorRef())
		if (Child2.GetActorRef() != None)
			IssueOrderNeverSpeakToYouAgain(Child2.GetActorRef())
		EndIf
	EndIf
EndFunction



;----------------------------------------------------------------------------------------------------
;WELCOME HOME SYSTEM
;-------------------


;Set up a Forcegreet Event when requested by a Location Change event.
Function ReadyForcegreetEvent()
; 	Debug.Trace("Attempting to ready Forcegreet Event")
	if (!ForcegreetEventReady)
		ForcegreetEventReady = True
		
		;Make sure the children aren't in any games.
		StopGames()
		
		;Declare some local variables.
		int eventNumber
		Actor eventChild
	
		;Select the event.
	
		;EVENT 1 - CHILD THANKS PLAYER FOR ADOPTION, NO CHEST INTRO
		;Occurs if: Child newly adopted, Initial Move is a Dirty Move
; 		Debug.Trace("New Adoption: " + (Child1NewlyAdopted || Child2NewlyAdopted) + "  Initial Move: " + initialMoveDone + "  Chest Intro'd: " + ChildChestIntroduced)
		If ((Child1NewlyAdopted || Child2NewlyAdopted) && (!initialMoveDone || ChildChestIntroduced))
			eventNumber = 1
			if (Child1NewlyAdopted)
				eventChild = Child1.GetActorRef()
			Else
				eventChild = Child2.GetActorRef()
			EndIf
		
		;EVENT 2 - CHILD THANKS PLAYER FOR ADOPTION, INCLUDES CHEST INTRO
		;Occurs if: Child newly adopted, Initial Move is a Clean Move
		ElseIf ((Child1NewlyAdopted || Child2NewlyAdopted) && initialMoveDone && !ChildChestIntroduced)
			ChildChestIntroduced = True
			eventNumber = 2
			if (Child1NewlyAdopted)
				eventChild = Child1.GetActorRef()
			Else
				eventChild = Child2.GetActorRef()
			EndIf
		
		;EVENT 3 - CHILD INTRODUCES CHEST SYSTEM
		;Occurs if: Child previously performed 
		ElseIf (initialMoveDone && !ChildChestIntroduced)
			ChildChestIntroduced = True
			eventNumber = 3
			eventChild = PickRandomChild()
			
		;EVENT 4 - ADOPT A PET
		;Occurs if: FamilyPet is empty, Player has an Animal Companion, time > petRetryTime
		ElseIf (FamilyPet.GetActorRef() == None && AnimalCompanion.GetActorRef() != None && Utility.GetCurrentGameTime() > petRetryTime)
			TransientPet.ForceRefTo(AnimalCompanion.GetActorRef())
			int suitability = EvaluatePetSuitability()
			eventNumber = 4
			eventChild = PickRandomChild()
		
		;EVENT 5 - ADOPT A CRITTER
		;Occurs if: FamilyCritter is empty, time > critterRetryTime, 10% Chance
		ElseIf (FamilyCritter.GetActorRef() == None && Utility.GetCurrentGameTime() > critterRetryTime && Utility.RandomInt(1, 100) < critterChance)
			eventNumber = 5
			eventChild = PickRandomChild()
			;We'll create the new Critter here. Set Variable07 early so SetupCritter can use it.
			eventChild.SetActorValue("Variable07", eventNumber)
			SetupCritter(eventChild)
			
			
		;EVENT 6 - NAME CALLING 1
		;Occurs if: Player has two children, they haven't played this scene before, they didn't do a Name Calling Scene last time, 10% chance.
		ElseIf (Child2.GetActorRef() != None && !NameCalling1Done && !NameCallingUsedLast && Utility.RandomInt(1, 100) < nameCallingChance)
			eventNumber = 6
			eventChild = PickRandomChild()	;Doesn't matter, since we're just going to override the usual behavior...
			NameCalling1Done = True
			NameCallingUsedLast = True
			ResortChildrenForNameCalling()
			BlockGames(Child1.GetActorRef())
			BlockGames(Child2.GetActorRef())
			Child1.GetActorRef().SetActorValue("Variable07", eventNumber)
			Child2.GetActorRef().SetActorValue("Variable07", eventNumber)
			Child1.GetActorRef().MoveTo(SchedulerSceneMarker1.GetReference())
			Child2.GetActorRef().MoveTo(SchedulerSceneMarker2.GetReference())
			RelationshipAdoption_SceneNameCalling01.Start()
		
			
		;EVENT 7 - NAME CALLING 2
		;Occurs if: Player has two children, they haven't played this scene before, they didn't do a Name Calling Scene last time, 10% chance.
		ElseIf (Child2.GetActorRef() != None && !NameCalling2Done && !NameCallingUsedLast && Utility.RandomInt(1, 100) < nameCallingChance)
			eventNumber = 7
			eventChild = PickRandomChild()	;Doesn't matter, since we're just going to override the usual behavior...
			NameCalling2Done = True
			NameCallingUsedLast = True
			ResortChildrenForNameCalling()
			BlockGames(Child1.GetActorRef())
			BlockGames(Child2.GetActorRef())
			Child1.GetActorRef().SetActorValue("Variable07", eventNumber)
			Child2.GetActorRef().SetActorValue("Variable07", eventNumber)
			Child1.GetActorRef().MoveTo(SchedulerSceneMarker1.GetReference())
			Child2.GetActorRef().MoveTo(SchedulerSceneMarker2.GetReference())
			RelationshipAdoption_SceneNameCalling02.Start()
		
		;EVENT 10-13 - GENERIC EVENTS
		;Randomly select from one of the other events.
		Else
			eventNumber = Utility.RandomInt(10, 13)
			eventChild = PickRandomChild()
		EndIf
		
		;For all of the non-Generic Forcegreets, block games on the selected child to prevent interference.
		if (eventNumber < 10)
			BlockGames(eventChild)
		EndIf
		
		;If we used a Name Calling scene last time around, but we're doing something different this time, clear the flag.
		if (NameCallingUsedLast && eventNumber != 6 && eventNumber != 7)
			NameCallingUsedLast = False
		EndIf

		;Trigger the selected event.
; 		Debug.Trace("Forcegreet: " + eventChild + " " + eventNumber)
		eventChild.SetActorValue("Variable07", eventNumber)
		eventChild.EvaluatePackage()
	EndIf
EndFunction

;Select a random child.
Actor Function PickRandomChild()
	if (Child2.GetActorRef() == None)
		return Child1.GetActorRef()
	ElseIf (Utility.RandomInt(0, 1) == 0)
		return Child1.GetActorRef()
	Else
		return Child2.GetActorRef()
	EndIf
EndFunction


;Evaluate the pet in the TransientPet alias to determine whether it's suitable for adoption, and whether it's a dog.
;Returns the int (Faction Rank) representing the suitability. Stores the 'dog' status as AV06=1.
int Function EvaluatePetSuitability()
	int suitability = 1	;1=None, 2=UNUSED, 3=All Children
	
	Race transient = TransientPet.GetActorRef().GetRace()
	Race current
	
	;Is this pet a dog? If so, store it as AV06=0.
	int i = 0
	While (i < BYOHRelationshipAdoption_PetDogsList.GetSize())
		current = BYOHRelationshipAdoption_PetDogsList.GetAt(i) as Race
; 		;Debug.Trace("Comparing: " + transient + " : " + current)
		if (current == transient)
; 			;Debug.Trace("Pet is a dog.")
			TransientPet.GetActorRef().SetAV("Variable06", 1)
			i = BYOHRelationshipAdoption_PetDogsList.GetSize()
		EndIf
		i = i + 1	
	EndWhile
	
	;Is this pet adoptable by any child?
	i = 0
	While (i < BYOHRelationshipAdoption_PetAllowedRacesList.GetSize())
		current = BYOHRelationshipAdoption_PetAllowedRacesList.GetAt(i) as Race
; 		;Debug.Trace("Comparing: " + transient + " : " + current)
		if (current == transient)
; 			;Debug.Trace("EvalPetSuitability found pet in General list.")
			suitability = 3
			i = BYOHRelationshipAdoption_PetAllowedRacesList.GetSize()
		EndIf
		i = i + 1	
	EndWhile
	
	;Update the faction rank to store this result for use as a dialogue condition.
	TransientPet.GetActorRef().SetFactionRank(BYOHRelationshipPotentialPetFaction, suitability)
	
	;Return the rank so we can decide which child to use for this event.
; 	;Debug.Trace("EvalPetSuitability returns: " + suitability)
	return suitability
EndFunction


;Name-Calling Events (6 & 7)
;Resort the player's children for the name-calling scene.
Function ResortChildrenForNameCalling()
	ActorBase current
	int i = 0
	While (i < BYOHRelationshipAdoption_NameCallingWinnersList.GetSize())
		current = (BYOHRelationshipAdoption_NameCallingWinnersList.GetAt(i) as ActorBase)
; 		;Debug.Trace("Resort checking: " + current)
		if (current == Child1.GetActorRef().GetActorBase())
; 			;Debug.Trace("Swap Children: " + current)
			SwapChildren()
			Return
		ElseIf (current == Child2.GetActorRef().GetActorBase())
; 			;Debug.Trace("Quick Return: " + current)
			Return
		EndIf
		i = i + 1	
	EndWhile
EndFunction

;Name-Calling Events (6 & 7)
;Player cuts off the Name-calling event.
Function BreakNameCalling(bool orderToRoom, Actor childToOrder)
	ForcegreetEventReady = False
	RelationshipAdoption_SceneNameCalling01.Stop()
	RelationshipAdoption_SceneNameCalling02.Stop()
	Child1.GetActorRef().SetActorValue("Variable07", 0)
	Child2.GetActorRef().SetActorValue("Variable07", 0)
	UnblockGames(Child1.GetActorRef())
	UnblockGames(Child2.GetActorRef())
	if (orderToRoom)
		OrderToConfirm = 1.3
		IssueOrder(childToOrder)
	EndIf
	Child1.GetActorRef().EvaluatePackage()
	Child2.GetActorRef().EvaluatePackage()
EndFunction

;Allowance Event: Add to the Poor Count if no allowance is given.
;When WeArePoorCount >= 3, the gift forcegreet has some unique dialogue.
Function IncrementPoorCount()
	WeArePoorCount = WeArePoorCount + 1
EndFunction

;Allowance Event: Decrement the Poor Count if an allowance is given.
;WeArePoorCount decrements faster if you give the child more money.
Function DecrementPoorCount(int amount)
	WeArePoorCount = WeArePoorCount - amount
	if (WeArePoorCount < 0)
		WeArePoorCount = 0
	EndIf
EndFunction

;Give Player Gift Event: Roll up a poor gift if appropriate.
Function FGReceivePoorGift(Actor child)
	Formlist temp = BYOHRelationshipAdoptionGifts_Poor
	int roll = Utility.RandomInt(0, temp.GetSize() - 1)
	Game.GetPlayer().AddItem(temp.GetAt(roll), 1)
	DecrementPoorCount(2)
EndFunction

;Give Player Gift Event: Roll up a gift, give it, and deduct the cost.
Function FGReceiveGift(Actor child)
	Formlist list = PickGiftList(child.GetItemCount(Gold001))			;Decide which list of gifts to use.
	int roll = Utility.RandomInt(0, list.GetSize())						;Roll for a gift on the list.
	int luck = 1																;Small chance of giving two of an item.
	if (Utility.RandomInt() > 90)
		luck = 2
	EndIf
	Game.GetPlayer().AddItem(list.GetAt(roll), luck)						;Give the gift
	int deduction = PickDeduction(child.GetItemCount(Gold001))			;Determine how much to 'charge' the child for the gift.
	deduction = (deduction / 2) + Utility.RandomInt(0, deduction / 2)
; 	;Debug.Trace("Deducting: " + deduction)
	child.RemoveItem(Gold001, deduction * luck)
EndFunction

;Give Player Gift Event: Helper Function to select the appropriate formlist.
FormList Function PickGiftList(int gold)
	if (gold < 25)
		return BYOHRelationshipAdoptionGifts_0000
	ElseIf (gold < 50)
; 		;Debug.Trace("Pick gift from list 0025")
		return BYOHRelationshipAdoptionGifts_0025
	ElseIf (gold < 100)
; 		;Debug.Trace("Pick gift from list 050")
		return BYOHRelationshipAdoptionGifts_0050
	ElseIf (gold < 250)
; 		;Debug.Trace("Pick gift from list 0100")
		return BYOHRelationshipAdoptionGifts_0100
	ElseIf (gold < 500)
; 		;Debug.Trace("Pick gift from list 0250")
		return BYOHRelationshipAdoptionGifts_0250
	ElseIf (gold < 1000)
; 		;Debug.Trace("Pick gift from list 500")
		return BYOHRelationshipAdoptionGifts_0500
	EndIf
	;Else case...
; 	;Debug.Trace("Pick gift from list 1000")
	return BYOHRelationshipAdoptionGifts_1000
EndFunction

;Give Player Gift Event: Helper Function to select the appropriate deduction.
int Function PickDeduction(int gold)
	if (gold < 25)
		return 1
	ElseIf (gold < 50)
		return 25
	ElseIf (gold < 100)
		return 50
	ElseIf (gold < 250)
		return 100
	ElseIf (gold < 500)
		return 250
	ElseIf (gold < 1000)
		return 500
	EndIf
	;Else case...
	return 1000
EndFunction


;----------------------------------------------------------------------------------------------------
;LOCATION CHANGED EVENTS
;-----------------------

;Respond to Player LocationChanged events. This includes things like:
; - Starting or stopping the Civil War Siege handler quest.
; - Moving the player's family to a new home.
; - Triggering Return Home events.
Function PlayerLocationChanged(Location newLoc, Location oldLoc)
; 	;Debug.Trace("LOCATION CHANGE: " + newLoc)
; 	;Debug.Trace("CW SIEGE IS RUNNING?" + CWSiege.IsRunning())
	
	;Edge Case Handling: If a CW Siege is running, move the family inside.
	if (!BYOHRelationshipAdoptionCWSiegeHandler.IsRunning() && CWSiege.IsRunning() && CWSiege.GetStage() > 0 && \
		CurrentHomeExterior.GetLocation() != None && CWSiegeCity.GetLocation() == CurrentHomeExterior.GetLocation())
; 		;Debug.Trace("CW Flee Package Triggered")
		BYOHRelationshipAdoptionCWSiegeHandler.SetStage(0)
	EndIf
	
	;Edge Case Handling: If the CW Siege has ended, shut off the CW Siege Handler quest.
	if (BYOHRelationshipAdoptionCWSiegeHandler.IsRunning() && !CWSiege.IsRunning())
; 		;Debug.Trace("CW Flee Package Ended")
		BYOHRelationshipAdoptionCWSiegeHandler.Stop()
	EndIf
	
	;Edge Case Handling: If we've adopted a Critter, but it got out of the house, move it back inside.
	;if (FamilyCritter.GetActorRef() != None && !FamilyCritter.GetActorRef().IsDead() && FamilyCritter.GetActorRef().GetCurrentLocation() != CurrentHomeHouse.GetLocation())
; 	;	Debug.Trace("Critter moved back.")
	;	FamilyCritter.GetActorRef().MoveTo(TranslateHouseIntToObj(currentHome))
	;EndIf
	
	;If a move has been queued, determine whether to move the family.
	if (moveQueued)
; 		;Debug.Trace("Move has been queued. Now testing...")
		if (newLoc != Child1.GetActorRef().GetCurrentLocation() && \
			(Spouse.GetActorRef() == None || Spouse.GetActorRef().IsInFaction(CurrentFollowerFaction) || newLoc != Spouse.GetActorRef().GetCurrentLocation()) && \
			(Child2.GetActorRef() == None || newLoc != Child2.GetActorRef().GetCurrentLocation()))
; 			;Debug.Trace("Children are not in the player's new location.")
			if (!Child1.GetActorRef().GetCurrentLocation().IsChild(newLoc) && \
				(Spouse.GetActorRef() == None || Spouse.GetActorRef().IsInFaction(CurrentFollowerFaction) || !Spouse.GetActorRef().GetCurrentLocation().IsChild(newLoc)) && \
				(Child2.GetActorRef() == None || !Child2.GetActorRef().GetCurrentLocation().IsChild(newLoc)))
; 				;Debug.Trace("Children are not in the parent of the player's new location.")
				if ((newLoc == TranslateHouseIntToLoc(newHome) && !newLoc.IsChild(oldLoc)) || \
					(!newLoc.IsChild(Child1.GetActorRef().GetCurrentLocation()) && \
					(Spouse.GetActorRef() == None || Spouse.GetActorRef().IsInFaction(CurrentFollowerFaction) || !newLoc.IsChild(Spouse.GetActorRef().GetCurrentLocation())) && \
					(Child2.GetActorRef() == None || !newLoc.IsChild(Child2.GetActorRef().GetCurrentLocation()))))
; 					;Debug.Trace("Player's new location is the new home city, OR player is not in the parent of the children's location.")
; 					;Debug.Trace("Passed test. now moving.")
					MoveFamily()
				EndIf
			EndIf
		EndIf
	EndIf
	
	;Determine whether to trigger a Welcome Home event.
; 	;Debug.Trace("Player last seen: " + (Utility.GetCurrentGameTime() - playerLastSeen))
	if ((newLoc == CurrentHomeHouse.GetLocation() || newLoc == CurrentHomeExterior.GetLocation()) && \
		(oldLoc != CurrentHomeHouse.GetLocation() && oldLoc != CurrentHomeExterior.GetLocation()))
		if (Utility.GetCurrentGameTime() - playerLastSeen > WelcomeHomeDelay)
			;If any gold has been stored from gift-giving events, give it to the child now.
			if (GiftStoredValueChild1 > 0)
; 				;Debug.Trace("Adding " + GiftStoredValueChild1 + " stored gold to Child 1.")
				Child1.GetActorRef().AddItem(Gold001, GiftStoredValueChild1)
				GiftStoredValueChild1 = 0
			EndIf
			if (GiftStoredValueChild2 > 0)
; 				;Debug.Trace("Adding " + GiftStoredValueChild2 + " stored gold to Child 2.")
				Child2.GetActorRef().AddItem(Gold001, GiftStoredValueChild2)
				GiftStoredValueChild2 = 0
			EndIf	
			
; 			;Debug.Trace("Forcegreet Readied")
			ReadyForcegreetEvent()
		EndIf
		playerLastSeen = Utility.GetCurrentGameTime()
	EndIf
EndFunction

;Respond to Child LocationChanged events. This includes things like:
; - Triggering a 'dirty' move for newly-adopted children when they reach their new home.
Function ChildLocationChanged(Actor child, Location newLoc, Location oldLoc)
; 	;Debug.Trace("CHILD LOCATION CHANGE: " + child + " " + newLoc)
	
	;Is this a newly-adopted child that hasn't been moved yet?
	if (((child == Child1.GetActorRef() && child1NewlyAdopted) || (child == Child2.GetActorRef() && child2NewlyAdopted)) && !MovingTogglePackageOn)
		;Has this child reached their new home?
		if (newLoc == TranslateHouseIntToInteriorLoc(newHome))
; 			;Debug.Trace("Newly-adopted child has reached their new home.")
			;Is the player in the home right now?
			if (Game.GetPlayer().GetCurrentLocation() == newLoc || Child1.GetActorRef().GetCurrentLocation() != newLoc)
				;Either the player is standing right here, or Child1 is still outside, so we can't warp the family in.
				;And we have to start the scheduler *right now*, or the new child won't behave properly.
				;So perform a 'dirty move', which omits the teleport. Your spouse and child will temporarily start walking home.
; 				;Debug.Trace("Performing a dirty move on the newly-adopted child.")
				DirtyMoveFamily()
			Else
				;The player is out of the house, so we can safely teleport the family in.
; 				;Debug.Trace("Performing a clean move on the newly-adopted child.")
				MoveFamily()
			EndIf
		EndIf	
	EndIf
	
	;If the child has moved to a new location, we can safely unblock games on them if they had been blocked.
	UnblockGames(child)
EndFunction


;----------------------------------------------------------------------------------------------------
;UTILITY FUNCTIONS
;------------------------

;Determine if a move destination is valid, with help from RelationshipAdoptable.
;Returns an int representing a valid move destination.
int Function ValidateMoveDestination(int destination)
	if (destination == 0)
		return currentHome
	EndIf
; 	;Debug.Trace("Adoption Validation: " + destination + " " + CurrentHomeExterior.GetLocation() + " " + CurrentHomeExterior.GetLocation())
	
	Location secondaryLoc = CurrentHomeExterior.GetLocation()
	if (secondaryLoc == None)
		secondaryLoc = CurrentHomeExterior.GetLocation()
	EndIf
	int secondary = TranslateLocationToHouseInt(secondaryLoc)
	
	;If destination = -1, the Scheduler has failed and the current location is probably bad. Don't move the children there again.
	if (destination == -1)
		secondary = -1
	EndIf
	
	if (CCHouse.IsCCHouseDestination(destination))
		return CCHouse.ValidateCCHouseMoveDestination(destination, secondary)
	else
	 	;Debug.Trace("Handoff to Adoptable Validation with: " + destination + " " + secondary)
		return (BYOHRelationshipAdoptable as BYOHRelationshipAdoptableScript).ValidateMoveDestination(destination, secondary)
	endif
EndFunction


;Given a location, return the corresponding int.
int Function TranslateLocationToHouseInt(Location newLoc)
	if (newLoc == SolitudeLocation)
		return 1
	ElseIf (newLoc == WindhelmLocation)
		return 2
	ElseIf (newLoc == MarkarthLocation)
		return 3
	ElseIf (newLoc == RiftenLocation)
		return 4
	ElseIf (newLoc == WhiterunLocation)
		return 5
	ElseIf (newLoc == FalkreathHouseLocation)
		return 6
	ElseIf (newLoc == HjaalmarchHouseLocation)
		return 7
	ElseIf (newLoc == PaleHouseLocation)
		return 8
	ElseIf (newLoc == None)
		return -1
	EndIf

	; Check CC Houses
	int newLocId = CCHouse.TranslateCCHouseExteriorLocToInt(newLoc)
	if newLocId > 0
		return newLocId
	endif

; 	;Debug.Trace("RelationshipAdoptionScript Loc Translation Error!")
	return 0
EndFunction
	
	
;Given an int, return the corresponding center marker.
ObjectReference Function TranslateHouseIntToObj(int newHouse)
	if (newHouse == 1)
		return HouseSolitudeMarker
	ElseIf (newHouse == 2)
		return HouseWindhelmMarker
	ElseIf (newHouse == 3)
		return HouseMarkarthMarker
	ElseIf (newHouse == 4)
		return HouseRiftenMarker
	ElseIf (newHouse == 5)
		return HouseWhiterunMarker
	ElseIf (newHouse == 6)
		return HouseFalkreathMarker
	ElseIf (newHouse == 7)
		return HouseHjaalmarchMarker
	ElseIf (newHouse == 8)
		return HousePaleMarker
	EndIf

	; Check CC Houses
	if (CCHouse.IsCCHouseDestination(newHouse))
		return CCHouse.TranslateCCHouseIntToObj(newHouse)
	endif

; 	;Debug.Trace("RelationshipAdoptionScript Int Translation Error!")
	return None
EndFunction

;Given an int, return the corresponding exterior location.
Location Function TranslateHouseIntToLoc(int newHouse)
	if (newHouse == 1)
		return SolitudeLocation
	ElseIf (newHouse == 2)
		return WindhelmLocation
	ElseIf (newHouse == 3)
		return MarkarthLocation
	ElseIf (newHouse == 4)
		return RiftenLocation
	ElseIf (newHouse == 5)
		return WhiterunLocation
	ElseIf (newHouse == 6)
		return FalkreathHouseLocation
	ElseIf (newHouse == 7)
		return HjaalmarchHouseLocation
	ElseIf (newHouse == 8)
		return PaleHouseLocation
	EndIf

	; Check CC Houses
	if (CCHouse.IsCCHouseDestination(newHouse))
		return CCHouse.TranslateCCHouseIntToExteriorLoc(newHouse)
	endif

; 	;Debug.Trace("RelationshipAdoptionScript Int Translation Error 2!")
	return None
EndFunction

;Given an int, return the corresponding interior location.
Location Function TranslateHouseIntToInteriorLoc(int newHouse)
	if (newHouse == 1)
		return SolitudeProudspireManorLocation
	ElseIf (newHouse == 2)
		return WindhelmHjerimLocation
	ElseIf (newHouse == 3)
		return MarkarthVlindrelHallLocation
	ElseIf (newHouse == 4)
		return RiftenHoneysideLocation
	ElseIf (newHouse == 5)
		return WhiterunBreezehomeLocation
	ElseIf (newHouse == 6)
		return FalkreathHouseInteriorLocation
	ElseIf (newHouse == 7)
		return HjaalmarchHouseInteriorLocation
	ElseIf (newHouse == 8)
		return PaleHouseInteriorLocation
	EndIf

	; Check CC Houses
	if (CCHouse.IsCCHouseDestination(newHouse))
		return CCHouse.TranslateCCHouseIntToExteriorLoc(newHouse)
	endif

; 	;Debug.Trace("RelationshipAdoptionScript Int Translation Error 3!")
	return None
EndFunction

;Swap Child1 and Child2.
Function SwapChildren()
	;Swap the children in their aliases.
	Actor tempChild = Child1.GetActorRef()
	Child1.ForceRefTo(Child2.GetActorRef())
	Child2.ForceRefTo(tempChild)
	
	;If the Scheduler is running, swap them there, too.
	if (BYOHRelationshipAdoptionScheduler.IsRunning())
		(BYOHRelationshipAdoptionScheduler as BYOHRelationshipAdoptionSc).SwapChildren()
	EndIf
	
	;Swap the childrens' gold pools.
	int tempValue = GiftStoredValueChild1
	GiftStoredValueChild1 = GiftStoredValueChild2
	GiftStoredValueChild2 = tempValue
	
	;Swap the childrens' 'Newly Adopted' flags.
	bool tempNewlyAdopted = child1NewlyAdopted
	child1NewlyAdopted = child2NewlyAdopted
	child2NewlyAdopted = tempNewlyAdopted
	
	;If the family has a critter, make sure it still follows the correct child.
	if (FamilyCritter.GetActorRef() != None)
		if (FamilyCritter.GetActorRef().GetAV("Variable06") == 1)
			FamilyCritter.GetActorRef().SetAV("Variable06", 2)
		Else
			FamilyCritter.GetActorRef().SetAV("Variable06", 1)
		EndIf
	EndIf
EndFunction