Scriptname CWMapActivatorScript extends ObjectReference Hidden 

Activator Property CWMapFlagBlueSolitude Auto
Activator Property CWMapFlagBlueWinterhold Auto
Activator Property CWMapFlagBlueWindhelm Auto
Activator Property CWMapFlagBlueRiften Auto
Activator Property CWMapFlagBlueWhiterun Auto
Activator Property CWMapFlagBlueFalkreath Auto
Activator Property CWMapFlagBlueMorthal Auto
Activator Property CWMapFlagBlueMarkarth Auto
Activator Property CWMapFlagBlueDawnstar Auto
Activator Property CWMapFlagBlueKarthwasten Auto
Activator Property CWMapFlagBlueDragonBridge Auto
Activator Property CWMapFlagBlueRorikstead Auto
Activator Property CWMapFlagBlueHelgen Auto
Activator Property CWMapFlagBlueIvarstead Auto
Activator Property CWMapFlagBlueShorsStone Auto
Activator Property CWMapFlagBlueRiverwood Auto
Activator Property CWMapFlagBlueFortGreymoor Auto
Activator Property CWMapFlagBlueFortSungard Auto
Activator Property CWMapFlagBlueFortHraggstad Auto
Activator Property CWMapFlagBlueFortDunstad Auto
Activator Property CWMapFlagBlueFortKastav Auto
Activator Property CWMapFlagBlueFortAmol Auto
Activator Property CWMapFlagBlueFortGreenwall Auto
Activator Property CWMapFlagBlueFortNeugrad Auto
Activator Property CWMapFlagBlueFortSnowhawk Auto
{List of all the Stormcloak Flag Activators}

Activator Property CWMapFlagRedSolitude Auto
Activator Property CWMapFlagRedWinterhold Auto
Activator Property CWMapFlagRedWindhelm Auto
Activator Property CWMapFlagRedRiften Auto
Activator Property CWMapFlagRedWhiterun Auto
Activator Property CWMapFlagRedFalkreath Auto
Activator Property CWMapFlagRedMorthal Auto
Activator Property CWMapFlagRedMarkarth Auto
Activator Property CWMapFlagRedDawnstar Auto
Activator Property CWMapFlagRedKarthwasten Auto
Activator Property CWMapFlagRedDragonBridge Auto
Activator Property CWMapFlagRedRorikstead Auto
Activator Property CWMapFlagRedHelgen Auto
Activator Property CWMapFlagRedIvarstead Auto
Activator Property CWMapFlagRedShorsStone Auto
Activator Property CWMapFlagRedRiverwood Auto
Activator Property CWMapFlagRedFortGreymoor Auto
Activator Property CWMapFlagRedFortSungard Auto
Activator Property CWMapFlagRedFortHraggstad Auto
Activator Property CWMapFlagRedFortDunstad Auto
Activator Property CWMapFlagRedFortKastav Auto
Activator Property CWMapFlagRedFortAmol Auto
Activator Property CWMapFlagRedFortGreenwall Auto
Activator Property CWMapFlagRedFortNeugrad Auto
Activator Property CWMapFlagRedFortSnowhawk Auto
{List of all the Imperial Flag Activators}

ObjectReference Property CWMapFlagSolitudeRef Auto Hidden
ObjectReference Property CWMapFlagWinterholdRef Auto Hidden
ObjectReference Property CWMapFlagWindhelmRef Auto Hidden
ObjectReference Property CWMapFlagRiftenRef Auto Hidden
ObjectReference Property CWMapFlagWhiterunRef Auto Hidden
ObjectReference Property CWMapFlagFalkreathRef Auto Hidden
ObjectReference Property CWMapFlagMorthalRef Auto Hidden
ObjectReference Property CWMapFlagMarkarthRef Auto Hidden
ObjectReference Property CWMapFlagDawnstarRef Auto Hidden
ObjectReference Property CWMapFlagKarthwastenRef Auto Hidden
ObjectReference Property CWMapFlagDragonBridgeRef Auto Hidden
ObjectReference Property CWMapFlagRoriksteadRef Auto Hidden
ObjectReference Property CWMapFlagHelgenRef Auto Hidden
ObjectReference Property CWMapFlagIvarsteadRef Auto Hidden
ObjectReference Property CWMapFlagShorsStoneRef Auto Hidden
ObjectReference Property CWMapFlagRiverwoodRef Auto Hidden
ObjectReference Property CWMapFlagFortGreymoorRef Auto Hidden
ObjectReference Property CWMapFlagFortSungardRef Auto Hidden
ObjectReference Property CWMapFlagFortHraggstadRef Auto Hidden
ObjectReference Property CWMapFlagFortDunstadRef Auto Hidden
ObjectReference Property CWMapFlagFortKastavRef Auto Hidden
ObjectReference Property CWMapFlagFortAmolRef Auto Hidden
ObjectReference Property CWMapFlagFortGreenwallRef Auto Hidden
ObjectReference Property CWMapFlagFortNeugradRef Auto Hidden
ObjectReference Property CWMapFlagFortSnowhawkRef Auto Hidden
{Pointers to all the flag references}

Location Property SolitudeLocation Auto
Location Property WinterholdLocation Auto
Location Property WindhelmLocation Auto
Location Property RiftenLocation Auto
Location Property WhiterunLocation Auto
Location Property FalkreathLocation Auto
Location Property MorthalLocation Auto
Location Property MarkarthLocation Auto
Location Property DawnstarLocation Auto
Location Property KarthwastenLocation Auto
Location Property DragonBridgeLocation Auto
Location Property RoriksteadLocation Auto
Location Property HelgenLocation Auto
Location Property IvarsteadLocation Auto
Location Property ShorsStoneLocation Auto
Location Property RiverwoodLocation Auto
Location Property FortGreymoorLocation Auto
Location Property FortSungardLocation Auto
Location Property FortHraggstadLocation Auto
Location Property FortDunstadLocation Auto
Location Property FortKastavLocation Auto
Location Property FortAmolLocation Auto
Location Property FortGreenwallLocation Auto
Location Property FortNeugradLocation Auto
Location Property FortSnowhawkLocation Auto
{List of all the locations}

Keyword Property CWOwner Auto
{Keyword to check on the location to figure out who owns it:
1 = Imperials
2 = Stormcloaks
-1 = nobody}

EVENT OnCellAttach()

	RemoveFlags()

	if Is3DLoaded()
		PlaceFlags()
	EndIf

	
endEVENT

function RemoveFlags()
	; Delete all the currently placed flags on the map
	TryToRemoveFlag(CWMapFlagsolitudeRef)
	TryToRemoveFlag(CWMapFlagWinterholdRef)
	TryToRemoveFlag(CWMapFlagWindhelmRef)
	TryToRemoveFlag(CWMapFlagRiftenRef)
	TryToRemoveFlag(CWMapFlagWhiterunRef)
	TryToRemoveFlag(CWMapFlagFalkreathRef)
	TryToRemoveFlag(CWMapFlagMorthalRef)
	TryToRemoveFlag(CWMapFlagMarkarthRef)
	TryToRemoveFlag(CWMapFlagDawnstarRef)
	TryToRemoveFlag(CWMapFlagKarthwastenRef)
	TryToRemoveFlag(CWMapFlagDragonBridgeRef)
	TryToRemoveFlag(CWMapFlagRoriksteadRef)
	TryToRemoveFlag(CWMapFlagHelgenRef)
	TryToRemoveFlag(CWMapFlagIvarsteadRef)
	TryToRemoveFlag(CWMapFlagShorsStoneRef)
	TryToRemoveFlag(CWMapFlagRiverwoodRef)
	TryToRemoveFlag(CWMapFlagFortGreymoorRef)
	TryToRemoveFlag(CWMapFlagFortSungardRef)
	TryToRemoveFlag(CWMapFlagFortHraggstadRef)
	TryToRemoveFlag(CWMapFlagFortDunstadRef)
	TryToRemoveFlag(CWMapFlagFortKastavRef)
	TryToRemoveFlag(CWMapFlagFortAmolRef)
	TryToRemoveFlag(CWMapFlagFortGreenwallRef)
	TryToRemoveFlag(CWMapFlagFortNeugradRef)
	TryToRemoveFlag(CWMapFlagFortSnowhawkRef)
	
EndFunction

function TryToRemoveFlag(ObjectReference FlagToDisable)
	if FlagToDisable
		FlagToDisable.Disable()
		FlagToDisable.Delete()
	endif
EndFunction


function PlaceFlags()

	; Check which locations are held by who and place the correct flag there.
	; Capitals
	
	CWMapFlagWindhelmRef = TryToPlaceFlag(CWMapFlagWindhelmRef, CWMapFlagRedWindhelm, CWMapFlagBlueWindhelm, WindhelmLocation, "CapWindhelm")
	CWMapFlagSolitudeRef = TryToPlaceFlag(CWMapFlagSolitudeRef, CWMapFlagRedSolitude, CWMapFlagBlueSolitude, SolitudeLocation, "CapSolitude")
	CWMapFlagWinterholdRef = TryToPlaceFlag(CWMapFlagWinterholdRef, CWMapFlagRedWinterhold, CWMapFlagBlueWinterhold, WinterholdLocation, "CapWinterhold")
	CWMapFlagRiftenRef = TryToPlaceFlag(CWMapFlagRiftenRef, CWMapFlagRedRiften, CWMapFlagBlueRiften, RiftenLocation, "CapRiften")
	CWMapFlagWhiterunRef = TryToPlaceFlag(CWMapFlagWhiterunRef, CWMapFlagRedWhiterun, CWMapFlagBlueWhiterun, WhiterunLocation, "CapWhiterun")
	CWMapFlagFalkreathRef = TryToPlaceFlag(CWMapFlagFalkreathRef, CWMapFlagRedFalkreath, CWMapFlagBlueFalkreath, FalkreathLocation, "CapFalkreath")
	CWMapFlagMorthalRef = TryToPlaceFlag(CWMapFlagMorthalRef, CWMapFlagRedMorthal, CWMapFlagBlueMorthal, MorthalLocation, "CapMorthal")
	CWMapFlagMarkarthRef = TryToPlaceFlag(CWMapFlagMarkarthRef, CWMapFlagRedMarkarth, CWMapFlagBlueMarkarth, MarkarthLocation, "CapMarkarth")
	CWMapFlagDawnstarRef = TryToPlaceFlag(CWMapFlagDawnstarRef, CWMapFlagRedDawnstar, CWMapFlagBlueDawnstar, DawnstarLocation, "CapDawnStar")	;string case sensitve, intentionally wrong here

	; Towns
	CWMapFlagKarthwastenRef = TryToPlaceFlag(CWMapFlagKarthwastenRef, CWMapFlagRedKarthwasten, CWMapFlagBlueKarthwasten, KarthwastenLocation, "TownKarthwasten")
	CWMapFlagDragonBridgeRef = TryToPlaceFlag(CWMapFlagDragonBridgeRef, CWMapFlagRedDragonBridge, CWMapFlagBlueDragonBridge, DragonBridgeLocation, "TownDragonBridge")
	CWMapFlagRoriksteadRef = TryToPlaceFlag(CWMapFlagRoriksteadRef, CWMapFlagRedRorikstead, CWMapFlagBlueRorikstead, RoriksteadLocation, "TownRorikstead")
	CWMapFlagHelgenRef = TryToPlaceFlag(CWMapFlagHelgenRef, CWMapFlagRedHelgen, CWMapFlagBlueHelgen, HelgenLocation, "TownHelgen")
	CWMapFlagIvarsteadRef = TryToPlaceFlag(CWMapFlagIvarsteadRef, CWMapFlagRedIvarstead, CWMapFlagBlueIvarstead, IvarsteadLocation, "TownIvarstead")
	CWMapFlagShorsStoneRef = TryToPlaceFlag(CWMapFlagShorsStoneRef, CWMapFlagRedShorsStone, CWMapFlagBlueShorsStone, ShorsStoneLocation, "TownShorsStone")
	CWMapFlagRiverwoodRef = TryToPlaceFlag(CWMapFlagRiverwoodRef, CWMapFlagRedRiverwood, CWMapFlagBlueRiverwood, RiverwoodLocation, "TownRiverwood")

	; Forts
	CWMapFlagFortSungardRef = TryToPlaceFlag(CWMapFlagFortSungardRef, CWMapFlagRedFortSungard, CWMapFlagBlueFortSungard, FortSungardLocation, "FortSungard")
	CWMapFlagFortGreymoorRef = TryToPlaceFlag(CWMapFlagFortGreymoorRef, CWMapFlagRedFortGreymoor, CWMapFlagBlueFortGreymoor, FortGreymoorLocation, "FortGreymoor")
	CWMapFlagFortHraggstadRef = TryToPlaceFlag(CWMapFlagFortHraggstadRef, CWMapFlagRedFortHraggstad, CWMapFlagBlueFortHraggstad, FortHraggstadLocation, "FortHraggstad")
	CWMapFlagFortDunstadRef = TryToPlaceFlag(CWMapFlagFortDunstadRef, CWMapFlagRedFortDunstad, CWMapFlagBlueFortDunstad, FortDunstadLocation, "FortDunstad")
	CWMapFlagFortKastavRef = TryToPlaceFlag(CWMapFlagFortKastavRef, CWMapFlagRedFortKastav, CWMapFlagBlueFortKastav, FortKastavLocation, "FortKastav")
	CWMapFlagFortAmolRef = TryToPlaceFlag(CWMapFlagFortAmolRef, CWMapFlagRedFortAmol, CWMapFlagBlueFortAmol, FortAmolLocation, "FortAmol")
	CWMapFlagFortGreenwallRef = TryToPlaceFlag(CWMapFlagFortGreenwallRef, CWMapFlagRedFortGreenwall, CWMapFlagBlueFortGreenwall, FortGreenwallLocation, "FortGreenwall")
	CWMapFlagFortNeugradRef = TryToPlaceFlag(CWMapFlagFortNeugradRef, CWMapFlagRedFortNeugrad, CWMapFlagBlueFortNeugrad, FortNeugradLocation, "FortNeugrad")
	CWMapFlagFortSnowhawkRef = TryToPlaceFlag(CWMapFlagFortSnowhawkRef, CWMapFlagRedFortSnowhawk, CWMapFlagBlueFortSnowhawk, FortSnowhawkLocation, "FortSnowhawk")

EndFunction

objectReference Function TryToPlaceFlag(ObjectReference RefOfFlag, Activator ActivatorOfFlagRed, Activator ActivatorOfFlagBlue, Location LocationOfFlag, String NodeOfFlag)
	if LocationOfFlag.GetKeywordData(CWOwner) == 1
		RefOfFlag = PlaceAtMe(ActivatorOfFlagRed)
		RefOfFlag.MoveToNode(self, NodeOfFlag)
	elseif LocationOfFlag.GetKeywordData(CWOwner) == 2
		RefOfFlag = PlaceAtMe(ActivatorOfFlagBlue)
		RefOfFlag.MoveToNode(self, NodeOfFlag)
	endif

	return RefOfFlag
EndFunction
