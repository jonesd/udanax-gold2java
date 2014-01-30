/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.nkernel;

import info.dgjones.abora.gold.be.basic.BeClub;
import info.dgjones.abora.gold.be.basic.BeGrandMap;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.basic.BeWork;
import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.be.locks.Lock;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.detect.FeWaitDetector;
import info.dgjones.abora.gold.filter.FilterSpace;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.lock.Encrypter;
import info.dgjones.abora.gold.nadmin.FeClubDescription;
import info.dgjones.abora.gold.nadmin.FeLockSmith;
import info.dgjones.abora.gold.nadmin.FeSession;
import info.dgjones.abora.gold.nkernel.FeClub;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.nkernel.FeKeyMaster;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.nkernel.FeServer;
import info.dgjones.abora.gold.nkernel.FeWork;
import info.dgjones.abora.gold.rcmain.ServerChunk;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.cross.CrossRegion;
import info.dgjones.abora.gold.spaces.cross.CrossSpace;
import info.dgjones.abora.gold.spaces.unordered.IDSpace;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.tumbler.SequenceRegion;
import info.dgjones.abora.gold.wrapper.FeWrapperSpec;
import info.dgjones.abora.gold.x.PrimPointerSpec;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Recipe;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * The fundamental Server object. Used for managing the global name space, creating Works,
 * Editions, and Clubs, and other general server management operations.
 * Many operations in the protocol use fluidly bound parameters. The possible parameters are:
 * FeServer defineClientFluid: #CurrentServer with: Listener emulsion with: [NULL].
 * CurrentKeyMaster - a KeyMaster for providing authority to read and/or edit
 * CurrentAuthor - the ID of the Club under whose name Work revisions are being done;
 * requires signature authority
 * InitialReadClub - the ID of the initial read Club of all newly created Works and Clubs
 * InitialEditClub - the ID of the initial edit Club of all newly created Works and Clubs
 * InitialOwner - the ID of the Club which owns newly created RangeElements
 * InitialSponsor - the ID of the Club which sponsors newly created Works and Clubs; requires
 * signature authority
 */
public class FeServer extends Heaper {

	protected Sequence myEncrypterName;
	protected Encrypter myEncrypter;
/*
udanax-top.st:22767:
Heaper subclass: #FeServer
	instanceVariableNames: '
		myEncrypterName {Sequence}
		myEncrypter {Encrypter}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nkernel'!
*/
/*
udanax-top.st:22773:
FeServer comment:
'The fundamental Server object. Used for managing the global name space, creating Works, Editions, and Clubs, and other general server management operations.
Many operations in the protocol use fluidly bound parameters. The possible parameters are:
	FeServer defineClientFluid: #CurrentServer with: Listener emulsion with: [NULL].
CurrentKeyMaster - a KeyMaster for providing authority to read and/or edit
CurrentAuthor - the ID of the Club under whose name Work revisions are being done; requires signature authority
InitialReadClub - the ID of the initial read Club of all newly created Works and Clubs
InitialEditClub - the ID of the initial edit Club of all newly created Works and Clubs
InitialOwner - the ID of the Club which owns newly created RangeElements
InitialSponsor - the ID of the Club which sponsors newly created Works and Clubs; requires signature authority'!
*/
/*
udanax-top.st:22785:
(FeServer getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #ON.CLIENT; add: #EQ; yourself)!
*/
/*
udanax-top.st:22829:
FeServer class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:22832:
(FeServer getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #ON.CLIENT; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeServer.class).setAttributes( new Set().add("CONCRETE").add("ONCLIENT").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Essential. A specification for arrays of pointers.
 */
public PrimPointerSpec pointerSpec() {
	return PrimSpec.pointer();
/*
udanax-top.st:22790:FeServer methodsFor: 'miscellaneous'!
{PrimPointerSpec} pointerSpec
	"Essential. A specification for arrays of pointers."
	
	^PrimSpec pointer!
*/
}
public FeServer(Sequence encrypterName, Encrypter encrypter) {
	super();
	myEncrypterName = encrypterName;
	myEncrypter = encrypter;
/*
udanax-top.st:22797:FeServer methodsFor: 'create'!
create: encrypterName {Sequence}
	with: encrypter {Encrypter}
	
	super create.
	myEncrypterName _ encrypterName.
	myEncrypter _ encrypter!
*/
}
/**
 * Return the Encrypter used for sending sensitive parameters to the Server. (e.g.
 * MatchLock::encryptedPassword ())
 */
public Encrypter encrypter() {
	return myEncrypter;
/*
udanax-top.st:22806:FeServer methodsFor: 'security'!
{Encrypter} encrypter
	"Return the Encrypter used for sending sensitive parameters to the Server. (e.g. MatchLock::encryptedPassword ())"
	
	^myEncrypter!
*/
}
/**
 * Essential. The encryption scheme to be used for sending sensitive parameters to the
 * Server. (e.g. MatchLock::encryptedPassword ())
 */
public Sequence getEncrypterName() {
	return myEncrypterName;
/*
udanax-top.st:22811:FeServer methodsFor: 'security'!
{Sequence} getEncrypterName
	"Essential. The encryption scheme to be used for sending sensitive parameters to the Server. (e.g. MatchLock::encryptedPassword ())"
	
	^myEncrypterName!
*/
}
/*
udanax-top.st:22818:FeServer methodsFor: 'smalltalk: defaults'!
{FeClubDescription} newClubDescription: membership {(FeSet of: FeClub) | NULL}
	^self newClubDescription: membership with: NULL!
*/
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:22824:FeServer methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:22826:FeServer methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static void cleanupGarbage() {
	linkTimeNonInherited();
/*
udanax-top.st:22837:FeServer class methodsFor: 'smalltalk: init'!
cleanupGarbage
	self linkTimeNonInherited!
*/
}
public static void exitTimeNonInherited() {
	CurrentServer.fluidSet(null);
/*
udanax-top.st:22841:FeServer class methodsFor: 'smalltalk: init'!
exitTimeNonInherited
	CurrentServer fluidSet: NULL!
*/
}
public static void linkTimeNonInherited() {
	Recipe.defineGlobal(FEBE_CUISINE, null);
/*
udanax-top.st:22845:FeServer class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	Recipe star defineGlobal: #FebeCuisine with: NULL.!
*/
}
public static void staticTimeNonInherited() {
	AboraSupport.defineFluid(FeServer.class, "CurrentServer", ServerChunk.emulsion(), null);
	AboraSupport.defineFluid(FeKeyMaster.class, "CurrentKeyMaster", ServerChunk.emulsion(), null);
	AboraSupport.defineFluid(ID.class, "CurrentAuthor", ServerChunk.emulsion(), null);
	AboraSupport.defineFluid(ID.class, "InitialReadClub", ServerChunk.emulsion(), null);
	AboraSupport.defineFluid(ID.class, "InitialEditClub", ServerChunk.emulsion(), null);
	AboraSupport.defineFluid(ID.class, "InitialOwner", ServerChunk.emulsion(), null);
	AboraSupport.defineFluid(ID.class, "InitialSponsor", ServerChunk.emulsion(), null);
/*
udanax-top.st:22848:FeServer class methodsFor: 'smalltalk: init'!
staticTimeNonInherited
	FeServer defineFluid: #CurrentServer with: ServerChunk emulsion with: [NULL].
	FeKeyMaster defineFluid: #CurrentKeyMaster with: ServerChunk emulsion with: [NULL].
	ID defineFluid: #CurrentAuthor with: ServerChunk emulsion with: [NULL].
	ID defineFluid: #InitialReadClub with: ServerChunk emulsion with: [NULL].
	ID defineFluid: #InitialEditClub with: ServerChunk emulsion with: [NULL].
	ID defineFluid: #InitialOwner with: ServerChunk emulsion with: [NULL].
	ID defineFluid: #InitialSponsor with: ServerChunk emulsion with: [NULL].!
*/
}
/**
 * Essential.  Assign a new global ID to a RangeElement. If NULL, then a new unique ID is
 * generated for it, and this requires no permissions. If an ID is supplied, the
 * CurrentKeyMaster must have been granted authority to assign this ID by the Adminer.
 * Returns the newly assigned ID.
 */
public static ID assignID(FeRangeElement range) {
	return assignID(range, null);
/*
udanax-top.st:22860:FeServer class methodsFor: 'smalltalk: defaults'!
{ID CLIENT} assignID: range {FeRangeElement}
	"Essential.  Assign a new global ID to a RangeElement. If NULL, then a new unique ID is generated for it, and this requires no permissions. If an ID is supplied, the CurrentKeyMaster must have been granted authority to assign this ID by the Adminer. Returns the newly assigned ID."
	 
	^self assignID: range with: NULL!
*/
}
/**
 * The Server object for the current connection to Xanadu
 * @deprecated
 */
public static FeServer current() {
	throw new PasseException();
/*
udanax-top.st:22867:FeServer class methodsFor: 'smalltalk: passe'!
{FeServer} current
	"The Server object for the current connection to Xanadu"
	"All messages should now be static, or go through the fluid variable."
	
	self passe!
*/
}
/**
 * @deprecated
 */
public static ID nullClubID() {
	throw new PasseException();
/*
udanax-top.st:22873:FeServer class methodsFor: 'smalltalk: passe'!
{ID} nullClubID
	self passe "emptyClubID"!
*/
}
/**
 * Looks up the ID of a named Club in the directory maintained by the System Admin Club.
 * Requires read permission on the directory. Blasts if there is no Club with that name.
 */
public static ID clubID(Sequence clubName) {
	return FeServer.iDOf((((FeWork) (FeServer.get(FeServer.clubDirectoryID()))).edition().get(clubName)));
/*
udanax-top.st:22879:FeServer class methodsFor: 'server library'!
{ID} clubID: clubName {Sequence}
	"Looks up the ID of a named Club in the directory maintained by the System Admin Club. Requires read permission on the directory. Blasts if there is no Club with that name."
	
	^FeServer iDOf: (((FeServer
		get: FeServer clubDirectoryID) cast: FeWork)
			edition get: clubName)!
*/
}
/**
 * Finds the name of a Club in the global directory maintained by the System Admin Club.
 * Blasts if there is no name for that Club, or if there is more than one. Requires read
 * permission on the clubDirectory Work
 */
public static Sequence clubName(ID iD) {
	FeWork club;
	club = (FeClub) (FeServer.get(iD));
	return (Sequence) (((FeWork) (FeServer.get(FeServer.clubDirectoryID()))).edition().keysOf(club)).theOne();
/*
udanax-top.st:22886:FeServer class methodsFor: 'server library'!
{Sequence} clubName: iD {ID}
	"Finds the name of a Club in the global directory maintained by the System Admin Club. Blasts if there is no name for that Club, or if there is more than one. Requires read permission on the clubDirectory Work"
	
	| club {FeWork} |
	club := (FeServer get: iD) cast: FeClub.
	^(((FeServer
		get: FeServer clubDirectoryID) cast: FeWork)
			edition keysOf: club) theOne cast: Sequence!
*/
}
/**
 * The names of all global Clubs. Requires read permission on the clubDirectory Work
 */
public static SequenceRegion clubNames() {
	return (SequenceRegion) ((FeWork) (FeServer.get(FeServer.clubDirectoryID()))).edition().domain();
/*
udanax-top.st:22895:FeServer class methodsFor: 'server library'!
{SequenceRegion} clubNames
	"The names of all global Clubs. Requires read permission on the clubDirectory Work"
	
	^((FeServer get: FeServer clubDirectoryID) cast: FeWork) edition domain cast: SequenceRegion!
*/
}
/**
 * Disable login access to a Club, by revoking its direct membership of the System Access
 * Club
 */
public static void disableAccess(ID clubID) {
	FeClub club;
	FeClubDescription desc;
	Ravi.thingToDo();
	/* kill outstanding KeyMasters */
	club = (FeClub) (FeServer.get(FeServer.accessClubID()));
	desc = (FeClubDescription) (FeClubDescription.spec().wrap(club.edition()));
	club.grab();
	club.revise((desc.withMembership((desc.membership().without(((FeClub) (FeServer.get(clubID))))))).edition());
	club.release();
/*
udanax-top.st:22900:FeServer class methodsFor: 'server library'!
{void} disableAccess: clubID {ID}
	"Disable login access to a Club, by revoking its direct membership of the System Access Club"
	
	| club {FeClub} desc {FeClubDescription} |
	Ravi thingToDo. "kill outstanding KeyMasters"
	club := (FeServer get: FeServer accessClubID) cast: FeClub.
	desc := (FeClubDescription spec wrap: club edition) cast: FeClubDescription.
	club grab.
	club revise: (desc withMembership: (desc membership
		without: ((FeServer get: clubID) cast: FeClub))) edition.
	club release!
*/
}
/**
 * Enable login access to a Club, by listing it as a direct member of the System Access Club
 */
public static void enableAccess(ID clubID) {
	FeClub club;
	FeClubDescription desc;
	club = (FeClub) (FeServer.get(FeServer.accessClubID()));
	desc = (FeClubDescription) (FeClubDescription.spec().wrap(club.edition()));
	club.grab();
	club.revise((desc.withMembership((desc.membership().with(((FeClub) (FeServer.get(clubID))))))).edition());
	club.release();
/*
udanax-top.st:22912:FeServer class methodsFor: 'server library'!
{void} enableAccess: clubID {ID}
	"Enable login access to a Club, by listing it as a direct member of the System Access Club"
	
	| club {FeClub} desc {FeClubDescription} |
	club := (FeServer get: FeServer accessClubID) cast: FeClub.
	desc := (FeClubDescription spec wrap: club edition) cast: FeClubDescription.
	club grab.
	club revise: (desc withMembership: (desc membership
		with: ((FeServer get: clubID) cast: FeClub))) edition.
	club release!
*/
}
/**
 * The CoordinateSpace used for filtering endorsements on this Server. Equivalent to
 * this->filterSpace (this->endorsementSpace ())
 */
public static FilterSpace endorsementFilterSpace() {
	Someone.thingToDo();
	/* This should go in CrossSpace */
	return ((BeGrandMap) CurrentGrandMap.fluidGet()).endorsementFilterSpace();
/*
udanax-top.st:22923:FeServer class methodsFor: 'server library'!
{FilterSpace} endorsementFilterSpace
	"The CoordinateSpace used for filtering endorsements on this Server. Equivalent to
		this->filterSpace (this->endorsementSpace ())"
	
	self thingToDo.  "This should go in CrossSpace"
	^CurrentGrandMap fluidGet endorsementFilterSpace!
*/
}
/**
 * A set of endorsements for each Club endorsing with each token
 */
public static CrossRegion endorsementRegion(IDRegion clubs, IDRegion tokens) {
	Someone.thingToDo();
	/* This should go in CrossSpace */
	return FeServer.endorsementSpace().crossOfRegions(((PtrArray) (PrimSpec.pointer().arrayWithTwo(clubs, tokens))));
/*
udanax-top.st:22930:FeServer class methodsFor: 'server library'!
{CrossRegion of: IDRegion and: IDRegion}
	endorsementRegion: clubs {IDRegion | NULL}
	with: tokens {IDRegion | NULL}
	"A set of endorsements for each Club endorsing with each token"
	
	self thingToDo.  "This should go in CrossSpace"
	^FeServer endorsementSpace crossOfRegions: ((PrimSpec pointer
		arrayWithTwo: clubs with: tokens) cast: PtrArray)!
*/
}
/**
 * A set of endorsements for each Club endorsing with each token
 */
public static CrossSpace endorsementSpace() {
	Someone.thingToDo();
	/* This should go in CrossSpace */
	return ((BeGrandMap) CurrentGrandMap.fluidGet()).endorsementSpace();
/*
udanax-top.st:22939:FeServer class methodsFor: 'server library'!
{CrossSpace of: IDSpace and: IDSpace} endorsementSpace
	"A set of endorsements for each Club endorsing with each token"
	
	self thingToDo.  "This should go in CrossSpace"
	^CurrentGrandMap fluidGet endorsementSpace!
*/
}
/**
 * The Work mapping names to global Club Works
 */
public static FeWork globalClubs() {
	return (FeWork) (FeServer.get(FeServer.clubDirectoryID()));
/*
udanax-top.st:22945:FeServer class methodsFor: 'server library'!
{FeWork} globalClubs
	"The Work mapping names to global Club Works"
	
	^(FeServer get: FeServer clubDirectoryID) cast: FeWork!
*/
}
/**
 * Return true if the current session has successfully logged into the Server yet.
 */
public static boolean isAdmitted() {
	AboraSupport.translateOnly();
	{
		Dean.thingToDo();
	}
	return true;
/*
udanax-top.st:22950:FeServer class methodsFor: 'server library'!
{BooleanVar} isAdmitted
	"Return true if the current session has successfully logged into the Server yet."
	
	[Dean thingToDo] translateOnly.
	^true!
*/
}
/**
 * Add a Club to the global list of club names. Blasts if there is already a Club by that
 * name.
 */
public static void nameClub(Sequence clubName, ID clubID) {
	FeWork clubNames;
	FeWork club;
	clubNames = FeServer.globalClubs();
	clubNames.grab();
	try {
		if (clubNames.edition().includesKey(clubName)) {
			throw new AboraRuntimeException(AboraRuntimeException.CLUB_NAME_IN_USE);
		}
		club = (FeClub) (FeServer.get(clubID));
		if ( ! ((clubNames.edition().keysOf(club)).isEmpty())) {
			throw new AboraRuntimeException(AboraRuntimeException.CLUB_ALREADY_NAMED);
		}
		clubNames.revise((clubNames.edition().with(clubName, club)));
	}
	finally {
		FeWork.bombReleaseWork(clubNames);
	}
/*
udanax-top.st:22956:FeServer class methodsFor: 'server library'!
{void} nameClub: clubName {Sequence} with: clubID {ID}
	"Add a Club to the global list of club names. Blasts if there is already a Club by that name."
	
	| clubNames {FeWork} club {FeWork} |
	clubNames := FeServer globalClubs.
	clubNames grab.
		[(clubNames edition includesKey: clubName) ifTrue:
			[Heaper BLAST: #ClubNameInUse].
		club := (FeServer get: clubID) cast: FeClub.
		(clubNames edition keysOf: club) isEmpty ifFalse:
			[Heaper BLAST: #ClubAlreadyNamed].
		clubNames revise: (clubNames edition with: clubName with: club)]
	valueNowOrOnUnwindDo:
		(FeWork bomb.ReleaseWork: clubNames)!
*/
}
/**
 * Changes the name of an existing Club. Blasts if there is no Club with the old name, or
 * there already is a Club with the new name.
 */
public static void renameClub(Sequence oldName, Sequence newName) {
	FeWork names;
	names = FeServer.globalClubs();
	names.grab();
	try {
		if ( ! (names.edition().includesKey(oldName))) {
			throw new AboraRuntimeException(AboraRuntimeException.NO_SUCH_CLUB);
		}
		if (names.edition().includesKey(newName)) {
			throw new AboraRuntimeException(AboraRuntimeException.CLUB_NAME_IN_USE);
		}
		names.revise(((names.edition().without(oldName)).with(newName, (names.edition().get(oldName)))));
	}
	finally {
		FeWork.bombReleaseWork(names);
	}
/*
udanax-top.st:22971:FeServer class methodsFor: 'server library'!
{void} renameClub: oldName {Sequence} with: newName {Sequence}
	"Changes the name of an existing Club. Blasts if there is no Club with the old name, or there already is a Club with the new name."
	
	| names {FeWork} |
	names := FeServer globalClubs.
	names grab.
		[(names edition includesKey: oldName) ifFalse:
			[Heaper BLAST: #NoSuchClub].
		(names edition includesKey: newName) ifTrue:
			[Heaper BLAST: #ClubNameInUse].
		names revise: ((names edition without: oldName)
			with: newName with: (names edition get: oldName))]
	valueNowOrOnUnwindDo:
		(FeWork bomb.ReleaseWork: names)!
*/
}
/**
 * Removes a naming for a Club. Blasts if there is no Club by that clubName.
 */
public static void unnameClub(Sequence clubName) {
	FeWork clubNames;
	clubNames = FeServer.globalClubs();
	clubNames.grab();
	try {
		if (clubNames.edition().includesKey(clubName)) {
			throw new AboraRuntimeException(AboraRuntimeException.NO_SUCH_CLUB);
		}
		clubNames.revise((clubNames.edition().without(clubName)));
	}
	finally {
		FeWork.bombReleaseWork(clubNames);
	}
/*
udanax-top.st:22986:FeServer class methodsFor: 'server library'!
{void} unnameClub: clubName {Sequence}
	"Removes a naming for a Club. Blasts if there is no Club by that clubName."
	
	| clubNames {FeWork} |
	clubNames := FeServer globalClubs.
	clubNames grab.
		[(clubNames edition includesKey: clubName) ifTrue:
			[Heaper BLAST: #NoSuchClub].
		clubNames revise: (clubNames edition without: clubName)]
	valueNowOrOnUnwindDo:
		(FeWork bomb.ReleaseWork: clubNames)!
*/
}
/**
 * Get the receiver for wire requests.
 */
public static FeServer implicitReceiver() {
	return ((FeServer) CurrentServer.fluidGet());
/*
udanax-top.st:23000:FeServer class methodsFor: 'create'!
{FeServer} implicitReceiver
	"Get the receiver for wire requests."
	^CurrentServer fluidGet!
*/
}
public static FeServer make() {
	Encrypter encrypter;
	FeServer result;
	Ravi.thingToDo();
	/* use a real Encrypter */
	Ravi.hack();
	/* to force wrappers to be initialized */
	FeWrapperSpec.get((Sequence.string("Wrapper")));
	encrypter = Encrypter.make((Sequence.string("NoEncrypter")));
	encrypter.randomizeKeys((UInt8Array.string("hello")));
	result = new FeServer((Sequence.string("NoEncrypter")), encrypter);
	CurrentServer.fluidSet(result);
	return ((FeServer) CurrentServer.fluidGet());
/*
udanax-top.st:23004:FeServer class methodsFor: 'create'!
make
	| encrypter {Encrypter}  result {FeServer} |
	Ravi thingToDo. "use a real Encrypter"
	Ravi hack. "to force wrappers to be initialized"
	FeWrapperSpec get: (Sequence string: 'Wrapper').
	encrypter := Encrypter make: (Sequence string: 'NoEncrypter').
	encrypter randomizeKeys: (UInt8Array string: 'hello').
	result _ self create: (Sequence string: 'NoEncrypter') with: encrypter.
	CurrentServer fluidSet: result.
	^CurrentServer fluidGet!
*/
}
/**
 * {ID CLIENT} accessClubID
 * {ID CLIENT} adminClubID
 * {FeAdminer CLIENT} adminer
 * {ID CLIENT} archiveClubID
 * {FeArchiver CLIENT} archiver
 * {ID CLIENT} assignID: range {FeRangeElement} with: iD {ID default: NULL}
 * {ID CLIENT} clubDirectoryID
 * {CrossSpace CLIENT} crossSpace: subSpaces {PtrArray of: CoordinateSpace}
 * {IntegerVar CLIENT} currentTime
 * {FilterSpace CLIENT} endorsementFilterSpace
 * {CrossRegion CLIENT of: IDRegion and: IDRegion} endorsementRegion: clubs {IDRegion | NULL}
 * with: tokens {IDRegion | NULL}
 * {CrossSpace CLIENT of: IDSpace and: IDSpace} endorsementSpace
 * {FilterSpace CLIENT} filterSpace: baseSpace {CoordinateSpace}
 * {FeRangeElement CLIENT} get: iD {ID}
 * {Sequence CLIENT} identifier
 * {ID CLIENT} iDOf: value {FeRangeElement}
 * {IDRegion CLIENT} iDsOf: value {FeRangeElement}
 * {IDRegion CLIENT} iDsOfRange: edition {FeEdition}
 * {PrimFloatSpec CLIENT} iEEESpec: precision {Int32}
 * {ID CLIENT} importID: data {UInt8Array}
 * {IDRegion CLIENT} importIDRegion: data {UInt8Array}
 * {IDSpace CLIENT} importIDSpace: data {UInt8Array}
 * {IntegerSpace CLIENT} integerSpace
 * {FeBooLockSmith CLIENT} newBooLockSmith
 * {FeChallengeLockSmith CLIENT} newChallengeLockSmith: publicKey {UInt8Array} with:
 * encrypterName {PrimIntegerArray}
 * {FeClub CLIENT} newClub: description {FeEdition}
 * {FeClubDescription CLIENT} newClubDescription: membership {(FeSet of: FeClub)
 * | NULL} with: lockSmith {FeLockSmith default: NULL}
 * {FeClubDescription CLIENT} newClubDescription: members {FeWorkSet} with: lockSmith
 * {FeLockSmith} with: home {FeWork | NULL}
 * {FeDataHolder CLIENT} newDataHolder: value {PrimValue}
 * {FeEdition CLIENT} newEdition: values {PrimArray of: FeRangeElement} with: positions
 * {XuRegion default: NULL} with: ordering {OrderSpec default: NULL}
 * {FeEdition CLIENT} newEditionWith: position {Position} with: value {FeRangeElement}
 * {FeEdition CLIENT} newEditionWithAll: domain {XuRegion} with: value {FeRangeElement}
 * {FeEdition CLIENT} newEmptyEdition: cs {CoordinateSpace}
 * {FeHyperLink CLIENT} newHyperLink: types {(FeSet of: FeWork)
 * default: NULL} with: leftEnd {FeHyperRef default: NULL} with: rightEnd {FeHyperRef
 * default: NULL}
 * {ID CLIENT} newID
 * {FeIDHolder CLIENT} newIDHolder: iD {ID}
 * {IDSpace CLIENT} newIDSpace
 * {FeLabel CLIENT} newLabel
 * {FeMatchLockSmith CLIENT} newMatchLockSmith: scrambledPassword {UInt8Array} with:
 * scramblerName {PrimIntegerArray}
 * {FeMultiLockSmith CLIENT} newMultiLockSmith
 * {FeMultiRef CLIENT} newMultiRef: refs {(PtrArray of: FeHyperRef)
 * default: NULL} with: workContext {FeWork default: NULL} with: originalContext {FeWork
 * default: NULL} with: pathContext {FePath default: NULL}
 * {FePath CLIENT} newPath: labels {(PtrArray of: FeLabel)
 * default: NULL}
 * {FeRangeElement CLIENT} newPlaceHolder
 * {FeEdition CLIENT} newPlaceHolders: domain {XuRegion}
 * {FeSet CLIENT} newSet: values {(PtrArray of: FeRangeElement)
 * default: NULL}
 * {FeSingleRef CLIENT} newSingleRef: excerpt {FeEdition | NULL} with: workContext {FeWork
 * default: NULL} with: originalContext {FeWork default: NULL} with: pathContext {FePath
 * default: NULL}
 * {FeText CLIENT} newText: data {PrimArray.X default: NULL}
 * {FeWallLockSmith CLIENT} newWallLockSmith
 * {FeWork CLIENT} newWork: contents {FeEdition}
 * {ID CLIENT} nullClubID
 * {PrimPointerSpec CLIENT} pointerSpec
 * {ID CLIENT} publicClubID
 * {FeKeyMaster CLIENT} publicKeyMaster
 * {RealSpace CLIENT} realSpace
 * {FeSession CLIENT} session
 * {void CLIENT} waitForConsequences: detector {PrWaitDetector}
 * {void CLIENT} waitForWrite: detector {PrWaitDetector}
 * {FeWrapperSpec CLIENT} wrapperSpec: name {Sequence}
 */
public static void infostProtocol() {
/*
udanax-top.st:23017:FeServer class methodsFor: 'smalltalk: system'!
info.stProtocol
"{ID CLIENT} accessClubID
{ID CLIENT} adminClubID
{FeAdminer CLIENT} adminer
{ID CLIENT} archiveClubID
{FeArchiver CLIENT} archiver
{ID CLIENT} assignID: range {FeRangeElement} with: iD {ID default: NULL}
{ID CLIENT} clubDirectoryID
{CrossSpace CLIENT} crossSpace: subSpaces {PtrArray of: CoordinateSpace}
{IntegerVar CLIENT} currentTime
{FilterSpace CLIENT} endorsementFilterSpace
{CrossRegion CLIENT of: IDRegion and: IDRegion} endorsementRegion: clubs {IDRegion | NULL} with: tokens {IDRegion | NULL}
{CrossSpace CLIENT of: IDSpace and: IDSpace} endorsementSpace
{FilterSpace CLIENT} filterSpace: baseSpace {CoordinateSpace}
{FeRangeElement CLIENT} get: iD {ID}
{Sequence CLIENT} identifier
{ID CLIENT} iDOf: value {FeRangeElement}
{IDRegion CLIENT} iDsOf: value {FeRangeElement}
{IDRegion CLIENT} iDsOfRange: edition {FeEdition}
{PrimFloatSpec CLIENT} iEEESpec: precision {Int32}
{ID CLIENT} importID: data {UInt8Array}
{IDRegion CLIENT} importIDRegion: data {UInt8Array}
{IDSpace CLIENT} importIDSpace: data {UInt8Array}
{IntegerSpace CLIENT} integerSpace
{FeBooLockSmith CLIENT} newBooLockSmith
{FeChallengeLockSmith CLIENT} newChallengeLockSmith: publicKey {UInt8Array} with: encrypterName {PrimIntegerArray}
{FeClub CLIENT} newClub: description {FeEdition}
{FeClubDescription CLIENT} newClubDescription: membership {(FeSet of: FeClub)
		| NULL} with: lockSmith {FeLockSmith default: NULL}
{FeClubDescription CLIENT} newClubDescription: members {FeWorkSet} with: lockSmith {FeLockSmith} with: home {FeWork | NULL}
{FeDataHolder CLIENT} newDataHolder: value {PrimValue}
{FeEdition CLIENT} newEdition: values {PrimArray of: FeRangeElement} with: positions {XuRegion default: NULL} with: ordering {OrderSpec default: NULL}
{FeEdition CLIENT} newEditionWith: position {Position} with: value {FeRangeElement}
{FeEdition CLIENT} newEditionWithAll: domain {XuRegion} with: value {FeRangeElement}
{FeEdition CLIENT} newEmptyEdition: cs {CoordinateSpace}
{FeHyperLink CLIENT} newHyperLink: types {(FeSet of: FeWork)
		default: NULL} with: leftEnd {FeHyperRef default: NULL} with: rightEnd {FeHyperRef default: NULL}
{ID CLIENT} newID
{FeIDHolder CLIENT} newIDHolder: iD {ID}
{IDSpace CLIENT} newIDSpace
{FeLabel CLIENT} newLabel
{FeMatchLockSmith CLIENT} newMatchLockSmith: scrambledPassword {UInt8Array} with: scramblerName {PrimIntegerArray}
{FeMultiLockSmith CLIENT} newMultiLockSmith
{FeMultiRef CLIENT} newMultiRef: refs {(PtrArray of: FeHyperRef)
		default: NULL} with: workContext {FeWork default: NULL} with: originalContext {FeWork default: NULL} with: pathContext {FePath default: NULL}
{FePath CLIENT} newPath: labels {(PtrArray of: FeLabel)
		default: NULL}
{FeRangeElement CLIENT} newPlaceHolder
{FeEdition CLIENT} newPlaceHolders: domain {XuRegion}
{FeSet CLIENT} newSet: values {(PtrArray of: FeRangeElement)
		default: NULL}
{FeSingleRef CLIENT} newSingleRef: excerpt {FeEdition | NULL} with: workContext {FeWork default: NULL} with: originalContext {FeWork default: NULL} with: pathContext {FePath default: NULL}
{FeText CLIENT} newText: data {PrimArray.X default: NULL}
{FeWallLockSmith CLIENT} newWallLockSmith
{FeWork CLIENT} newWork: contents {FeEdition}
{ID CLIENT} nullClubID
{PrimPointerSpec CLIENT} pointerSpec
{ID CLIENT} publicClubID
{FeKeyMaster CLIENT} publicKeyMaster
{RealSpace CLIENT} realSpace
{FeSession CLIENT} session
{void CLIENT} waitForConsequences: detector {PrWaitDetector}
{void CLIENT} waitForWrite: detector {PrWaitDetector}
{FeWrapperSpec CLIENT} wrapperSpec: name {Sequence}
"!
*/
}
/**
 * Essential.  The ID of the System Access Club.
 */
public static ID accessClubID() {
	return ((BeGrandMap) CurrentGrandMap.fluidGet()).accessClubID();
/*
udanax-top.st:23085:FeServer class methodsFor: 'managing clubs'!
{ID CLIENT} accessClubID
	"Essential.  The ID of the System Access Club."
	^CurrentGrandMap fluidGet accessClubID!
*/
}
/**
 * Essential.  The ID of the System Admin Club.
 */
public static ID adminClubID() {
	return ((BeGrandMap) CurrentGrandMap.fluidGet()).adminClubID();
/*
udanax-top.st:23090:FeServer class methodsFor: 'managing clubs'!
{ID CLIENT} adminClubID
	"Essential.  The ID of the System Admin Club."
	^CurrentGrandMap fluidGet adminClubID!
*/
}
/**
 * Essential.  The ID of the System Archive Club.
 */
public static ID archiveClubID() {
	Someone.knownBug();
	/* logging into this Club does not actually give you full read/edit authority */
	return ((BeGrandMap) CurrentGrandMap.fluidGet()).archiveClubID();
/*
udanax-top.st:23095:FeServer class methodsFor: 'managing clubs'!
{ID CLIENT} archiveClubID
	"Essential.  The ID of the System Archive Club."
	self knownBug. "logging into this Club does not actually give you full read/edit authority"
	^CurrentGrandMap fluidGet archiveClubID!
*/
}
/**
 * Essential.  The ID of the Universal Empty Club.
 */
public static ID emptyClubID() {
	return ((BeGrandMap) CurrentGrandMap.fluidGet()).emptyClubID();
/*
udanax-top.st:23101:FeServer class methodsFor: 'managing clubs'!
{ID CLIENT} emptyClubID
	"Essential.  The ID of the Universal Empty Club."
	^CurrentGrandMap fluidGet emptyClubID!
*/
}
/**
 * Essential. The encryption scheme to be used for sending sensitive parameters to the
 * Server. (e.g. MatchLock::encryptedPassword ())
 */
public static Sequence encrypterName() {
	return ((FeServer) CurrentServer.fluidGet()).getEncrypterName();
/*
udanax-top.st:23106:FeServer class methodsFor: 'managing clubs'!
{Sequence CLIENT login} encrypterName
	"Essential. The encryption scheme to be used for sending sensitive parameters to the Server. (e.g. MatchLock::encryptedPassword ())"
	
	^CurrentServer fluidGet getEncrypterName!
*/
}
/**
 * Essential.  Return a lock which, if satisfied, will give a KeyMaster logged in to that
 * Club. It will be able to exercise the authority of all of its superClubs.
 * The club must be in the System Access Club or another club must have been logged in during
 * this session.
 * If that doesn't hold, or there is no such club, returns the gateLockSpec chosen by the
 * Administrator if there is no such Club
 */
public static Lock login(ID clubID) {
	BeClub club;
	BeGrandMap cgm;
	Ravi.thingToDo();
	/* Check this please. */
	cgm = ((BeGrandMap) CurrentGrandMap.fluidGet());
	club = cgm.fetchClub(clubID);
	if (club != null && (FeSession.current().isLoggedIn() || ((cgm.getClub(FeServer.accessClubID())).membershipIncludes(club)))) {
		return ((FeClubDescription) (FeClubDescription.spec().wrap(club.edition()))).lockSmith().newLock(clubID);
	}
	else {
		return FeServer.gateLockSmith().newLock(null);
	}
/*
udanax-top.st:23111:FeServer class methodsFor: 'managing clubs'!
{Lock CLIENT login} login: clubID {ID}
	"Essential.  Return a lock which, if satisfied, will give a KeyMaster logged in to that Club. It will be able to exercise the authority of all of its superClubs.
	 The club must be in the System Access Club or another club must have been logged in during this session.
	 If that doesn't hold, or there is no such club, returns the gateLockSpec chosen by the Administrator if there is no such Club"
	
	| club {BeClub} cgm {BeGrandMap} |
	Ravi thingToDo.  "Check this please."
	cgm := CurrentGrandMap fluidGet.
	club _ cgm fetchClub: clubID.
	(club ~~ NULL
		and: [FeSession current isLoggedIn
			or: [(cgm getClub: FeServer accessClubID) membershipIncludes: club]])
		ifTrue: [^((FeClubDescription spec wrap: club edition) cast: FeClubDescription)
						lockSmith newLock: clubID]
		ifFalse: [^FeServer gateLockSmith newLock: NULL]!
*/
}
/**
 * Essential.  Return a lock which, if satisfied, will give a KeyMaster logged in to the
 * named Club. It will be able to exercise the authority of all of its superClubs.
 * The club must be in the System Access Club or another club must have been logged in during
 * this session.
 * If that doesn't hold, or there is no such club, returns the gateLockSpec chosen by the
 * Administrator if there is no such Club
 */
public static Lock loginByName(Sequence clubName) {
	BeClub club;
	BeGrandMap cgm;
	Ravi.thingToDo();
	/* Check this please. */
	cgm = ((BeGrandMap) CurrentGrandMap.fluidGet());
	Heaper cast1 = (((BeWork) (cgm.get(cgm.clubDirectoryID()))).edition().fetch(clubName));
	if (cast1 instanceof FeClub) {
		FeClub feclub = (FeClub) cast1;
		club = feclub.beClub();
	}
	else {
		club = null;
	}
	if (club != null && (FeSession.current().isLoggedIn() || ((cgm.getClub(FeServer.accessClubID())).membershipIncludes(club)))) {
		return ((FeClubDescription) (FeClubDescription.spec().wrap(club.edition()))).lockSmith().newLock((cgm.iDOf(club)));
	}
	else {
		return FeServer.gateLockSmith().newLock(null);
	}
/*
udanax-top.st:23127:FeServer class methodsFor: 'managing clubs'!
{Lock CLIENT login} loginByName: clubName {Sequence}
	"Essential.  Return a lock which, if satisfied, will give a KeyMaster logged in to the named Club. It will be able to exercise the authority of all of its superClubs.
		 The club must be in the System Access Club or another club must have been logged in during this session.
	 If that doesn't hold, or there is no such club, returns the gateLockSpec chosen by the Administrator if there is no such Club"
	| club {BeClub} cgm {BeGrandMap} |
	Ravi thingToDo.  "Check this please."
	cgm := CurrentGrandMap fluidGet.
	(((cgm get: cgm clubDirectoryID) 
			cast: BeWork) edition fetch: clubName) 
		cast: FeClub into: [:feclub | club _ feclub beClub] others: [club _ NULL].
	(club ~~ NULL
		and: [FeSession current isLoggedIn
			or: [(cgm getClub: FeServer accessClubID) membershipIncludes: club]])
		ifTrue: [^((FeClubDescription spec wrap: club edition) cast: FeClubDescription)
						lockSmith newLock: (cgm iDOf: club)]
		ifFalse: [^FeServer gateLockSmith newLock: NULL]!
*/
}
/**
 * Essential.  The ID of the Universal Public Club.
 */
public static ID publicClubID() {
	return ((BeGrandMap) CurrentGrandMap.fluidGet()).publicClubID();
/*
udanax-top.st:23145:FeServer class methodsFor: 'managing clubs'!
{ID CLIENT} publicClubID
	"Essential.  The ID of the Universal Public Club."
	^CurrentGrandMap fluidGet publicClubID!
*/
}
/**
 * Essential. The public key to be used for sending sensitive parameters to the Server. (e.g.
 * MatchLock::encryptedPassword ())
 */
public static UInt8Array publicKey() {
	return ((FeServer) CurrentServer.fluidGet()).encrypter().publicKey();
/*
udanax-top.st:23150:FeServer class methodsFor: 'managing clubs'!
{UInt8Array CLIENT login} publicKey
	"Essential. The public key to be used for sending sensitive parameters to the Server. (e.g. MatchLock::encryptedPassword ())"
	
	^CurrentServer fluidGet encrypter publicKey!
*/
}
/**
 * Flush the Server's output buffers.
 */
public static void force() {
	Dean.shouldImplement();
/*
udanax-top.st:23157:FeServer class methodsFor: 'comm requests'!
{NOACK CLIENT login} force
	"Flush the Server's output buffers."
	
	Dean shouldImplement!
*/
}
/**
 * Set the Server side fluid for the CurrentAuthor.
 */
public static void setCurrentAuthor(ID iD) {
	CurrentAuthor.fluidSet(iD);
/*
udanax-top.st:23162:FeServer class methodsFor: 'comm requests'!
{NOACK CLIENT} setCurrentAuthor: iD {ID}
	"Set the Server side fluid for the CurrentAuthor."
	
	CurrentAuthor fluidSet: iD!
*/
}
/**
 * Set the Server side fluid for the CurrentKeyMaster.
 */
public static void setCurrentKeyMaster(FeKeyMaster km) {
	CurrentKeyMaster.fluidSet(km);
/*
udanax-top.st:23167:FeServer class methodsFor: 'comm requests'!
{NOACK CLIENT} setCurrentKeyMaster: km {FeKeyMaster}
	"Set the Server side fluid for the CurrentKeyMaster."
	
	CurrentKeyMaster fluidSet: km!
*/
}
/**
 * Set the Server side fluid for the InitialEditClub.
 */
public static void setInitialEditClub(ID iD) {
	InitialEditClub.fluidSet(iD);
/*
udanax-top.st:23172:FeServer class methodsFor: 'comm requests'!
{NOACK CLIENT} setInitialEditClub: iD {ID}
	"Set the Server side fluid for the InitialEditClub."
	
	InitialEditClub fluidSet: iD!
*/
}
/**
 * Set the Server side fluid for the InitialOwner.
 */
public static void setInitialOwner(ID iD) {
	InitialOwner.fluidSet(iD);
/*
udanax-top.st:23177:FeServer class methodsFor: 'comm requests'!
{NOACK CLIENT} setInitialOwner: iD {ID}
	"Set the Server side fluid for the InitialOwner."
	
	InitialOwner fluidSet: iD!
*/
}
/**
 * Set the Server side fluid for the InitialReadClub.
 */
public static void setInitialReadClub(ID iD) {
	InitialReadClub.fluidSet(iD);
/*
udanax-top.st:23182:FeServer class methodsFor: 'comm requests'!
{NOACK CLIENT} setInitialReadClub: iD {ID}
	"Set the Server side fluid for the InitialReadClub."
	
	InitialReadClub fluidSet: iD!
*/
}
/**
 * Set the Server side fluid for the InitialSponsor.
 */
public static void setInitialSponsor(ID iD) {
	InitialSponsor.fluidSet(iD);
/*
udanax-top.st:23187:FeServer class methodsFor: 'comm requests'!
{NOACK CLIENT} setInitialSponsor: iD {ID}
	"Set the Server side fluid for the InitialSponsor."
	
	InitialSponsor fluidSet: iD!
*/
}
/**
 * Essential.  Assign a new global ID to a RangeElement. If NULL, then a new unique ID is
 * generated for it, and this requires no permissions. If an ID is supplied, the
 * CurrentKeyMaster must have been granted authority to assign this ID by the Adminer.
 * Returns the newly assigned ID.
 */
public static ID assignID(FeRangeElement range, ID iD) {
	BeGrandMap gm;
	gm = ((BeGrandMap) CurrentGrandMap.fluidGet());
	if (iD == null) {
		return gm.assignID(range.getOrMakeBe());
	}
	if ( ! (((FeKeyMaster) CurrentKeyMaster.fluidGet()).hasAuthority((gm.grantAt(iD))))) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_HAVE_BEEN_GRANTED_AUTHORITY);
	}
	if ( ! (gm.tryIntroduce(iD, range.getOrMakeBe()))) {
		throw new AboraRuntimeException(AboraRuntimeException.IDALREADY_ASSIGNED);
	}
	return iD;
/*
udanax-top.st:23194:FeServer class methodsFor: 'global ids'!
{ID CLIENT} assignID: range {FeRangeElement} with: iD {ID default: NULL}
	"Essential.  Assign a new global ID to a RangeElement. If NULL, then a new unique ID is generated for it, and this requires no permissions. If an ID is supplied, the CurrentKeyMaster must have been granted authority to assign this ID by the Adminer. Returns the newly assigned ID."
	 
	| gm {BeGrandMap} |
	gm _ CurrentGrandMap fluidGet.
	iD == NULL ifTrue:
		[^gm assignID: range getOrMakeBe].
	(CurrentKeyMaster fluidGet hasAuthority: (gm grantAt: iD)) ifFalse:
		[Heaper BLAST: #MustHaveBeenGrantedAuthority].
	(gm at: iD tryIntroduce: range getOrMakeBe) ifFalse:
		[Heaper BLAST: #IDAlreadyAssigned].
	^iD!
*/
}
/**
 * The ID of a Work mapping Club names to FeClubs
 */
public static ID clubDirectoryID() {
	return ((BeGrandMap) CurrentGrandMap.fluidGet()).clubDirectoryID();
/*
udanax-top.st:23207:FeServer class methodsFor: 'global ids'!
{ID CLIENT} clubDirectoryID
	"The ID of a Work mapping Club names to FeClubs"
	
	^CurrentGrandMap fluidGet clubDirectoryID!
*/
}
/**
 * Essential.  Get the object associated with the given global ID. Typically, it will be a
 * Work. Blast if there is nothing there
 */
public static FeRangeElement get(ID iD) {
	return ((BeGrandMap) CurrentGrandMap.fluidGet()).getFe(iD);
/*
udanax-top.st:23212:FeServer class methodsFor: 'global ids'!
{FeRangeElement CLIENT} get: iD {ID}
	"Essential.  Get the object associated with the given global ID. Typically, it will be a Work. Blast if there is nothing there"
	
	^CurrentGrandMap fluidGet getFe: iD!
*/
}
/**
 * Find the unique global ID on this Server that has been assigned to this RangeElement.
 * Blast if there is none, or more than one.
 * Equivalent to
 * CAST(ID, this->iDsOf (value)->theOne ())
 */
public static ID iDOf(FeRangeElement value) {
	BeRangeElement be;
	be = value.fetchBe();
	if (be == null) {
		throw new AboraRuntimeException(AboraRuntimeException.DOES_NOT_HAVE_AN_ID);
	}
	else {
		return ((BeGrandMap) CurrentGrandMap.fluidGet()).iDOf(be);
	}
/*
udanax-top.st:23217:FeServer class methodsFor: 'global ids'!
{ID CLIENT} iDOf: value {FeRangeElement}
	"Find the unique global ID on this Server that has been assigned to this RangeElement. Blast if there is none, or more than one.
	Equivalent to
		CAST(ID, this->iDsOf (value)->theOne ())"
	
	| be {BeRangeElement} |
	be := value fetchBe.
	be == NULL 
		ifTrue: [Heaper BLAST: #DoesNotHaveAnID.  ^NULL]
		ifFalse: [^CurrentGrandMap fluidGet iDOf: be]!
*/
}
/**
 * Essential.  Find all the global IDs on this Server that have been assigned to this
 * RangeElement
 */
public static IDRegion iDsOf(FeRangeElement value) {
	BeRangeElement be;
	be = value.fetchBe();
	if (be == null) {
		return ((IDRegion) IDSpace.global().emptyRegion());
	}
	else {
		return ((BeGrandMap) CurrentGrandMap.fluidGet()).iDsOf(be);
	}
/*
udanax-top.st:23228:FeServer class methodsFor: 'global ids'!
{IDRegion CLIENT} iDsOf: value {FeRangeElement}
	"Essential.  Find all the global IDs on this Server that have been assigned to this RangeElement"
	
	| be {BeRangeElement} |
	be := value fetchBe.
	be == NULL 
		ifTrue: [^(IDSpace global emptyRegion cast: IDRegion)]
		ifFalse: [^CurrentGrandMap fluidGet iDsOf: be]!
*/
}
/**
 * Find all the global IDs on this Server that have been assigned to any of the RangeElements
 * in an Edition
 */
public static IDRegion iDsOfRange(FeEdition edition) {
	XnRegion result;
	Someone.thingToDo();
	/* fix this grossly inefficient algorithm so that at least it doesn't check every single virtual object in the range */
	if ( ! (edition.isFinite())) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_FINITE);
	}
	result = IDSpace.global().emptyRegion();
	Stepper stomper = edition.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		FeRangeElement value = (FeRangeElement) stomper.fetch();
		if (value == null) {
			continue ;
		}
		BeRangeElement be;
		be = value.fetchBe();
		if (be != null) {
			result = result.unionWith((((BeGrandMap) CurrentGrandMap.fluidGet()).iDsOf(be)));
		}
	}
	stomper.destroy();
	return (IDRegion) result;
/*
udanax-top.st:23237:FeServer class methodsFor: 'global ids'!
{IDRegion CLIENT} iDsOfRange: edition {FeEdition}
	"Find all the global IDs on this Server that have been assigned to any of the RangeElements in an Edition"
	
	| result {XnRegion} |
	self thingToDo. "fix this grossly inefficient algorithm so that at least it doesn't check every single virtual object in the range"
	edition isFinite ifFalse:
		[Heaper BLAST: #MustBeFinite].
	result := IDSpace global emptyRegion.
	edition stepper forEach: [ :value {FeRangeElement} | | be {BeRangeElement} |
		be := value fetchBe.
		be ~~ NULL ifTrue:
			[result := result unionWith: (CurrentGrandMap fluidGet iDsOf: be)]].
	^result cast: IDRegion!
*/
}
/**
 * The current clock time on the Server, in seconds since the 'beginning of time'
 */
public static int currentTime() {
	return AboraSupport.xuTime();
/*
udanax-top.st:23253:FeServer class methodsFor: 'accessing'!
{IntegerVar CLIENT} currentTime
	"The current clock time on the Server, in seconds since the 'beginning of time'"
	
	^Time xuTime!
*/
}
/**
 * The LockSmith which hands out locks when a client tries to login through the GateKeeper
 * with an invalid Club ID or name.
 */
public static FeLockSmith gateLockSmith() {
	return (FeLockSmith) (FeLockSmith.spec().wrap(((BeGrandMap) CurrentGrandMap.fluidGet()).gateLockSmithEdition()));
/*
udanax-top.st:23258:FeServer class methodsFor: 'accessing'!
{FeLockSmith} gateLockSmith
	"The LockSmith which hands out locks when a client tries to login through the GateKeeper with an invalid Club ID or name."
	
	^(FeLockSmith spec wrap: CurrentGrandMap fluidGet gateLockSmithEdition) cast: FeLockSmith!
*/
}
/**
 * Essential. A sequence of numbers uniquely identifying this Server
 */
public static Sequence identifier() {
	return ((BeGrandMap) CurrentGrandMap.fluidGet()).identifier();
/*
udanax-top.st:23263:FeServer class methodsFor: 'accessing'!
{Sequence CLIENT} identifier
	"Essential. A sequence of numbers uniquely identifying this Server"
	
	^CurrentGrandMap fluidGet identifier!
*/
}
/**
 * This is currently a no-op.
 */
public static void removeWaitDetector(FeWaitDetector detector) {
/*
udanax-top.st:23268:FeServer class methodsFor: 'accessing'!
{void} removeWaitDetector: detector {FeWaitDetector}
	"This is currently a no-op."!
*/
}
/**
 * Essential.  The Detector will be triggered when the consequences of all previous local
 * requests have finished propagating through this Server. (e.g. Edition::transclusions may
 * take a while to collect all of the results.)
 * If you want to remove the Detector before it is triggered, destroy it.
 * Note that this is NOT a request to speed up the completion of the outstanding requests.
 * See WaitDetector::done ()
 */
public static FeWaitDetector waitForConsequences() {
	MarkM.shouldImplement();
	FeServer.waitForConsequences(null);
	return null;
/*
udanax-top.st:23271:FeServer class methodsFor: 'accessing'!
{FeWaitDetector CLIENT} waitForConsequences
	"Essential.  The Detector will be triggered when the consequences of all previous local requests have finished propagating through this Server. (e.g. Edition::transclusions may take a while to collect all of the results.)
	If you want to remove the Detector before it is triggered, destroy it.
	Note that this is NOT a request to speed up the completion of the outstanding requests.
	See WaitDetector::done ()"
	
	MarkM shouldImplement.
	FeServer waitForConsequences: NULL.
	^NULL "fodder"!
*/
}
/**
 * Essential.  The Detector will be triggered when the consequences of all previous local
 * requests have finished propagating through this Server. (e.g. Edition::transclusions may
 * take a while to collect all of the results.)
 * If you want to remove the Detector before it is triggered, destroy it.
 * Note that this is NOT a request to speed up the completion of the outstanding requests.
 * See WaitDetector::done ()
 */
public static void waitForConsequences(FeWaitDetector detector) {
	MarkM.shouldImplement();
/*
udanax-top.st:23281:FeServer class methodsFor: 'accessing'!
{void} waitForConsequences: detector {FeWaitDetector}
	"Essential.  The Detector will be triggered when the consequences of all previous local requests have finished propagating through this Server. (e.g. Edition::transclusions may take a while to collect all of the results.)
	If you want to remove the Detector before it is triggered, destroy it.
	Note that this is NOT a request to speed up the completion of the outstanding requests.
	See WaitDetector::done ()"
	
	MarkM shouldImplement!
*/
}
/**
 * Essential.  The Detector will be triggered when the current state of the Server has been
 * reliably written to disk.
 * If you want to remove the Detector before it is triggered, destroy it.
 * See WaitDetector::done ()
 */
public static FeWaitDetector waitForWrite() {
	Dean.shouldImplement();
	FeServer.waitForWrite(null);
	return null;
/*
udanax-top.st:23289:FeServer class methodsFor: 'accessing'!
{FeWaitDetector CLIENT} waitForWrite
	"Essential.  The Detector will be triggered when the current state of the Server has been reliably written to disk.
	If you want to remove the Detector before it is triggered, destroy it.
	See WaitDetector::done ()"
	
	Dean shouldImplement.
	FeServer waitForWrite: NULL.
	^NULL "fodder"!
*/
}
/**
 * Essential.  The Detector will be triggered when the current state of the Server has been
 * reliably written to disk.
 * If you want to remove the Detector before it is triggered, destroy it.
 * See WaitDetector::done ()
 */
public static void waitForWrite(FeWaitDetector detector) {
	((DiskManager) CurrentPacker.fluidGet()).purge();
	detector.done();
/*
udanax-top.st:23298:FeServer class methodsFor: 'accessing'!
{void} waitForWrite: detector {FeWaitDetector}
	"Essential.  The Detector will be triggered when the current state of the Server has been reliably written to disk.
	If you want to remove the Detector before it is triggered, destroy it.
	See WaitDetector::done ()"
	[DiskManager] USES.
	CurrentPacker fluidGet purge.
	detector done!
*/
}
public FeServer() {
/*

Generated during transformation
*/
}
public FeServer(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
