/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.basic;

import info.dgjones.abora.gold.be.basic.BeCarrier;
import info.dgjones.abora.gold.be.basic.BeClub;
import info.dgjones.abora.gold.be.basic.BeDataHolder;
import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.basic.BeGrandMap;
import info.dgjones.abora.gold.be.basic.BeIDHolder;
import info.dgjones.abora.gold.be.basic.BeLabel;
import info.dgjones.abora.gold.be.basic.BePlaceHolder;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.basic.BeWork;
import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.be.canopy.BertCrum;
import info.dgjones.abora.gold.be.canopy.CanopyCrum;
import info.dgjones.abora.gold.be.ents.ActualOrglRoot;
import info.dgjones.abora.gold.be.ents.Ent;
import info.dgjones.abora.gold.be.ents.Loaf;
import info.dgjones.abora.gold.be.ents.OrglRoot;
import info.dgjones.abora.gold.collection.basic.PrimDataArray;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.collection.grand.GrandHashTable;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.collection.tables.ImmuTable;
import info.dgjones.abora.gold.collection.tables.MuTable;
import info.dgjones.abora.gold.collection.tables.ScruTable;
import info.dgjones.abora.gold.counter.Counter;
import info.dgjones.abora.gold.filter.FilterSpace;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nadmin.FeLockSmith;
import info.dgjones.abora.gold.nbacken.GrantStepper;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.nkernel.FeKeyMaster;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.cross.CrossRegion;
import info.dgjones.abora.gold.spaces.cross.CrossSpace;
import info.dgjones.abora.gold.spaces.cross.Tuple;
import info.dgjones.abora.gold.spaces.integers.IntegerMapping;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.spaces.integers.IntegerRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerSpace;
import info.dgjones.abora.gold.spaces.unordered.HeaperAsPosition;
import info.dgjones.abora.gold.spaces.unordered.HeaperSpace;
import info.dgjones.abora.gold.spaces.unordered.IDSpace;
import info.dgjones.abora.gold.traces.TracePosition;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.tumbler.SequenceSpace;
import info.dgjones.abora.gold.wrapper.FeWrapperSpec;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.x.PrimValue;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Rewrite notes
 * 3/7/92 ravi
 * - we had decided to have myRangeElementIDs be a GrandSetTable, but for now its just a
 * Table onto IDRegions, since that is what we have implemented right now
 */
public class BeGrandMap extends Abraham {

	protected Sequence myIdentifier;
	protected IDSpace myGlobalIDSpace;
	protected Counter myLocalIDSpaceCounter;
	protected FilterSpace myGlobalIDFilterSpace;
	protected CrossSpace myEndorsementSpace;
	protected FilterSpace myEndorsementFilterSpace;
	protected MuTable myIDHolders;
	protected MuTable myIDCounters;
	protected MuTable myRangeElements;
	protected MuTable myRangeElementIDs;
	protected Ent myEnt;
	protected ID myEmptyClubID;
	protected ID myPublicClubID;
	protected ID myAdminClubID;
	protected ID myArchiveClubID;
	protected ID myAccessClubID;
	protected ID myClubDirectoryID;
	protected BeEdition myGateLockSmithEdition;
	protected ImmuTable myWrapperEndorsements;
	protected PtrArray myEndorsementFlags;
	protected boolean myPurgeable;
	protected BeEdition myGrants;
	protected boolean myAcceptingConnectionsFlag;
	protected static int BackendCount;
/*
udanax-top.st:1466:
Abraham subclass: #BeGrandMap
	instanceVariableNames: '
		myIdentifier {Sequence}
		myGlobalIDSpace {IDSpace}
		myLocalIDSpaceCounter {Counter}
		myGlobalIDFilterSpace {FilterSpace of: IDSpace}
		myEndorsementSpace {CrossSpace}
		myEndorsementFilterSpace {FilterSpace of: CrossSpace}
		myIDHolders {MuTable of: ID with: IDHolder}
		myIDCounters {MuTable of: (Tuple of: Sequence with: IntegerPos)
	with: Counter}
		myRangeElements {MuTable of: ID with: BeRangeElement}
		myRangeElementIDs {MuTable of: (HeaperAsPosition of: BeRangeElement)
	with: IDRegion | ID}
		myEnt {Ent}
		myEmptyClubID {ID}
		myPublicClubID {ID}
		myAdminClubID {ID}
		myArchiveClubID {ID}
		myAccessClubID {ID}
		myClubDirectoryID {ID}
		myGateLockSmithEdition {BeEdition}
		myWrapperEndorsements {ImmuTable of: Sequence with: CrossRegion}
		myEndorsementFlags {PtrArray of: Tuple | CrossRegion}
		myPurgeable {BooleanVar NOCOPY}
		myGrants {BeEdition of: Club}
		myAcceptingConnectionsFlag {BooleanVar NOCOPY}'
	classVariableNames: 'BackendCount {IntegerVar smalltalk} '
	poolDictionaries: ''
	category: 'Xanadu-Be-Basic'!
*/
/*
udanax-top.st:1495:
BeGrandMap comment:
'Rewrite notes
3/7/92 ravi
- we had decided to have myRangeElementIDs be a GrandSetTable, but for now its just a Table onto IDRegions, since that is what we have implemented right now'!
*/
/*
udanax-top.st:1499:
(BeGrandMap getOrMakeCxxClassDescription)
	friends:
'friend class BackendBootMaker;
';
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:2209:
BeGrandMap class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:2212:
(BeGrandMap getOrMakeCxxClassDescription)
	friends:
'friend class BackendBootMaker;
';
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BeGrandMap.class).setAttributes( new Set().add("LOCKED").add("COPY").add("SHEPHERDPATRIARCH").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Check that the BeClub structure matches the Editions underneath them
 */
public void clubConsistencyCheck() {
	Ravi.thingToDo();
/*
udanax-top.st:1507:BeGrandMap methodsFor: 'private: booting'!
{void} clubConsistencyCheck
	"Check that the BeClub structure matches the Editions underneath them"
	
	Ravi thingToDo!
*/
}
public void coldBoot() {
	FeEdition emptyDesc;
	BeClub emptyClub;
	FeEdition publicDesc;
	BeClub publicClub;
	BeClub adminClub;
	BeClub archiveClub;
	BeEdition clubNames;
	MuTable endorsements;
	int number;
	IDSpace iDSpace;
	BeEdition endorseTokenWorks;
	/* set up the initial set of Clubs */
	myEmptyClubID = ID.make(null, null, -1);
	myPublicClubID = ID.make(null, null, -2);
	Someone.thingToDo();
	/* ensure that the following IDs are deterministic */
	myAdminClubID = myGlobalIDSpace.newID();
	myArchiveClubID = myGlobalIDSpace.newID();
	myAccessClubID = myGlobalIDSpace.newID();
	/* figure out the IDs of the Wrapper endorsement Works */
	endorsements = MuTable.make(SequenceSpace.make());
	number = -3;
	Stepper stomper = FeWrapperSpec.knownWrappers().stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Sequence name = (Sequence) stomper.fetch();
		if (name == null) {
			continue ;
		}
		ID iD;
		Ravi.thingToDo();
		/* put something more descriptive here */
		iD = ID.make(null, null, number);
		number = number - 1;
		endorsements.introduce(name, (myEndorsementSpace.crossOfRegions(((PtrArray) (PrimSpec.pointer().arrayWithTwo(myArchiveClubID.asRegion(), iD.asRegion()))))));
	}
	stomper.destroy();
	myWrapperEndorsements = endorsements.asImmuTable();
	/* set up the special flag bits used by the canopy */
	myEndorsementFlags = PtrArray.nulls(5 + 10);
	myEndorsementFlags.store(0, ((XnRegion) (endorsements.get((Sequence.string("Text"))))).theOne());
	myEndorsementFlags.store(1, ((XnRegion) (endorsements.get((Sequence.string("HyperLink"))))).theOne());
	myEndorsementFlags.store(2, ((XnRegion) (endorsements.get((Sequence.string("HyperRef"))))).theOne());
	myEndorsementFlags.store(3, ((XnRegion) (endorsements.get((Sequence.string("SingleRef"))))).theOne());
	myEndorsementFlags.store(4, ((XnRegion) (endorsements.get((Sequence.string("MultiRef"))))).theOne());
	for (int i = 
	/* generate some IDs to use as endorsement tokens */
	5; i < myEndorsementFlags.count(); i ++ ) {
		myEndorsementFlags.store(i, (myEndorsementSpace.crossOfRegions(((PtrArray) (PrimSpec.pointer().arrayWithTwo(myGlobalIDSpace.fullRegion(), myGlobalIDSpace.newID().asRegion()))))));
	}
	CanopyCrum.useEndorsementFlags(myEndorsementFlags);
	CurrentAuthor.fluidSet(myEmptyClubID);
	InitialReadClub.fluidSet(myPublicClubID);
	InitialEditClub.fluidSet(myEmptyClubID);
	InitialOwner.fluidSet(myEmptyClubID);
	InitialSponsor.fluidSet(myEmptyClubID);
	Dean.knownBug();
	/* Who sponsors clubs? */
	emptyDesc = (FeEdition) (carrier((newEmptyEdition(SequenceSpace.make())))).makeFe();
	emptyClub = newClub(emptyDesc, myEmptyClubID);
	emptyClub.setEditClub(null);
	publicDesc = (FeEdition) (carrier((newEditionWith((Sequence.string("ClubDescription:LockSmith")), (carrier((newDataEdition((UInt8Array.string("boo")), (IntegerRegion.make(0, 3)), IntegerSpace.make().getAscending())))))))).makeFe();
	publicClub = newClub(publicDesc, myPublicClubID);
	publicClub.setEditClub(null);
	emptyClub.sponsor(((IDRegion) myPublicClubID.asRegion()));
	publicClub.sponsor(((IDRegion) myPublicClubID.asRegion()));
	InitialSponsor.fluidSet(myPublicClubID);
	InitialReadClub.fluidSet(myAdminClubID);
	InitialEditClub.fluidSet(myAdminClubID);
	InitialOwner.fluidSet(myAdminClubID);
	Someone.thingToDo();
	/* This should probably still be the Null Club. */
	adminClub = newClub(publicDesc, myAdminClubID);
	InitialReadClub.fluidSet(myArchiveClubID);
	InitialEditClub.fluidSet(myArchiveClubID);
	InitialOwner.fluidSet(myArchiveClubID);
	archiveClub = newClub(publicDesc, myArchiveClubID);
	CurrentKeyMaster.fluidSet((FeKeyMaster.make(publicClubID())));
	InitialReadClub.fluidSet(myAdminClubID);
	InitialEditClub.fluidSet(myAdminClubID);
	iDSpace = IDSpace.unique();
	newClub(((FeEdition) (carrier((newEditionWith((Sequence.string("ClubDescription:Membership")), (carrier((((newEditionWith(iDSpace.newID(), (carrier(publicClub)))).with(iDSpace.newID(), (carrier(adminClub)))).with(iDSpace.newID(), (carrier(archiveClub)))))))))).makeFe()), myAccessClubID);
	InitialReadClub.fluidSet(myPublicClubID);
	InitialSponsor.fluidSet(myAdminClubID);
	InitialEditClub.fluidSet(myAdminClubID);
	clubNames = (((newEditionWith((Sequence.string("System Admin")), (carrier(adminClub)))).combine((newEditionWith((Sequence.string("System Archive")), (carrier(archiveClub)))))).combine((newEditionWith((Sequence.string("Universal Null")), (carrier(emptyClub)))))).combine((newEditionWith((Sequence.string("Universal Public")), (carrier(publicClub)))));
	myClubDirectoryID = assignID((newWork((FeEdition.on(clubNames)))));
	TableStepper stomper2 = 
	/* actually create the Wrapper description Works */
	endorsements.stepper();
	for (; stomper2.hasValue(); stomper2.step()) {
		Sequence name1 = (Sequence) stomper2.position();
		CrossRegion end = (CrossRegion) stomper2.fetch();
		if (end == null) {
			continue ;
		}
		Ravi.thingToDo();
		/* put something more descriptive in the Work */
		tryIntroduce(((ID) (((Tuple) end.theOne()).coordinate(1))), (newWork((FeEdition.on((newDataEdition(name1.integers(), (IntegerRegion.make(0, name1.integers().count())), IntegerSpace.make().ascending())))))));
	}
	stomper2.destroy();
	/* actually create the endorsement token Works */
	iDSpace = IDSpace.unique();
	endorseTokenWorks = newEmptyEdition(iDSpace);
	for (int i1 = 5; i1 < myEndorsementFlags.count(); i1 ++ ) {
		BeWork work;
		work = newWork(emptyDesc);
		/* contents don't matter */
		tryIntroduce(((ID) ((IDRegion) (((CrossRegion) (myEndorsementFlags.get(i1))).projection(1))).theOne()), work);
		endorseTokenWorks = endorseTokenWorks.with(iDSpace.newID(), (carrier(work)));
	}
	/* attach & endorse them so they can be found */
	Object initialReadClubOldValue = AboraBlockSupport.enterFluidBindDuring(InitialReadClub, myAdminClubID);
	try {
		Object initialEditClubOldValue = AboraBlockSupport.enterFluidBindDuring(InitialEditClub, null);
		try {
			BeEdition edition;
			edition = (newEditionWith((Sequence.string("Universal Public")), (carrier(publicClub)))).with((Sequence.string("Fast Tokens")), (carrier(endorseTokenWorks)));
			newWork((FeEdition.on(edition)));
			edition.endorse((myEndorsementSpace.crossOfRegions(((PtrArray) (PrimSpec.pointer().arrayWithTwo(myEmptyClubID.asRegion(), myEmptyClubID.asRegion()))))));
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(InitialEditClub, initialEditClubOldValue);
		}
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(InitialReadClub, initialReadClubOldValue);
	}
	myGateLockSmithEdition = newDataEdition((UInt8Array.string("wall")), (IntegerRegion.make(0, 4)), IntegerSpace.make().ascending());
	myGrants = newEditionWithAll(myGlobalIDSpace.fullRegion(), (carrier(adminClub)));
	InitialOwner.fluidSet(((ID) null));
	InitialSponsor.fluidSet(((ID) null));
	InitialReadClub.fluidSet(myEmptyClubID);
	InitialEditClub.fluidSet(((ID) null));
	CurrentAuthor.fluidSet(((ID) null));
	CurrentKeyMaster.fluidSet(((FeKeyMaster) null));
/*
udanax-top.st:1512:BeGrandMap methodsFor: 'private: booting'!
{void} coldBoot
	
	| emptyDesc {FeEdition} emptyClub {BeClub}
	  publicDesc {FeEdition} publicClub {BeClub}
	  adminClub {BeClub} archiveClub {BeClub}
	  clubNames {BeEdition} 
	  endorsements {MuTable of: Sequence and: CrossRegion} number {IntegerVar}
	  iDSpace {IDSpace}  endorseTokenWorks {BeEdition} |
	
	"set up the initial set of Clubs"
	myEmptyClubID := ID make: NULL with: NULL with: -1.
	myPublicClubID := ID make: NULL with: NULL with: -2.
	self thingToDo. "ensure that the following IDs are deterministic"
	myAdminClubID := myGlobalIDSpace newID.
	myArchiveClubID := myGlobalIDSpace newID.
	myAccessClubID := myGlobalIDSpace newID.
	
	"figure out the IDs of the Wrapper endorsement Works"
	endorsements := MuTable make: SequenceSpace make.
	number := -3.
	FeWrapperSpec knownWrappers stepper forEach: [ :name {Sequence} |
		| iD {ID}  |
		Ravi thingToDo. "put something more descriptive here"
		iD := ID make: NULL with: NULL with: number.
		number := number - 1.
		endorsements at: name
			introduce: (myEndorsementSpace
				crossOfRegions: ((PrimSpec pointer
					arrayWithTwo: myArchiveClubID asRegion
					with: iD asRegion) cast: PtrArray))].
	myWrapperEndorsements := endorsements asImmuTable.
	
	"set up the special flag bits used by the canopy"
	myEndorsementFlags := PtrArray nulls: 5+10.
	myEndorsementFlags at: UInt32Zero
		store: ((endorsements get: (Sequence string: 'Text')) cast: XnRegion) theOne.
	myEndorsementFlags at: 1
		store: ((endorsements get: (Sequence string: 'HyperLink')) cast: XnRegion) theOne.
	myEndorsementFlags at: 2
		store: ((endorsements get: (Sequence string: 'HyperRef')) cast: XnRegion) theOne.
	myEndorsementFlags at: 3
		store: ((endorsements get: (Sequence string: 'SingleRef')) cast: XnRegion) theOne.
	myEndorsementFlags at: 4
		store: ((endorsements get: (Sequence string: 'MultiRef')) cast: XnRegion) theOne.
	"generate some IDs to use as endorsement tokens"
	5 almostTo: myEndorsementFlags count do: [ :i {Int32} |
		myEndorsementFlags at: i store: (myEndorsementSpace
			crossOfRegions: ((PrimSpec pointer
					arrayWithTwo: myGlobalIDSpace fullRegion
					with: myGlobalIDSpace newID asRegion) cast: PtrArray))].
	CanopyCrum useEndorsementFlags: myEndorsementFlags.
	
	CurrentAuthor fluidSet: myEmptyClubID.
	InitialReadClub fluidSet: myPublicClubID.
	InitialEditClub fluidSet: myEmptyClubID.
	InitialOwner fluidSet: myEmptyClubID.
	InitialSponsor fluidSet: myEmptyClubID.  Dean knownBug.  "Who sponsors clubs?"
	emptyDesc := (self carrier: (self newEmptyEdition: SequenceSpace make)) makeFe cast: FeEdition.
	emptyClub := self newClub: emptyDesc with: myEmptyClubID.
	emptyClub setEditClub: NULL.
	publicDesc := (self carrier: (self
		newEditionWith: (Sequence string: 'ClubDescription:LockSmith')
			with: (self carrier:
					(self newDataEdition: (UInt8Array string: 'boo')
						with: (IntegerRegion make: IntegerVarZero with: 3)
						with: IntegerSpace make getAscending)))) makeFe cast: FeEdition.
	publicClub := self newClub: publicDesc with: myPublicClubID.
	publicClub setEditClub: NULL.
	
	emptyClub sponsor: (myPublicClubID asRegion cast: IDRegion).
	publicClub sponsor: (myPublicClubID asRegion cast: IDRegion).
	InitialSponsor fluidSet: myPublicClubID.
	
	InitialReadClub fluidSet: myAdminClubID.
	InitialEditClub fluidSet: myAdminClubID.
	InitialOwner fluidSet: myAdminClubID.  self thingToDo.  "This should probably still be the Null Club."
	adminClub := self newClub: publicDesc with: myAdminClubID.
	
	InitialReadClub fluidSet: myArchiveClubID.
	InitialEditClub fluidSet: myArchiveClubID.
	InitialOwner fluidSet: myArchiveClubID.
	archiveClub := self newClub: publicDesc with: myArchiveClubID.
	
	CurrentKeyMaster fluidSet: (FeKeyMaster make: self publicClubID).
	InitialReadClub fluidSet: myAdminClubID.
	InitialEditClub fluidSet: myAdminClubID.
	iDSpace := IDSpace unique.
	self newClub: ((self carrier: (self
			newEditionWith: (Sequence string: 'ClubDescription:Membership')
			with: (self carrier: (((self
					newEditionWith: iDSpace newID with: (self carrier: publicClub))
					with: iDSpace newID with: (self carrier: adminClub))
					with: iDSpace newID with: (self carrier: archiveClub))))) makeFe cast: FeEdition)
		with: myAccessClubID.
	
	InitialReadClub fluidSet: myPublicClubID.
	InitialSponsor fluidSet: myAdminClubID.
	InitialEditClub fluidSet: myAdminClubID.
	
	clubNames := (((self newEditionWith: (Sequence string: 'System Admin') with:  (self carrier: adminClub))
		combine: (self newEditionWith: (Sequence string: 'System Archive') with: (self carrier: archiveClub)))
		combine: (self newEditionWith: (Sequence string: 'Universal Null') with: (self carrier: emptyClub)))
		combine: (self newEditionWith: (Sequence string: 'Universal Public') with:  (self carrier: publicClub)).
	myClubDirectoryID := self assignID: (self newWork: (FeEdition on: clubNames)).
	
	"actually create the Wrapper description Works"
	endorsements stepper forPositions: [ :name {Sequence} :end {CrossRegion} |
		Ravi thingToDo. "put something more descriptive in the Work"
		self at: (((end theOne cast: Tuple) coordinate: 1) cast: ID)
			tryIntroduce: (self
				newWork: (FeEdition on: (self
					newDataEdition: name integers
					with: (IntegerRegion make: IntegerVarZero with: name integers count)
					with: IntegerSpace make ascending)))].
	"actually create the endorsement token Works"
	iDSpace := IDSpace unique.
	endorseTokenWorks := self newEmptyEdition: iDSpace.
	5 almostTo: myEndorsementFlags count do: [ :i {Int32} | | work {BeWork} |
		work := self newWork: emptyDesc. "contents don't matter"
		self at: (((((myEndorsementFlags get: i) cast: CrossRegion)
				projection: 1) cast: IDRegion) theOne cast: ID)
			tryIntroduce: work.
		endorseTokenWorks := endorseTokenWorks
			with: iDSpace newID
			with: (self carrier: work)].
	"attach & endorse them so they can be found"
	InitialReadClub fluidBind: myAdminClubID during:
	[InitialEditClub fluidBind: NULL during:
		[ | edition {BeEdition} |
		edition := (self
			newEditionWith: (Sequence string: 'Universal Public')
				with: (self carrier: publicClub))
			with: (Sequence string: 'Fast Tokens')
				with: (self carrier: endorseTokenWorks).
		self newWork: (FeEdition on: edition).
		edition endorse: (myEndorsementSpace
			crossOfRegions: ((PrimSpec pointer
					arrayWithTwo: myEmptyClubID asRegion
					with: myEmptyClubID asRegion) cast: PtrArray))]].
	
	myGateLockSmithEdition := self
		newDataEdition: (UInt8Array string: 'wall')
		with: (IntegerRegion make: IntegerVarZero with: 4)
		with: IntegerSpace make ascending.
	myGrants := self newEditionWithAll: myGlobalIDSpace fullRegion
		with: (self carrier: adminClub).
	
	InitialOwner fluidSet: (NULL basicCast: ID).
	InitialSponsor fluidSet: (NULL basicCast: ID).
	InitialReadClub fluidSet: myEmptyClubID.
	InitialEditClub fluidSet: (NULL basicCast: ID).
	CurrentAuthor fluidSet: (NULL basicCast: ID).
	CurrentKeyMaster fluidSet: (NULL basicCast: FeKeyMaster).!
*/
}
public BeGrandMap(Sequence identifier) {
	super();
	AboraBlockSupport.enterConsistent();
	try {
		Counter counter;
		newShepherd();
		/* newShepherd must be first in GrandMap so that it is the boot object. */
		myPurgeable = false;
		/* The GrandMap cannot be purged until it is explicitly allowed. */
		myEnt = Ent.make();
		myIdentifier = identifier;
		/* The counters table must be setup before we try to make any IDSpaces */
		myIDCounters = MuTable.make((CrossSpace.make(SequenceSpace.make(), IntegerSpace.make())));
		counter = Counter.make(1, 20);
		myGlobalIDSpace = IDSpace.make(null, -1, counter);
		myIDCounters.introduce((Tuple.two(Sequence.zero(), IntegerPos.make(-1))), counter);
		myLocalIDSpaceCounter = Counter.make(1, 256);
		myGlobalIDFilterSpace = FilterSpace.make((myGlobalIDSpace));
		myEndorsementSpace = CrossSpace.make(((PtrArray) (PrimSpec.pointer().arrayWithTwo(myGlobalIDSpace, myGlobalIDSpace))));
		myEndorsementFilterSpace = FilterSpace.make((myEndorsementSpace));
		myRangeElements = GrandHashTable.make(myGlobalIDSpace);
		myIDHolders = GrandHashTable.make(myGlobalIDSpace);
		myRangeElementIDs = GrandHashTable.make(HeaperSpace.make());
		Someone.hack();
		/* how does this connect */
		Object currentGrandMapOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentGrandMap, this);
		try {
			coldBoot();
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(CurrentGrandMap, currentGrandMapOldValue);
		}
		remember();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
	Object currentGrandMapOldValue1 = AboraBlockSupport.enterFluidBindDuring(CurrentGrandMap, this);
	try {
		clubConsistencyCheck();
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(CurrentGrandMap, currentGrandMapOldValue1);
	}
	myPurgeable = false;
	myAcceptingConnectionsFlag = true;
/*
udanax-top.st:1670:BeGrandMap methodsFor: 'private: create'!
create: identifier {Sequence}
	super create.
	DiskManager consistent:
		[ | counter {Counter} |
		self newShepherd. 
		"newShepherd must be first in GrandMap so that it is the boot object."
		myPurgeable := false.  "The GrandMap cannot be purged until it is explicitly allowed."
		myEnt := Ent make. 
		myIdentifier := identifier. 
		"The counters table must be setup before we try to make any IDSpaces"
		myIDCounters := MuTable make: (CrossSpace
			make: SequenceSpace make with: IntegerSpace make).
		counter := Counter make: 1 with: 20.
		myGlobalIDSpace := IDSpace make: NULL with: -1 with: counter.
		myIDCounters at: (Tuple two: Sequence zero with: -1 integer)
			introduce: counter.
		myLocalIDSpaceCounter := Counter make: 1 with: 256.
		myGlobalIDFilterSpace := FilterSpace make: (myGlobalIDSpace cast: CoordinateSpace).
		
		myEndorsementSpace := CrossSpace make: ((PrimSpec pointer
			arrayWithTwo: myGlobalIDSpace with: myGlobalIDSpace) cast: PtrArray).
		myEndorsementFilterSpace := FilterSpace make: (myEndorsementSpace cast: CoordinateSpace).
		
		myRangeElements := GrandHashTable make: myGlobalIDSpace.
		myIDHolders := GrandHashTable make: myGlobalIDSpace.
		myRangeElementIDs := GrandHashTable make: HeaperSpace make.
		self hack. "how does this connect"
		CurrentGrandMap fluidBind: self during:
			[self coldBoot].
		self remember].
	CurrentGrandMap fluidBind: self during:
		[self clubConsistencyCheck].
	myPurgeable _ false.
	myAcceptingConnectionsFlag _ true.!
*/
}
public void restartBeGrandMap(Rcvr rcvr) {
	myPurgeable = false;
	myAcceptingConnectionsFlag = true;
	CanopyCrum.useEndorsementFlags(myEndorsementFlags);
/*
udanax-top.st:1709:BeGrandMap methodsFor: 'hooks:'!
{void RECEIVE.HOOK} restartBeGrandMap: rcvr {Rcvr unused}
	myPurgeable _ false.
	myAcceptingConnectionsFlag _ true.
	CanopyCrum useEndorsementFlags: myEndorsementFlags!
*/
}
/**
 * Allow the GrandMap to be purged.  The GrandMap should NOT be used after this is called.
 */
public void bePurgeable() {
	myPurgeable = true;
/*
udanax-top.st:1716:BeGrandMap methodsFor: 'purging'!
{void} bePurgeable
	"Allow the GrandMap to be purged.  The GrandMap should NOT be used after this is called."
	myPurgeable := true.!
*/
}
/**
 * The Grandmap never gets purged unless explicitly allowed by calling bePurgeable.
 */
public boolean isPurgeable() {
	return myPurgeable;
/*
udanax-top.st:1720:BeGrandMap methodsFor: 'purging'!
{BooleanVar} isPurgeable
	"The Grandmap never gets purged unless explicitly allowed by calling bePurgeable."
	
	^ myPurgeable!
*/
}
public int contentsHash() {
	return (((((super.contentsHash() ^ myIdentifier.hashForEqual()) ^ myLocalIDSpaceCounter.hashForEqual()) ^ myEnt.hashForEqual()) ^ myEmptyClubID.hashForEqual()) ^ myPublicClubID.hashForEqual()) ^ myAdminClubID.hashForEqual();
/*
udanax-top.st:1727:BeGrandMap methodsFor: 'testing'!
{UInt32} contentsHash
	^(((((super contentsHash
		bitXor: myIdentifier hashForEqual)
		bitXor: myLocalIDSpaceCounter hashForEqual)
		bitXor: myEnt hashForEqual)
		bitXor: myEmptyClubID hashForEqual)
		bitXor: myPublicClubID hashForEqual)
		bitXor: myAdminClubID hashForEqual!
*/
}
/**
 * See FeAdminer
 */
public void acceptConnections(boolean open) {
	myAcceptingConnectionsFlag = open;
/*
udanax-top.st:1739:BeGrandMap methodsFor: 'accessing'!
{void} acceptConnections: open {BooleanVar}
	"See FeAdminer"
	
	myAcceptingConnectionsFlag := open!
*/
}
/**
 * Remember the two way association between value and its new ID.
 */
public ID assignID(BeRangeElement value) {
	ID iD;
	Ravi.knownBug();
	/* what if the ID has already been assigned by the grantee? */
	iD = newID();
	if ( ! (tryIntroduce(iD, value))) {
		throw new AboraRuntimeException(AboraRuntimeException.IDALREADY_USED);
	}
	return iD;
/*
udanax-top.st:1744:BeGrandMap methodsFor: 'accessing'!
{ID} assignID: value {BeRangeElement}
	"Remember the two way association between value and its new ID."
	
	| iD {ID} |
	Ravi knownBug. "what if the ID has already been assigned by the grantee?"
	iD _ self newID.
	(self at: iD tryIntroduce: value) ifFalse: [Heaper BLAST: #IDAlreadyUsed].
	^iD!
*/
}
/**
 * Remember the two way association between value and the supplied ID.
 */
public boolean tryIntroduce(ID iD, BeRangeElement value) {
	if (myRangeElements.includesKey(iD)) {
		return false;
	}
	Someone.hack();
	/* The number below comes frojm my memory of how big a GrandMap assign can be. */
	AboraBlockSupport.enterConsistent(6);
	try {
		HeaperAsPosition hap;
		IDRegion already;
		Someone.thingToDo();
		/* Decide about multiple IDs */
		hap = HeaperAsPosition.make(value);
		already = (IDRegion) (myRangeElementIDs.fetch(hap));
		if (already == null) {
			myRangeElementIDs.introduce(hap, iD.asRegion());
		}
		else {
			if (value instanceof BeClub) {
				throw new AboraRuntimeException(AboraRuntimeException.CLUB_MUST_HAVE_UNIQUE_ID);
			}
			myRangeElementIDs.replace(hap, (already.with(iD)));
		}
		myRangeElements.introduce(iD, value);
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
	return true;
/*
udanax-top.st:1753:BeGrandMap methodsFor: 'accessing'!
{BooleanVar} at: iD {ID} tryIntroduce: value {BeRangeElement}
	"Remember the two way association between value and the supplied ID."
	
	(myRangeElements includesKey: iD) ifTrue: [^false].
	self hack.  "The number below comes frojm my memory of how big a GrandMap assign can be."
	DiskManager consistent: 6 with:
		[| hap {HeaperAsPosition} already {IDRegion | NULL} |
		self thingToDo. "Decide about multiple IDs"
		hap := HeaperAsPosition make: value.
		already := (myRangeElementIDs fetch: hap) cast: IDRegion.
		already == NULL
			ifTrue: [myRangeElementIDs at: hap introduce: iD asRegion]
			ifFalse: 
				[(value isKindOf: BeClub) ifTrue: [Heaper BLAST: #ClubMustHaveUniqueID].
				myRangeElementIDs at: hap replace: (already with: iD)].
		myRangeElements at: iD introduce: value].
	^true!
*/
}
public ID clubDirectoryID() {
	return myClubDirectoryID;
/*
udanax-top.st:1771:BeGrandMap methodsFor: 'accessing'!
{ID} clubDirectoryID
	^myClubDirectoryID!
*/
}
public FilterSpace endorsementFilterSpace() {
	return myEndorsementFilterSpace;
/*
udanax-top.st:1775:BeGrandMap methodsFor: 'accessing'!
{FilterSpace} endorsementFilterSpace
	^myEndorsementFilterSpace!
*/
}
public CrossSpace endorsementSpace() {
	return myEndorsementSpace;
/*
udanax-top.st:1779:BeGrandMap methodsFor: 'accessing'!
{CrossSpace} endorsementSpace
	^myEndorsementSpace!
*/
}
/**
 * The actual BeRangeElement at that ID, or NULL if there is none
 */
public BeRangeElement fetch(ID iD) {
	return (BeRangeElement) (myRangeElements.fetch(iD));
/*
udanax-top.st:1783:BeGrandMap methodsFor: 'accessing'!
{BeRangeElement | NULL} fetch: iD {ID}
	"The actual BeRangeElement at that ID, or NULL if there is none"
	
	^(myRangeElements fetch: iD) cast: BeRangeElement!
*/
}
/**
 * If there is a club at the given ID, return it.
 */
public BeClub fetchClub(ID iD) {
	if (iD == null) {
		return null;
	}
	Heaper cast1 = (get(iD));
	if (cast1 instanceof BeClub) {
		BeClub club = (BeClub) cast1;
		return club;
	}
	return null;
/*
udanax-top.st:1788:BeGrandMap methodsFor: 'accessing'!
{BeClub | NULL} fetchClub: iD {ID | NULL}
	"If there is a club at the given ID, return it."
	
	iD == NULL ifTrue: [^NULL].
	(self get: iD) cast: BeClub into: [:club | ^club] others: [].
	^NULL!
*/
}
public FeEdition gateLockSmithEdition() {
	return FeEdition.on((myGateLockSmithEdition));
/*
udanax-top.st:1795:BeGrandMap methodsFor: 'accessing'!
{FeEdition} gateLockSmithEdition
	^FeEdition on: (myGateLockSmithEdition)!
*/
}
/**
 * The actual BeRangeElement at that ID, or blast if there is none
 */
public BeRangeElement get(ID iD) {
	return (BeRangeElement) (myRangeElements.get(iD));
/*
udanax-top.st:1799:BeGrandMap methodsFor: 'accessing'!
{BeRangeElement} get: iD {ID}
	"The actual BeRangeElement at that ID, or blast if there is none"
	
	^(myRangeElements get: iD) cast: BeRangeElement!
*/
}
/**
 * Get a BeClub from the GrandMap.
 */
public BeClub getClub(ID iD) {
	return (BeClub) (get(iD));
/*
udanax-top.st:1804:BeGrandMap methodsFor: 'accessing'!
{BeClub} getClub: iD {ID}
	"Get a BeClub from the GrandMap."
	
	^(self get: iD) cast: BeClub!
*/
}
/**
 * Get what is at the the given ID as a front end object; blast if there is nothing there
 */
public FeRangeElement getFe(ID iD) {
	Someone.knownBug();
	/* This doesn't supply a label for Editions. */
	return (get(iD)).makeFe(null);
/*
udanax-top.st:1809:BeGrandMap methodsFor: 'accessing'!
{FeRangeElement} getFe: iD {ID}
	"Get what is at the the given ID as a front end object; blast if there is nothing there"
	
	self knownBug.  "This doesn't supply a label for Editions."
	^(self get: iD) makeFe: NULL!
*/
}
/**
 * Get a canonical Counter for an IDSpace, or make a new one
 */
public Counter getOrMakeIDCounter(Sequence backend, int number) {
	Counter result;
	Sequence theBackend;
	if (backend != null) {
		theBackend = backend;
	}
	else {
		if (number < 0) {
			theBackend = Sequence.zero();
		}
		else {
			theBackend = identifier();
		}
	}
	result = (Counter) (myIDCounters.fetch((Tuple.two(theBackend, IntegerPos.make(number)))));
	if (result == null) {
		Someone.thingToDo();
		/* figure out good batching */
		result = Counter.make(1, 20);
		myIDCounters.introduce((Tuple.two(theBackend, IntegerPos.make(number))), result);
	}
	return result;
/*
udanax-top.st:1815:BeGrandMap methodsFor: 'accessing'!
{Counter} getOrMakeIDCounter: backend {Sequence | NULL} with: number {IntegerVar}
	"Get a canonical Counter for an IDSpace, or make a new one"
	
	| result {Counter} theBackend {Sequence} |
	backend ~~ NULL ifTrue:
		[theBackend := backend]
	ifFalse: [number < IntegerVarZero ifTrue:
		[theBackend := Sequence zero]
	ifFalse:
		[theBackend := self identifier]].
	result := (myIDCounters fetch: (Tuple two: theBackend with: number integer)) cast: Counter.
	result == NULL ifTrue:
		[self thingToDo. "figure out good batching"
		result := Counter make: 1 with: 20.
		myIDCounters at: (Tuple two: theBackend with: number integer) introduce: result].
	^result!
*/
}
/**
 * If there is already an IDHolder for the ID then return it, otherwise make one
 */
public BeIDHolder getOrMakeIDHolder(ID iD) {
	BeIDHolder result;
	result = (BeIDHolder) (myIDHolders.fetch(iD));
	if (result == null) {
		DiskManager diskManager1 = 
		/* Make one and remember it for canonicalization */
		((DiskManager) CurrentPacker.fluidGet());
		AboraBlockSupport.enterConsistent(666, diskManager1);
		try {
			result = BeIDHolder.make(iD);
			myIDHolders.introduce(iD, result);
		}
		finally {
			AboraBlockSupport.exitConsistent(diskManager1);
		}
	}
	return result;
/*
udanax-top.st:1832:BeGrandMap methodsFor: 'accessing'!
{BeIDHolder} getOrMakeIDHolder: iD {ID}
	"If there is already an IDHolder for the ID then return it, otherwise make one"
	
	| result {BeIDHolder} |
	result := (myIDHolders fetch: iD) cast: BeIDHolder.
	result == NULL ifTrue:
		["Make one and remember it for canonicalization"
		CurrentPacker fluidGet consistent: 666 with:
			[result := BeIDHolder make: iD.
			myIDHolders at: iD introduce: result]].
	^result!
*/
}
/**
 * The FilterSpace on global IDSpace
 */
public FilterSpace globalIDFilterSpace() {
	return myGlobalIDFilterSpace;
/*
udanax-top.st:1844:BeGrandMap methodsFor: 'accessing'!
{FilterSpace} globalIDFilterSpace
	"The FilterSpace on global IDSpace"
	
	^myGlobalIDFilterSpace!
*/
}
/**
 * The global IDSpace
 */
public IDSpace globalIDSpace() {
	return myGlobalIDSpace;
/*
udanax-top.st:1849:BeGrandMap methodsFor: 'accessing'!
{IDSpace} globalIDSpace
	"The global IDSpace"
	
	^myGlobalIDSpace!
*/
}
/**
 * See FeAdminer
 */
public void grant(ID clubID, IDRegion globalIDs) {
	BeEdition newGrants;
	newGrants = myGrants.replace((newEditionWithAll(globalIDs, (carrier((getClub(clubID)))))));
	AboraBlockSupport.enterConsistent(1);
	try {
		myGrants = newGrants;
		diskUpdate();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:1854:BeGrandMap methodsFor: 'accessing'!
{void} grant: clubID {ID} with: globalIDs {IDRegion}
	"See FeAdminer"
	
	| newGrants {BeEdition} |
	newGrants := myGrants replace: (self
		newEditionWithAll: globalIDs
		with: (self carrier: (self getClub: clubID))).
	DiskManager consistent: 1 with:
		[myGrants := newGrants.
		self diskUpdate]!
*/
}
/**
 * Who has been granted authority to assign that ID
 */
public ID grantAt(ID iD) {
	return iDOf((myGrants.get(iD)).getOrMakeBe());
/*
udanax-top.st:1865:BeGrandMap methodsFor: 'accessing'!
{ID} grantAt: iD {ID}
	"Who has been granted authority to assign that ID"
	
	^self iDOf: (myGrants get: iD) getOrMakeBe!
*/
}
/**
 * See FeAdminer
 */
public TableStepper grants(IDRegion clubIDs, IDRegion globalIDs) {
	BeEdition theEdition;
	if (globalIDs == null) {
		theEdition = myGrants;
	}
	else {
		theEdition = myGrants.copy(globalIDs);
	}
	return GrantStepper.make(theEdition, clubIDs);
/*
udanax-top.st:1870:BeGrandMap methodsFor: 'accessing'!
{TableStepper of: ID and: IDRegion} grants: clubIDs {IDRegion | NULL}
	with: globalIDs {IDRegion | NULL}
	"See FeAdminer"
	
	| theEdition {BeEdition} |
	globalIDs == NULL
		ifTrue: [theEdition := myGrants]
		ifFalse: [theEdition := myGrants copy: globalIDs].
	^GrantStepper make: theEdition with: clubIDs!
*/
}
public Sequence identifier() {
	return myIdentifier;
/*
udanax-top.st:1880:BeGrandMap methodsFor: 'accessing'!
{Sequence} identifier
	^myIdentifier!
*/
}
/**
 * Find the ID of a BeRangeElement. Blast if there is no ID or if there is more than one
 */
public ID iDOf(BeRangeElement value) {
	IDRegion result;
	result = (IDRegion) (myRangeElementIDs.fetch((HeaperAsPosition.make(value))));
	if (result == null) {
		throw new AboraRuntimeException(AboraRuntimeException.DOES_NOT_HAVE_AN_ID);
	}
	if ( ! (result.count() == 1)) {
		throw new AboraRuntimeException(AboraRuntimeException.HAS_MULTIPLE_IDS);
	}
	return (ID) result.theOne();
/*
udanax-top.st:1884:BeGrandMap methodsFor: 'accessing'!
{ID} iDOf: value {BeRangeElement}
	"Find the ID of a BeRangeElement. Blast if there is no ID or if there is more than one"
	
	| result {IDRegion | NULL} |
	result := (myRangeElementIDs fetch: (HeaperAsPosition make: value)) cast: IDRegion.
	result == NULL ifTrue: [Heaper BLAST: #DoesNotHaveAnID].
	result count == 1 ifFalse: [Heaper BLAST: #HasMultipleIDs].
	^result theOne cast: ID!
*/
}
/**
 * Find the IDs of a BeRangeElement, whether there are none, one, or several
 */
public IDRegion iDsOf(BeRangeElement value) {
	IDRegion result;
	result = (IDRegion) (myRangeElementIDs.fetch((HeaperAsPosition.make(value))));
	if (result == null) {
		return (IDRegion) myGlobalIDSpace.emptyRegion();
	}
	return result;
/*
udanax-top.st:1893:BeGrandMap methodsFor: 'accessing'!
{IDRegion} iDsOf: value {BeRangeElement}
	"Find the IDs of a BeRangeElement, whether there are none, one, or several"
	
	| result {IDRegion | NULL} |
	result := (myRangeElementIDs fetch: (HeaperAsPosition make: value)) cast: IDRegion.
	result == NULL ifTrue: [^myGlobalIDSpace emptyRegion cast: IDRegion].
	^result!
*/
}
/**
 * See FeAdminer
 */
public boolean isAcceptingConnections() {
	return myAcceptingConnectionsFlag;
/*
udanax-top.st:1901:BeGrandMap methodsFor: 'accessing'!
{BooleanVar} isAcceptingConnections
	"See FeAdminer"
	
	^myAcceptingConnectionsFlag!
*/
}
public ID newID() {
	return myGlobalIDSpace.newID();
/*
udanax-top.st:1906:BeGrandMap methodsFor: 'accessing'!
{ID} newID
	^myGlobalIDSpace newID!
*/
}
/**
 * Make a new globally unique IDSpace
 */
public IDSpace newIDSpace() {
	return IDSpace.make(identifier(), myLocalIDSpaceCounter.increment());
/*
udanax-top.st:1910:BeGrandMap methodsFor: 'accessing'!
{IDSpace} newIDSpace
	"Make a new globally unique IDSpace"
	
	^IDSpace make: self identifier with: myLocalIDSpaceCounter increment!
*/
}
/**
 * The ID of the Club which owns whatever is at the given ID
 */
public ID placeOwnerID(ID iD) {
	BeRangeElement value;
	value = fetch(iD);
	if (value != null) {
		return value.owner();
	}
	Ravi.shouldImplement()
	/* Figure out who owns PlaceHolders */
	;
	return null;
/*
udanax-top.st:1915:BeGrandMap methodsFor: 'accessing'!
{ID} placeOwnerID: iD {ID}
	"The ID of the Club which owns whatever is at the given ID"
	| value {BeRangeElement} |
	value := self fetch: iD.
	value ~~ NULL ifTrue: [^value owner].
	Ravi shouldImplement "Figure out who owns PlaceHolders".
	^NULL "fodder"!
*/
}
public void setGateLockSmithEdition(FeEdition edition) {
	if ( ! (FeLockSmith.spec().certify(edition))) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_VALID_LOCK_SMITH);
	}
	myGateLockSmithEdition = edition.beEdition();
/*
udanax-top.st:1924:BeGrandMap methodsFor: 'accessing'!
{void} setGateLockSmithEdition: edition {FeEdition}
	(FeLockSmith spec certify: edition) ifFalse:
		[Heaper BLAST: #MustBeValidLockSmith].
	myGateLockSmithEdition := edition beEdition.!
*/
}
/**
 * A mapping from wrapper names to endorsements
 */
public ScruTable wrapperEndorsements() {
	Ravi.thingToDo();
	/* Figure out if there is a better way to do this */
	return myWrapperEndorsements;
/*
udanax-top.st:1930:BeGrandMap methodsFor: 'accessing'!
{ScruTable of: Sequence with: CrossRegion} wrapperEndorsements
	"A mapping from wrapper names to endorsements"
	
	Ravi thingToDo."Figure out if there is a better way to do this"
	^myWrapperEndorsements!
*/
}
/**
 * Creates an Edition mapping from a Region of keys to the values in an array. The ordering
 * specifies the correspondance between the keys and the indices in the array.
 * The Region must have the same count as the array.
 * You must give an owner for the newly created DataHolders.
 */
public BeEdition newDataEdition(PrimDataArray values, XnRegion keys, OrderSpec ordering) {
	OrglRoot result;
	int offset;
	XnRegion remainder;
	if (keys.isEmpty()) {
		return newEmptyEdition(keys.coordinateSpace());
	}
	Object currentTraceOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentTrace, myEnt.newTrace());
	try {
		Object currentBertCrumOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentBertCrum, BertCrum.make());
		try {
			if (values.count() <= Ent.tableSegmentMaxSize()) {
				return BeEdition.make((OrglRoot.makeData(keys, ordering, values)));
			}
			result = OrglRoot.makeCoordinateSpace(ordering.coordinateSpace());
			offset = 0;
			remainder = keys;
			while (offset < values.count()) {
				int count;
				OrglRoot oroot;
				PrimDataArray array;
				XnRegion region;
				count = Math.min(Ent.tableSegmentMaxSize(), values.count() - offset);
				array = (PrimDataArray) (values.copy(count, offset));
				region = remainder.chooseMany(count, ordering);
				oroot = OrglRoot.makeData(((IntegerMapping.make( - offset)).ofAll(region)), ordering, array);
				result = result.combine((oroot.transformedBy((IntegerMapping.make(offset)))));
				remainder = remainder.minus(region);
				offset = offset + count;
			}
			return BeEdition.make(result);
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(CurrentBertCrum, currentBertCrumOldValue);
		}
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(CurrentTrace, currentTraceOldValue);
	}
/*
udanax-top.st:1938:BeGrandMap methodsFor: 'making editions'!
{BeEdition} newDataEdition: values {PrimDataArray}
	with: keys {XnRegion}
	with: ordering {OrderSpec}
	"Creates an Edition mapping from a Region of keys to the values in an array. The ordering specifies the correspondance between the keys and the indices in the array.
	The Region must have the same count as the array.
	You must give an owner for the newly created DataHolders."
	
	| result {OrglRoot} offset {IntegerVar} remainder {XnRegion} |
	keys isEmpty ifTrue:
		[^self newEmptyEdition: keys coordinateSpace].
	CurrentTrace fluidBind: myEnt newTrace during: 
	[CurrentBertCrum fluidBind: BertCrum make during: 
		[values count <= Ent tableSegmentMaxSize DOTasLong ifTrue:
			[^BeEdition make: (OrglRoot makeData: keys with: ordering with: values)].
		result _ OrglRoot make.CoordinateSpace: ordering coordinateSpace.
		offset _ Int32Zero.
		remainder _ keys.
		[offset < values count] whileTrue:
			[| count {Int32} oroot {OrglRoot} array {PrimDataArray} region {XnRegion} |
			count _ Ent tableSegmentMaxSize DOTasLong  min: values count - offset DOTasLong .
			array _ (values copy: count with: offset DOTasLong) cast: PrimDataArray.
			region _ remainder chooseMany: count with: ordering.
			oroot _ 
				OrglRoot 
					makeData: ((IntegerMapping make: offset negated) ofAll: region)
					with: ordering
					with: array.
			result _ result combine: (oroot transformedBy: (IntegerMapping make: offset)).
			remainder _ remainder minus: region.
			offset _ offset + count].
		^BeEdition make: result]]!
*/
}
/**
 * A single key-value mapping
 */
public BeEdition newEditionWith(Position key, BeCarrier value) {
	Dean.hack();
	/* What should the bertCrum be? */
	Object currentTraceOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentTrace, value.rangeElement().hCrum().hCut().newSuccessor());
	try {
		Object currentBertCrumOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentBertCrum, value.rangeElement().bertCrum());
		try {
			XnRegion region;
			region = key.asRegion();
			return BeEdition.make((ActualOrglRoot.make((Loaf.makeRegion(region, value)), region)));
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(CurrentBertCrum, currentBertCrumOldValue);
		}
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(CurrentTrace, currentTraceOldValue);
	}
/*
udanax-top.st:1970:BeGrandMap methodsFor: 'making editions'!
{BeEdition} newEditionWith: key {Position} with: value {BeCarrier}
	"A single key-value mapping"
	[HistoryCrum] USES.
	Dean hack.  "What should the bertCrum be?"
	CurrentTrace fluidBind: value rangeElement hCrum hCut newSuccessor
		during: [CurrentBertCrum fluidBind: value rangeElement bertCrum
		during: [| region {XnRegion} |
				region _ key asRegion.
				^BeEdition make: 
					(ActualOrglRoot 
						make: (Loaf make.Region: region with: value)
						with: region)]]!
*/
}
/**
 * A single key-value mapping
 */
public BeEdition newEditionWithAll(XnRegion keys, BeCarrier value) {
	Dean.hack();
	/* What should the bertCrum be? */
	if (keys.isEmpty()) {
		return newEmptyEdition(keys.coordinateSpace());
	}
	Object currentTraceOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentTrace, value.rangeElement().hCrum().hCut().newSuccessor());
	try {
		Object currentBertCrumOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentBertCrum, value.rangeElement().bertCrum());
		try {
			return BeEdition.make((ActualOrglRoot.make((Loaf.makeRegion(keys, value)), keys)));
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(CurrentBertCrum, currentBertCrumOldValue);
		}
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(CurrentTrace, currentTraceOldValue);
	}
/*
udanax-top.st:1983:BeGrandMap methodsFor: 'making editions'!
{BeEdition} newEditionWithAll: keys {XnRegion} with: value {BeCarrier}
	"A single key-value mapping"
	
	Dean hack.  "What should the bertCrum be?"
	keys isEmpty ifTrue:
		[^self newEmptyEdition: keys coordinateSpace].
	CurrentTrace fluidBind: value rangeElement hCrum hCut newSuccessor
		during: [CurrentBertCrum fluidBind: value rangeElement bertCrum
		during: [^BeEdition make: 
					(ActualOrglRoot 
						make: (Loaf make.Region: keys with: value)
						with: keys)]]!
*/
}
/**
 * Create an empty Edition.  This should really be canonicalized.
 */
public BeEdition newEmptyEdition(CoordinateSpace cs) {
	Object currentTraceOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentTrace, myEnt.newTrace());
	try {
		Object currentBertCrumOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentBertCrum, BertCrum.make());
		try {
			AboraBlockSupport.enterConsistent(4);
			try {
				return BeEdition.make((OrglRoot.makeCoordinateSpace(cs)));
			}
			finally {
				AboraBlockSupport.exitConsistent();
			}
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(CurrentBertCrum, currentBertCrumOldValue);
		}
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(CurrentTrace, currentTraceOldValue);
	}
/*
udanax-top.st:1996:BeGrandMap methodsFor: 'making editions'!
{BeEdition} newEmptyEdition: cs {CoordinateSpace}
	"Create an empty Edition.  This should really be canonicalized."
	
	CurrentTrace fluidBind: myEnt newTrace during: [
		CurrentBertCrum fluidBind: BertCrum make during: [
			DiskManager consistent: 4 with: [
				^BeEdition make: (OrglRoot make.CoordinateSpace: cs)]]]!
*/
}
/**
 * Make an Edition with a region full of unique PlaceHolders
 */
public BeEdition newPlaceHolders(XnRegion region) {
	Ravi.thingToDo();
	/* rename to newPlaceHolders */
	if (region.isEmpty()) {
		return newEmptyEdition(region.coordinateSpace());
	}
	Object currentTraceOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentTrace, myEnt.newTrace());
	try {
		Object currentBertCrumOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentBertCrum, BertCrum.make());
		try {
			return BeEdition.make((OrglRoot.makeXnRegion(region)));
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(CurrentBertCrum, currentBertCrumOldValue);
		}
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(CurrentTrace, currentTraceOldValue);
	}
/*
udanax-top.st:2004:BeGrandMap methodsFor: 'making editions'!
{BeEdition} newPlaceHolders: region {XnRegion}
	"Make an Edition with a region full of unique PlaceHolders"
	
	Ravi thingToDo. "rename to newPlaceHolders"
	region isEmpty ifTrue:
		[^self newEmptyEdition: region coordinateSpace].
	CurrentTrace fluidBind: myEnt newTrace
		during: [CurrentBertCrum fluidBind: BertCrum make
		during: [^BeEdition make: (OrglRoot make.XnRegion: region)]]!
*/
}
/**
 * Creates an Edition mapping from a Region of keys to the values in an array. The ordering
 * specifies the correspondance between the keys and the indices in the array.
 * The Region must have the same count as the array.
 */
public BeEdition newValueEdition(PtrArray values, XnRegion keys, OrderSpec ordering) {
	/* compute the join of the existing traces and bert crums in the table */
	/* make new ones if there are none */
	TracePosition trace;
	CanopyCrum crum;
	BeRangeElement rangeElement;
	if (keys.count() != values.count()) {
		throw new AboraRuntimeException(AboraRuntimeException.COUNT_MISMATCH);
	}
	if (keys.isEmpty()) {
		return newEmptyEdition(keys.coordinateSpace());
	}
	FeRangeElement fe = (FeRangeElement) (values.fetch(0));
	if (fe != null ) {
		rangeElement = fe.getOrMakeBe();
	}
	else {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_NOT_HAVE_NULL_ELEMENTS);
	}
	trace = rangeElement.hCrum().hCut();
	crum = rangeElement.bertCrum();
	for (int i = 1; i < values.count(); i ++ ) {
		FeRangeElement fe1 = (FeRangeElement) (values.fetch(i));
		if (fe1 != null ) {
			rangeElement = fe1.getOrMakeBe();
		}
		else {
			throw new AboraRuntimeException(AboraRuntimeException.MUST_NOT_HAVE_NULL_ELEMENTS);
		}
		/* Neither of these should need a consistent block. */
		trace = trace.newSuccessorAfter(rangeElement.hCrum().hCut());
		crum = crum.computeJoin(rangeElement.bertCrum());
	}
	Object currentTraceOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentTrace, trace);
	try {
		Object currentBertCrumOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentBertCrum, ((BertCrum) crum));
		try {
			return BeEdition.make((OrglRoot.make(keys, ordering, values)));
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(CurrentBertCrum, currentBertCrumOldValue);
		}
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(CurrentTrace, currentTraceOldValue);
	}
/*
udanax-top.st:2014:BeGrandMap methodsFor: 'making editions'!
{BeEdition} newValueEdition: values {PtrArray of: FeRangeElement}
	with: keys {XnRegion}
	with: ordering {OrderSpec}
	"Creates an Edition mapping from a Region of keys to the values in an array. The ordering specifies the correspondance between the keys and the indices in the array.
	The Region must have the same count as the array."
	
	"compute the join of the existing traces and bert crums in the table"
	"make new ones if there are none"
	| trace {TracePosition} crum {CanopyCrum} rangeElement {BeRangeElement} |
	keys count ~~ values count ifTrue: [Heaper BLAST: #CountMismatch].
	keys isEmpty ifTrue: [^self newEmptyEdition: keys coordinateSpace].
	(values fetch: Int32Zero) 
		notNULL: [:fe {FeRangeElement} | rangeElement _ fe getOrMakeBe]
		else: [Heaper BLAST: #MustNotHaveNullElements].
	trace _ rangeElement hCrum hCut.
	crum _ rangeElement bertCrum.
	1 almostTo: values count do:
		[:i {Int32} |
		(values fetch: i) 
			notNULL: [:fe {FeRangeElement} | rangeElement _ fe getOrMakeBe]
			else: [Heaper BLAST: #MustNotHaveNullElements].
		"Neither of these should need a consistent block."
		trace _ trace newSuccessorAfter: rangeElement hCrum hCut.
		crum _ crum computeJoin: rangeElement bertCrum].
	CurrentTrace fluidBind: trace
		during: [CurrentBertCrum fluidBind: (crum cast: BertCrum)
		during: [ ^BeEdition make: (OrglRoot make: keys with: ordering with: values)]]!
*/
}
/**
 * Return a carrier that has the rangeElement with a new Label if appropriate.
 */
public BeCarrier carrier(BeRangeElement element) {
	if (element instanceof BeEdition) {
		return BeCarrier.make(newLabel(), element);
	}
	else {
		return BeCarrier.make(element);
	}
/*
udanax-top.st:2044:BeGrandMap methodsFor: 'making other things'!
{BeCarrier} carrier: element {BeRangeElement}
	"Return a carrier that has the rangeElement with a new Label if appropriate."
	
	(element isKindOf: BeEdition) 
		ifTrue: [^BeCarrier make: self newLabel with: element]
		ifFalse: [^BeCarrier make: element]!
*/
}
/**
 * Make a new Club assigned to either iD or a generated ID id iD is NULL.
 */
public BeClub newClub(FeEdition desc, ID iD) {
	BeClub result;
	Object currentTraceOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentTrace, myEnt.newTrace());
	try {
		Object currentBertCrumOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentBertCrum, BertCrum.make());
		try {
			result = (BeClub) BeClub.make(desc);
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(CurrentBertCrum, currentBertCrumOldValue);
		}
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(CurrentTrace, currentTraceOldValue);
	}
	AboraBlockSupport.enterConsistent();
	try {
		if (iD == null) {
			assignID(result);
		}
		else {
			if ( ! (tryIntroduce(iD, result))) {
				throw new AboraRuntimeException(AboraRuntimeException.ILLEGAL_ID);
			}
		}
		/* If we allow multiple IDs for clubs, we'll have to do this in the grandMap. */
		result.updateTransitiveMemberIDs();
		result.updateTransitiveSuperClubIDs();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
	return result;
/*
udanax-top.st:2051:BeGrandMap methodsFor: 'making other things'!
{BeClub} newClub: desc {FeEdition} with: iD {ID default: NULL}
	"Make a new Club assigned to either iD or a generated ID id iD is NULL."
	
	| result {BeClub} |
	CurrentTrace fluidBind: myEnt newTrace
		during: [CurrentBertCrum fluidBind: BertCrum make
		during: [result := BeClub make: desc]].
	DiskManager consistent: 
		[iD == NULL
			ifTrue: [self assignID: result]
			ifFalse: [(self at: iD tryIntroduce: result) ifFalse: [Heaper BLAST: #IllegalID]].
		"If we allow multiple IDs for clubs, we'll have to do this in the grandMap."
		result updateTransitiveMemberIDs.
		result updateTransitiveSuperClubIDs].
	^result!
*/
}
/**
 * Make a new DataHolder with the given contents.
 */
public BeDataHolder newDataHolder(PrimValue value) {
	Object currentTraceOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentTrace, myEnt.newTrace());
	try {
		Object currentBertCrumOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentBertCrum, BertCrum.make());
		try {
			AboraBlockSupport.enterConsistent(1);
			try {
				return new BeDataHolder(value);
			}
			finally {
				AboraBlockSupport.exitConsistent();
			}
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(CurrentBertCrum, currentBertCrumOldValue);
		}
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(CurrentTrace, currentTraceOldValue);
	}
/*
udanax-top.st:2067:BeGrandMap methodsFor: 'making other things'!
{BeDataHolder} newDataHolder: value {PrimValue}
	"Make a new DataHolder with the given contents."
	
	CurrentTrace fluidBind: myEnt newTrace
		during: [CurrentBertCrum fluidBind: BertCrum make
		during: [
			DiskManager consistent: 1 with: [
				^BeDataHolder create: value]]]!
*/
}
/**
 * Make a new IDHolder for the given ID. Uses an existing one if it exists.
 */
public BeIDHolder newIDHolder(ID iD) {
	BeIDHolder result;
	result = (BeIDHolder) (myIDHolders.fetch(iD));
	if (result == null) {
		AboraBlockSupport.enterConsistent();
		try {
			Object currentTraceOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentTrace, myEnt.newTrace());
			try {
				Object currentBertCrumOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentBertCrum, BertCrum.make());
				try {
					result = BeIDHolder.make(iD);
					myIDHolders.introduce(iD, result);
				}
				finally {
					AboraBlockSupport.exitFluidBindDuring(CurrentBertCrum, currentBertCrumOldValue);
				}
			}
			finally {
				AboraBlockSupport.exitFluidBindDuring(CurrentTrace, currentTraceOldValue);
			}
		}
		finally {
			AboraBlockSupport.exitConsistent();
		}
	}
	return result;
/*
udanax-top.st:2076:BeGrandMap methodsFor: 'making other things'!
{BeIDHolder} newIDHolder: iD {ID}
	"Make a new IDHolder for the given ID. Uses an existing one if it exists."
	
	| result {BeIDHolder} |
	result := (myIDHolders fetch: iD) cast: BeIDHolder.
	result == NULL ifTrue:
		[DiskManager consistent:
			[CurrentTrace fluidBind: myEnt newTrace
			during: [CurrentBertCrum fluidBind: BertCrum make
			during:
				[result := BeIDHolder make: iD.
				myIDHolders at: iD introduce: result]]]].
	^result!
*/
}
/**
 * Make a new label.
 */
public BeLabel newLabel() {
	Object currentTraceOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentTrace, myEnt.newTrace());
	try {
		Object currentBertCrumOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentBertCrum, BertCrum.make());
		try {
			AboraBlockSupport.enterConsistent(1);
			try {
				return new BeLabel();
			}
			finally {
				AboraBlockSupport.exitConsistent();
			}
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(CurrentBertCrum, currentBertCrumOldValue);
		}
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(CurrentTrace, currentTraceOldValue);
	}
/*
udanax-top.st:2090:BeGrandMap methodsFor: 'making other things'!
{BeLabel} newLabel
	"Make a new label."
	
	CurrentTrace fluidBind: myEnt newTrace
		during: [CurrentBertCrum fluidBind: BertCrum make
		during: [
			DiskManager consistent: 1 with: [^BeLabel create]]]!
*/
}
/**
 * Make a new PlaceHolder.
 */
public BePlaceHolder newPlaceHolder() {
	Object currentTraceOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentTrace, myEnt.newTrace());
	try {
		Object currentBertCrumOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentBertCrum, BertCrum.make());
		try {
			AboraBlockSupport.enterConsistent(3);
			try {
				return new BePlaceHolder();
			}
			finally {
				AboraBlockSupport.exitConsistent();
			}
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(CurrentBertCrum, currentBertCrumOldValue);
		}
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(CurrentTrace, currentTraceOldValue);
	}
/*
udanax-top.st:2098:BeGrandMap methodsFor: 'making other things'!
{BePlaceHolder} newPlaceHolder
	"Make a new PlaceHolder."
	
	CurrentTrace fluidBind: myEnt newTrace
		during: [CurrentBertCrum fluidBind: BertCrum make
		during: [DiskManager consistent: 3 with: [^BePlaceHolder create]]]!
*/
}
/**
 * Make a new Work (without an ID) with the given contents.  Everything
 * else comes from the fluid environment.
 */
public BeWork newWork(FeEdition contents) {
	Object currentTraceOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentTrace, myEnt.newTrace());
	try {
		Object currentBertCrumOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentBertCrum, BertCrum.make());
		try {
			return BeWork.make(contents);
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(CurrentBertCrum, currentBertCrumOldValue);
		}
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(CurrentTrace, currentTraceOldValue);
	}
/*
udanax-top.st:2105:BeGrandMap methodsFor: 'making other things'!
{BeWork} newWork: contents {FeEdition}
	"Make a new Work (without an ID) with the given contents.  Everything 
	 else comes from the fluid environment."
	
	CurrentTrace fluidBind: myEnt newTrace
		during: [CurrentBertCrum fluidBind: BertCrum make
		during: [^BeWork make: contents]]!
*/
}
public ID accessClubID() {
	return myAccessClubID;
/*
udanax-top.st:2115:BeGrandMap methodsFor: 'clubs'!
{ID} accessClubID
	^myAccessClubID!
*/
}
public ID adminClubID() {
	return myAdminClubID;
/*
udanax-top.st:2119:BeGrandMap methodsFor: 'clubs'!
{ID} adminClubID
	^myAdminClubID!
*/
}
public ID archiveClubID() {
	return myArchiveClubID;
/*
udanax-top.st:2123:BeGrandMap methodsFor: 'clubs'!
{ID} archiveClubID
	^myArchiveClubID!
*/
}
public ID emptyClubID() {
	return myEmptyClubID;
/*
udanax-top.st:2127:BeGrandMap methodsFor: 'clubs'!
{ID} emptyClubID
	^myEmptyClubID!
*/
}
public ID publicClubID() {
	return myPublicClubID;
/*
udanax-top.st:2131:BeGrandMap methodsFor: 'clubs'!
{ID} publicClubID
	^myPublicClubID!
*/
}
public BeClub newClub(FeEdition desc) {
	return newClub(desc, null);
/*
udanax-top.st:2137:BeGrandMap methodsFor: 'smalltalk: defaults'!
{BeClub} newClub: desc {FeEdition}
	^self newClub: desc with: NULL!
*/
}
/**
 * Get what is at the the given ID as a front end object; if there is nothing there, then
 * make the appropriate PlaceHolder
 * @deprecated
 */
public FeRangeElement getOrMakeFe(ID iD) {
	throw new PasseException();
/*
udanax-top.st:2142:BeGrandMap methodsFor: 'smalltalk: passe'!
{FeRangeElement} getOrMakeFe: iD {ID}
	"Get what is at the the given ID as a front end object; if there is nothing there, then make the appropriate PlaceHolder"
	
	| result {BeRangeElement} |
	result := self fetch: iD.
	self knownBug.  "This doesn't supply a label for Editions."
	result ~~ NULL
		ifTrue: [^result makeFe: NULL]
		ifFalse: [^FePlaceHolder grand: iD]!
*/
}
/**
 * Recreate an old IDSpace from externally stored numbers
 * @deprecated
 */
public IDSpace iDSpace(Sequence identifier) {
	throw new PasseException();
/*
udanax-top.st:2152:BeGrandMap methodsFor: 'smalltalk: passe'!
{IDSpace} iDSpace: identifier {Sequence}
	"Recreate an old IDSpace from externally stored numbers"
	
	self passe "IDSpace::import"!
*/
}
public BeGrandMap(Rcvr receiver) {
	super(receiver);
	myIdentifier = (Sequence) receiver.receiveHeaper();
	myGlobalIDSpace = (IDSpace) receiver.receiveHeaper();
	myLocalIDSpaceCounter = (Counter) receiver.receiveHeaper();
	myGlobalIDFilterSpace = (FilterSpace) receiver.receiveHeaper();
	myEndorsementSpace = (CrossSpace) receiver.receiveHeaper();
	myEndorsementFilterSpace = (FilterSpace) receiver.receiveHeaper();
	myIDHolders = (MuTable) receiver.receiveHeaper();
	myIDCounters = (MuTable) receiver.receiveHeaper();
	myRangeElements = (MuTable) receiver.receiveHeaper();
	myRangeElementIDs = (MuTable) receiver.receiveHeaper();
	myEnt = (Ent) receiver.receiveHeaper();
	myEmptyClubID = (ID) receiver.receiveHeaper();
	myPublicClubID = (ID) receiver.receiveHeaper();
	myAdminClubID = (ID) receiver.receiveHeaper();
	myArchiveClubID = (ID) receiver.receiveHeaper();
	myAccessClubID = (ID) receiver.receiveHeaper();
	myClubDirectoryID = (ID) receiver.receiveHeaper();
	myGateLockSmithEdition = (BeEdition) receiver.receiveHeaper();
	myWrapperEndorsements = (ImmuTable) receiver.receiveHeaper();
	myEndorsementFlags = (PtrArray) receiver.receiveHeaper();
	myGrants = (BeEdition) receiver.receiveHeaper();
	restartBeGrandMap(receiver);
/*
udanax-top.st:2159:BeGrandMap methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myIdentifier _ receiver receiveHeaper.
	myGlobalIDSpace _ receiver receiveHeaper.
	myLocalIDSpaceCounter _ receiver receiveHeaper.
	myGlobalIDFilterSpace _ receiver receiveHeaper.
	myEndorsementSpace _ receiver receiveHeaper.
	myEndorsementFilterSpace _ receiver receiveHeaper.
	myIDHolders _ receiver receiveHeaper.
	myIDCounters _ receiver receiveHeaper.
	myRangeElements _ receiver receiveHeaper.
	myRangeElementIDs _ receiver receiveHeaper.
	myEnt _ receiver receiveHeaper.
	myEmptyClubID _ receiver receiveHeaper.
	myPublicClubID _ receiver receiveHeaper.
	myAdminClubID _ receiver receiveHeaper.
	myArchiveClubID _ receiver receiveHeaper.
	myAccessClubID _ receiver receiveHeaper.
	myClubDirectoryID _ receiver receiveHeaper.
	myGateLockSmithEdition _ receiver receiveHeaper.
	myWrapperEndorsements _ receiver receiveHeaper.
	myEndorsementFlags _ receiver receiveHeaper.
	myGrants _ receiver receiveHeaper.
	self restartBeGrandMap: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myIdentifier);
	xmtr.sendHeaper(myGlobalIDSpace);
	xmtr.sendHeaper(myLocalIDSpaceCounter);
	xmtr.sendHeaper(myGlobalIDFilterSpace);
	xmtr.sendHeaper(myEndorsementSpace);
	xmtr.sendHeaper(myEndorsementFilterSpace);
	xmtr.sendHeaper(myIDHolders);
	xmtr.sendHeaper(myIDCounters);
	xmtr.sendHeaper(myRangeElements);
	xmtr.sendHeaper(myRangeElementIDs);
	xmtr.sendHeaper(myEnt);
	xmtr.sendHeaper(myEmptyClubID);
	xmtr.sendHeaper(myPublicClubID);
	xmtr.sendHeaper(myAdminClubID);
	xmtr.sendHeaper(myArchiveClubID);
	xmtr.sendHeaper(myAccessClubID);
	xmtr.sendHeaper(myClubDirectoryID);
	xmtr.sendHeaper(myGateLockSmithEdition);
	xmtr.sendHeaper(myWrapperEndorsements);
	xmtr.sendHeaper(myEndorsementFlags);
	xmtr.sendHeaper(myGrants);
/*
udanax-top.st:2184:BeGrandMap methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myIdentifier.
	xmtr sendHeaper: myGlobalIDSpace.
	xmtr sendHeaper: myLocalIDSpaceCounter.
	xmtr sendHeaper: myGlobalIDFilterSpace.
	xmtr sendHeaper: myEndorsementSpace.
	xmtr sendHeaper: myEndorsementFilterSpace.
	xmtr sendHeaper: myIDHolders.
	xmtr sendHeaper: myIDCounters.
	xmtr sendHeaper: myRangeElements.
	xmtr sendHeaper: myRangeElementIDs.
	xmtr sendHeaper: myEnt.
	xmtr sendHeaper: myEmptyClubID.
	xmtr sendHeaper: myPublicClubID.
	xmtr sendHeaper: myAdminClubID.
	xmtr sendHeaper: myArchiveClubID.
	xmtr sendHeaper: myAccessClubID.
	xmtr sendHeaper: myClubDirectoryID.
	xmtr sendHeaper: myGateLockSmithEdition.
	xmtr sendHeaper: myWrapperEndorsements.
	xmtr sendHeaper: myEndorsementFlags.
	xmtr sendHeaper: myGrants.!
*/
}
public static BeGrandMap make() {
	return new BeGrandMap((Sequence.two(666, 42)));
/*
udanax-top.st:2220:BeGrandMap class methodsFor: 'private: pseudo constructors'!
make
	^self create: (Sequence two: 666 with: 42)!
*/
}
public static void staticTimeNonInherited() {
	AboraSupport.defineFluid(BeGrandMap.class, "CurrentGrandMap", DiskManager.emulsion(), null);
/*
udanax-top.st:2226:BeGrandMap class methodsFor: 'smalltalk: init'!
staticTimeNonInherited
	BeGrandMap defineFluid: #CurrentGrandMap with: DiskManager emulsion with: [NULL]!
*/
}
/**
 * Seconds since the beginning of time
 */
public static int xuTime() {
	Someone.knownBug();
	/* Removed translateOnly */
	return AboraSupport.xuTime();
/*
udanax-top.st:2232:BeGrandMap class methodsFor: 'global: time'!
{IntegerVar} xuTime
	"Seconds since the beginning of time"
	
	self knownBug.
	'return 3;' translateOnly.
	[^Time xuTime] smalltalkOnly!
*/
}
public BeGrandMap() {
/*

Generated during transformation
*/
}
}
