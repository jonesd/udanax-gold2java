/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.fossil;

import info.dgjones.abora.gold.backrec.ResultRecorder;
import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.canopy.SensorCrum;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.fossil.DirectEditionRecorderFossil;
import info.dgjones.abora.gold.fossil.DirectWorkRecorderFossil;
import info.dgjones.abora.gold.fossil.IndirectEditionRecorderFossil;
import info.dgjones.abora.gold.fossil.IndirectWorkRecorderFossil;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeKeyMaster;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.tclude.TrailBlazer;
import info.dgjones.abora.gold.turtle.Agenda;
import info.dgjones.abora.gold.turtle.AgendaItem;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * A Fossil for a ResultRecorder, which also stores its permissions, filters, and a cache of
 * the results which have already been recorded.
 */
public class RecorderFossil extends Abraham {

	protected IDRegion myLoginAuthority;
	protected TrailBlazer myTrailBlazer;
	protected ResultRecorder myRecorder;
	protected int myRecorderCount;
	protected int myAgendaCount;
/*
udanax-top.st:10544:
Abraham subclass: #RecorderFossil
	instanceVariableNames: '
		myLoginAuthority {IDRegion}
		myTrailBlazer {TrailBlazer | NULL}
		myRecorder {ResultRecorder NOCOPY | NULL}
		myRecorderCount {IntegerVar NOCOPY}
		myAgendaCount {IntegerVar}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-fossil'!
*/
/*
udanax-top.st:10553:
RecorderFossil comment:
'A Fossil for a ResultRecorder, which also stores its permissions, filters, and a cache of the results which have already been recorded.'!
*/
/*
udanax-top.st:10555:
(RecorderFossil getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #DEFERRED.LOCKED; yourself)!
*/
/*
udanax-top.st:10749:
RecorderFossil class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:10752:
(RecorderFossil getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #DEFERRED.LOCKED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(RecorderFossil.class).setAttributes( new Set().add("DEFERRED").add("COPY").add("SHEPHERDPATRIARCH").add("DEFERREDLOCKED"));
/*

Generated during transformation: AddMethod
*/
}
public void addItem(AgendaItem item) {
	AboraBlockSupport.enterInsistent(1);
	try {
		myAgendaCount = myAgendaCount + 1;
		diskUpdate();
		memoryCheck();
	}
	finally {
		AboraBlockSupport.exitInsistent();
	}
/*
udanax-top.st:10560:RecorderFossil methodsFor: 'accessing'!
{void} addItem: item {AgendaItem unused}
	DiskManager insistent: 1 with:
		[myAgendaCount _ myAgendaCount + 1.
		self diskUpdate.
		self memoryCheck]!
*/
}
/**
 * Should only be called from BeEdition::fossilRelease().  Results in my becoming extinct.
 */
public void extinguish(TrailBlazer trailBlazer) {
	if (myTrailBlazer == null) {
		throw new AboraRuntimeException(AboraRuntimeException.ALREADY_EXTINCT);
	}
	if ( ! (myTrailBlazer.isEqual(trailBlazer))) {
		throw new AboraRuntimeException(AboraRuntimeException.WHO_SAYS);
	}
	if (myRecorderCount != 0) {
		throw new AboraRuntimeException(AboraRuntimeException.RECORDERS_STILL_OUTSTANDING);
	}
	if (myRecorder != null) {
		myRecorder.destroy();
		myRecorder = null;
	}
	AboraBlockSupport.enterInsistent(1);
	try {
		myTrailBlazer = null;
		diskUpdate();
		memoryCheck();
	}
	finally {
		AboraBlockSupport.exitInsistent();
	}
/*
udanax-top.st:10567:RecorderFossil methodsFor: 'accessing'!
{void} extinguish: trailBlazer {TrailBlazer}
	"Should only be called from BeEdition::fossilRelease().  Results in my becoming extinct."
	
	myTrailBlazer == NULL
		ifTrue: [Heaper BLAST: #AlreadyExtinct].
	(myTrailBlazer isEqual: trailBlazer) not
		ifTrue: [Heaper BLAST: #WhoSays].
	myRecorderCount ~= Int32Zero
		ifTrue: [Heaper BLAST: #RecordersStillOutstanding].
	myRecorder ~~ NULL
		ifTrue:
			[myRecorder destroy.
			myRecorder _ NULL].
	DiskManager insistent: 1 with:
		[myTrailBlazer _ NULL.
		self diskUpdate.
		self memoryCheck]!
*/
}
/**
 * As a premature optimization, we don't destroy the waldo when the count goes to zero, but
 * rather when we consider purging while the count is zero.
 */
public void releaseRecorder() {
	if ( ! (myRecorderCount >= 1)) {
		throw new AboraAssertionException();
	}
	myRecorderCount = myRecorderCount - 1;
/*
udanax-top.st:10585:RecorderFossil methodsFor: 'accessing'!
{void} releaseRecorder
	"As a premature optimization, we don't destroy the waldo when the count goes to zero, but rather when we consider purging while the count is zero."
	
	(myRecorderCount >= 1) assert.
	myRecorderCount _ myRecorderCount - 1!
*/
}
public void removeItem(AgendaItem item) {
	if ( ! (myAgendaCount >= 1)) {
		throw new AboraAssertionException();
	}
	AboraBlockSupport.enterInsistent(1);
	try {
		myAgendaCount = myAgendaCount - 1;
		diskUpdate();
		memoryCheck();
	}
	finally {
		AboraBlockSupport.exitInsistent();
	}
/*
udanax-top.st:10591:RecorderFossil methodsFor: 'accessing'!
{void} removeItem: item {AgendaItem unused}
	(myAgendaCount >= 1) assert.
	DiskManager insistent: 1 with:
		[myAgendaCount _ myAgendaCount - 1.
		self diskUpdate.
		self memoryCheck]!
*/
}
/**
 * The Recorder of which this Fossil is the imprint. If necessary, reconstruct it using the
 * information stored in the imprint.
 * Should only be called if I am not extinct
 * Should only be called from the reanimate macro.
 */
public ResultRecorder secretRecorder() {
	/* If I'm extinct, somebody goofed.
		Blow 'em up.
	If we haven't already reanimated a recorder (because this is the outermost reanimate for this fossil)
		bind a new current KeyMaster (recovering the fossilized permissions)
					make a recorder implicitly using the fossilized permissions
						and explicitly using the fossilized endorsements
						and trail.
	bump the refcount on myRecorder
	return myRecorder */
	if (isExtinct()) {
		throw new AboraRuntimeException(AboraRuntimeException.FOSSIL_EXTINCT);
	}
	if (myRecorder == null) {
		Object currentKeyMasterOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentKeyMaster, (FeKeyMaster.makeAll(myLoginAuthority)));
		try {
			myRecorder = actualRecorder();
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(CurrentKeyMaster, currentKeyMasterOldValue);
		}
	}
	myRecorderCount = myRecorderCount + 1;
	return myRecorder;
/*
udanax-top.st:10599:RecorderFossil methodsFor: 'accessing'!
{ResultRecorder} secretRecorder
	"The Recorder of which this Fossil is the imprint. If necessary, reconstruct it using the information stored in the imprint.
	Should only be called if I am not extinct
	Should only be called from the reanimate macro."
	| |
	
	"If I'm extinct, somebody goofed.
		Blow 'em up.
	If we haven't already reanimated a recorder (because this is the outermost reanimate for this fossil)
		bind a new current KeyMaster (recovering the fossilized permissions)
					make a recorder implicitly using the fossilized permissions
						and explicitly using the fossilized endorsements
						and trail.
	bump the refcount on myRecorder
	return myRecorder"
	
	self isExtinct
		ifTrue: [Heaper BLAST: #FossilExtinct].
	myRecorder == NULL ifTrue:
		[CurrentKeyMaster fluidBind: (FeKeyMaster makeAll: myLoginAuthority)
			during: [myRecorder := self actualRecorder]].
	myRecorderCount := myRecorderCount + 1.
	^myRecorder!
*/
}
/*
udanax-top.st:10626:RecorderFossil methodsFor: 'smalltalk: reanimation'!
{void} reanimate: aBlock {BlockClosure of: RecorderFossil}
	"Should only be called if I am not extinct.
	
	f reanimate: [:w {RecorderFossil} | ...]
		should translate to
	BEGIN_REANIMATE(f,RecorderFossil,w) {
		...
	} END_REANIMATE;"
	
	[aBlock value: self secretRecorder]
		valueNowOrOnUnwindDo: (RecorderFossil bomb.ReleaseRecorder: self)!
*/
/**
 * A Fossil (unlike a Grabber or an Orgl) does not prevent the grabbed IObject from being
 * dismantled.  Instead, if the IObject does get dismantled, then the Fossil is considered
 * extinct.  A waldo may not be gotten from an extinct fossil (if the species is really
 * extinct, then it cannot be revived from its remaining fossils).
 */
public boolean isExtinct() {
	return myTrailBlazer == null;
/*
udanax-top.st:10640:RecorderFossil methodsFor: 'testing'!
{BooleanVar} isExtinct
	"A Fossil (unlike a Grabber or an Orgl) does not prevent the grabbed IObject from being dismantled.  Instead, if the IObject does get dismantled, then the Fossil is considered extinct.  A waldo may not be gotten from an extinct fossil (if the species is really extinct, then it cannot be revived from its remaining fossils)."
	 
	^myTrailBlazer == NULL!
*/
}
/**
 * I can`t go to disk while someone has my WaldoSocket and might be doing
 * something with the Waldo in it.
 */
public boolean isPurgeable() {
	if (super.isPurgeable() && (myRecorderCount == 0)) {
		if (myRecorder != null) {
			myRecorder.destroy();
			myRecorder = null;
		}
		return true;
	}
	else {
		return false;
	}
/*
udanax-top.st:10645:RecorderFossil methodsFor: 'testing'!
{BooleanVar} isPurgeable
	"I can`t go to disk while someone has my WaldoSocket and might be doing 
	something with the Waldo in it."
	
	(super isPurgeable and: [myRecorderCount == Int32Zero])
		ifTrue: 
			[myRecorder ~~ NULL
				ifTrue: 
					[myRecorder destroy.
					myRecorder _ NULL].
			^true]
		ifFalse: [^false]!
*/
}
public void restartRecorderFossil(Rcvr rcvr) {
	myRecorder = null;
	myRecorderCount = 0;
/*
udanax-top.st:10660:RecorderFossil methodsFor: 'hooks:'!
{void RECEIVE.HOOK} restartRecorderFossil: rcvr {Rcvr unused default: NULL}
	myRecorder _ NULL.
	myRecorderCount _ Int32Zero!
*/
}
public void dismantle() {
	if ( ! (myRecorderCount == 0)) {
		throw new AboraAssertionException();
	}
	/* (myAgendaCount = Int32Zero) assert. */
	if (myRecorder != null) {
		myRecorder.destroy();
		myRecorder = null;
	}
	AboraBlockSupport.enterConsistent(2);
	try {
		if (Heaper.isConstructed(myTrailBlazer)) {
			myTrailBlazer.removeReference(this);
		}
		myTrailBlazer = null;
		super.dismantle();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:10667:RecorderFossil methodsFor: 'protected: destruction'!
{void} dismantle
	(myRecorderCount = Int32Zero) assert.
	"(myAgendaCount = Int32Zero) assert."
	myRecorder ~~ NULL ifTrue:
		[myRecorder destroy.
		myRecorder _ NULL].
	DiskManager consistent: 2 with:
		[(Heaper isConstructed: myTrailBlazer) ifTrue:
			[myTrailBlazer removeReference: self].
		myTrailBlazer := NULL.
		super dismantle]!
*/
}
/**
 * Make the right kind of Recorder for this fossil
 */
public ResultRecorder actualRecorder() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:10682:RecorderFossil methodsFor: 'protected: accessing'!
{ResultRecorder} actualRecorder
	"Make the right kind of Recorder for this fossil"
	
	self subclassResponsibility!
*/
}
public void memoryCheck() {
	if (myTrailBlazer == null
	/* and: [myAgendaCount = Int32Zero] */
	) {
		forget();
	}
	else {
		remember();
	}
/*
udanax-top.st:10687:RecorderFossil methodsFor: 'protected: accessing'!
{void} memoryCheck
	(myTrailBlazer == NULL "and: [myAgendaCount = Int32Zero]")
		ifTrue: [self forget]
		ifFalse: [self remember]!
*/
}
public TrailBlazer trailBlazer() {
	if (myTrailBlazer == null) {
		throw new AboraRuntimeException(AboraRuntimeException.FATAL_ERROR);
	}
	/* should have already been checked */
	return myTrailBlazer;
/*
udanax-top.st:10693:RecorderFossil methodsFor: 'protected: accessing'!
{TrailBlazer} trailBlazer
	myTrailBlazer == NULL ifTrue:
		[Heaper BLAST: #FatalError]. "should have already been checked"
	^myTrailBlazer!
*/
}
public RecorderFossil(IDRegion loginAuthority, TrailBlazer trailBlazer) {
	super();
	myLoginAuthority = loginAuthority;
	myTrailBlazer = trailBlazer;
	myTrailBlazer.addReference(this);
	myAgendaCount = 0;
	restartRecorderFossil(null);
/*
udanax-top.st:10701:RecorderFossil methodsFor: 'create'!
create: loginAuthority {IDRegion}
	with: trailBlazer {TrailBlazer}
	
	super create.
	myLoginAuthority := loginAuthority.
	myTrailBlazer := trailBlazer.
	myTrailBlazer addReference: self.
	
	myAgendaCount _ Int32Zero.
	self restartRecorderFossil: NULL.!
*/
}
/**
 * Store recording agents into a SensorCrum on data in the original Edition that was a source
 * of the query
 */
public void storeDataRecordingAgents(SensorCrum sensorCrum, Agenda agenda) {
	agenda.registerItem((sensorCrum.recordingAgent(this))
	/* default behaviour */
	);
/*
udanax-top.st:10714:RecorderFossil methodsFor: 'backfollow'!
{void} storeDataRecordingAgents: sensorCrum {SensorCrum}
	with: agenda {Agenda}
	"Store recording agents into a SensorCrum on data in the original Edition that was a source of the query"
	
	agenda registerItem: (sensorCrum recordingAgent: self) "default behaviour"!
*/
}
/**
 * Store recording agents into a SensorCrum on partiality in the original Edition that was a
 * source of the query
 */
public void storePartialityRecordingAgents(SensorCrum sensorCrum, Agenda agenda) {
	agenda.registerItem((sensorCrum.recordingAgent(this))
	/* default behaviour */
	);
/*
udanax-top.st:10720:RecorderFossil methodsFor: 'backfollow'!
{void} storePartialityRecordingAgents: sensorCrum {SensorCrum}
	with: agenda {Agenda}
	"Store recording agents into a SensorCrum on partiality in the original Edition that was a source of the query"
	
	agenda registerItem: (sensorCrum recordingAgent: self) "default behaviour"!
*/
}
/**
 * Store recording agents into a SensorCrum on a RangeElement in the original Edition that
 * was a source of the query
 */
public void storeRangeElementRecordingAgents(BeRangeElement rangeElement, SensorCrum sensorCrum, Agenda agenda) {
	agenda.registerItem((sensorCrum.recordingAgent(this))
	/* default behaviour */
	);
/*
udanax-top.st:10726:RecorderFossil methodsFor: 'backfollow'!
{void} storeRangeElementRecordingAgents: rangeElement {BeRangeElement unused}
	with: sensorCrum {SensorCrum}
	with: agenda {Agenda}
	"Store recording agents into a SensorCrum on a RangeElement in the original Edition that was a source of the query"
	
	agenda registerItem: (sensorCrum recordingAgent: self) "default behaviour"!
*/
}
public RecorderFossil(Rcvr receiver) {
	super(receiver);
	myLoginAuthority = (IDRegion) receiver.receiveHeaper();
	myTrailBlazer = (TrailBlazer) receiver.receiveHeaper();
	myAgendaCount = receiver.receiveIntegerVar();
	restartRecorderFossil(receiver);
/*
udanax-top.st:10735:RecorderFossil methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myLoginAuthority _ receiver receiveHeaper.
	myTrailBlazer _ receiver receiveHeaper.
	myAgendaCount _ receiver receiveIntegerVar.
	self restartRecorderFossil: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myLoginAuthority);
	xmtr.sendHeaper(myTrailBlazer);
	xmtr.sendIntegerVar(myAgendaCount);
/*
udanax-top.st:10742:RecorderFossil methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myLoginAuthority.
	xmtr sendHeaper: myTrailBlazer.
	xmtr sendIntegerVar: myAgendaCount.!
*/
}
public static RecorderFossil transcluders(boolean isDirectOnly, IDRegion loginAuthority, Filter directFilter, Filter indirectFilter, TrailBlazer trailBlazer) {
	AboraBlockSupport.enterConsistent(2);
	try {
		if (isDirectOnly) {
			return new DirectEditionRecorderFossil(loginAuthority, directFilter, indirectFilter, trailBlazer);
		}
		else {
			return new IndirectEditionRecorderFossil(loginAuthority, directFilter, indirectFilter, trailBlazer);
		}
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:10757:RecorderFossil class methodsFor: 'create'!
{RecorderFossil} transcluders: isDirectOnly {BooleanVar} 
	with: loginAuthority {IDRegion}
	with: directFilter {Filter of: (Tuple of: ID with: ID)}
	with: indirectFilter {Filter of: (Tuple of: ID with: ID)}
	with: trailBlazer {TrailBlazer} 
	DiskManager consistent: 2 with:
		[isDirectOnly
			ifTrue: [^DirectEditionRecorderFossil
					create: loginAuthority
					with: directFilter
					with: indirectFilter
					with: trailBlazer]
			ifFalse: [^IndirectEditionRecorderFossil
					create: loginAuthority
					with: directFilter
					with: indirectFilter
					with: trailBlazer]]!
*/
}
public static RecorderFossil works(boolean isDirectOnly, IDRegion loginAuthority, Filter endorsementsFilter, TrailBlazer trailBlazer) {
	AboraBlockSupport.enterConsistent(2);
	try {
		if (isDirectOnly) {
			return new DirectWorkRecorderFossil(loginAuthority, endorsementsFilter, trailBlazer);
		}
		else {
			return new IndirectWorkRecorderFossil(loginAuthority, endorsementsFilter, trailBlazer);
		}
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:10775:RecorderFossil class methodsFor: 'create'!
{RecorderFossil} works: isDirectOnly {BooleanVar}
	with: loginAuthority {IDRegion}
	with: endorsementsFilter {Filter of: (Tuple of: ID with: ID)}
	with: trailBlazer {TrailBlazer}
	DiskManager consistent: 2 with:
		[isDirectOnly
			ifTrue: [^DirectWorkRecorderFossil create: loginAuthority
				with: endorsementsFilter
				with: trailBlazer]
			ifFalse: [^IndirectWorkRecorderFossil create: loginAuthority
				with: endorsementsFilter
				with: trailBlazer]]!
*/
}
public static void bombReleaseRecorder(RecorderFossil CHARGE) {
	CHARGE.releaseRecorder();
/*
udanax-top.st:10790:RecorderFossil class methodsFor: 'exceptions: exceptions'!
bomb.ReleaseRecorder: CHARGE {RecorderFossil}
	^[CHARGE releaseRecorder]!
*/
}
/**
 * @deprecated
 */
public static RecorderFossil make(IDRegion loginAuthority, Filter eFilter, BeEdition trail) {
	throw new PasseException();
/*
udanax-top.st:10796:RecorderFossil class methodsFor: 'smalltalk: passe'!
make: loginAuthority {IDRegion}
	with: eFilter {Filter of: (Tuple of: ID with: ID)}
	with: trail {BeEdition}
	self passe!
*/
}
public RecorderFossil() {
/*

Generated during transformation
*/
}
}
