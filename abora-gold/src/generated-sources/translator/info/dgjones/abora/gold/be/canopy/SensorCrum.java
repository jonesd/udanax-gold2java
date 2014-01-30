/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.canopy;

import info.dgjones.abora.gold.backrec.ResultRecorder;
import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.canopy.CanopyCache;
import info.dgjones.abora.gold.be.canopy.CanopyCrum;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.SensorCrum;
import info.dgjones.abora.gold.be.canopy.prop.Prop;
import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.Stamp;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.props.PropChange;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.spaces.cross.CrossRegion;
import info.dgjones.abora.gold.turtle.Agenda;
import info.dgjones.abora.gold.turtle.AgendaItem;
import info.dgjones.abora.gold.turtle.RecorderHoister;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import java.io.PrintWriter;
import java.io.StringWriter;

/**
 * This implementation is the same as BertCrums.  This will require
 * pointers into the ent to implement delete (for archiving).  Canopy
 * reorganization could be achieved by removing several orgls, then
 * re-adding them (archive then restore).
 */
public class SensorCrum extends CanopyCrum {

	protected ImmuSet myBackfollowRecorders;
/*
udanax-top.st:5277:
CanopyCrum subclass: #SensorCrum
	instanceVariableNames: 'myBackfollowRecorders {ImmuSet of: RecorderFossil}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Canopy'!
*/
/*
udanax-top.st:5281:
SensorCrum comment:
'This implementation is the same as BertCrums.  This will require 
pointers into the ent to implement delete (for archiving).  Canopy 
reorganization could be achieved by removing several orgls, then 
re-adding them (archive then restore).'!
*/
/*
udanax-top.st:5286:
(SensorCrum getOrMakeCxxClassDescription)
	friends:
'friend class RecorderHoister;
';
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:5539:
SensorCrum class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:5542:
(SensorCrum getOrMakeCxxClassDescription)
	friends:
'friend class RecorderHoister;
';
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SensorCrum.class).setAttributes( new Set().add("LOCKED").add("COPY").add("SHEPHERDPATRIARCH").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Make a canopyCrum for a root:  it has no children.
 */
public SensorCrum() {
	super(0);
	myBackfollowRecorders = ImmuSet.make();
	newShepherd();
/*
udanax-top.st:5294:SensorCrum methodsFor: 'private: creation'!
create
	"Make a canopyCrum for a root:  it has no children."
	super create: UInt32Zero.
	myBackfollowRecorders _ ImmuSet make.
	self newShepherd!
*/
}
/**
 * Make a canopyCrum for a root:  it has no children.
 */
public SensorCrum(int flags) {
	super(flags);
	myBackfollowRecorders = ImmuSet.make();
	newShepherd();
/*
udanax-top.st:5300:SensorCrum methodsFor: 'private: creation'!
create: flags {UInt32}
	"Make a canopyCrum for a root:  it has no children."
	super create: flags.
	myBackfollowRecorders _ ImmuSet make.
	self newShepherd!
*/
}
/**
 * SensorCrum create verify2.
 */
public CanopyCrum another() {
	return new SensorCrum();
/*
udanax-top.st:5309:SensorCrum methodsFor: 'smalltalk:'!
{CanopyCrum} another
	"SensorCrum create verify2."
	^SensorCrum create!
*/
}
public String displayString() {
	StringWriter stringWriter = new StringWriter();
	PrintWriter aStream = new PrintWriter(stringWriter);
	aStream.print(maxHeight());
	if ( ! (maxHeight() == minHeight())) {
		aStream.print('-');
		aStream.print(minHeight());
	}
	return stringWriter.toString();
/*
udanax-top.st:5314:SensorCrum methodsFor: 'smalltalk:'!
displayString
	^String
		streamContents: 
			[:aStream | 
			aStream print: self maxHeight.
			self maxHeight == self minHeight 
				ifFalse: [aStream nextPut: $-; print: self minHeight]]!
*/
}
/*
udanax-top.st:5322:SensorCrum methodsFor: 'smalltalk:'!
inspectMenuArray
	^#(
		('inspect oparts'	inspectOParts		''))!
*/
/*
udanax-top.st:5326:SensorCrum methodsFor: 'smalltalk:'!
inspectOParts
	| owners |
	owners _ self allOwners select: [ :each | each isKindOf: OPart].
	owners isEmpty ifTrue:
		[Transcript show: 'Nobody'; cr]
	ifFalse: [owners size = 1 ifTrue:
		[owners first inspect]
	ifFalse:
		[owners inspect]]!
*/
public void printOn(PrintWriter aStream) {
	if (myBackfollowRecorders == null) {
		aStream.print(getAboraClass().name());
		aStream.print("(nil)");
		return ;
	}
	aStream.print(getAboraClass().name());
	aStream.print("(");
	aStream.print((Integer.toString(flags(), 2)));
	aStream.print(")");
	if ( ! (myBackfollowRecorders.isEmpty())) {
		aStream.print(" *");
	}
/*
udanax-top.st:5336:SensorCrum methodsFor: 'smalltalk:'!
{void} printOn: aStream 
	[myBackfollowRecorders == nil ifTrue: [
		aStream << self getCategory name << '(nil)'. ^ VOID]] smalltalkOnly.
		
	aStream << self getCategory name << '(' << (self flags printStringRadix: 2) << ')'.
	myBackfollowRecorders isEmpty ifFalse:
		[aStream << ' *']!
*/
}
/**
 * should have one per Ent
 */
public CanopyCache canopyCache() {
	return ((CanopyCache) CurrentSensorCanopyCache.fluidGet());
/*
udanax-top.st:5346:SensorCrum methodsFor: 'protected:'!
{CanopyCache wimpy} canopyCache
	"should have one per Ent"
	^CurrentSensorCanopyCache fluidGet!
*/
}
public CanopyCrum makeNew() {
	Dean.thingToDo();
	/* is this right? I want to preserve the partiality flag when a partial loaf splits /ravi/5/7/92/ */
	if (isPartial()) {
		return new SensorCrum(SensorCrum.isPartialFlag());
	}
	else {
		return new SensorCrum();
	}
/*
udanax-top.st:5350:SensorCrum methodsFor: 'protected:'!
{CanopyCrum} makeNew
	Dean thingToDo. "is this right? I want to preserve the partiality flag when a partial loaf splits /ravi/5/7/92/"
	self isPartial ifTrue:
		[^SensorCrum create: SensorCrum isPartialFlag]
	ifFalse:
		[^SensorCrum create]!
*/
}
/**
 * Set off all recorders that respond to the change either in me or in any of my ancestors up
 * to but not including sCrum
 * (If I am the same as sCrum, skip me as well.)
 * (If sCrum is null, search through all my ancestors to a root of the sensor canopy.)
 * return simplest finder for looking at children
 */
public PropFinder checkRecorders(PropFinder finder, SensorCrum scrum) {
	SensorCrum next;
	/* from self rootward until told to stop (at sCrum or the root)
		trigger any matching recorders
	return a simplified finder for examining children. */
	next = this;
	while (next != null) {
		next = next.fetchNextAfterTriggeringRecorders(finder, scrum);
	}
	return finder.pass(this);
/*
udanax-top.st:5360:SensorCrum methodsFor: 'accessing'!
{PropFinder} checkRecorders: finder {PropFinder} 
	with: scrum {SensorCrum | NULL} 
	
	"Set off all recorders that respond to the change either in me or in any of my ancestors up to but not including sCrum
	(If I am the same as sCrum, skip me as well.)
	(If sCrum is null, search through all my ancestors to a root of the sensor canopy.)
	return simplest finder for looking at children"
	
	| next {SensorCrum | NULL} |
	
	"from self rootward until told to stop (at sCrum or the root)
		trigger any matching recorders
	return a simplified finder for examining children."
	
	next := self.
	[next ~~ NULL] whileTrue:
		[next := next fetchNextAfterTriggeringRecorders: finder with: scrum].
	^finder pass: self!
*/
}
/**
 * Set off all recorders in me that respond to the change, if appropriate
 * (If I am the same as sCrum, skip me.)
 * If sCrum is null or not me, return my parent so caller can iterate through my ancestors to
 * sCrum or a root.
 */
public SensorCrum fetchNextAfterTriggeringRecorders(PropFinder finder, SensorCrum sCrum) {
	/* One step of the leafward walk of the O-plane, triggering recorders:
	Walk rootward on the sensor canopy, where many steps may correspond to this single leafward step. */
	/* If we're the designated sCrum (where this work was already done)
	 	return without doing anything.  We're done.
	For each of our recorders
		if it hasn't gone extinct
			reanimate it long enough to
				trigger it, recording stamp if finder matches.
	Return a pointer to our parent (so caller can iterate this operation rootward). */
	if (sCrum != null && (isEqual(sCrum))) {
		return null;
	}
	Stepper stomper = myBackfollowRecorders.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		RecorderFossil fossil = (RecorderFossil) stomper.fetch();
		if (fossil == null) {
			continue ;
		}
		if ( ! (fossil.isExtinct())) {
			ResultRecorder recorder = AboraBlockSupport.enterRecorderFossilReanimate(fossil);
			try {
				recorder.triggerIfMatching(finder, fossil);
			}
			finally {
				AboraBlockSupport.exitRecorderFossilReanimate();
			}
		}
	}
	stomper.destroy();
	return (SensorCrum) fetchParent();
/*
udanax-top.st:5379:SensorCrum methodsFor: 'accessing'!
{SensorCrum | NULL} fetchNextAfterTriggeringRecorders: finder {PropFinder} 
	with: sCrum {SensorCrum | NULL}
	
	"Set off all recorders in me that respond to the change, if appropriate
	(If I am the same as sCrum, skip me.)
	If sCrum is null or not me, return my parent so caller can iterate through my ancestors to sCrum or a root."
	
	|  |
	
	"One step of the leafward walk of the O-plane, triggering recorders:
	Walk rootward on the sensor canopy, where many steps may correspond to this single leafward step."
	
	"If we're the designated sCrum (where this work was already done)
	 	return without doing anything.  We're done.
	For each of our recorders
		if it hasn't gone extinct
			reanimate it long enough to
				trigger it, recording stamp if finder matches.
	Return a pointer to our parent (so caller can iterate this operation rootward)."
	
	(sCrum ~~ NULL and: [self isEqual: sCrum]) ifTrue:
		[^NULL].
	myBackfollowRecorders stepper forEach: [ :fossil {RecorderFossil} |
		fossil isExtinct ifFalse:
			[fossil reanimate: [:recorder {ResultRecorder} |
				recorder triggerIfMatching: finder with: fossil]]].
	^self fetchParent cast: SensorCrum.!
*/
}
public boolean isPartial() {
	return (flags() & SensorCrum.isPartialFlag()) != 0;
/*
udanax-top.st:5407:SensorCrum methodsFor: 'accessing'!
{BooleanVar} isPartial
	^(self flags bitAnd: SensorCrum isPartialFlag) ~= UInt32Zero!
*/
}
public ImmuSet recorders() {
	return myBackfollowRecorders;
/*
udanax-top.st:5411:SensorCrum methodsFor: 'accessing'!
{ImmuSet of: RecorderFossil} recorders
	^myBackfollowRecorders!
*/
}
/**
 * NOTE: The AgendaItem returned is not yet scheduled.  Doing so is up to my caller.
 */
public AgendaItem recordingAgent(RecorderFossil recorder) {
	/* If the recorder we're adding isn't already present here
		pack up the fossil for shipment to the hoister
		atomically
			Install the recorder here
			return a RecorderHoister to propagate the side-effects and anneal the canopy
			(The RecorderHoister will update myFlags)
	return an empty agenda (to satisfy our contract) */
	if ( ! (myBackfollowRecorders.hasMember(recorder))) {
		ImmuSet cargo;
		cargo = ImmuSet.make().with(recorder);
		AboraBlockSupport.enterConsistent(2);
		try {
			installRecorders(cargo);
			diskUpdate();
			return RecorderHoister.make(this, cargo);
		}
		finally {
			AboraBlockSupport.exitConsistent();
		}
	}
	return Agenda.make();
/*
udanax-top.st:5414:SensorCrum methodsFor: 'accessing'!
{AgendaItem} recordingAgent: recorder {RecorderFossil}
	"NOTE: The AgendaItem returned is not yet scheduled.  Doing so is up to my caller."
	
	|  |
	
	"If the recorder we're adding isn't already present here
		pack up the fossil for shipment to the hoister
		atomically
			Install the recorder here
			return a RecorderHoister to propagate the side-effects and anneal the canopy
			(The RecorderHoister will update myFlags)
	return an empty agenda (to satisfy our contract)"
	
	(myBackfollowRecorders hasMember: recorder) ifFalse:
		[ | cargo {ImmuSet of: RecorderFossil} |
		cargo := ImmuSet make with: recorder.
		DiskManager consistent: 2 with:
			[self installRecorders: cargo.
			self diskUpdate.
			^RecorderHoister make: self with: cargo]].
	^Agenda make!
*/
}
/**
 * Remove recorders because they have migrated rootward.
 * Recalculate myOwnFlags and myFlags.
 */
public void removeRecorders(ImmuSet recorders) {
	int f;
	myBackfollowRecorders = myBackfollowRecorders.minus(recorders);
	diskUpdate();
	f = 0;
	Stepper stomper = myBackfollowRecorders.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		RecorderFossil fossil = (RecorderFossil) stomper.fetch();
		if (fossil == null) {
			continue ;
		}
		if ( ! (fossil.isExtinct())) {
			ResultRecorder recorder = AboraBlockSupport.enterRecorderFossilReanimate(fossil);
			try {
				f = f | recorder.sensorProp().flags();
			}
			finally {
				AboraBlockSupport.exitRecorderFossilReanimate();
			}
		}
	}
	stomper.destroy();
	setOwnFlags(f);
	changeCanopy();
/*
udanax-top.st:5436:SensorCrum methodsFor: 'accessing'!
{void} removeRecorders: recorders {ImmuSet of: RecorderFossil}
	"Remove recorders because they have migrated rootward.
	Recalculate myOwnFlags and myFlags."
	
	| f {UInt32} |
	myBackfollowRecorders _ myBackfollowRecorders minus: recorders.
	self diskUpdate.
	f := UInt32Zero.
	myBackfollowRecorders stepper forEach: [ :fossil {RecorderFossil} |
		fossil isExtinct ifFalse:
			[fossil reanimate: [:recorder {ResultRecorder} |
				f := f bitOr: recorder sensorProp flags]]].
	self setOwnFlags: f.
	self changeCanopy!
*/
}
/**
 * Installs the recorders in my set and updates myOwnProp accordingly.
 * The caller has already checked that none of these recorders are already installed here.
 * The caller also handles updating myFlags.
 * The caller also handles all issues of rootward propagation of these changes.
 * The caller also does the 'diskUpdate'.
 * This is a separate method because it's called once by the code that installs a new
 * recorder, and again by the code that recursively hoists recurders up the canopy.
 * add the new recorders to my set
 * for each new recorder
 * if it hasn't gone extinct
 * extract its properties
 * union them into my own
 */
public void installRecorders(ImmuSet recorders) {
	myBackfollowRecorders = myBackfollowRecorders.unionWith(recorders);
	Stepper stomper = recorders.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		RecorderFossil fossil = (RecorderFossil) stomper.fetch();
		if (fossil == null) {
			continue ;
		}
		if ( ! (fossil.isExtinct())) {
			Prop prop;
			ResultRecorder recorder = AboraBlockSupport.enterRecorderFossilReanimate(fossil);
			try {
				prop = recorder.sensorProp();
			}
			finally {
				AboraBlockSupport.exitRecorderFossilReanimate();
			}
			setOwnFlags((ownFlags() | prop.flags()));
		}
	}
	stomper.destroy();
/*
udanax-top.st:5453:SensorCrum methodsFor: 'private:'!
{void} installRecorders: recorders {ImmuSet of: RecorderFossil}
	"Installs the recorders in my set and updates myOwnProp accordingly.
	The caller has already checked that none of these recorders are already installed here.
	The caller also handles updating myFlags.
	The caller also handles all issues of rootward propagation of these changes.
	The caller also does the 'diskUpdate'.
	
	This is a separate method because it's called once by the code that installs a new recorder, and again by the code that recursively hoists recurders up the canopy.
	
	add the new recorders to my set
	for each new recorder
		if it hasn't gone extinct
			extract its properties
			union them into my own"
	myBackfollowRecorders _ myBackfollowRecorders unionWith: recorders.
	recorders stepper forEach: [ :fossil {RecorderFossil} |
		fossil isExtinct ifFalse: [ | prop {Prop} |
			fossil reanimate: [:recorder {ResultRecorder} |
				prop := recorder sensorProp].
			self setOwnFlags: (self ownFlags bitOr: prop flags)]]!
*/
}
public CanopyCrum makeNewParent(CanopyCrum first, CanopyCrum second) {
	AboraBlockSupport.enterConsistent(3);
	try {
		return new SensorCrum(((SensorCrum) first), ((SensorCrum) second));
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:5477:SensorCrum methodsFor: 'protected'!
{CanopyCrum} makeNewParent: first {CanopyCrum} with: second {CanopyCrum}
	DiskManager consistent: 3 with:
		[^SensorCrum create: (first cast: SensorCrum)
	 		with: (second cast: SensorCrum)]!
*/
}
/**
 * @deprecated
 */
public PropFinder checkRecorders(BeEdition stamp, PropFinder finder, SensorCrum sCrum) {
	throw new PasseException();
/*
udanax-top.st:5485:SensorCrum methodsFor: 'smalltalk: passe'!
{PropFinder} checkRecorders: stamp {BeEdition} 
	with: finder {PropFinder} 
	with: sCrum {SensorCrum | NULL}
	
self passe "fewer args"!
*/
}
/**
 * @deprecated
 */
public SensorCrum fetchNextAfterTriggeringRecorders(BeEdition stamp, PropFinder finder, SensorCrum sCrum) {
	throw new PasseException();
/*
udanax-top.st:5491:SensorCrum methodsFor: 'smalltalk: passe'!
{SensorCrum | NULL} fetchNextAfterTriggeringRecorders: stamp {BeEdition} 
	with: finder {PropFinder} 
	with: sCrum {SensorCrum | NULL}
	
	self passe "fewer args"!
*/
}
/**
 * @deprecated
 */
public void record(RecorderFossil recorder) {
	throw new PasseException();
/*
udanax-top.st:5497:SensorCrum methodsFor: 'smalltalk: passe'!
{void} record: recorder {RecorderFossil}
	
	self passe. "equivalent to '(self recordingAgent: recorder) schedule"!
*/
}
/**
 * @deprecated
 */
public void triggerRecorders(Stamp stamp, PropFinder finder, SensorCrum sCrum) {
	throw new PasseException();
/*
udanax-top.st:5501:SensorCrum methodsFor: 'smalltalk: passe'!
{void} triggerRecorders: stamp {Stamp} 
	with: finder {PropFinder} 
	with: sCrum {SensorCrum | NULL}
	
	self passe.	"Use fetchNextAfterTriggeringRecorders:with:with:"!
*/
}
/**
 * Create a new parent for two SensorCrums.
 * This constructor just makes a new parent whose properties are empty. My client must bring
 * my properties up to date.
 */
public SensorCrum(SensorCrum first, SensorCrum second) {
	super(0, first, second);
	/* Have the super do the basic creation. */
	newShepherd();
	myBackfollowRecorders = ImmuSet.make();
	canopyCache().updateCacheForParent(fetchChild1(), this);
	canopyCache().updateCacheForParent(fetchChild2(), this);
/*
udanax-top.st:5509:SensorCrum methodsFor: 'instance creation'!
create: first {SensorCrum} with: second {SensorCrum}
	"Create a new parent for two SensorCrums.
	This constructor just makes a new parent whose properties are empty. My client must bring my properties up to date."
	| |
	"Have the super do the basic creation."
	
	super create: UInt32Zero with: first with: second.
	self newShepherd.
	myBackfollowRecorders _ ImmuSet make.
	self canopyCache updateCache: self fetchChild1 forParent: self.
	self canopyCache updateCache: self fetchChild2 forParent: self!
*/
}
public void changeCanopy(Object f) {
/*
udanax-top.st:5523:SensorCrum methodsFor: 'smalltalk: suspended'!
changeCanopy: f!
*/
}
public PropChange fullChange() {
	return PropChange.sensorPropChange();
/*
udanax-top.st:5525:SensorCrum methodsFor: 'smalltalk: suspended'!
{PropChange} fullChange
	^PropChange sensorPropChange!
*/
}
public SensorCrum(Rcvr receiver) {
	super(receiver);
	myBackfollowRecorders = (ImmuSet) receiver.receiveHeaper();
/*
udanax-top.st:5530:SensorCrum methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myBackfollowRecorders _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myBackfollowRecorders);
/*
udanax-top.st:5534:SensorCrum methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myBackfollowRecorders.!
*/
}
public static void staticTimeNonInherited() {
	AboraSupport.defineFluid(CanopyCache.class, "CurrentSensorCanopyCache", DiskManager.emulsion(), CanopyCache.make());
/*
udanax-top.st:5550:SensorCrum class methodsFor: 'smalltalk: init'!
staticTimeNonInherited
	
	CanopyCache defineFluid: #CurrentSensorCanopyCache with: DiskManager emulsion with: [CanopyCache make]!
*/
}
public static SensorCrum make() {
	AboraBlockSupport.enterConsistent(2);
	try {
		return new SensorCrum();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:5556:SensorCrum class methodsFor: 'pseudo constructors'!
make
	DiskManager consistent: 2 with: [
		^SensorCrum create]!
*/
}
public static SensorCrum partial() {
	AboraBlockSupport.enterConsistent(1);
	try {
		return new SensorCrum(SensorCrum.isPartialFlag());
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:5560:SensorCrum class methodsFor: 'pseudo constructors'!
{SensorCrum} partial
	DiskManager consistent: 1 with: [
		^SensorCrum create: SensorCrum isPartialFlag]!
*/
}
/**
 * The flag word corresponding to the given props
 */
public static int flagsFor(IDRegion permissions, CrossRegion endorsements, boolean isPartial) {
	int result;
	result = 0;
	if (permissions != null) {
		result = result | (CanopyCrum.permissionsFlags(permissions));
	}
	if (endorsements != null) {
		result = result | (CanopyCrum.endorsementsFlags(endorsements));
	}
	if (isPartial) {
		result = result | isPartialFlag();
	}
	return result;
/*
udanax-top.st:5566:SensorCrum class methodsFor: 'flags'!
{UInt32} flagsFor: permissions {IDRegion | NULL}
	with: endorsements {CrossRegion | NULL}
	with: isPartial {BooleanVar}
	"The flag word corresponding to the given props"
	
	| result {UInt32} |
	result := UInt32Zero.
	permissions ~~ NULL ifTrue:
		[result := result bitOr: (CanopyCrum permissionsFlags: permissions)].
	endorsements ~~ NULL ifTrue:
		[result := result bitOr: (CanopyCrum endorsementsFlags: endorsements)].
	isPartial ifTrue:
		[result := result bitOr: self isPartialFlag].
	^result!
*/
}
/**
 * Flag bit for existence of partiality
 */
public static int isPartialFlag() {
	return 0x8000000;
/*
udanax-top.st:5581:SensorCrum class methodsFor: 'flags'!
{UInt32 constFn} isPartialFlag
	"Flag bit for existence of partiality" 
	^16r08000000!
*/
}
}
