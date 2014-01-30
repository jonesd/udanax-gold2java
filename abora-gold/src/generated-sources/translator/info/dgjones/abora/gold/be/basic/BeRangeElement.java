/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.basic;

import info.dgjones.abora.gold.backrec.ResultRecorder;
import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.basic.BeLabel;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.be.canopy.BertCrum;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.SensorCrum;
import info.dgjones.abora.gold.be.ents.HUpperCrum;
import info.dgjones.abora.gold.be.ents.HistoryCrum;
import info.dgjones.abora.gold.be.ents.Loaf;
import info.dgjones.abora.gold.be.ents.OPart;
import info.dgjones.abora.gold.collection.cache.HashSetCache;
import info.dgjones.abora.gold.collection.tables.MuTable;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.primtab.PrimSet;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.spaces.basic.Mapping;
import info.dgjones.abora.gold.traces.TracePosition;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * This is the actual representation on disk; the Fe versions of these classes hide the
 * actual representation.�
 */
public class BeRangeElement extends Abraham {

	protected HUpperCrum myHCrum;
	protected SensorCrum mySensorCrum;
	protected ID myOwner;
	protected PrimSet myFeRangeElements;
/*
udanax-top.st:2239:
Abraham subclass: #BeRangeElement
	instanceVariableNames: '
		myHCrum {HUpperCrum}
		mySensorCrum {SensorCrum}
		myOwner {ID}
		myFeRangeElements {PrimSet NOCOPY | NULL of: FeRangeElement}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Basic'!
*/
/*
udanax-top.st:2247:
BeRangeElement comment:
'This is the actual representation on disk; the Fe versions of these classes hide the actual representation.�'!
*/
/*
udanax-top.st:2249:
(BeRangeElement getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #COPY; add: #SHEPHERD.ANCESTOR; add: #DEFERRED.LOCKED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BeRangeElement.class).setAttributes( new Set().add("DEFERRED").add("COPY").add("SHEPHERDANCESTOR").add("DEFERREDLOCKED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Add a new session level pointer
 */
public void addFeRangeElement(FeRangeElement element) {
	if (myFeRangeElements == null) {
		myFeRangeElements = PrimSet.weak();
	}
	myFeRangeElements.introduce(element);
/*
udanax-top.st:2254:BeRangeElement methodsFor: 'accessing'!
{void} addFeRangeElement: element {FeRangeElement}
	"Add a new session level pointer"
	
	myFeRangeElements == NULL ifTrue:
		[myFeRangeElements := PrimSet weak].
	myFeRangeElements introduce: element!
*/
}
public boolean isPurgeable() {
	return myFeRangeElements == null || (myFeRangeElements.isEmpty());
/*
udanax-top.st:2261:BeRangeElement methodsFor: 'accessing'!
{BooleanVar} isPurgeable
	^myFeRangeElements == NULL or: [myFeRangeElements isEmpty]!
*/
}
/**
 * Make a front end object (session level) for this backend object.  If the receiver is an
 * Edition, there had better be a label.
 */
public FeRangeElement makeFe(BeLabel label) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:2264:BeRangeElement methodsFor: 'accessing'!
{FeRangeElement} makeFe: label {BeLabel | NULL}
	"Make a front end object (session level) for this backend object.  If the receiver is an Edition, there had better be a label."
	
	self subclassResponsibility!
*/
}
/**
 * Change the identity of this object to that of the other.
 * Only placeHolders implement it at the moment, so the
 * default is to reject the operation (return false).
 */
public boolean makeIdentical(BeRangeElement other) {
	return false;
/*
udanax-top.st:2269:BeRangeElement methodsFor: 'accessing'!
{BooleanVar} makeIdentical: other {BeRangeElement unused}
	"Change the identity of this object to that of the other.
	 Only placeHolders implement it at the moment, so the 
	 default is to reject the operation (return false)."
	
	^false!
*/
}
/**
 * The Club who has ownership
 */
public ID owner() {
	return myOwner;
/*
udanax-top.st:2276:BeRangeElement methodsFor: 'accessing'!
{ID} owner
	"The Club who has ownership"
	^myOwner!
*/
}
/**
 * Remove a session level pointer
 */
public void removeFeRangeElement(FeRangeElement element) {
	if (myFeRangeElements == null || ( ! (myFeRangeElements.hasMember(element)))) {
		throw new AboraRuntimeException(AboraRuntimeException.NEVER_ADDED_FE_RANGE_ELEMENT);
	}
	myFeRangeElements.wipe(element);
	if (myFeRangeElements.isEmpty()) {
		myFeRangeElements.destroy();
		myFeRangeElements = null;
	}
/*
udanax-top.st:2280:BeRangeElement methodsFor: 'accessing'!
{void} removeFeRangeElement: element {FeRangeElement}
	"Remove a session level pointer"
	
	(myFeRangeElements == NULL or: [(myFeRangeElements hasMember: element) not]) ifTrue:
		[Heaper BLAST: #NeverAddedFeRangeElement].
	myFeRangeElements wipe: element.
	myFeRangeElements isEmpty ifTrue:
		[myFeRangeElements destroy.
		myFeRangeElements := NULL]!
*/
}
/**
 * Change the Club who has ownership
 */
public void setOwner(ID club) {
	AboraBlockSupport.enterConsistent(1);
	try {
		myOwner = club;
		diskUpdate();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:2290:BeRangeElement methodsFor: 'accessing'!
{void} setOwner: club {ID}
	"Change the Club who has ownership"
	
	DiskManager consistent: 1 with:
		[myOwner := club.
		self diskUpdate]!
*/
}
/**
 * add oparent to the set of upward pointers.  Editions may
 * also have to propagate BertCrum change downward.
 */
public void addOParent(Loaf oparent) {
	AboraBlockSupport.enterInsistent(5);
	try {
		if (myHCrum.isEmpty()) {
			remember();
		}
		myHCrum.addOParent(oparent);
		diskUpdate();
	}
	finally {
		AboraBlockSupport.exitInsistent();
	}
/*
udanax-top.st:2299:BeRangeElement methodsFor: 'be accessing'!
{void} addOParent: oparent {Loaf} 
	"add oparent to the set of upward pointers.  Editions may
	 also have to propagate BertCrum change downward."
	DiskManager insistent: 5 with:
		[myHCrum isEmpty ifTrue: [self remember].
		myHCrum addOParent: oparent.
		self diskUpdate]!
*/
}
public boolean anyPasses(PropFinder finder) {
	return myHCrum.anyPasses(finder);
/*
udanax-top.st:2308:BeRangeElement methodsFor: 'be accessing'!
{BooleanVar} anyPasses: finder {PropFinder}
	^myHCrum anyPasses: finder!
*/
}
public BertCrum bertCrum() {
	return myHCrum.bertCrum();
/*
udanax-top.st:2311:BeRangeElement methodsFor: 'be accessing'!
{BertCrum} bertCrum
	^ myHCrum bertCrum!
*/
}
/**
 * does nothing.  Overrides do something.
 */
public void checkRecorders(PropFinder finder, SensorCrum scrum) {
/*
udanax-top.st:2314:BeRangeElement methodsFor: 'be accessing'!
{void} checkRecorders: finder {PropFinder} 
	with: scrum {SensorCrum | NULL}
	
	"does nothing.  Overrides do something."!
*/
}
public int contentsHash() {
	return ((super.contentsHash() ^ myHCrum.hashForEqual()) ^ mySensorCrum.hashForEqual()) ^ myOwner.hashForEqual();
/*
udanax-top.st:2319:BeRangeElement methodsFor: 'be accessing'!
{UInt32} contentsHash
	^((super contentsHash
		bitXor: myHCrum hashForEqual)
		bitXor: mySensorCrum hashForEqual)
		bitXor: myOwner hashForEqual!
*/
}
public void delayedStoreBackfollow(PropFinder finder, RecorderFossil fossil, ResultRecorder recorder, HashSetCache hCrumCache) {
	myHCrum.delayedStoreBackfollow(finder, fossil, recorder, hCrumCache);
/*
udanax-top.st:2326:BeRangeElement methodsFor: 'be accessing'!
{void} delayedStoreBackfollow: finder {PropFinder} 
	with: fossil {RecorderFossil} 
	with: recorder {ResultRecorder}
	with: hCrumCache {HashSetCache of: HistoryCrum}
	
	myHCrum delayedStoreBackfollow: finder with: fossil with: recorder with: hCrumCache!
*/
}
public PrimSet feRangeElements() {
	if (myFeRangeElements == null) {
		return PrimSet.make();
	}
	else {
		return myFeRangeElements;
	}
/*
udanax-top.st:2333:BeRangeElement methodsFor: 'be accessing'!
{PrimSet of: FeRangeElement} feRangeElements
	myFeRangeElements == NULL
		ifTrue: 
			[^PrimSet make]
		ifFalse:
			[^myFeRangeElements]!
*/
}
public HistoryCrum hCrum() {
	return myHCrum;
/*
udanax-top.st:2341:BeRangeElement methodsFor: 'be accessing'!
{HistoryCrum} hCrum
	^myHCrum!
*/
}
/**
 * Return true if the receiver can backfollow to trace.
 */
public boolean inTrace(TracePosition trace) {
	return myHCrum.inTrace(trace);
/*
udanax-top.st:2344:BeRangeElement methodsFor: 'be accessing'!
{BooleanVar} inTrace: trace {TracePosition}
	"Return true if the receiver can backfollow to trace."
	^myHCrum inTrace: trace!
*/
}
/**
 * return a mapping from my data to corresponding stuff in the given trace
 */
public Mapping mappingTo(TracePosition trace, Mapping mapping) {
	return myHCrum.mappingTo(trace, mapping);
/*
udanax-top.st:2349:BeRangeElement methodsFor: 'be accessing'!
{Mapping} mappingTo: trace {TracePosition} with: mapping {Mapping}
	"return a mapping from my data to corresponding stuff in the given trace"
	^myHCrum mappingTo: trace with: mapping!
*/
}
/**
 * remove oparent from the set of upward pointers.
 */
public void removeOParent(OPart oparent) {
	myHCrum.removeOParent(oparent);
	diskUpdate();
	/* myHCrum isEmpty 
		ifTrue: 
			[ */
	/* Now we get into the risky part of deletion.  myHCrum
			 canForget iff all the downward pointers to it are gone. */
	/* 
			self destroy] */
/*
udanax-top.st:2353:BeRangeElement methodsFor: 'be accessing'!
{void} removeOParent: oparent {OPart}
	"remove oparent from the set of upward pointers."
	myHCrum removeOParent: oparent.
	self diskUpdate.
	"myHCrum isEmpty 
		ifTrue: 
			[""Now we get into the risky part of deletion.  myHCrum
			 canForget iff all the downward pointers to it are gone.""
			self destroy]"!
*/
}
public SensorCrum sensorCrum() {
	return mySensorCrum;
/*
udanax-top.st:2364:BeRangeElement methodsFor: 'be accessing'!
{SensorCrum} sensorCrum
	^mySensorCrum!
*/
}
/**
 * Ensure the my bertCrum is not be leafward of newBCrum.
 */
public boolean updateBCrumTo(BertCrum newBCrum) {
	if (myHCrum.propagateBCrum(newBCrum)) {
		diskUpdate();
		return true;
	}
	return false;
/*
udanax-top.st:2367:BeRangeElement methodsFor: 'be accessing'!
{BooleanVar} updateBCrumTo: newBCrum {BertCrum} 
	"Ensure the my bertCrum is not be leafward of newBCrum."
	(myHCrum propagateBCrum: newBCrum)
		ifTrue:
			[self diskUpdate.
			^true].
	^false!
*/
}
public BeRangeElement() {
	super();
	myOwner = ((ID) InitialOwner.fluidGet());
	myHCrum = HUpperCrum.make();
	mySensorCrum = SensorCrum.make();
	myFeRangeElements = null;
/*
udanax-top.st:2378:BeRangeElement methodsFor: 'protected:'!
create
	super create.
	myOwner _ InitialOwner fluidGet.
	myHCrum _ HUpperCrum make.
	mySensorCrum _ SensorCrum make.
	myFeRangeElements _ NULL!
*/
}
public BeRangeElement(SensorCrum sensorCrum) {
	super();
	myOwner = ((ID) InitialOwner.fluidGet());
	myHCrum = HUpperCrum.make();
	mySensorCrum = sensorCrum;
	myFeRangeElements = null;
/*
udanax-top.st:2385:BeRangeElement methodsFor: 'protected:'!
create: sensorCrum {SensorCrum}
	super create.
	myOwner _ InitialOwner fluidGet.
	myHCrum _ HUpperCrum make.
	mySensorCrum _ sensorCrum.
	myFeRangeElements _ NULL!
*/
}
public void dismantle() {
	AboraBlockSupport.enterConsistent(2);
	try {
		if (Heaper.isConstructed(mySensorCrum)) {
			mySensorCrum.removePointer(this);
		}
		if ((Heaper.isConstructed(myHCrum)) && (Heaper.isConstructed(myHCrum.bertCrum()))) {
			myHCrum.bertCrum().removePointer(myHCrum);
		}
		myHCrum = null;
		super.dismantle();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:2392:BeRangeElement methodsFor: 'protected:'!
{void} dismantle
	DiskManager consistent: 2 with:
		[(Heaper isConstructed: mySensorCrum) 
			ifTrue: [mySensorCrum removePointer: self].
		((Heaper isConstructed: myHCrum)
				and: [Heaper isConstructed: myHCrum bertCrum])
			ifTrue: [myHCrum bertCrum removePointer: myHCrum].
		myHCrum _ NULL.
		super dismantle]!
*/
}
public void restartRE(Rcvr rcvr) {
	myFeRangeElements = null;
/*
udanax-top.st:2404:BeRangeElement methodsFor: 'hooks:'!
{void RECEIVE.HOOK} restartRE: rcvr {Rcvr unused}
	myFeRangeElements _ NULL!
*/
}
/*
udanax-top.st:2409:BeRangeElement methodsFor: 'smalltalk:'!
inspect
	"Sensor leftShiftDown" true
		ifTrue: [self basicInspect]
		ifFalse: [EntView openOn: (TreeBarnacle new
					buildOn: self
					gettingChildren: [:crum | crum crums]
					gettingImage: [:crum | DisplayText text: crum displayString asText textStyle: (TextStyle styleNamed: #small)]
					at: 0 @ 0
					vertical: true
					separation: 5 @ 10)]!
*/
/**
 * See comment in FeRangeElement
 */
public BeEdition works(IDRegion permissions, Filter endorsementsFilter, int flags) {
	MarkM.shouldImplement();
	return null;
/*
udanax-top.st:2422:BeRangeElement methodsFor: 'comparing'!
{BeEdition} works: permissions {IDRegion}
	with: endorsementsFilter {Filter}
	with: flags {Int32}
	"See comment in FeRangeElement"
	
	MarkM shouldImplement.
	^NULL "fodder"!
*/
}
/**
 * @deprecated
 */
public boolean becomeOther(BeRangeElement other) {
	throw new PasseException();
/*
udanax-top.st:2432:BeRangeElement methodsFor: 'smalltalk: passe'!
{BooleanVar} becomeOther: other {BeRangeElement}
	self passe "makeIdentical"!
*/
}
/**
 * @deprecated
 */
public void checkRecorders(BeEdition edition, PropFinder finder, SensorCrum scrum) {
	throw new PasseException();
/*
udanax-top.st:2436:BeRangeElement methodsFor: 'smalltalk: passe'!
{void} checkRecorders: edition {BeEdition} 
	with: finder {PropFinder} 
	with: scrum {SensorCrum | NULL}
	
	self passe "fewer args"!
*/
}
/**
 * @deprecated
 */
public void delayedStoreBackfollow(PropFinder finder, RecorderFossil recorder, HashSetCache hCrumCache) {
	throw new PasseException();
/*
udanax-top.st:2442:BeRangeElement methodsFor: 'smalltalk: passe'!
{void} delayedStoreBackfollow: finder {PropFinder} 
	with: recorder {RecorderFossil}
	with: hCrumCache {HashSetCache of: HistoryCrum}
	
	self passe "extra argument"!
*/
}
/**
 * @deprecated
 */
public void storeBackfollow(PropFinder finder, MuTable table, HashSetCache hCrumCache) {
	throw new PasseException();
/*
udanax-top.st:2448:BeRangeElement methodsFor: 'smalltalk: passe'!
{void} storeBackfollow: finder {PropFinder} 
	with: table {MuTable of: ID and: BeEdition} 
	with: hCrumCache {HashSetCache of: HistoryCrum}
	
	self passe!
*/
}
public BeRangeElement(Rcvr receiver) {
	super(receiver);
	myHCrum = (HUpperCrum) receiver.receiveHeaper();
	mySensorCrum = (SensorCrum) receiver.receiveHeaper();
	myOwner = (ID) receiver.receiveHeaper();
	restartRE(receiver);
/*
udanax-top.st:2456:BeRangeElement methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myHCrum _ receiver receiveHeaper.
	mySensorCrum _ receiver receiveHeaper.
	myOwner _ receiver receiveHeaper.
	self restartRE: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myHCrum);
	xmtr.sendHeaper(mySensorCrum);
	xmtr.sendHeaper(myOwner);
/*
udanax-top.st:2463:BeRangeElement methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myHCrum.
	xmtr sendHeaper: mySensorCrum.
	xmtr sendHeaper: myOwner.!
*/
}
}
