/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.ents;

import info.dgjones.abora.gold.arrange.Arrangement;
import info.dgjones.abora.gold.backrec.ResultRecorder;
import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.basic.BeLabel;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.be.canopy.BertCrum;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.SensorCrum;
import info.dgjones.abora.gold.be.ents.ActualOrglRoot;
import info.dgjones.abora.gold.be.ents.HistoryCrum;
import info.dgjones.abora.gold.be.ents.InnerLoaf;
import info.dgjones.abora.gold.be.ents.Loaf;
import info.dgjones.abora.gold.be.ents.OExpandingLoaf;
import info.dgjones.abora.gold.be.ents.OrglRoot;
import info.dgjones.abora.gold.collection.basic.PrimDataArray;
import info.dgjones.abora.gold.collection.cache.HashSetCache;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.tables.Pair;
import info.dgjones.abora.gold.detect.FeFillRangeDetector;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.HRoot;
import info.dgjones.abora.gold.java.missing.SplayEntLoaf;
import info.dgjones.abora.gold.java.missing.XnSensor;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.Dsp;
import info.dgjones.abora.gold.spaces.basic.Mapping;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.tclude.TrailBlazer;
import info.dgjones.abora.gold.traces.TracePosition;
import info.dgjones.abora.gold.turtle.Agenda;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class ActualOrglRoot extends OrglRoot {

	protected Loaf myO;
	protected XnRegion myRegion;
/*
udanax-top.st:9886:
OrglRoot subclass: #ActualOrglRoot
	instanceVariableNames: '
		myO {Loaf}
		myRegion {XnRegion}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Ents'!
*/
/*
udanax-top.st:9892:
(ActualOrglRoot getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #SHEPHERD.PATRIARCH; add: #COPY; add: #LOCKED; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:10235:
ActualOrglRoot class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:10238:
(ActualOrglRoot getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #SHEPHERD.PATRIARCH; add: #COPY; add: #LOCKED; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ActualOrglRoot.class).setAttributes( new Set().add("SHEPHERDPATRIARCH").add("COPY").add("LOCKED").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public XnRegion attachTrailBlazer(TrailBlazer blazer) {
	return myO.attachTrailBlazer(blazer);
/*
udanax-top.st:9897:ActualOrglRoot methodsFor: 'backfollow'!
{XnRegion} attachTrailBlazer: blazer {TrailBlazer}
	
	^myO attachTrailBlazer: blazer!
*/
}
public void checkRecorders(PropFinder finder, SensorCrum scrum) {
	myO.checkRecorders(finder, scrum);
/*
udanax-top.st:9901:ActualOrglRoot methodsFor: 'backfollow'!
{void} checkRecorders: finder {PropFinder} 
	with: scrum {SensorCrum | NULL}
	
	myO checkRecorders: finder with: scrum!
*/
}
public void checkTrailBlazer(TrailBlazer blazer) {
	myO.checkTrailBlazer(blazer);
/*
udanax-top.st:9906:ActualOrglRoot methodsFor: 'backfollow'!
{void} checkTrailBlazer: blazer {TrailBlazer}
	
	myO checkTrailBlazer: blazer!
*/
}
public void delayedFindMatching(PropFinder finder, RecorderFossil fossil, ResultRecorder recorder) {
	HashSetCache hCrumCache;
	/* Cache for optimization: Frequently, in going northwards on the h-tree, one will encounter an h-crum already encountered during this very delayedFindMatching: operation.  In this case, the cache helps us avoid *much* redundant work.  We can get away with a bounded size cache because redundant work is still correct. */
	hCrumCache = HashSetCache.make(100);
	/* Tell my O crum to do its flavor of the work.  It will tell its children recursively. */
	myO.delayedStoreMatching(finder, fossil, recorder, hCrumCache);
	hCrumCache.destroy();
/*
udanax-top.st:9910:ActualOrglRoot methodsFor: 'backfollow'!
{void} delayedFindMatching: finder {PropFinder}
	with: fossil {RecorderFossil}
	with: recorder {ResultRecorder}
	
	| hCrumCache {HashSetCache of: HistoryCrum} |	
	
	"Cache for optimization: Frequently, in going northwards on the h-tree, one will encounter an h-crum already encountered during this very delayedFindMatching: operation.  In this case, the cache helps us avoid *much* redundant work.  We can get away with a bounded size cache because redundant work is still correct."
	hCrumCache _ HashSetCache make: 100.
	
	"Tell my O crum to do its flavor of the work.  It will tell its children recursively."
	myO delayedStoreMatching: finder with: fossil with: recorder with: hCrumCache.
	
	hCrumCache destroy.!
*/
}
public TrailBlazer fetchTrailBlazer() {
	return myO.fetchTrailBlazer();
/*
udanax-top.st:9924:ActualOrglRoot methodsFor: 'backfollow'!
{TrailBlazer | NULL} fetchTrailBlazer
	^myO fetchTrailBlazer!
*/
}
public void storeRecordingAgents(RecorderFossil recorder, Agenda agenda) {
	myO.storeRecordingAgents(recorder, agenda);
/*
udanax-top.st:9928:ActualOrglRoot methodsFor: 'backfollow'!
{void} storeRecordingAgents: recorder {RecorderFossil}
	with: agenda {Agenda}
	
	myO storeRecordingAgents: recorder with: agenda!
*/
}
public void triggerDetector(FeFillRangeDetector detect) {
	myO.triggerDetector(detect);
/*
udanax-top.st:9933:ActualOrglRoot methodsFor: 'backfollow'!
{void} triggerDetector: detect {FeFillRangeDetector}
	myO triggerDetector: detect!
*/
}
/**
 * My bertCrum must not be leafward of newBCrum.
 * Thus it must be LE to newCrum. Otherwise correct it and recur.
 */
public boolean updateBCrumTo(BertCrum newBCrum) {
	if (super.updateBCrumTo(newBCrum)) {
		myO.updateBCrumTo(newBCrum);
		return true;
	}
	return false;
/*
udanax-top.st:9937:ActualOrglRoot methodsFor: 'backfollow'!
{BooleanVar} updateBCrumTo: newBCrum {BertCrum} 
	"My bertCrum must not be leafward of newBCrum. 
	Thus it must be LE to newCrum. Otherwise correct it and recur."
	(super updateBCrumTo: newBCrum)
		ifTrue: 
			[myO updateBCrumTo: newBCrum.
			^true].
	^false!
*/
}
/**
 * the kind of domain elements allowed
 */
public CoordinateSpace coordinateSpace() {
	return myRegion.coordinateSpace();
/*
udanax-top.st:9949:ActualOrglRoot methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	"the kind of domain elements allowed"
	^myRegion coordinateSpace!
*/
}
public int count() {
	return myO.count();
/*
udanax-top.st:9954:ActualOrglRoot methodsFor: 'accessing'!
{IntegerVar} count 
	^myO count!
*/
}
public XnRegion domain() {
	return myO.domain();
/*
udanax-top.st:9957:ActualOrglRoot methodsFor: 'accessing'!
{XnRegion} domain
	^myO domain!
*/
}
/**
 * get an individual element
 */
public FeRangeElement fetch(Position key, BeEdition edition) {
	return myO.fetch(key, edition, key);
/*
udanax-top.st:9960:ActualOrglRoot methodsFor: 'accessing'!
{FeRangeElement | NULL} fetch: key {Position} with: edition {BeEdition}
	"get an individual element"
	^myO fetch: key with: edition with: key!
*/
}
public Loaf fullcrum() {
	return myO;
/*
udanax-top.st:9965:ActualOrglRoot methodsFor: 'accessing'!
{Loaf} fullcrum
	^myO!
*/
}
/**
 * Get or Make the BeRangeElement at the location.
 */
public BeRangeElement getBe(Position key) {
	/* Separate the position from the rest of the oplane with copy.  Then instantiate it. */
	Object currentTraceOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentTrace, hCrum().hCut());
	try {
		Object currentBertCrumOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentBertCrum, hCrum().bertCrum());
		try {
			return ((ActualOrglRoot) (copy(key.asRegion()))).fullcrum().getBe(key);
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(CurrentBertCrum, currentBertCrumOldValue);
		}
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(CurrentTrace, currentTraceOldValue);
	}
/*
udanax-top.st:9968:ActualOrglRoot methodsFor: 'accessing'!
{BeRangeElement} getBe: key {Position}
	"Get or Make the BeRangeElement at the location."
	"Separate the position from the rest of the oplane with copy.  Then instantiate it."
	
	CurrentTrace fluidBind: self hCrum hCut
		during: [CurrentBertCrum fluidBind: self hCrum bertCrum
		during: [^((self copy: key asRegion) cast: ActualOrglRoot) fullcrum getBe: key]]!
*/
}
/**
 * ActualOrglRoots believe they have stuff beneath them.
 */
public boolean isEmpty() {
	return false;
/*
udanax-top.st:9977:ActualOrglRoot methodsFor: 'accessing'!
{BooleanVar} isEmpty
	"ActualOrglRoots believe they have stuff beneath them."
	
	^false!
*/
}
/**
 * Just search for now.
 */
public XnRegion keysLabelled(BeLabel label) {
	return myO.keysLabelled(label);
/*
udanax-top.st:9982:ActualOrglRoot methodsFor: 'accessing'!
{XnRegion} keysLabelled: label {BeLabel}
	"Just search for now."
	^myO keysLabelled: label!
*/
}
/**
 * return a mapping from my data to corresponding stuff in the given trace
 */
public Mapping mapSharedTo(TracePosition trace) {
	return myO.compare(trace, myRegion);
/*
udanax-top.st:9987:ActualOrglRoot methodsFor: 'accessing'!
{Mapping} mapSharedTo: trace {TracePosition}
	"return a mapping from my data to corresponding stuff in the given trace"
	^myO compare: trace with: myRegion!
*/
}
/**
 * Return the owner for the given position in the receiver.
 */
public ID ownerAt(Position key) {
	OExpandingLoaf loaf;
	loaf = myO.fetchBottomAt(key);
	if (loaf == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
	}
	return loaf.owner();
/*
udanax-top.st:9991:ActualOrglRoot methodsFor: 'accessing'!
{ID} ownerAt: key {Position}
	"Return the owner for the given position in the receiver."
	
	| loaf {OExpandingLoaf} |
	loaf _ myO fetchBottomAt: key.
	loaf == NULL ifTrue: [Heaper BLAST: #NotInTable].
	^loaf owner!
*/
}
public XnRegion rangeOwners(XnRegion positions) {
	return myO.rangeOwners(positions);
/*
udanax-top.st:9999:ActualOrglRoot methodsFor: 'accessing'!
{XnRegion} rangeOwners: positions {XnRegion | NULL} 
	
	^myO rangeOwners: positions!
*/
}
/**
 * Recur assigning owners.  Return the portion of the receiver that
 * couldn't be assigned.
 */
public OrglRoot setAllOwners(ID owner) {
	return myO.setAllOwners(owner);
/*
udanax-top.st:10003:ActualOrglRoot methodsFor: 'accessing'!
{OrglRoot} setAllOwners: owner {ID}
	"Recur assigning owners.  Return the portion of the receiver that
	 couldn't be assigned."
		
	^myO setAllOwners: owner!
*/
}
/**
 * Return a region for all the stuff in this orgl that can backfollow to trace.
 */
public XnRegion sharedRegion(TracePosition trace) {
	return myO.sharedRegion(trace, myRegion);
/*
udanax-top.st:10009:ActualOrglRoot methodsFor: 'accessing'!
{XnRegion} sharedRegion: trace {TracePosition}
	"Return a region for all the stuff in this orgl that can backfollow to trace."
	^myO sharedRegion: trace with: myRegion!
*/
}
public XnRegion simpleDomain() {
	return myRegion;
/*
udanax-top.st:10014:ActualOrglRoot methodsFor: 'accessing'!
{XnRegion} simpleDomain
	^myRegion!
*/
}
/**
 * Return the owner for the given position in the receiver.
 */
public PrimSpec specAt(Position key) {
	OExpandingLoaf loaf;
	loaf = myO.fetchBottomAt(key);
	if (loaf == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
	}
	return loaf.spec();
/*
udanax-top.st:10017:ActualOrglRoot methodsFor: 'accessing'!
{PrimSpec} specAt: key {Position}
	"Return the owner for the given position in the receiver."
	
	| loaf {OExpandingLoaf} |
	loaf _ myO fetchBottomAt: key.
	loaf == NULL ifTrue: [Heaper BLAST: #NotInTable].
	^loaf spec!
*/
}
/**
 * Change the identities of the RangeElements of this Edition to those at the same key in the
 * other Edition. The left piece of the result contains those object which are know to not be
 * able to become, because of
 * - lack of ownership authority
 * - different contents
 * - incompatible types
 * - no corresponding new identity
 * The right piece of the result is NULL if there is nothing more that might be done, or else
 * the remainder of the receiver on which we might be able to proceed. This material might
 * fail at a later time because of any of the reasons above; or it might succeed , even
 * though it failed this time because of
 * - synchronization problem
 * - just didn't feel like it
 * This is always required to make progress if it can, although it isn't required to make all
 * the progress that it might. Returns right=NULL when it can't make further progress.
 */
public Pair tryAllBecome(OrglRoot other) {
	Dean.shouldImplement();
	return null;
/*
udanax-top.st:10025:ActualOrglRoot methodsFor: 'accessing'!
{Pair of: OrglRoot} tryAllBecome: other {OrglRoot}
	"Change the identities of the RangeElements of this Edition to those at the same key in the other Edition. The left piece of the result contains those object which are know to not be able to become, because of
		- lack of ownership authority
		- different contents
		- incompatible types
		- no corresponding new identity
	The right piece of the result is NULL if there is nothing more that might be done, or else the remainder of the receiver on which we might be able to proceed. This material might fail at a later time because of any of the reasons above; or it might succeed , even though it failed this time because of
		- synchronization problem
		- just didn't feel like it
	This is always required to make progress if it can, although it isn't required to make all the progress that it might. Returns right=NULL when it can't make further progress."
	
	Dean shouldImplement.
	^NULL "fodder"!
*/
}
public XnRegion usedDomain() {
	return myO.usedDomain();
/*
udanax-top.st:10040:ActualOrglRoot methodsFor: 'accessing'!
{XnRegion} usedDomain
	^myO usedDomain!
*/
}
/**
 * Return a stepper of bundles according to the order.
 */
public Stepper bundleStepper(XnRegion region, OrderSpec order) {
	return myO.bundleStepper(region, order, region.coordinateSpace().identityDsp());
/*
udanax-top.st:10045:ActualOrglRoot methodsFor: 'operations'!
{Stepper} bundleStepper: region {XnRegion} with: order {OrderSpec}
	"Return a stepper of bundles according to the order."
	
	^myO bundleStepper: region with: order with: region coordinateSpace identityDsp!
*/
}
public OrglRoot combine(OrglRoot another) {
	ActualOrglRoot him;
	OrglRoot result;
	if (another.isEmpty()) {
		return this;
	}
	him = (ActualOrglRoot) another;
	result = fetchEasyCombine(him);
	if (result != null) {
		return result;
	}
	result = him.fetchEasyCombine(this);
	if (result != null) {
		return result;
	}
	/* both Ins are non-empty & both Outs are empty */
	return myO.combine(him, myRegion, coordinateSpace().identityDsp());
/*
udanax-top.st:10050:ActualOrglRoot methodsFor: 'operations'!
{OrglRoot} combine: another {OrglRoot}
	| him {ActualOrglRoot} result {OrglRoot} |
	another isEmpty ifTrue: [^self].
	him _ another cast: ActualOrglRoot.
	result _ self fetchEasyCombine: him.
	result ~~ NULL ifTrue: [^result].
	result _ him fetchEasyCombine: self.
	result ~~ NULL ifTrue: [^result].
		
	"both Ins are non-empty & both Outs are empty"
	^myO combine: him with: myRegion with: self coordinateSpace identityDsp!
*/
}
/**
 * Copy out each simple region and then combine them.
 */
public OrglRoot copy(XnRegion region) {
	if (region.isSimple()) {
		return copySimple(region);
	}
	else {
		OrglRoot result;
		result = OrglRoot.make(coordinateSpace());
		Stepper stomper = (region.disjointSimpleRegions());
		for (; stomper.hasValue(); stomper.step()) {
			XnRegion simple = (XnRegion) stomper.fetch();
			if (simple == null) {
				continue ;
			}
			result = result.combine((copySimple(simple)));
		}
		stomper.destroy();
		return result;
	}
/*
udanax-top.st:10063:ActualOrglRoot methodsFor: 'operations'!
{OrglRoot} copy: region {XnRegion} 
	"Copy out each simple region and then combine them."
	region isSimple
		ifTrue: [^self copySimple: region]
		ifFalse: 
			[| result {OrglRoot} |
			result _ OrglRoot make: self coordinateSpace.
			(region disjointSimpleRegions) forEach: [:simple {XnRegion} | 
				result _ result combine: (self copySimple: simple)].
			^result]!
*/
}
/**
 * region must be a valid thing to store as a split.
 */
public OrglRoot copyDistinction(XnRegion region) {
	int cnt;
	cnt = splay(region);
	if (0 == cnt) {
		return OrglRoot.make(coordinateSpace());
	}
	else {
		if (2 == cnt) {
			return this;
		}
		else {
			return ActualOrglRoot.make(((InnerLoaf) myO).inPart(), (myRegion.intersect(region)));
		}
	}
/*
udanax-top.st:10075:ActualOrglRoot methodsFor: 'operations'!
{OrglRoot} copyDistinction: region {XnRegion} 
	"region must be a valid thing to store as a split."
	| cnt {UInt8} |
	cnt _ self splay: region.
	Int0 == cnt ifTrue: [^OrglRoot make: self coordinateSpace]
	ifFalse: [2 == cnt ifTrue: [^self]
	ifFalse: [^ActualOrglRoot make: (myO cast: InnerLoaf) inPart with: (myRegion intersect: region)]]!
*/
}
/**
 * simpleRegion must be simple!!  Copy out each distinction.
 */
public OrglRoot copySimple(XnRegion simpleRegion) {
	OrglRoot result;
	result = this;
	if ( ! (simpleRegion.isSimple())) {
		throw new AboraAssertionException("This must be a simple region.");
	}
	Stepper stomper = simpleRegion.distinctions().stepper();
	for (; stomper.hasValue(); stomper.step()) {
		XnRegion distinct = (XnRegion) stomper.fetch();
		if (distinct == null) {
			continue ;
		}
		if (result.isEmpty()) {
			return result;
		}
		result = ((ActualOrglRoot) result).copyDistinction(distinct);
	}
	stomper.destroy();
	return result;
/*
udanax-top.st:10084:ActualOrglRoot methodsFor: 'operations'!
{OrglRoot} copySimple: simpleRegion {XnRegion} 
	"simpleRegion must be simple!!  Copy out each distinction."
	| result {OrglRoot} |
	[ImmuSet] USES.
	result _ self.
	simpleRegion isSimple assert: 'This must be a simple region.'.
	simpleRegion distinctions stepper forEach: 
		[:distinct {XnRegion} | 
		result isEmpty ifTrue: [^result].
		result _ (result cast: ActualOrglRoot) copyDistinction: distinct].
	^result!
*/
}
public void fill(XnRegion keys, Arrangement toArrange, PrimDataArray toArray, Dsp dsp, BeEdition edition) {
	myO.fill(keys, toArrange, toArray, dsp, edition);
/*
udanax-top.st:10097:ActualOrglRoot methodsFor: 'operations'!
{void} fill: keys {XnRegion} with: toArrange {Arrangement} with: toArray {PrimDataArray} with: dsp {Dsp} with: edition {BeEdition} 
	
	myO fill: keys with: toArrange with: toArray with: dsp with: edition!
*/
}
public ActualOrglRoot makeNew(XnRegion newSplit, ActualOrglRoot newIn, ActualOrglRoot newOut) {
	return ActualOrglRoot.make((InnerLoaf.make(newSplit, newIn.fullcrum(), newOut.fullcrum())), (newIn.simpleDomain().simpleUnion(newOut.simpleDomain())));
/*
udanax-top.st:10101:ActualOrglRoot methodsFor: 'operations'!
{ActualOrglRoot} makeNew: newSplit {XnRegion} with: newIn {ActualOrglRoot} with: newOut {ActualOrglRoot}
	^ActualOrglRoot 
			make: (InnerLoaf make: newSplit with: newIn fullcrum with: newOut fullcrum) 
			with: (newIn simpleDomain simpleUnion: newOut simpleDomain)!
*/
}
/**
 * Return a copy with externalDsp added to the receiver's dsp.
 */
public OrglRoot transformedBy(Dsp externalDsp) {
	if (externalDsp.isIdentity()) {
		return this;
	}
	return ActualOrglRoot.make((myO.transformedBy(externalDsp)), (externalDsp.ofAll(myRegion)));
/*
udanax-top.st:10106:ActualOrglRoot methodsFor: 'operations'!
{OrglRoot} transformedBy: externalDsp {Dsp}
	"Return a copy with externalDsp added to the receiver's dsp."
	externalDsp isIdentity ifTrue: [^self].
	^ActualOrglRoot make: (myO transformedBy: externalDsp) with: (externalDsp ofAll: myRegion)!
*/
}
/**
 * Return a copy with externalDsp removed from the receiver's dsp.
 */
public OrglRoot unTransformedBy(Dsp externalDsp) {
	if (externalDsp.isIdentity()) {
		return this;
	}
	return ActualOrglRoot.make((myO.unTransformedBy(externalDsp)), (externalDsp.inverseOfAll(myRegion)));
/*
udanax-top.st:10112:ActualOrglRoot methodsFor: 'operations'!
{OrglRoot} unTransformedBy: externalDsp {Dsp}
	"Return a copy with externalDsp removed from the receiver's dsp."
	externalDsp isIdentity ifTrue: [^self].
	^ActualOrglRoot 
		make: (myO unTransformedBy: externalDsp) 
		with: (externalDsp inverseOfAll: myRegion)!
*/
}
public ActualOrglRoot(Loaf fullcrum, XnRegion region) {
	super(fullcrum.sensorCrum());
	myO = fullcrum;
	myRegion = region;
	myO.addOParent(this);
	newShepherd();
/*
udanax-top.st:10122:ActualOrglRoot methodsFor: 'create'!
create: fullcrum {Loaf} with: region {XnRegion} 
	super create: fullcrum sensorCrum.
	myO _ fullcrum.
	myRegion _ region.
	myO addOParent: self.
	self newShepherd!
*/
}
/*
udanax-top.st:10131:ActualOrglRoot methodsFor: 'smalltalk:'!
crums
	^Array with: myO!
*/
public String displayString() {
	return getAboraClass().name() + (myO.displayString());
/*
udanax-top.st:10134:ActualOrglRoot methodsFor: 'smalltalk:'!
displayString
	^self getCategory name , (myO displayString)!
*/
}
/*
udanax-top.st:10137:ActualOrglRoot methodsFor: 'smalltalk:'!
inspect
	Sensor leftShiftDown
		ifTrue: [self basicInspect]
		ifFalse: [EntView openOn: (TreeBarnacle new
					buildOn: self
					gettingChildren: [:crum | crum crums]
					gettingImage: [:crum | DisplayText text: crum displayString asText textStyle: (TextStyle styleNamed: #small)]
					at: 0 @ 0
					vertical: Sensor ctrlDown
					separation: 5 @ 10)]!
*/
/*
udanax-top.st:10148:ActualOrglRoot methodsFor: 'smalltalk:'!
inspectTraces
	Sensor leftShiftDown
		ifTrue: [self basicInspect]
		ifFalse: [EntView openOn: (TreeBarnacle new
					buildOn: myO
					gettingChildren: [:crum | crum crums]
					gettingImage: [:crum | DisplayText text: crum hCrum hCut displayString asText textStyle: (TextStyle styleNamed: #small)]
					at: 0 @ 0
					vertical: true
					separation: 20 @ 20)]!
*/
/**
 * Return true if child is a child.  Used for debugging.
 */
public boolean testChild(SplayEntLoaf child) {
	return myO == child;
/*
udanax-top.st:10159:ActualOrglRoot methodsFor: 'smalltalk:'!
{BooleanVar} testChild: child {SplayEntLoaf}
	"Return true if child is a child.  Used for debugging."
	
	^myO == child!
*/
}
/**
 * Return true if child is a child.  Used for debugging.
 */
public boolean testHChild(HistoryCrum child) {
	return myO.hCrum() == child;
/*
udanax-top.st:10164:ActualOrglRoot methodsFor: 'smalltalk:'!
{BooleanVar} testHChild: child {HistoryCrum}
	"Return true if child is a child.  Used for debugging."
	
	^ myO hCrum == child!
*/
}
public void printOn(PrintWriter aStream) {
	aStream.print(getAboraClass().name());
	aStream.print("(");
	aStream.print(myRegion);
	aStream.print(", ");
	aStream.print(myO);
	aStream.print(")");
/*
udanax-top.st:10171:ActualOrglRoot methodsFor: 'printing'!
{void} printOn: aStream {ostream reference}
	aStream << self getCategory name << '(' << myRegion << ', ' << myO << ')'!
*/
}
public ActualOrglRoot fetchEasyCombine(ActualOrglRoot another) {
	Stepper stomper = another.simpleDomain().distinctions().stepper();
	for (; stomper.hasValue(); stomper.step()) {
		XnRegion bound = (XnRegion) stomper.fetch();
		if (bound == null) {
			continue ;
		}
		OrglRoot myIn;
		OrglRoot myOut;
		myIn = copy(bound);
		myOut = copy(bound.complement());
		if (myIn.isEmpty()) {
			return makeNew(bound, another, ((ActualOrglRoot) myOut));
		}
		if ( ! myOut.isEmpty()) {
			return makeNew(bound, ((ActualOrglRoot) (another.combine(myIn))), ((ActualOrglRoot) myOut));
		}
	}
	stomper.destroy();
	return null;
/*
udanax-top.st:10176:ActualOrglRoot methodsFor: 'private:'!
{ActualOrglRoot | NULL} fetchEasyCombine: another {ActualOrglRoot} 
	another simpleDomain distinctions stepper
		forEach: 
			[:bound {XnRegion} | 
			| myIn {OrglRoot} myOut {OrglRoot} |
			myIn _ self copy: bound.
			myOut _ self copy: bound complement.
			myIn isEmpty ifTrue: 
				[^self makeNew: bound
					with: another
					with: (myOut cast: ActualOrglRoot)].
			myOut isEmpty not ifTrue: 
				[^self makeNew: bound
					with: ((another combine: myIn) cast: ActualOrglRoot)
					with: (myOut cast: ActualOrglRoot)]].
	^NULL!
*/
}
/**
 * Splay a region into its own subtree as close as possible to the root
 */
public int splay(XnRegion region) {
	return myO.splay(region, myRegion);
/*
udanax-top.st:10193:ActualOrglRoot methodsFor: 'private:'!
{UInt8} splay: region {XnRegion}
	"Splay a region into its own subtree as close as possible to the root"
	^myO splay: region with: myRegion!
*/
}
public void dismantle() {
	AboraBlockSupport.enterConsistent(4);
	try {
		if (Heaper.isConstructed(myO)) {
			myO.removeOParent(this);
		}
		super.dismantle();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:10199:ActualOrglRoot methodsFor: 'protected: delete'!
{void} dismantle
	DiskManager consistent: 4 with:
		[(Heaper isConstructed: myO) ifTrue: [myO removeOParent: self].
		super dismantle]!
*/
}
public int contentsHash() {
	return (super.contentsHash() ^ myO.hashForEqual()) ^ myRegion.hashForEqual();
/*
udanax-top.st:10206:ActualOrglRoot methodsFor: 'testing'!
{UInt32} contentsHash
	^(super contentsHash
		bitXor: myO hashForEqual)
		bitXor: myRegion hashForEqual!
*/
}
/**
 * @deprecated
 */
public void inform(Position key, HRoot value) {
	throw new PasseException();
/*
udanax-top.st:10214:ActualOrglRoot methodsFor: 'smalltalk: passe'!
{void} inform: key {Position} with: value {HRoot}
	
	self passe!
*/
}
/**
 * @deprecated
 */
public void wait(XnSensor sensor) {
	throw new PasseException();
/*
udanax-top.st:10218:ActualOrglRoot methodsFor: 'smalltalk: passe'!
{void} wait: sensor {XnSensor} 
	
	self passe!
*/
}
public ActualOrglRoot(Rcvr receiver) {
	super(receiver);
	myO = (Loaf) receiver.receiveHeaper();
	myRegion = (XnRegion) receiver.receiveHeaper();
/*
udanax-top.st:10224:ActualOrglRoot methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myO _ receiver receiveHeaper.
	myRegion _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myO);
	xmtr.sendHeaper(myRegion);
/*
udanax-top.st:10229:ActualOrglRoot methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myO.
	xmtr sendHeaper: myRegion.!
*/
}
/**
 * create a new orgl root
 */
public static ActualOrglRoot make(Loaf loaf, XnRegion region) {
	if ( ! ( ! region.isEmpty())) {
		throw new AboraAssertionException("Attempt to make an empty ActualOrglRoot");
	}
	AboraBlockSupport.enterConsistent(13);
	try {
		return new ActualOrglRoot(loaf, region);
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:10243:ActualOrglRoot class methodsFor: 'creation'!
make: loaf {Loaf} with: region {XnRegion}
	"create a new orgl root"
	region isEmpty not assert: 'Attempt to make an empty ActualOrglRoot'.
	DiskManager consistent: 13 with: 
		[^ActualOrglRoot create: loaf with: region]!
*/
}
public ActualOrglRoot() {
/*

Generated during transformation
*/
}
}
