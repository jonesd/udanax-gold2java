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
import info.dgjones.abora.gold.be.basic.BePlaceHolder;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.be.canopy.BertCrum;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.ents.ActualOrglRoot;
import info.dgjones.abora.gold.be.ents.HUpperCrum;
import info.dgjones.abora.gold.be.ents.HistoryCrum;
import info.dgjones.abora.gold.be.ents.Loaf;
import info.dgjones.abora.gold.be.ents.OExpandingLoaf;
import info.dgjones.abora.gold.be.ents.OPart;
import info.dgjones.abora.gold.be.ents.OrglRoot;
import info.dgjones.abora.gold.be.ents.RegionLoaf;
import info.dgjones.abora.gold.be.ents.SplitLoaf;
import info.dgjones.abora.gold.collection.basic.PrimArray;
import info.dgjones.abora.gold.collection.cache.HashSetCache;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.detect.FeFillRangeDetector;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.XnSensor;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeElementBundle;
import info.dgjones.abora.gold.nkernel.FeKeyMaster;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.snarf.FlockInfo;
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

public class RegionLoaf extends OExpandingLoaf {

	protected BeRangeElement myRangeElement;
	protected BeLabel myLabel;
/*
udanax-top.st:9284:
OExpandingLoaf subclass: #RegionLoaf
	instanceVariableNames: '
		myRangeElement {BeRangeElement}
		myLabel {BeLabel}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Ents'!
*/
/*
udanax-top.st:9290:
(RegionLoaf getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #COPY; add: #SHEPHERD.ANCESTOR; add: #LOCKED; add: #NOT.A.TYPE; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(RegionLoaf.class).setAttributes( new Set().add("COPY").add("SHEPHERDANCESTOR").add("LOCKED").add("NOTATYPE").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * return a mapping from my data to corresponding stuff in the given trace
 */
public Mapping compare(TracePosition trace, XnRegion region) {
	return myRangeElement.mappingTo(trace, (region.coordinateSpace().identityDsp().restrict(region)));
/*
udanax-top.st:9295:RegionLoaf methodsFor: 'accessing'!
{Mapping} compare: trace {TracePosition} with: region {XnRegion}
	"return a mapping from my data to corresponding stuff in the given trace"
	^myRangeElement mappingTo: trace with: (region coordinateSpace identityDsp restrict: region)!
*/
}
/**
 * Make a virtual DataHolder.
 */
public FeRangeElement fetch(Position key, BeEdition edition, Position globalKey) {
	if (domain().hasMember(key)) {
		return myRangeElement.makeFe(myLabel);
	}
	else {
		return null;
	}
/*
udanax-top.st:9299:RegionLoaf methodsFor: 'accessing'!
{FeRangeElement | NULL} fetch: key {Position} with: edition {BeEdition} with: globalKey {Position}
	"Make a virtual DataHolder."
	(self domain hasMember: key)
		ifTrue: [^myRangeElement makeFe: myLabel]
		ifFalse: [^NULL]!
*/
}
/**
 * Make an FeRangeElement for each position.
 */
public void fill(XnRegion keys, Arrangement toArrange, PrimArray toArray, Dsp dsp, BeEdition edition) {
	Stepper stomper = (keys.intersect(domain())).stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Position key = (Position) stomper.fetch();
		if (key == null) {
			continue ;
		}
		Position globalKey;
		FeRangeElement fe;
		globalKey = dsp.of(key);
		fe = myRangeElement.makeFe(myLabel);
		toArray.storeValue((toArrange.indexOf(globalKey)), fe);
	}
	stomper.destroy();
/*
udanax-top.st:9306:RegionLoaf methodsFor: 'accessing'!
{void} fill: keys {XnRegion} with: toArrange {Arrangement} with: toArray {PrimArray} with: dsp {Dsp} with: edition {BeEdition} 
	"Make an FeRangeElement for each position."
	
	(keys intersect: self domain) stepper forEach: 
		[:key {Position} |
		| globalKey {Position} fe {FeRangeElement} |
		globalKey _ dsp of: key.
		fe := myRangeElement makeFe: myLabel.
		toArray at: (toArrange indexOf: globalKey) DOTasLong
			storeValue: fe]!
*/
}
public void forwardTo(BeRangeElement rangeElement) {
	AboraBlockSupport.enterConsistent();
	try {
		rangeElement.addOParent(this);
		myRangeElement.removeOParent(this);
		myRangeElement = rangeElement;
		diskUpdate();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
	Ravi.thingToDo();
	/* Is there a lazier way to make the FeEdition? */
	if (hCrum().bertCrum().isSensorWaiting()) {
		hCrum().ringDetectors(asFeEdition());
	}
/*
udanax-top.st:9317:RegionLoaf methodsFor: 'accessing'!
{void} forwardTo: rangeElement {BeRangeElement}
	DiskManager consistent:
		[rangeElement addOParent: self.
		myRangeElement removeOParent: self.
		myRangeElement _ rangeElement.
		self diskUpdate].
	Ravi thingToDo. "Is there a lazier way to make the FeEdition?"
	self hCrum bertCrum isSensorWaiting ifTrue:
		[self hCrum ringDetectors: self asFeEdition]!
*/
}
/**
 * If I'm here it must be non-virtual.
 */
public BeRangeElement getBe(Position key) {
	if (domain().hasMember(key)) {
		return myRangeElement;
	}
	else {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
	}
/*
udanax-top.st:9328:RegionLoaf methodsFor: 'accessing'!
{BeRangeElement} getBe: key {Position}
	"If I'm here it must be non-virtual."
	(self domain hasMember: key)
		ifTrue: [^myRangeElement]
		ifFalse: [Heaper BLAST: #NotInTable.  ^NULL]!
*/
}
/**
 * The keys in this Edition at which there are Editions with the given label.
 */
public XnRegion keysLabelled(BeLabel label) {
	if (myLabel != null && (myLabel.isEqual(label))) {
		return domain();
	}
	else {
		return domain().coordinateSpace().emptyRegion();
	}
/*
udanax-top.st:9335:RegionLoaf methodsFor: 'accessing'!
{XnRegion} keysLabelled: label {BeLabel}
	"The keys in this Edition at which there are Editions with the given label."
	
	(myLabel ~~ NULL and: [myLabel isEqual: label])
		ifTrue: [^self domain]
		ifFalse: [^self domain coordinateSpace emptyRegion]!
*/
}
/**
 * return the mapping into the domain space of the given trace
 */
public Mapping mappingTo(TracePosition trace, Mapping initial) {
	return hCrum().mappingTo(trace, ((Mapping.make(initial.coordinateSpace(), domain())).restrict(initial.domain())));
/*
udanax-top.st:9342:RegionLoaf methodsFor: 'accessing'!
{Mapping} mappingTo: trace {TracePosition} with: initial {Mapping}
	"return the mapping into the domain space of the given trace"
	^self hCrum mappingTo: trace
		with: ((Mapping make: initial coordinateSpace with: self domain) restrict: initial domain)!
*/
}
/**
 * Return the owner of the atoms represented by the receiver.
 */
public ID owner() {
	return myRangeElement.owner();
/*
udanax-top.st:9347:RegionLoaf methodsFor: 'accessing'!
{ID} owner
	"Return the owner of the atoms represented by the receiver."
	
	^myRangeElement owner!
*/
}
/**
 * Return a region describing the stuff that can backfollow to trace.  Redefine this to pass
 * down to my hRoot.
 */
public XnRegion sharedRegion(TracePosition trace, XnRegion limitRegion) {
	if (myRangeElement.inTrace(trace)) {
		return domain();
	}
	else {
		return domain().coordinateSpace().emptyRegion();
	}
/*
udanax-top.st:9352:RegionLoaf methodsFor: 'accessing'!
{XnRegion} sharedRegion: trace {TracePosition} with: limitRegion {XnRegion unused}
	"Return a region describing the stuff that can backfollow to trace.  Redefine this to pass down to my hRoot."
	(myRangeElement inTrace: trace)
		ifTrue: [^self domain]
		ifFalse: [^self domain coordinateSpace emptyRegion]!
*/
}
/**
 * Return the PrimSpec that describes the representation of the data.
 */
public PrimSpec spec() {
	throw new UnimplementedException();
/*
udanax-top.st:9358:RegionLoaf methodsFor: 'accessing'!
{PrimSpec} spec
	"Return the PrimSpec that describes the representation of the data."
	
	self unimplemented.
	^PrimSpec pointer!
*/
}
public XnRegion usedDomain() {
	return domain();
/*
udanax-top.st:9364:RegionLoaf methodsFor: 'accessing'!
{XnRegion} usedDomain
	^self domain!
*/
}
/**
 * Return a stepper of bundles according to the order.
 */
public Stepper bundleStepper(XnRegion region, OrderSpec order, Dsp globalDsp) {
	XnRegion bundleRegion;
	bundleRegion = region.intersect((globalDsp.ofAll(domain())));
	if (bundleRegion.isEmpty()) {
		return Stepper.emptyStepper();
	}
	return Stepper.itemStepper((FeElementBundle.make(bundleRegion, (myRangeElement.makeFe(myLabel)))));
/*
udanax-top.st:9369:RegionLoaf methodsFor: 'operations'!
{Stepper} bundleStepper: region {XnRegion} with: order {OrderSpec} with: globalDsp {Dsp} 
	"Return a stepper of bundles according to the order."
	| bundleRegion {XnRegion} |
	bundleRegion _ region intersect: (globalDsp ofAll: self domain).
	bundleRegion isEmpty ifTrue: [^Stepper emptyStepper].
	^Stepper itemStepper: 
		(FeElementBundle 
			make: bundleRegion 
			with: (myRangeElement makeFe: myLabel))!
*/
}
public void informTo(OrglRoot orgl) {
	throw new UnimplementedException();
/*
udanax-top.st:9380:RegionLoaf methodsFor: 'operations'!
{void} informTo: orgl {OrglRoot unused}
	self unimplemented!
*/
}
/**
 * If the CurrentKeyMaster includes the owner of this loaf
 * then change the owner and return NULL
 * else just return self.
 */
public OrglRoot setAllOwners(ID owner) {
	if (((FeKeyMaster) CurrentKeyMaster.fluidGet()).hasAuthority(myRangeElement.owner())) {
		myRangeElement.setOwner(owner);
		return OrglRoot.make(domain().coordinateSpace());
	}
	else {
		return ActualOrglRoot.make(this, domain());
	}
/*
udanax-top.st:9383:RegionLoaf methodsFor: 'operations'!
{OrglRoot} setAllOwners: owner {ID} 
	"If the CurrentKeyMaster includes the owner of this loaf
		then change the owner and return NULL
		else just return self."
	
	(CurrentKeyMaster fluidGet hasAuthority: myRangeElement owner)
		ifTrue:
			[myRangeElement setOwner: owner.
			^OrglRoot make: self domain coordinateSpace]
		ifFalse: [^ActualOrglRoot make: self with: self domain]!
*/
}
public void printOn(PrintWriter aStream) {
	aStream.print(getAboraClass().name());
	aStream.print("(");
	aStream.print(domain());
	aStream.print(", ");
	aStream.print(myRangeElement);
	aStream.print(")");
/*
udanax-top.st:9396:RegionLoaf methodsFor: 'printing'!
{void} printOn: aStream {ostream reference}
	aStream << self getCategory name << '(' << self domain << ', ' << myRangeElement << ')'!
*/
}
/**
 * Don't expand me in place.  Just move it closer to the top.
 */
public int actualSoftSplay(XnRegion region, XnRegion limitRegion) {
	return 2;
/*
udanax-top.st:9401:RegionLoaf methodsFor: 'protected: splay'!
{Int8} actualSoftSplay: region {XnRegion} with: limitRegion {XnRegion unused} 
	"Don't expand me in place.  Just move it closer to the top."
	
	^2!
*/
}
/**
 * Expand my partial tree in place.  The area in the region must go
 * into the leftCrum of my substitute, or the splay algorithm will fail!!
 */
public int actualSplay(XnRegion region, XnRegion limitRegion) {
	Loaf tmp1;
	Loaf tmp2;
	AboraBlockSupport.enterConsistent(4);
	try {
		tmp1 = new RegionLoaf((domain().intersect(region)), myLabel, myRangeElement, (HUpperCrum.make(((HUpperCrum) hCrum()))));
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
	AboraBlockSupport.enterConsistent(4);
	try {
		tmp2 = new RegionLoaf((domain().intersect(region.complement())), myLabel, myRangeElement, (HUpperCrum.make(((HUpperCrum) hCrum()))));
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
	AboraBlockSupport.enterConsistent(4);
	try {
		HUpperCrum hcrum;
		int hash;
		FlockInfo info;
		hcrum = (HUpperCrum) hCrum();
		hash = hashForEqual();
		info = fetchInfo();
		/* TODO newBecome */
		new SplitLoaf(region, tmp1, tmp2, hcrum, hash, info);
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
	return 1;
/*
udanax-top.st:9406:RegionLoaf methodsFor: 'protected: splay'!
{Int8} actualSplay: region {XnRegion} with: limitRegion {XnRegion unused}
	"Expand my partial tree in place.  The area in the region must go
	 into the leftCrum of my substitute, or the splay algorithm will fail!!"
	
	| tmp1 {Loaf} tmp2 {Loaf} |
	DiskManager consistent: 4 with:
		[tmp1 _ RegionLoaf
			create: (self domain intersect: region)
			with: myLabel
			with: myRangeElement
			with: (HUpperCrum make: (self hCrum cast: HUpperCrum))].
	DiskManager consistent: 4 with:
		[tmp2 _ RegionLoaf
			create: (self domain intersect: region complement)
			with: myLabel
			with: myRangeElement
			with: (HUpperCrum make: (self hCrum cast: HUpperCrum))].
	DiskManager consistent: 4 with:
		[ | hcrum {HUpperCrum} hash {UInt32} info {FlockInfo} |
		hcrum _ self hCrum cast: HUpperCrum.
		hash _ self hashForEqual.
		info _ self fetchInfo.
		(SplitLoaf new.Become: self) 
			create: region 
			with: tmp1 
			with: tmp2 
			with: hcrum 
			with: hash 
			with: info].
	^1!
*/
}
public RegionLoaf(XnRegion region, BeLabel label, BeRangeElement element, HUpperCrum hcrum) {
	super(region, hcrum, element.sensorCrum());
	myLabel = label;
	myRangeElement = element;
	newShepherd();
	myRangeElement.addOParent(this);
/*
udanax-top.st:9439:RegionLoaf methodsFor: 'create'!
create: region {XnRegion} with: label {BeLabel | NULL} with: element {BeRangeElement} with: hcrum {HUpperCrum | NULL}
	super create: region with: hcrum with: element sensorCrum.
	myLabel _ label.
	myRangeElement _ element.
	self newShepherd.
	myRangeElement addOParent: self.!
*/
}
public RegionLoaf(XnRegion region, BeRangeElement element, HUpperCrum hcrum, int hash, FlockInfo info) {
	super(hash, region, hcrum, element.sensorCrum());
	if (element instanceof BeEdition) {
		throw new AboraRuntimeException(AboraRuntimeException.EDITIONS_REQUIRE_LABELS);
	}
	myLabel = null;
	Someone.knownBug();
	/* This doesn't deal with labels. */
	flockInfo(info);
	myRangeElement = element;
	myRangeElement.addOParent(this);
	diskUpdate();
/*
udanax-top.st:9446:RegionLoaf methodsFor: 'create'!
create: region {XnRegion} with: element {BeRangeElement} with: hcrum {HUpperCrum} with: hash {UInt32} with: info {FlockInfo}
	super create: hash with: region with: hcrum with: element sensorCrum.
	(element isKindOf: BeEdition) ifTrue: [Heaper BLAST: #EditionsRequireLabels].
	myLabel _ NULL.  self knownBug.  "This doesn't deal with labels."
	self flockInfo: info.
	myRangeElement _ element.
	myRangeElement addOParent: self.
	self diskUpdate!
*/
}
/**
 * add oparent to the set of upward pointers and update the bertCrums my child.
 */
public void addOParent(OPart oparent) {
	BertCrum bCrum;
	BertCrum newBCrum;
	bCrum = hCrum().bertCrum();
	super.addOParent(oparent);
	newBCrum = hCrum().bertCrum();
	if ( ! (bCrum.isLE(newBCrum))) {
		myRangeElement.updateBCrumTo(newBCrum);
	}
/*
udanax-top.st:9457:RegionLoaf methodsFor: 'backfollow'!
{void} addOParent: oparent {OPart} 
	"add oparent to the set of upward pointers and update the bertCrums my child."
	| bCrum {BertCrum} newBCrum {BertCrum} |
	bCrum _ self hCrum bertCrum.
	super addOParent: oparent.
	newBCrum _ self hCrum bertCrum.
	(bCrum isLE: newBCrum) not
		ifTrue: [myRangeElement updateBCrumTo: newBCrum]!
*/
}
public XnRegion attachTrailBlazer(TrailBlazer blazer) {
	if (myRangeElement instanceof BePlaceHolder) {
		BePlaceHolder p = (BePlaceHolder) myRangeElement;
		p.attachTrailBlazer(blazer);
		return domain();
	}
	else {
		return domain().coordinateSpace().emptyRegion();
	}
/*
udanax-top.st:9467:RegionLoaf methodsFor: 'backfollow'!
{XnRegion} attachTrailBlazer: blazer {TrailBlazer}
	
	myRangeElement cast: BePlaceHolder into: [ :p |
		p attachTrailBlazer: blazer.
		^self domain]
	others:
		[^self domain coordinateSpace emptyRegion]!
*/
}
public void checkChildRecorders(PropFinder finder) {
	myRangeElement.checkRecorders(finder, sensorCrum());
/*
udanax-top.st:9475:RegionLoaf methodsFor: 'backfollow'!
{void} checkChildRecorders: finder {PropFinder}
	myRangeElement checkRecorders: finder with: self sensorCrum!
*/
}
public void checkTrailBlazer(TrailBlazer blazer) {
	if (myRangeElement instanceof BePlaceHolder) {
		BePlaceHolder p = (BePlaceHolder) myRangeElement;
		p.checkTrailBlazer(blazer);
	}
	else {
		/* OK */
		;
	}
/*
udanax-top.st:9479:RegionLoaf methodsFor: 'backfollow'!
{void} checkTrailBlazer: blazer {TrailBlazer}
	
	myRangeElement cast: BePlaceHolder into: [ :p |
		p checkTrailBlazer: blazer]
	others:
		["OK"]!
*/
}
/**
 * RegionLoaf is the one kind of o-leaf which actually shares range-element identity with
 * other o-leafs.  The range element identity is in myRangeElement rather than myself, so I
 * override my super's version of this method to forward it south one more step to
 * myRangeElement.
 */
public void delayedStoreMatching(PropFinder finder, RecorderFossil fossil, ResultRecorder recorder, HashSetCache hCrumCache) {
	recorder.delayedStoreMatching(myRangeElement, finder, fossil, hCrumCache);
/*
udanax-top.st:9486:RegionLoaf methodsFor: 'backfollow'!
{void} delayedStoreMatching: finder {PropFinder} 
	with: fossil {RecorderFossil} 
	with: recorder {ResultRecorder}
	with: hCrumCache {HashSetCache of: HistoryCrum}
	"RegionLoaf is the one kind of o-leaf which actually shares range-element identity with other o-leafs.  The range element identity is in myRangeElement rather than myself, so I override my super's version of this method to forward it south one more step to myRangeElement."
	
	 recorder delayedStoreMatching: myRangeElement
	 	with: finder
	 	with: fossil
	 	with: hCrumCache!
*/
}
public TrailBlazer fetchTrailBlazer() {
	if (myRangeElement instanceof BePlaceHolder) {
		BePlaceHolder p = (BePlaceHolder) myRangeElement;
		return p.fetchTrailBlazer();
	}
	else {
		return null;
	}
/*
udanax-top.st:9497:RegionLoaf methodsFor: 'backfollow'!
{TrailBlazer | NULL} fetchTrailBlazer
	
	myRangeElement cast: BePlaceHolder into: [ :p |
		^p fetchTrailBlazer]
	others:
		[^NULL]!
*/
}
public void storeRecordingAgents(RecorderFossil recorder, Agenda agenda) {
	recorder.storeRangeElementRecordingAgents(myRangeElement, myRangeElement.sensorCrum(), agenda);
/*
udanax-top.st:9504:RegionLoaf methodsFor: 'backfollow'!
{void} storeRecordingAgents: recorder {RecorderFossil}
	with: agenda {Agenda}  
	
	recorder storeRangeElementRecordingAgents: myRangeElement
		with: myRangeElement sensorCrum
		with: agenda!
*/
}
/**
 * Return true if child is a child.  Used for debugging.
 */
public boolean testHChild(HistoryCrum child) {
	return ((Heaper) myRangeElement.hCrum()) == child;
/*
udanax-top.st:9511:RegionLoaf methodsFor: 'backfollow'!
{BooleanVar} testHChild: child {HistoryCrum}
	"Return true if child is a child.  Used for debugging."
	
	^(myRangeElement hCrum basicCast: Heaper star) == child!
*/
}
public void triggerDetector(FeFillRangeDetector detect) {
	if ( ! (myRangeElement instanceof BePlaceHolder)) {
		detect.rangeFilled(asFeEdition());
	}
/*
udanax-top.st:9516:RegionLoaf methodsFor: 'backfollow'!
{void} triggerDetector: detect {FeFillRangeDetector}
	
	(myRangeElement isKindOf: BePlaceHolder) ifFalse:
		[detect rangeFilled: self asFeEdition]!
*/
}
/**
 * My bertCrum must not be leafward of newBCrum.
 * Thus it must be LE to newCrum. Otherwise correct it and recur.
 */
public boolean updateBCrumTo(BertCrum newBCrum) {
	if (super.updateBCrumTo(newBCrum)) {
		myRangeElement.updateBCrumTo(newBCrum);
		return true;
	}
	return false;
/*
udanax-top.st:9521:RegionLoaf methodsFor: 'backfollow'!
{BooleanVar} updateBCrumTo: newBCrum {BertCrum} 
	"My bertCrum must not be leafward of newBCrum. 
	Thus it must be LE to newCrum. Otherwise correct it and recur."
	(super updateBCrumTo: newBCrum) ifTrue: 
		[myRangeElement updateBCrumTo: newBCrum.
		^true].
	^false!
*/
}
public void dismantle() {
	AboraBlockSupport.enterConsistent(4);
	try {
		if (Heaper.isConstructed(myRangeElement)) {
			myRangeElement.removeOParent(this);
		}
		super.dismantle();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:9532:RegionLoaf methodsFor: 'protected: delete'!
{void} dismantle
	DiskManager consistent: 4 with:
		[(Heaper isConstructed: myRangeElement) 
			ifTrue: [myRangeElement removeOParent: self].
		super dismantle]!
*/
}
public int contentsHash() {
	return super.contentsHash() ^ myRangeElement.hashForEqual();
/*
udanax-top.st:9540:RegionLoaf methodsFor: 'testing'!
{UInt32} contentsHash
	^super contentsHash
		bitXor: myRangeElement hashForEqual!
*/
}
/**
 * @deprecated
 */
public void wait(XnSensor sensor) {
	throw new PasseException();
/*
udanax-top.st:9547:RegionLoaf methodsFor: 'smalltalk: passe'!
{void} wait: sensor {XnSensor}
	
	self passe!
*/
}
public RegionLoaf(Rcvr receiver) {
	super(receiver);
	myRangeElement = (BeRangeElement) receiver.receiveHeaper();
	myLabel = (BeLabel) receiver.receiveHeaper();
/*
udanax-top.st:9553:RegionLoaf methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myRangeElement _ receiver receiveHeaper.
	myLabel _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myRangeElement);
	xmtr.sendHeaper(myLabel);
/*
udanax-top.st:9558:RegionLoaf methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myRangeElement.
	xmtr sendHeaper: myLabel.!
*/
}
public RegionLoaf() {
/*

Generated during transformation
*/
}
}
