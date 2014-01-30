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
import info.dgjones.abora.gold.be.ents.ActualOrglRoot;
import info.dgjones.abora.gold.be.ents.DspLoaf;
import info.dgjones.abora.gold.be.ents.HistoryCrum;
import info.dgjones.abora.gold.be.ents.InnerLoaf;
import info.dgjones.abora.gold.be.ents.Loaf;
import info.dgjones.abora.gold.be.ents.OExpandingLoaf;
import info.dgjones.abora.gold.be.ents.OPart;
import info.dgjones.abora.gold.be.ents.OrglRoot;
import info.dgjones.abora.gold.collection.basic.PrimArray;
import info.dgjones.abora.gold.collection.cache.HashSetCache;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.detect.FeFillRangeDetector;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.XnSensor;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.spaces.basic.Dsp;
import info.dgjones.abora.gold.spaces.basic.Mapping;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.unordered.IDSpace;
import info.dgjones.abora.gold.tclude.TrailBlazer;
import info.dgjones.abora.gold.traces.TracePosition;
import info.dgjones.abora.gold.turtle.Agenda;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class DspLoaf extends InnerLoaf {

	protected Dsp myDsp;
	protected Loaf myO;
/*
udanax-top.st:7853:
InnerLoaf subclass: #DspLoaf
	instanceVariableNames: '
		myDsp {Dsp}
		myO {Loaf}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Ents'!
*/
/*
udanax-top.st:7859:
(DspLoaf getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #COPY; add: #SHEPHERD.ANCESTOR; add: #LOCKED; add: #NOT.A.TYPE; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(DspLoaf.class).setAttributes( new Set().add("COPY").add("SHEPHERDANCESTOR").add("LOCKED").add("NOTATYPE").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * return a mapping from my data to corresponding stuff in the given trace
 */
public Mapping compare(TracePosition trace, XnRegion region) {
	return (myO.compare(trace, (myDsp.inverseOfAll(region)))).transformedBy(((Dsp) myDsp.inverse()));
/*
udanax-top.st:7864:DspLoaf methodsFor: 'accessing'!
{Mapping} compare: trace {TracePosition} with: region {XnRegion}
	"return a mapping from my data to corresponding stuff in the given trace"
	^(myO compare: trace with: (myDsp inverseOfAll: region))
		transformedBy: (myDsp inverse cast: Dsp)!
*/
}
public int count() {
	return myO.count();
/*
udanax-top.st:7869:DspLoaf methodsFor: 'accessing'!
{IntegerVar} count
	^myO count!
*/
}
public XnRegion domain() {
	return myDsp.ofAll(myO.domain());
/*
udanax-top.st:7872:DspLoaf methodsFor: 'accessing'!
{XnRegion} domain
	^myDsp ofAll: myO domain!
*/
}
/**
 * Look up the range element for the key.  If it is embedded within a virtual
 * structure, then make a virtual range element using the edition and globalKey.
 */
public FeRangeElement fetch(Position key, BeEdition edition, Position globalKey) {
	return myO.fetch((myDsp.inverseOf(key)), edition, globalKey);
/*
udanax-top.st:7875:DspLoaf methodsFor: 'accessing'!
{FeRangeElement | NULL} fetch: key {Position} with: edition {BeEdition} with: globalKey {Position}
	"Look up the range element for the key.  If it is embedded within a virtual
	 structure, then make a virtual range element using the edition and globalKey."
	^myO fetch: (myDsp inverseOf: key) with: edition with: globalKey!
*/
}
/**
 * Return the bottom-most Loaf.  Used to get the owner and such of a position.
 */
public OExpandingLoaf fetchBottomAt(Position key) {
	return myO.fetchBottomAt((myDsp.inverseOf(key)));
/*
udanax-top.st:7881:DspLoaf methodsFor: 'accessing'!
{OExpandingLoaf} fetchBottomAt: key {Position}
	"Return the bottom-most Loaf.  Used to get the owner and such of a position."
	^myO fetchBottomAt: (myDsp inverseOf: key)!
*/
}
/**
 * Make an FeRangeElement for each position.
 */
public void fill(XnRegion keys, Arrangement toArrange, PrimArray toArray, Dsp globalDsp, BeEdition edition) {
	if ( ! (keys.isEmpty())) {
		myO.fill((myDsp.inverseOfAll(keys)), toArrange, toArray, (globalDsp.compose(myDsp)), edition);
	}
/*
udanax-top.st:7886:DspLoaf methodsFor: 'accessing'!
{void} fill: keys {XnRegion} with: toArrange {Arrangement} with: toArray {PrimArray} with: globalDsp {Dsp} with: edition {BeEdition} 
	"Make an FeRangeElement for each position."
	keys isEmpty ifFalse:
		[myO fill: (myDsp inverseOfAll: keys)
			with: toArrange
			with: toArray
			with: (globalDsp compose: myDsp)
			with: edition]!
*/
}
/**
 * Get or Make the BeRangeElement at the location.
 */
public BeRangeElement getBe(Position key) {
	return myO.getBe((myDsp.inverseOf(key)));
/*
udanax-top.st:7896:DspLoaf methodsFor: 'accessing'!
{BeRangeElement} getBe: key {Position}
	"Get or Make the BeRangeElement at the location."
		
	^myO getBe: (myDsp inverseOf: key)!
*/
}
/**
 * This is used by the splay algorithms.
 */
public Loaf inPart() {
	return ((InnerLoaf) myO).inPart().transformedBy(myDsp);
/*
udanax-top.st:7901:DspLoaf methodsFor: 'accessing'!
{Loaf} inPart
	"This is used by the splay algorithms."
	^(myO cast: InnerLoaf) inPart transformedBy: myDsp!
*/
}
/**
 * return the mapping into the domain space of the given trace
 */
public Mapping mappingTo(TracePosition trace, Mapping initial) {
	return hCrum().mappingTo(trace, (initial.preCompose(myDsp)));
/*
udanax-top.st:7906:DspLoaf methodsFor: 'accessing'!
{Mapping} mappingTo: trace {TracePosition} with: initial {Mapping}
	"return the mapping into the domain space of the given trace"
	^self hCrum mappingTo: trace with: (initial preCompose: myDsp)!
*/
}
/**
 * This is used by the splay algorithms.
 */
public Loaf outPart() {
	return ((InnerLoaf) myO).outPart().transformedBy(myDsp);
/*
udanax-top.st:7910:DspLoaf methodsFor: 'accessing'!
{Loaf} outPart
	"This is used by the splay algorithms."
	^(myO cast: InnerLoaf) outPart transformedBy: myDsp!
*/
}
public XnRegion rangeOwners(XnRegion positions) {
	if (positions == null) {
		return myO.rangeOwners(null);
	}
	if (positions.isEmpty()) {
		return IDSpace.global().emptyRegion();
	}
	else {
		return myO.rangeOwners((myDsp.inverseOfAll(positions)));
	}
/*
udanax-top.st:7915:DspLoaf methodsFor: 'accessing'!
{XnRegion} rangeOwners: positions {XnRegion | NULL} 
	
	positions == NULL ifTrue: [^myO rangeOwners: NULL].
	positions isEmpty
		ifTrue: [^IDSpace global emptyRegion]
		ifFalse: [^myO rangeOwners: (myDsp inverseOfAll: positions)]!
*/
}
/**
 * Recur assigning owners.  Return the portion of the o-tree that couldn't be assigned.
 */
public OrglRoot setAllOwners(ID owner) {
	return (myO.setAllOwners(owner)).transformedBy(myDsp);
/*
udanax-top.st:7922:DspLoaf methodsFor: 'accessing'!
{OrglRoot} setAllOwners: owner {ID}
	"Recur assigning owners.  Return the portion of the o-tree that couldn't be assigned."
		
	^(myO setAllOwners: owner) transformedBy: myDsp!
*/
}
public XnRegion usedDomain() {
	return myDsp.ofAll(myO.usedDomain());
/*
udanax-top.st:7927:DspLoaf methodsFor: 'accessing'!
{XnRegion} usedDomain
	^myDsp ofAll: myO usedDomain!
*/
}
/**
 * Make each child completely contained or completely outside
 * the region.  Return the number of children completely in the region.
 */
public int actualSplay(XnRegion region, XnRegion limitRegion) {
	Dsp dsp;
	dsp = myDsp;
	return myO.splay((dsp.inverseOfAll(region)), (dsp.inverseOfAll(limitRegion)));
/*
udanax-top.st:7932:DspLoaf methodsFor: 'protected: splay'!
{Int8} actualSplay: region {XnRegion} with: limitRegion {XnRegion}
	"Make each child completely contained or completely outside
	 the region.  Return the number of children completely in the region."
	 
	 | dsp {Dsp} |
	 dsp _ myDsp.
	 ^myO splay: (dsp inverseOfAll: region) 
	 		with: (dsp inverseOfAll: limitRegion)!
*/
}
/**
 * Return a stepper of bundles according to the order.
 */
public Stepper bundleStepper(XnRegion region, OrderSpec order, Dsp globalDsp) {
	return myO.bundleStepper(region, order, (globalDsp.compose(myDsp)));
/*
udanax-top.st:7943:DspLoaf methodsFor: 'operations'!
{Stepper} bundleStepper: region {XnRegion} with: order {OrderSpec} with: globalDsp {Dsp}
	"Return a stepper of bundles according to the order."
	
	^myO bundleStepper: region
		with: order
		with: (globalDsp compose: myDsp)!
*/
}
/**
 * Accumulate dsp downward.
 */
public OrglRoot combine(ActualOrglRoot another, XnRegion limitRegion, Dsp globalDsp) {
	return myO.combine(another, limitRegion, (globalDsp.compose(myDsp)));
/*
udanax-top.st:7950:DspLoaf methodsFor: 'operations'!
{OrglRoot} combine: another {ActualOrglRoot} with: limitRegion {XnRegion} with: globalDsp {Dsp}
	"Accumulate dsp downward."
	
	^myO combine: another with: limitRegion with: (globalDsp compose: myDsp)!
*/
}
/**
 * Just search for now.
 */
public XnRegion keysLabelled(BeLabel label) {
	return myDsp.ofAll((myO.keysLabelled(label)));
/*
udanax-top.st:7955:DspLoaf methodsFor: 'operations'!
{XnRegion} keysLabelled: label {BeLabel}
	"Just search for now."
	
	^myDsp ofAll: (myO keysLabelled: label)!
*/
}
/**
 * Return a region describing the stuff that can backfollow to trace.
 */
public XnRegion sharedRegion(TracePosition trace, XnRegion limitRegion) {
	if (hCrum().inTrace(trace)) {
		return domain();
	}
	else {
		return myDsp.ofAll((myO.sharedRegion(trace, (myDsp.inverseOfAll(limitRegion)))));
	}
/*
udanax-top.st:7960:DspLoaf methodsFor: 'operations'!
{XnRegion} sharedRegion: trace {TracePosition} with: limitRegion {XnRegion}
	"Return a region describing the stuff that can backfollow to trace."
	(self hCrum inTrace: trace)
		ifTrue: [^self domain]
		ifFalse:  [^myDsp ofAll: (myO
			sharedRegion: trace with: (myDsp inverseOfAll: limitRegion))]!
*/
}
/**
 * Return a copy with externalDsp added to the receiver's dsp.
 */
public Loaf transformedBy(Dsp externalDsp) {
	if (externalDsp.isIdentity()) {
		return this;
	}
	else {
		return myO.transformedBy((externalDsp.compose(myDsp)));
	}
/*
udanax-top.st:7967:DspLoaf methodsFor: 'operations'!
{Loaf} transformedBy: externalDsp {Dsp}
	"Return a copy with externalDsp added to the receiver's dsp."
	externalDsp isIdentity 
		ifTrue: [^self]
		ifFalse: [^myO transformedBy: (externalDsp compose: myDsp)]!
*/
}
/**
 * Return a copy with externalDsp removed from the receiver's dsp.
 */
public Loaf unTransformedBy(Dsp externalDsp) {
	if (externalDsp.isIdentity()) {
		return this;
	}
	else {
		return myO.unTransformedBy((myDsp.minus(externalDsp)));
	}
/*
udanax-top.st:7974:DspLoaf methodsFor: 'operations'!
{Loaf} unTransformedBy: externalDsp {Dsp}
	"Return a copy with externalDsp removed from the receiver's dsp."
	externalDsp isIdentity 
		ifTrue: [^self]
		ifFalse: [^myO unTransformedBy: (myDsp minus: externalDsp)]!
*/
}
public void printOn(PrintWriter aStream) {
	aStream.print("(");
	aStream.print(myDsp);
	aStream.print(")");
/*
udanax-top.st:7983:DspLoaf methodsFor: 'printing'!
{void} printOn: aStream {ostream reference}
	aStream << '(' << myDsp << ')'!
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
		myO.updateBCrumTo(newBCrum);
	}
/*
udanax-top.st:7988:DspLoaf methodsFor: 'backfollow'!
{void} addOParent: oparent {OPart} 
	"add oparent to the set of upward pointers and update the bertCrums my child."
	| bCrum {BertCrum} newBCrum {BertCrum} |
	bCrum _ self hCrum bertCrum.
	super addOParent: oparent.
	newBCrum _ self hCrum bertCrum.
	(bCrum isLE: newBCrum) not
		ifTrue: [myO updateBCrumTo: newBCrum]!
*/
}
public XnRegion attachTrailBlazer(TrailBlazer blazer) {
	return myDsp.ofAll((myO.attachTrailBlazer(blazer)));
/*
udanax-top.st:7998:DspLoaf methodsFor: 'backfollow'!
{XnRegion} attachTrailBlazer: blazer {TrailBlazer}
	
	^myDsp ofAll: (myO attachTrailBlazer: blazer)!
*/
}
/**
 * send checkRecorders to all children
 */
public void checkChildRecorders(PropFinder finder) {
	myO.checkRecorders(finder, sensorCrum());
/*
udanax-top.st:8002:DspLoaf methodsFor: 'backfollow'!
{void} checkChildRecorders: finder {PropFinder}
	"send checkRecorders to all children"
	myO checkRecorders: finder with: self sensorCrum!
*/
}
public void checkTrailBlazer(TrailBlazer blazer) {
	myO.checkTrailBlazer(blazer);
/*
udanax-top.st:8006:DspLoaf methodsFor: 'backfollow'!
{void} checkTrailBlazer: blazer {TrailBlazer}
	
	myO checkTrailBlazer: blazer!
*/
}
public void delayedStoreMatching(PropFinder finder, RecorderFossil fossil, ResultRecorder recorder, HashSetCache hCrumCache) {
	myO.delayedStoreMatching(finder, fossil, recorder, hCrumCache);
/*
udanax-top.st:8010:DspLoaf methodsFor: 'backfollow'!
{void} delayedStoreMatching: finder {PropFinder} 
	with: fossil {RecorderFossil} 
	with: recorder {ResultRecorder}
	with: hCrumCache {HashSetCache of: HistoryCrum}
	
	myO delayedStoreMatching: finder with: fossil with: recorder with: hCrumCache!
*/
}
public TrailBlazer fetchTrailBlazer() {
	return myO.fetchTrailBlazer();
/*
udanax-top.st:8017:DspLoaf methodsFor: 'backfollow'!
{TrailBlazer | NULL} fetchTrailBlazer
	
	^myO fetchTrailBlazer!
*/
}
public void storeRecordingAgents(RecorderFossil recorder, Agenda agenda) {
	myO.storeRecordingAgents(recorder, agenda);
/*
udanax-top.st:8021:DspLoaf methodsFor: 'backfollow'!
{void} storeRecordingAgents: recorder {RecorderFossil}
	with: agenda {Agenda} 
	
	myO storeRecordingAgents: recorder with: agenda!
*/
}
public void triggerDetector(FeFillRangeDetector detect) {
	if (sensorCrum().isPartial()) {
		myO.triggerDetector(detect);
	}
	else {
		detect.rangeFilled(asFeEdition());
	}
/*
udanax-top.st:8026:DspLoaf methodsFor: 'backfollow'!
{void} triggerDetector: detect {FeFillRangeDetector}
	
	self sensorCrum isPartial ifTrue:
		[myO triggerDetector: detect]
	ifFalse:
		[detect rangeFilled: self asFeEdition]!
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
udanax-top.st:8033:DspLoaf methodsFor: 'backfollow'!
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
public DspLoaf(Loaf loaf, Dsp dsp) {
	super(null, loaf.sensorCrum());
	myO = loaf;
	myDsp = dsp;
	/* Connect the HTrees. */
	newShepherd();
	myO.addOParent(this);
/*
udanax-top.st:8045:DspLoaf methodsFor: 'create'!
create: loaf {Loaf} with: dsp {Dsp}
	super create: NULL with: loaf sensorCrum.
	myO _ loaf.
	myDsp _ dsp.
	"Connect the HTrees."
	self newShepherd.
	myO addOParent: self.!
*/
}
/*
udanax-top.st:8055:DspLoaf methodsFor: 'smalltalk:'!
crums
	^ Array with: myO!
*/
/**
 * Return true if child is a child.  Used for debugging.
 */
public boolean testChild(Loaf child) {
	return myO.isEqual(child);
/*
udanax-top.st:8058:DspLoaf methodsFor: 'smalltalk:'!
{BooleanVar} testChild: child {Loaf}
	"Return true if child is a child.  Used for debugging."
	
	^myO isEqual: child!
*/
}
/**
 * Return true if child is a child.  Used for debugging.
 */
public boolean testHChild(HistoryCrum child) {
	return myO.hCrum() == child;
/*
udanax-top.st:8063:DspLoaf methodsFor: 'smalltalk:'!
{BooleanVar} testHChild: child {HistoryCrum}
	"Return true if child is a child.  Used for debugging."
	
	^myO hCrum == child!
*/
}
public void dismantle() {
	AboraBlockSupport.enterConsistent(3);
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
udanax-top.st:8070:DspLoaf methodsFor: 'protected: delete'!
{void} dismantle
	DiskManager consistent: 3 with:
		[(Heaper isConstructed: myO) ifTrue: [myO removeOParent: self].
		super dismantle]!
*/
}
public int contentsHash() {
	return (super.contentsHash() ^ myDsp.hashForEqual()) ^ myO.hashForEqual();
/*
udanax-top.st:8077:DspLoaf methodsFor: 'testing'!
{UInt32} contentsHash
	^(super contentsHash
		bitXor: myDsp hashForEqual)
		bitXor: myO hashForEqual!
*/
}
/**
 * @deprecated
 */
public void wait(XnSensor sensor) {
	throw new PasseException();
/*
udanax-top.st:8085:DspLoaf methodsFor: 'smalltalk: passe'!
{void} wait: sensor {XnSensor}
	
	self passe!
*/
}
public DspLoaf(Rcvr receiver) {
	super(receiver);
	myDsp = (Dsp) receiver.receiveHeaper();
	myO = (Loaf) receiver.receiveHeaper();
/*
udanax-top.st:8091:DspLoaf methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myDsp _ receiver receiveHeaper.
	myO _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myDsp);
	xmtr.sendHeaper(myO);
/*
udanax-top.st:8096:DspLoaf methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myDsp.
	xmtr sendHeaper: myO.!
*/
}
public DspLoaf() {
/*

Generated during transformation
*/
}
}
