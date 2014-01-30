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
import info.dgjones.abora.gold.be.ents.HUpperCrum;
import info.dgjones.abora.gold.be.ents.HistoryCrum;
import info.dgjones.abora.gold.be.ents.InnerLoaf;
import info.dgjones.abora.gold.be.ents.Loaf;
import info.dgjones.abora.gold.be.ents.MergeBundlesStepper;
import info.dgjones.abora.gold.be.ents.OExpandingLoaf;
import info.dgjones.abora.gold.be.ents.OPart;
import info.dgjones.abora.gold.be.ents.OrglRoot;
import info.dgjones.abora.gold.be.ents.SplitLoaf;
import info.dgjones.abora.gold.collection.basic.PrimArray;
import info.dgjones.abora.gold.collection.cache.HashSetCache;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.detect.FeFillRangeDetector;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.XnSensor;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.snarf.FlockInfo;
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

public class SplitLoaf extends InnerLoaf {

	protected XnRegion mySplit;
	protected Loaf myIn;
	protected Loaf myOut;
/*
udanax-top.st:8101:
InnerLoaf subclass: #SplitLoaf
	instanceVariableNames: '
		mySplit {XnRegion}
		myIn {Loaf}
		myOut {Loaf}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Ents'!
*/
/*
udanax-top.st:8108:
(SplitLoaf getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(MAY.BECOME.ANY.SUBCLASS.OF OExpandingLoaf ); add: #COPY; add: #SHEPHERD.ANCESTOR; add: #LOCKED; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SplitLoaf.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"MAYBECOMEANYSUBCLASSOF", "OExpandingLoaf"}).add("COPY").add("SHEPHERDANCESTOR").add("LOCKED").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * return a mapping from my data to corresponding stuff in the given trace
 */
public Mapping compare(TracePosition trace, XnRegion region) {
	return (myIn.compare(trace, (region.intersect(mySplit)))).combine((myOut.compare(trace, (region.minus(mySplit)))));
/*
udanax-top.st:8113:SplitLoaf methodsFor: 'accessing'!
{Mapping} compare: trace {TracePosition} with: region {XnRegion}
	"return a mapping from my data to corresponding stuff in the given trace"
	^(myIn compare: trace with: (region intersect: mySplit)) combine:
		(myOut compare: trace with: (region minus: mySplit))!
*/
}
public int count() {
	return myIn.count() + myOut.count();
/*
udanax-top.st:8118:SplitLoaf methodsFor: 'accessing'!
{IntegerVar} count
	^myIn count + myOut count!
*/
}
public XnRegion domain() {
	return myIn.domain().unionWith(myOut.domain());
/*
udanax-top.st:8121:SplitLoaf methodsFor: 'accessing'!
{XnRegion} domain
	^myIn domain unionWith: myOut domain!
*/
}
/**
 * Look up the range element for the key.  If it is embedded within a virtual
 * structure, then make a virtual range element using the edition and globalKey.
 */
public FeRangeElement fetch(Position key, BeEdition edition, Position globalKey) {
	if (mySplit.hasMember(key)) {
		return myIn.fetch(key, edition, globalKey);
	}
	else {
		return myOut.fetch(key, edition, globalKey);
	}
/*
udanax-top.st:8124:SplitLoaf methodsFor: 'accessing'!
{FeRangeElement | NULL} fetch: key {Position} with: edition {BeEdition} with: globalKey {Position}
	"Look up the range element for the key.  If it is embedded within a virtual
	 structure, then make a virtual range element using the edition and globalKey."
	(mySplit hasMember: key) 
		ifTrue: [^myIn fetch: key with: edition with: globalKey]
		ifFalse: [^myOut fetch: key with: edition with: globalKey]!
*/
}
/**
 * Return the bottom-most Loaf.  Used to get the owner and such of a position.
 */
public OExpandingLoaf fetchBottomAt(Position key) {
	Someone.thingToDo();
	/* This should be splaying!! */
	if (mySplit.hasMember(key)) {
		return myIn.fetchBottomAt(key);
	}
	else {
		return myOut.fetchBottomAt(key);
	}
/*
udanax-top.st:8132:SplitLoaf methodsFor: 'accessing'!
{OExpandingLoaf} fetchBottomAt: key {Position}
	"Return the bottom-most Loaf.  Used to get the owner and such of a position."
	self thingToDo.  "This should be splaying!!"
	(mySplit hasMember: key) 
		ifTrue: [^myIn fetchBottomAt: key]
		ifFalse: [^myOut fetchBottomAt: key]!
*/
}
/**
 * Get or Make the BeRangeElement at the location.
 */
public BeRangeElement getBe(Position key) {
	Someone.thingToDo();
	/* This should be splaying!! */
	if (mySplit.hasMember(key)) {
		return myIn.getBe(key);
	}
	else {
		return myOut.getBe(key);
	}
/*
udanax-top.st:8140:SplitLoaf methodsFor: 'accessing'!
{BeRangeElement} getBe: key {Position}
	"Get or Make the BeRangeElement at the location."
		
	self thingToDo.  "This should be splaying!!"
	(mySplit hasMember: key) 
		ifTrue: [^myIn getBe: key]
		ifFalse: [^myOut getBe: key]!
*/
}
/**
 * This effectively copies the region represented by my distinction.
 */
public Loaf inPart() {
	return myIn;
/*
udanax-top.st:8148:SplitLoaf methodsFor: 'accessing'!
{Loaf} inPart
	"This effectively copies the region represented by my distinction."
	^myIn!
*/
}
public boolean isLeaf() {
	return false;
/*
udanax-top.st:8153:SplitLoaf methodsFor: 'accessing'!
{BooleanVar} isLeaf
	^false!
*/
}
/**
 * This is used by the splay algorithms.
 */
public Loaf outPart() {
	return myOut;
/*
udanax-top.st:8156:SplitLoaf methodsFor: 'accessing'!
{Loaf} outPart
	"This is used by the splay algorithms."
	^myOut!
*/
}
public XnRegion rangeOwners(XnRegion positions) {
	XnRegion result;
	if (positions == null) {
		return (myIn.rangeOwners(null)).unionWith((myIn.rangeOwners(null)));
	}
	result = IDSpace.global().emptyRegion();
	if (mySplit.intersects(positions)) {
		result = myIn.rangeOwners(positions);
	}
	if (mySplit.complement().intersects(positions)) {
		result = (myIn.rangeOwners(positions)).unionWith(result);
	}
	return result;
/*
udanax-top.st:8161:SplitLoaf methodsFor: 'accessing'!
{XnRegion} rangeOwners: positions {XnRegion | NULL} 
	
	| result {XnRegion} |
	positions == NULL 
		ifTrue: [^(myIn rangeOwners: NULL) unionWith: (myIn rangeOwners: NULL)].
	result _ IDSpace global emptyRegion.
	(mySplit intersects: positions)
		ifTrue: [result _ myIn rangeOwners: positions].
	(mySplit complement intersects: positions)
		ifTrue: [result _ (myIn rangeOwners: positions) unionWith: result].
	^result!
*/
}
/**
 * Recur assigning owners.  Return the portion of the o-tree that couldn't be assigned.
 */
public OrglRoot setAllOwners(ID owner) {
	OrglRoot in;
	OrglRoot out;
	in = myIn.setAllOwners(owner);
	out = myOut.setAllOwners(owner);
	if (in.isEmpty()) {
		return out;
	}
	if (out.isEmpty()) {
		return in;
	}
	if (((ActualOrglRoot) in).fullcrum() == myIn && (((ActualOrglRoot) out).fullcrum() == myOut)) {
		return ActualOrglRoot.make(this, (in.simpleDomain().simpleUnion(out.simpleDomain())));
	}
	return ((ActualOrglRoot) in).makeNew(mySplit, ((ActualOrglRoot) in), ((ActualOrglRoot) out));
/*
udanax-top.st:8173:SplitLoaf methodsFor: 'accessing'!
{OrglRoot} setAllOwners: owner {ID}
	"Recur assigning owners.  Return the portion of the o-tree that couldn't be assigned."
		
	| in {OrglRoot} out {OrglRoot} |
	in _ myIn setAllOwners: owner.
	out _ myOut setAllOwners: owner.
	in isEmpty ifTrue: [^out].
	out isEmpty ifTrue: [^in].
	((in cast: ActualOrglRoot) fullcrum == myIn and: [(out cast: ActualOrglRoot) fullcrum == myOut]) 
		ifTrue: [^ActualOrglRoot make: self 
					with: (in simpleDomain simpleUnion: out simpleDomain)].
	^(in cast: ActualOrglRoot) makeNew: mySplit
		with: (in cast: ActualOrglRoot)
		with: (out cast: ActualOrglRoot)!
*/
}
public XnRegion usedDomain() {
	return myIn.usedDomain().unionWith(myOut.usedDomain());
/*
udanax-top.st:8188:SplitLoaf methodsFor: 'accessing'!
{XnRegion} usedDomain
	^myIn usedDomain unionWith: myOut usedDomain!
*/
}
/**
 * Return a stepper of bundles according to the order.
 */
public Stepper bundleStepper(XnRegion region, OrderSpec order, Dsp globalDsp) {
	XnRegion local;
	Stepper in;
	Stepper out;
	local = globalDsp.inverseOfAll(region);
	in = out = null;
	if (mySplit.intersects(local)) {
		in = myIn.bundleStepper(region, order, globalDsp);
	}
	if (mySplit.complement().intersects(local)) {
		out = myOut.bundleStepper(region, order, globalDsp);
	}
	if (in == null) {
		if (out == null) {
			return Stepper.emptyStepper();
		}
		else {
			return out;
		}
	}
	else {
		if (out == null) {
			return in;
		}
		else {
			return MergeBundlesStepper.make(in, out, order);
		}
	}
/*
udanax-top.st:8193:SplitLoaf methodsFor: 'operations'!
{Stepper} bundleStepper: region {XnRegion} with: order {OrderSpec} with: globalDsp {Dsp}
	"Return a stepper of bundles according to the order."
	
	| local {XnRegion} in {Stepper} out {Stepper} |
	local _ globalDsp inverseOfAll: region.
	in _ out _ NULL.
	(mySplit intersects: local)
		ifTrue: [in _ myIn bundleStepper: region with: order with: globalDsp].
	(mySplit complement intersects: local)
		ifTrue: [out _ myOut bundleStepper: region with: order with: globalDsp].
	in == NULL
		ifTrue: 
			[out == NULL 
				ifTrue: [^Stepper emptyStepper]
				ifFalse: [^out]]
		ifFalse:
			[out == NULL 
				ifTrue: [^in]
				ifFalse: [^MergeBundlesStepper make: in with: out with: order]]!
*/
}
/**
 * Break another into pieces according to mySplit, and combine
 * the corresponding pieces with my children transformed to global
 * coordinates.  Combine the two non-overlapping results.
 */
public OrglRoot combine(ActualOrglRoot another, XnRegion limitRegion, Dsp globalDsp) {
	ActualOrglRoot newIn;
	ActualOrglRoot newOut;
	OrglRoot hisIn;
	OrglRoot hisOut;
	XnRegion globalIn;
	XnRegion globalOut;
	globalIn = globalDsp.ofAll(mySplit);
	globalOut = globalIn.complement();
	newIn = ActualOrglRoot.make((myIn.transformedBy(globalDsp)), (limitRegion.intersect(globalIn)));
	newOut = ActualOrglRoot.make((myOut.transformedBy(globalDsp)), (limitRegion.intersect(globalOut)));
	hisIn = another.copy(globalIn);
	hisOut = another.copy(globalOut);
	/* Can this assume that the results don't overlap? */
	return newIn.makeNew(globalIn, ((ActualOrglRoot) (newIn.combine(hisIn))), ((ActualOrglRoot) (newOut.combine(hisOut))));
/*
udanax-top.st:8213:SplitLoaf methodsFor: 'operations'!
{OrglRoot} combine: another {ActualOrglRoot} with: limitRegion {XnRegion} with: globalDsp {Dsp}
	"Break another into pieces according to mySplit, and combine
	 the corresponding pieces with my children transformed to global 
	 coordinates.  Combine the two non-overlapping results."
	
	| newIn {ActualOrglRoot} newOut {ActualOrglRoot} hisIn {OrglRoot} hisOut {OrglRoot}
	  globalIn {XnRegion} globalOut {XnRegion} |
	globalIn _ globalDsp ofAll: mySplit.
	globalOut _ globalIn complement.
	newIn _ ActualOrglRoot make: (myIn transformedBy: globalDsp) 
				with: (limitRegion intersect: globalIn).
	newOut _ ActualOrglRoot make: (myOut transformedBy: globalDsp) 
				with: (limitRegion intersect: globalOut).
	hisIn _ another copy: globalIn.
	hisOut _ another copy: globalOut.
	"Can this assume that the results don't overlap?"
	^newIn makeNew: globalIn 
		with: ((newIn combine: hisIn) cast: ActualOrglRoot) 
		with: ((newOut combine: hisOut) cast: ActualOrglRoot)!
*/
}
/**
 * Make an FeRangeElement for each position.
 */
public void fill(XnRegion keys, Arrangement toArrange, PrimArray toArray, Dsp globalDsp, BeEdition edition) {
	myIn.fill((keys.intersect(mySplit)), toArrange, toArray, globalDsp, edition);
	myOut.fill((keys.intersect(mySplit.complement())), toArrange, toArray, globalDsp, edition);
/*
udanax-top.st:8233:SplitLoaf methodsFor: 'operations'!
{void} fill: keys {XnRegion} with: toArrange {Arrangement} with: toArray {PrimArray} with: globalDsp {Dsp} with: edition {BeEdition} 
	"Make an FeRangeElement for each position."
	myIn fill: (keys intersect: mySplit)
		with: toArrange
		with: toArray
		with: globalDsp
		with: edition.
	myOut fill: (keys intersect: mySplit complement)
		with: toArrange
		with: toArray
		with: globalDsp
		with: edition.!
*/
}
/**
 * Copy the enclosure in orgl appropriate for this crum, then hand it down to the
 * subCrums.
 */
public void informTo(OrglRoot orgl) {
	throw new UnimplementedException();
/*
udanax-top.st:8247:SplitLoaf methodsFor: 'operations'!
{void} informTo: orgl {OrglRoot unused} 
	"Copy the enclosure in orgl appropriate for this crum, then hand it down to the 
	subCrums."
	self unimplemented.
	"orgl isKnownEmpty ifFalse:
		[myLeft informTo: ((orgl copy: leftWisp externalRegion) unTransformedBy: leftWisp dsp).
		myRight informTo: ((orgl copy: rightWisp externalRegion) unTransformedBy: rightWisp dsp)]"!
*/
}
/**
 * Just search for now.
 */
public XnRegion keysLabelled(BeLabel label) {
	return (myIn.keysLabelled(label)).unionWith((myOut.keysLabelled(label)));
/*
udanax-top.st:8256:SplitLoaf methodsFor: 'operations'!
{XnRegion} keysLabelled: label {BeLabel}
	"Just search for now."
	
	^(myIn keysLabelled: label) unionWith: (myOut keysLabelled: label)!
*/
}
/**
 * Return a region describing the stuff I share with the orgl under trace.
 */
public XnRegion sharedRegion(TracePosition trace, XnRegion limitRegion) {
	if (hCrum().inTrace(trace)) {
		return domain();
	}
	else {
		return (myIn.sharedRegion(trace, (limitRegion.intersect(mySplit)))).unionWith((myOut.sharedRegion(trace, (limitRegion.intersect(mySplit.complement())))));
	}
/*
udanax-top.st:8261:SplitLoaf methodsFor: 'operations'!
{XnRegion} sharedRegion: trace {TracePosition} with: limitRegion {XnRegion}
	"Return a region describing the stuff I share with the orgl under trace."
	(self hCrum inTrace: trace)
		ifTrue: [^self domain]
		ifFalse:  [^(myIn sharedRegion: trace with: (limitRegion intersect: mySplit))
				unionWith: (myOut sharedRegion: trace with: (limitRegion intersect: mySplit complement))]!
*/
}
public void printOn(PrintWriter aStream) {
	if (myIn == null) {
		aStream.print(getAboraClass().name());
		aStream.print("(nil)");
		return ;
	}
	aStream.print("(");
	aStream.print(mySplit);
	aStream.print(", ");
	aStream.print(myIn);
	aStream.print(", ");
	aStream.print(myOut);
	aStream.print(")");
/*
udanax-top.st:8270:SplitLoaf methodsFor: 'printing'!
{void} printOn: aStream {ostream reference} 
	[myIn == nil ifTrue: [aStream << self getCategory name << '(nil)'.
		^VOID]] smalltalkOnly.
	aStream << '(' << mySplit << ', ' << myIn << ', ' << myOut << ')'!
*/
}
public SplitLoaf(XnRegion split, Loaf inLoaf, Loaf outLoaf) {
	super(null, ((SensorCrum) (inLoaf.sensorCrum().computeJoin(outLoaf.sensorCrum()))));
	myIn = inLoaf;
	myOut = outLoaf;
	mySplit = split;
	/* Connect the HTrees. */
	newShepherd();
	myIn.addOParent(this);
	myOut.addOParent(this);
/*
udanax-top.st:8277:SplitLoaf methodsFor: 'create'!
create: split {XnRegion} with: inLoaf {Loaf} with: outLoaf {Loaf}
	super create: NULL 
		with: ((inLoaf sensorCrum computeJoin: outLoaf sensorCrum) cast: SensorCrum).
	myIn _ inLoaf.
	myOut _ outLoaf.
	mySplit _ split.
	"Connect the HTrees."
	self newShepherd.
	myIn addOParent: self.
	myOut addOParent: self.!
*/
}
public SplitLoaf(XnRegion split, Loaf inLoaf, Loaf outLoaf, HUpperCrum hcrum) {
	super(hcrum, ((SensorCrum) (inLoaf.sensorCrum().computeJoin(outLoaf.sensorCrum()))));
	myIn = inLoaf;
	myOut = outLoaf;
	mySplit = split;
	/* Connect the HTrees. */
	newShepherd();
	myIn.addOParent(this);
	myOut.addOParent(this);
/*
udanax-top.st:8288:SplitLoaf methodsFor: 'create'!
create: split {XnRegion} with: inLoaf {Loaf} with: outLoaf {Loaf} with: hcrum {HUpperCrum}
	super create: hcrum
		with: ((inLoaf sensorCrum computeJoin: outLoaf sensorCrum) cast: SensorCrum).
	myIn _ inLoaf.
	myOut _ outLoaf.
	mySplit _ split.
	"Connect the HTrees."
	self newShepherd.
	myIn addOParent: self.
	myOut addOParent: self.!
*/
}
public SplitLoaf(XnRegion split, Loaf inLoaf, Loaf outLoaf, HUpperCrum hcrum, int hash) {
	super(hash, hcrum, ((SensorCrum) (inLoaf.sensorCrum().computeJoin(outLoaf.sensorCrum()))));
	myIn = inLoaf;
	myOut = outLoaf;
	mySplit = split;
	/* Connect the HTrees. */
	newShepherd();
	myIn.addOParent(this);
	myOut.addOParent(this);
/*
udanax-top.st:8299:SplitLoaf methodsFor: 'create'!
create: split {XnRegion} with: inLoaf {Loaf} with: outLoaf {Loaf} with: hcrum {HUpperCrum} with: hash {UInt32}
	super create: hash with: hcrum
		with: ((inLoaf sensorCrum computeJoin: outLoaf sensorCrum) cast: SensorCrum).
	myIn _ inLoaf.
	myOut _ outLoaf.
	mySplit _ split.
	"Connect the HTrees."
	self newShepherd.
	myIn addOParent: self.
	myOut addOParent: self.!
*/
}
/**
 * Special constructor for becoming this class
 */
public SplitLoaf(XnRegion split, Loaf inLoaf, Loaf outLoaf, HUpperCrum hcrum, int hash, FlockInfo info) {
	super(hash, hcrum, ((SensorCrum) (inLoaf.sensorCrum().computeJoin(outLoaf.sensorCrum()))));
	myIn = inLoaf;
	myOut = outLoaf;
	mySplit = split;
	/* Connect the HTrees. */
	flockInfo(info);
	myIn.addOParent(this);
	myOut.addOParent(this);
	diskUpdate();
/*
udanax-top.st:8310:SplitLoaf methodsFor: 'create'!
create: split {XnRegion} with: inLoaf {Loaf} with: outLoaf {Loaf} with: hcrum {HUpperCrum} with: hash {UInt32} with: info {FlockInfo}
	"Special constructor for becoming this class"
	super create: hash with: hcrum
		with: ((inLoaf sensorCrum computeJoin: outLoaf sensorCrum) cast: SensorCrum).
	myIn _ inLoaf.
	myOut _ outLoaf.
	mySplit _ split.
	"Connect the HTrees."
	self flockInfo: info.
	myIn addOParent: self.
	myOut addOParent: self.
	self diskUpdate!
*/
}
/*
udanax-top.st:8326:SplitLoaf methodsFor: 'smalltalk:'!
crums
	^((mySplit respondsTo: #isBoundedAbove) and: [mySplit isBoundedAbove])
		ifTrue: [Array with: myIn with: myOut]
		ifFalse: [Array with: myOut with: myIn]!
*/
public String displayString() {
	return "<" + mySplit.displayString() + ">";
/*
udanax-top.st:8331:SplitLoaf methodsFor: 'smalltalk:'!
displayString
	^'<', mySplit displayString, '>'!
*/
}
/**
 * Return true if child is a child.  Used for debugging.
 */
public boolean testChild(Loaf child) {
	return (myIn.isEqual(child)) || (myOut.isEqual(child));
/*
udanax-top.st:8334:SplitLoaf methodsFor: 'smalltalk:'!
{BooleanVar} testChild: child {Loaf}
	"Return true if child is a child.  Used for debugging."
	
	^(myIn isEqual: child) or: [myOut isEqual: child]!
*/
}
/**
 * Return true if child is a child.  Used for debugging.
 */
public boolean testHChild(HistoryCrum child) {
	return (myIn.hCrum() == child) || (myOut.hCrum() == child);
/*
udanax-top.st:8339:SplitLoaf methodsFor: 'smalltalk:'!
{BooleanVar} testHChild: child {HistoryCrum}
	"Return true if child is a child.  Used for debugging."
	
	^(myIn hCrum == child) or: [myOut hCrum == child]!
*/
}
/**
 * add oparent to the set of upward pointers and update the bertCrums in
 * southern children.
 */
public void addOParent(OPart oparent) {
	BertCrum bCrum;
	BertCrum newBCrum;
	bCrum = hCrum().bertCrum();
	super.addOParent(oparent);
	/* My bertCrum may have been changed by the last operation. */
	newBCrum = hCrum().bertCrum();
	if ( ! (bCrum.isLE(newBCrum))) {
		myIn.updateBCrumTo(newBCrum);
		myOut.updateBCrumTo(newBCrum);
	}
	else {
		if ( ! (newBCrum.isLE(bCrum))) {
			throw new AboraAssertionException("unrelated bertCrums.  Call dean!");
		}
	}
/*
udanax-top.st:8346:SplitLoaf methodsFor: 'backfollow'!
{void} addOParent: oparent {OPart} 
	"add oparent to the set of upward pointers and update the bertCrums in 
	southern children."
	| bCrum {BertCrum} newBCrum {BertCrum} |
	bCrum _ self hCrum bertCrum.
	super addOParent: oparent.
	"My bertCrum may have been changed by the last operation."
	newBCrum _ self hCrum bertCrum.
	(bCrum isLE: newBCrum) not
		ifTrue:  
			[myIn updateBCrumTo: newBCrum.
			myOut updateBCrumTo: newBCrum]
		ifFalse: [(newBCrum isLE: bCrum) assert: 'unrelated bertCrums.  Call dean!!']!
*/
}
public XnRegion attachTrailBlazer(TrailBlazer blazer) {
	return (myIn.attachTrailBlazer(blazer)).unionWith((myOut.attachTrailBlazer(blazer)));
/*
udanax-top.st:8361:SplitLoaf methodsFor: 'backfollow'!
{XnRegion} attachTrailBlazer: blazer {TrailBlazer}
	
	^(myIn attachTrailBlazer: blazer)
		unionWith: (myOut attachTrailBlazer: blazer)!
*/
}
public void checkChildRecorders(PropFinder finder) {
	myIn.checkRecorders(finder, sensorCrum());
	myOut.checkRecorders(finder, sensorCrum());
/*
udanax-top.st:8366:SplitLoaf methodsFor: 'backfollow'!
{void} checkChildRecorders: finder {PropFinder}
	myIn checkRecorders: finder with: self sensorCrum.
	myOut checkRecorders: finder with: self sensorCrum!
*/
}
public void checkTrailBlazer(TrailBlazer blazer) {
	myIn.checkTrailBlazer(blazer);
	myOut.checkTrailBlazer(blazer);
/*
udanax-top.st:8371:SplitLoaf methodsFor: 'backfollow'!
{void} checkTrailBlazer: blazer {TrailBlazer}
	
	myIn checkTrailBlazer: blazer.
	myOut checkTrailBlazer: blazer.!
*/
}
public void delayedStoreMatching(PropFinder finder, RecorderFossil fossil, ResultRecorder recorder, HashSetCache hCrumCache) {
	myIn.delayedStoreMatching(finder, fossil, recorder, hCrumCache);
	myOut.delayedStoreMatching(finder, fossil, recorder, hCrumCache);
/*
udanax-top.st:8376:SplitLoaf methodsFor: 'backfollow'!
{void} delayedStoreMatching: finder {PropFinder} 
	with: fossil {RecorderFossil} 
	with: recorder {ResultRecorder}
	with: hCrumCache {HashSetCache of: HistoryCrum}
	
	myIn delayedStoreMatching: finder with: fossil with: recorder with: hCrumCache.
	myOut delayedStoreMatching: finder with: fossil with: recorder with: hCrumCache!
*/
}
public TrailBlazer fetchTrailBlazer() {
	TrailBlazer result;
	result = myIn.fetchTrailBlazer();
	if (result != null) {
		return result;
	}
	else {
		return myOut.fetchTrailBlazer();
	}
/*
udanax-top.st:8384:SplitLoaf methodsFor: 'backfollow'!
{TrailBlazer | NULL} fetchTrailBlazer
	
	| result {TrailBlazer | NULL} |
	result := myIn fetchTrailBlazer.
	result ~~ NULL
		ifTrue: [^result]
		ifFalse: [^myOut fetchTrailBlazer]!
*/
}
public void storeRecordingAgents(RecorderFossil recorder, Agenda agenda) {
	myIn.storeRecordingAgents(recorder, agenda);
	myOut.storeRecordingAgents(recorder, agenda);
/*
udanax-top.st:8392:SplitLoaf methodsFor: 'backfollow'!
{void} storeRecordingAgents: recorder {RecorderFossil}
	with: agenda {Agenda} 
	
	myIn storeRecordingAgents: recorder with: agenda.
	myOut storeRecordingAgents: recorder with: agenda!
*/
}
public void triggerDetector(FeFillRangeDetector detect) {
	if (sensorCrum().isPartial()) {
		myIn.triggerDetector(detect);
		myOut.triggerDetector(detect);
	}
	else {
		/* there is no partiality below me so I can just trigger it with everything */
		detect.rangeFilled(asFeEdition());
	}
/*
udanax-top.st:8398:SplitLoaf methodsFor: 'backfollow'!
{void} triggerDetector: detect {FeFillRangeDetector}
	
	self sensorCrum isPartial ifTrue:
		[myIn triggerDetector: detect.
		myOut triggerDetector: detect]
	ifFalse:
		["there is no partiality below me so I can just trigger it with everything"
		detect rangeFilled: self asFeEdition]!
*/
}
/**
 * My bertCrum must not be leafward of newBCrum.
 * Thus it must be LE to newCrum. Otherwise correct it and recur.
 */
public boolean updateBCrumTo(BertCrum newBCrum) {
	if (super.updateBCrumTo(newBCrum)) {
		myIn.updateBCrumTo(newBCrum);
		myOut.updateBCrumTo(newBCrum);
		return true;
	}
	return false;
/*
udanax-top.st:8407:SplitLoaf methodsFor: 'backfollow'!
{BooleanVar} updateBCrumTo: newBCrum {BertCrum} 
	"My bertCrum must not be leafward of newBCrum. 
	Thus it must be LE to newCrum. Otherwise correct it and recur."
	(super updateBCrumTo: newBCrum)
		ifTrue: 
			[myIn updateBCrumTo: newBCrum.
			myOut updateBCrumTo: newBCrum.
			^true].
	^false!
*/
}
/**
 * Make each child completely contained or completely outside
 * the region.  Return the number of children completely in the region.
 * The transformation table follows:
 * #   in 	    out 	  return 	operation		rearrange
 * 1|	 0		0		0		none			none
 * 2|	 0		1		1		swap #4		(A (B* C)) -> (B* (A C))
 * 3|	 0		2		1		swap #7		(A B*) -> (B* A)
 * 4|	 1		0		1		rotateRight		((A* B) C) -> (A* (B C))
 * 5|	 1		1		1		interleave		((A* B) (C* D)) -> ((A* C*) (B D))
 * 6|	 1		2		1		swap #8		((A* B) C*) -> ((A* C*) B)
 * 7|	 2		0		1		none			none
 * 8|	 2		1		1		rotateLeft		(A* (B* C)) -> ((A* B*) C)
 * 9|	 2		2		2		none			none
 */
public int actualSplay(XnRegion region, XnRegion limitRegion) {
	int in;
	int out;
	/* For each child, compute the number of grandchildren completely contained in region. */
	in = myIn.splay(region, (mySplit.intersect(limitRegion)));
	out = myOut.splay(region, (mySplit.complement().intersect(limitRegion)));
	AboraBlockSupport.enterConsistent(19);
	try {
		/* Swap the out and in sides if necessary to reduce the number of cases. */
		if (out > in) {
			int cnt;
			cnt = out;
			out = in;
			in = cnt;
			swapChildren();
		}
		/* The hard cases are when a child is partially contained (in or out = 1).  For those
		 cases, construct the two new children, then install them. */
		if (in == 1 || (out == 1)) {
			Loaf newIn;
			Loaf newOut;
			if (out == 0) {
				newIn = ((InnerLoaf) myIn).inPart();
				newOut = makeNew(((InnerLoaf) myIn).outPart(), myOut);
			}
			else {
				if (in == 2) {
					newIn = makeNew(myIn, ((InnerLoaf) myOut).inPart());
					newOut = ((InnerLoaf) myOut).outPart();
				}
				else {
					newIn = makeNew(((InnerLoaf) myIn).inPart(), ((InnerLoaf) myOut).inPart());
					newOut = makeNew(((InnerLoaf) myIn).outPart(), ((InnerLoaf) myOut).outPart());
				}
			}
			/* The splayed region represents the newDistinction for me in the split cases. */
			install(newIn, newOut, region);
			return 1;
		}
		else {
			/* The non-rotating cases:
					^in==0 ifTrue: [0] ifFalse: [ out==0 ifTrue: [1] ifFalse: [2] ] */
			/* The 1 case here should change mySplit to the incoming one. */
			return (in + out) / 2;
		}
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:8420:SplitLoaf methodsFor: 'protected: splay'!
{Int8} actualSplay: region {XnRegion} with: limitRegion {XnRegion}
	"Make each child completely contained or completely outside
	 the region.  Return the number of children completely in the region. 
	 The transformation table follows:
 #   in 	    out 	  return 	operation		rearrange
1|	 0		0		0		none			none
2|	 0		1		1		swap #4		(A (B* C)) -> (B* (A C))
3|	 0		2		1		swap #7		(A B*) -> (B* A)
4|	 1		0		1		rotateRight		((A* B) C) -> (A* (B C))
5|	 1		1		1		interleave		((A* B) (C* D)) -> ((A* C*) (B D))
6|	 1		2		1		swap #8		((A* B) C*) -> ((A* C*) B)
7|	 2		0		1		none			none
8|	 2		1		1		rotateLeft		(A* (B* C)) -> ((A* B*) C)
9|	 2		2		2		none			none"
	| in {UInt8} out {UInt8} |
	"For each child, compute the number of grandchildren completely contained in region."
	in _ myIn splay: region with: (mySplit intersect: limitRegion).
	out _ myOut splay: region with: (mySplit complement intersect: limitRegion).
	
	DiskManager consistent: 19 with:
		["Swap the out and in sides if necessary to reduce the number of cases."
		out > in ifTrue: 
			[| cnt {UInt8} | 
			cnt _ out.  out _ in.  in _ cnt.
			self swapChildren].
		
		"The hard cases are when a child is partially contained (in or out = 1).  For those
		 cases, construct the two new children, then install them."
		(in == 1 or: [out == 1])
			ifTrue: 
				[| newIn {Loaf} newOut {Loaf} |
				out == Int0 ifTrue:
					[newIn _ (myIn cast: InnerLoaf) inPart.
					newOut _ self makeNew: (myIn cast: InnerLoaf) outPart with: myOut]
				ifFalse: [in == 2 ifTrue:
					[newIn _ self makeNew: myIn with: (myOut cast: InnerLoaf) inPart.
					newOut _ (myOut cast: InnerLoaf) outPart]
				ifFalse: 
					[newIn _ self makeNew: (myIn cast: InnerLoaf) inPart with: (myOut cast: InnerLoaf) inPart.
					newOut _ self makeNew: (myIn cast: InnerLoaf) outPart with: (myOut cast: InnerLoaf) outPart]].
				"The splayed region represents the newDistinction for me in the split cases."
				self install: newIn with: newOut with: region.
				^1]
			ifFalse:
				["The non-rotating cases:
					^in==0 ifTrue: [0] ifFalse: [ out==0 ifTrue: [1] ifFalse: [2] ]"
				"The 1 case here should change mySplit to the incoming one."
				^in + out // 2]]!
*/
}
/**
 * Install new in and out children at the same
 * time. This will need to be in a critical section.  Add me as
 * parent to the new loaves first in case the only ent reference
 * to the new loaf is through one of my children (which might
 * delete it if I'm *their* last reference).
 */
public void install(Loaf newIn, Loaf newOut, XnRegion newSplit) {
	newIn.addOParent(this);
	newOut.addOParent(this);
	myIn.removeOParent(this);
	myIn = newIn;
	myOut.removeOParent(this);
	myOut = newOut;
	mySplit = newSplit;
	Someone.thingToDo();
	/* This shouldn't update the disk if the swapChildren already did. */
	diskUpdate();
/*
udanax-top.st:8472:SplitLoaf methodsFor: 'private: splay'!
{void} install: newIn {Loaf} with: newOut {Loaf} with: newSplit {XnRegion}
	"Install new in and out children at the same 
	 time. This will need to be in a critical section.  Add me as
	 parent to the new loaves first in case the only ent reference
	 to the new loaf is through one of my children (which might 
	 delete it if I'm *their* last reference)."
	newIn addOParent: self.
	newOut addOParent: self.
	myIn removeOParent: self.
	myIn _ newIn.
	myOut removeOParent: self.
	myOut _ newOut.
	mySplit _ newSplit.
	self thingToDo.  "This shouldn't update the disk if the swapChildren already did."
	self diskUpdate!
*/
}
/**
 * Make a new crum to replace some existing crums during a splay
 * operation. The new crum must have the same trace as me to
 * guarantee the hTree property. Optimization: look at parents of the
 * new loaves to find a pre-existing parent with the same trace and
 * wisps. This will coalesce the shearing that splaying causes.
 */
public Loaf makeNew(Loaf newIn, Loaf newOut) {
	/* The new loaf is made from pieces of me, so they are distinguished by my split. */
	return InnerLoaf.make(mySplit, newIn, newOut, (HUpperCrum.make(((HUpperCrum) hCrum()))));
/*
udanax-top.st:8489:SplitLoaf methodsFor: 'private: splay'!
{Loaf} makeNew: newIn {Loaf} with: newOut {Loaf} 
	"Make a new crum to replace some existing crums during a splay 
	operation. The new crum must have the same trace as me to 
	guarantee the hTree property. Optimization: look at parents of the 
	new loaves to find a pre-existing parent with the same trace and 
	wisps. This will coalesce the shearing that splaying causes."
	"The new loaf is made from pieces of me, so they are distinguished by my split."
	^InnerLoaf
		make: mySplit
		with: newIn
		with: newOut
		with: (HUpperCrum make: (self hCrum cast: HUpperCrum))!
*/
}
/**
 * This is a support for the splay routine. Swapping the children
 * reduces the number of cases. This way, if this crum is partially
 * in a region being splayed, the part contained in the region
 * resides in the left slot.
 */
public void swapChildren() {
	Loaf loaf;
	mySplit = mySplit.complement();
	loaf = myIn;
	myIn = myOut;
	myOut = loaf;
	Someone.thingToDo();
	/* Swapping may be expensive if it's
			unnecessary.  Check more cases in the splay routine. */
	diskUpdate();
/*
udanax-top.st:8503:SplitLoaf methodsFor: 'private: splay'!
{void} swapChildren
	"This is a support for the splay routine. Swapping the children 
	reduces the number of cases. This way, if this crum is partially 
	in a region being splayed, the part contained in the region 
	resides in the left slot."
	| loaf {Loaf} |
	mySplit _ mySplit complement.
	loaf _ myIn.
	myIn _ myOut.
	myOut _ loaf.
	self thingToDo.   "Swapping may be expensive if it's
			unnecessary.  Check more cases in the splay routine."
	self diskUpdate!
*/
}
public void dismantle() {
	AboraBlockSupport.enterConsistent(4);
	try {
		if (Heaper.isConstructed(myIn)) {
			myIn.removeOParent(this);
		}
		if (Heaper.isConstructed(myOut)) {
			myOut.removeOParent(this);
		}
		super.dismantle();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:8520:SplitLoaf methodsFor: 'protected: delete'!
{void} dismantle
	DiskManager consistent: 4 with:
		[(Heaper isConstructed: myIn) ifTrue: [myIn removeOParent: self].
		(Heaper isConstructed: myOut) ifTrue: [myOut removeOParent: self].
		super dismantle]!
*/
}
public int contentsHash() {
	return ((super.contentsHash() ^ mySplit.hashForEqual()) ^ myIn.hashForEqual()) ^ myOut.hashForEqual();
/*
udanax-top.st:8528:SplitLoaf methodsFor: 'testing'!
{UInt32} contentsHash
	^((super contentsHash
		bitXor: mySplit hashForEqual)
		bitXor: myIn hashForEqual)
		bitXor: myOut hashForEqual!
*/
}
/**
 * @deprecated
 */
public void wait(XnSensor sensor) {
	throw new PasseException();
/*
udanax-top.st:8537:SplitLoaf methodsFor: 'smalltalk: passe'!
{void} wait: sensor {XnSensor}
	
	self passe!
*/
}
public SplitLoaf(Rcvr receiver) {
	super(receiver);
	mySplit = (XnRegion) receiver.receiveHeaper();
	myIn = (Loaf) receiver.receiveHeaper();
	myOut = (Loaf) receiver.receiveHeaper();
/*
udanax-top.st:8543:SplitLoaf methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	mySplit _ receiver receiveHeaper.
	myIn _ receiver receiveHeaper.
	myOut _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(mySplit);
	xmtr.sendHeaper(myIn);
	xmtr.sendHeaper(myOut);
/*
udanax-top.st:8549:SplitLoaf methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: mySplit.
	xmtr sendHeaper: myIn.
	xmtr sendHeaper: myOut.!
*/
}
public SplitLoaf() {
/*

Generated during transformation
*/
}
}
