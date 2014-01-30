/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.ents;

import info.dgjones.abora.gold.backrec.ResultRecorder;
import info.dgjones.abora.gold.be.canopy.BertCrum;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.ents.HistoryCrum;
import info.dgjones.abora.gold.collection.cache.HashSetCache;
import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.collection.tables.MuTable;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.spaces.basic.Mapping;
import info.dgjones.abora.gold.traces.TracePosition;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * invariant:  the parent''s trace >= the child''s trace
 * The subclasses should differentiate between the number
 * of children:  0, 1, or more.  ORoots have 0 children and
 * always have a canopyCrum.  HCrums for OCrums in the
 * body of the ent have one child if they are at the top
 * of an unshared subtreee, and more if they are at the top
 * of a shared subtree.  HCrums with more than one child
 * almost always have a canopyCrum to represent the join
 * between the canopies of their multiple hchildren.
 * The change would make the updateH method return a
 * new crum, which the oCrums would install.
 * They don''t do so now because I''m not sure if a crum with
 * no parents can appear in the middle of the ent.  If so, then
 * the version compare operations would gag.  Hmmm.  The
 * change doesn''t make any difference for that....
 */
public class HistoryCrum extends Heaper {

	protected int myHash;
	protected static int SequenceNumber;
/*
udanax-top.st:27103:
Heaper subclass: #HistoryCrum
	instanceVariableNames: 'myHash {UInt32}'
	classVariableNames: 'SequenceNumber {UInt32} '
	poolDictionaries: ''
	category: 'Xanadu-Be-Ents'!
*/
/*
udanax-top.st:27107:
HistoryCrum comment:
'invariant:  the parent''s trace >= the child''s trace
The subclasses should differentiate between the number 
of children:  0, 1, or more.  ORoots have 0 children and 
always have a canopyCrum.  HCrums for OCrums in the 
body of the ent have one child if they are at the top 
of an unshared subtreee, and more if they are at the top 
of a shared subtree.  HCrums with more than one child 
almost always have a canopyCrum to represent the join 
between the canopies of their multiple hchildren.
The change would make the updateH method return a 
new crum, which the oCrums would install.
They don''t do so now because I''m not sure if a crum with 
no parents can appear in the middle of the ent.  If so, then 
the version compare operations would gag.  Hmmm.  The 
change doesn''t make any difference for that....'!
*/
/*
udanax-top.st:27126:
(HistoryCrum getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #COPY; yourself)!
*/
/*
udanax-top.st:27292:
HistoryCrum class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:27295:
(HistoryCrum getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(HistoryCrum.class).setAttributes( new Set().add("DEFERRED").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public String displayString() {
	return hCut().printString();
/*
udanax-top.st:27131:HistoryCrum methodsFor: 'smalltalk:'!
displayString
	self hCut printString!
*/
}
/*
udanax-top.st:27134:HistoryCrum methodsFor: 'smalltalk:'!
inspect
	Sensor leftShiftDown
		ifTrue: [self basicInspect]
		ifFalse: 
			[EntView openOn: (TreeBarnacle new
			buildOn: self
			gettingChildren: [:htree | htree oParents asOrderedCollection collect: [:hc | hc hCrum]]
			gettingImage: [:htree | htree printString asDisplayText]
			at: 0 @ 0
			vertical: true
			separation: 5 @ 10)]!
*/
/*
udanax-top.st:27146:HistoryCrum methodsFor: 'smalltalk:'!
inspectCanopy
	self bertCrum inspect!
*/
/*
udanax-top.st:27149:HistoryCrum methodsFor: 'smalltalk:'!
inspectMenuArray
	^#(
		('inspect orgls'	inspectOrgls	'')
		('bert canopy'	inspectCanopy	''))!
*/
/*
udanax-top.st:27154:HistoryCrum methodsFor: 'smalltalk:'!
inspectOrgls
	self subclassResponsibility!
*/
public void printOn(PrintWriter aStream) {
	aStream.print(getAboraClass().name());
	aStream.print("(");
	aStream.print(hCut());
	aStream.print(")");
/*
udanax-top.st:27157:HistoryCrum methodsFor: 'smalltalk:'!
{void} printOn: aStream {ostream reference}
	aStream << self getCategory name << '(' << self hCut << ')'!
*/
}
public void showOn(PrintWriter oo) {
	oo.print(hCut());
	oo.print(", ");
	oo.print(asOop());
/*
udanax-top.st:27160:HistoryCrum methodsFor: 'smalltalk:'!
showOn: oo
	oo << self hCut << ', ' << self asOop!
*/
}
/**
 * See comment in HistoryCrum>>delayedStoreBackfollow:with:with:
 */
public void actualDelayedStoreBackfollow(PropFinder finder, RecorderFossil fossil, ResultRecorder recorder, HashSetCache hCrumCache) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:27165:HistoryCrum methodsFor: 'deferred filtering'!
{void} actualDelayedStoreBackfollow: finder {PropFinder} 
	with: fossil {RecorderFossil}
	with: recorder {ResultRecorder}
	with: hCrumCache {HashSetCache of: HistoryCrum}
	
	"See comment in HistoryCrum>>delayedStoreBackfollow:with:with:"
	
	self subclassResponsibility!
*/
}
public boolean anyPasses(PropFinder finder) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:27174:HistoryCrum methodsFor: 'deferred filtering'!
{BooleanVar} anyPasses: finder {PropFinder}
	self subclassResponsibility!
*/
}
/**
 * These objects must have a crum in the bert canopy.
 */
public BertCrum bertCrum() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:27177:HistoryCrum methodsFor: 'deferred filtering'!
{BertCrum} bertCrum
	"These objects must have a crum in the bert canopy."
	self subclassResponsibility!
*/
}
/**
 * Do the northward H-tree walk for the 'now' part of a backfollow.
 */
public void delayedStoreBackfollow(PropFinder finder, RecorderFossil fossil, ResultRecorder recorder, HashSetCache hCrumCache) {
	/* Check cache, call polymorphic actualDelayedStoreBackfollow if miss. */
	if ( ! (hCrumCache.hasMember(this))) {
		hCrumCache.store(this);
		actualDelayedStoreBackfollow(finder, fossil, recorder, hCrumCache);
	}
/*
udanax-top.st:27184:HistoryCrum methodsFor: 'filtering'!
{void} delayedStoreBackfollow: finder {PropFinder} 
	with: fossil {RecorderFossil}
	with: recorder {ResultRecorder}
	with: hCrumCache {HashSetCache of: HistoryCrum}
	
	"Do the northward H-tree walk for the 'now' part of a backfollow."
	
	||
	
	"Check cache, call polymorphic actualDelayedStoreBackfollow if miss."
	
	(hCrumCache hasMember: self) not ifTrue: 
		[hCrumCache store: self.
		self actualDelayedStoreBackfollow: finder
			with: fossil
			with: recorder
			with: hCrumCache]!
*/
}
/**
 * Ring all the detectors north of me with the given Edition as argument
 */
public void ringDetectors(FeEdition edition) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:27202:HistoryCrum methodsFor: 'filtering'!
{void} ringDetectors: edition {FeEdition}
	"Ring all the detectors north of me with the given Edition as argument"
	
	self subclassResponsibility!
*/
}
public int actualHashForEqual() {
	return myHash;
/*
udanax-top.st:27209:HistoryCrum methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^myHash!
*/
}
/**
 * Return true if their are no upward pointers.  This is used
 * by OParts to determine if they can be forgotten.
 */
public boolean isEmpty() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:27212:HistoryCrum methodsFor: 'testing'!
{BooleanVar} isEmpty
	"Return true if their are no upward pointers.  This is used
	 by OParts to determine if they can be forgotten."
	
	self subclassResponsibility!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:27218:HistoryCrum methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	^self == other!
*/
}
public HistoryCrum() {
	super();
	myHash = HistoryCrum.nextHistoryCrumSequenceNumber();
/*
udanax-top.st:27223:HistoryCrum methodsFor: 'create'!
create	
	super create.
	myHash _ HistoryCrum nextHistoryCrumSequenceNumber.!
*/
}
/**
 * Return true if the receiver can backfollow to trace.
 */
public boolean inTrace(TracePosition trace) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:27229:HistoryCrum methodsFor: 'deferred testing'!
{Boolean} inTrace: trace {TracePosition}
	"Return true if the receiver can backfollow to trace."
	self subclassResponsibility!
*/
}
public TracePosition hCut() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:27236:HistoryCrum methodsFor: 'deferred accessing'!
{TracePosition} hCut
	self subclassResponsibility!
*/
}
/**
 * return the mapping into the domain space of the given trace
 */
public Mapping mappingTo(TracePosition trace, Mapping initial) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:27239:HistoryCrum methodsFor: 'deferred accessing'!
{Mapping} mappingTo: trace {TracePosition} with: initial {Mapping}
	"return the mapping into the domain space of the given trace"
	self subclassResponsibility!
*/
}
public ImmuSet oParents() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:27243:HistoryCrum methodsFor: 'deferred accessing'!
{ImmuSet of: OPart} oParents
	self subclassResponsibility!
*/
}
/**
 * If bertCrum is leafward of newBCrum then change it and return true,
 * otherwise return false.
 */
public boolean propagateBCrum(BertCrum newBCrum) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:27248:HistoryCrum methodsFor: 'deferred updating'!
{Boolean} propagateBCrum: newBCrum {BertCrum} 
	"If bertCrum is leafward of newBCrum then change it and return true, 
	otherwise return false."
	self subclassResponsibility!
*/
}
/**
 * @deprecated
 */
public void actualDelayedStoreBackfollow(PropFinder finder, RecorderFossil recorder, HashSetCache hCrumCache) {
	throw new PasseException();
/*
udanax-top.st:27255:HistoryCrum methodsFor: 'smalltalk: passe'!
{void} actualDelayedStoreBackfollow: finder {PropFinder} 
	with: recorder {RecorderFossil}
	with: hCrumCache {HashSetCache of: HistoryCrum}
	self passe "extra argument"!
*/
}
/**
 * @deprecated
 */
public void actualStoreBackfollow(PropFinder finder, MuTable table, HashSetCache hCrumCache) {
	throw new PasseException();
/*
udanax-top.st:27261:HistoryCrum methodsFor: 'smalltalk: passe'!
{void} actualStoreBackfollow: finder {PropFinder} 
	with: table {MuTable of: ID and: BeEdition} 
	with: hCrumCache {HashSetCache of: HistoryCrum}
	self passe!
*/
}
/**
 * @deprecated
 */
public void delayedStoreBackfollow(PropFinder finder, RecorderFossil recorder, HashSetCache hCrumCache) {
	throw new PasseException();
/*
udanax-top.st:27267:HistoryCrum methodsFor: 'smalltalk: passe'!
{void} delayedStoreBackfollow: finder {PropFinder} 
	with: recorder {RecorderFossil}
	with: hCrumCache {HashSetCache of: HistoryCrum}
	
	self passe "extra argument"!
*/
}
/**
 * @deprecated
 */
public ImmuSet hCrums() {
	throw new PasseException();
/*
udanax-top.st:27273:HistoryCrum methodsFor: 'smalltalk: passe'!
{ImmuSet of: OPart} hCrums
	self passe. "use oParents"!
*/
}
/**
 * @deprecated
 */
public void storeBackfollow(PropFinder finder, MuTable table, HashSetCache hCrumCache) {
	throw new PasseException();
/*
udanax-top.st:27276:HistoryCrum methodsFor: 'smalltalk: passe'!
{void} storeBackfollow: finder {PropFinder} 
	with: table {MuTable of: ID and: BeEdition} 
	with: hCrumCache {HashSetCache of: HistoryCrum}
	self passe!
*/
}
public HistoryCrum(Rcvr receiver) {
	super(receiver);
	myHash = receiver.receiveUInt32();
/*
udanax-top.st:27283:HistoryCrum methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myHash _ receiver receiveUInt32.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendUInt32(myHash);
/*
udanax-top.st:27287:HistoryCrum methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendUInt32: myHash.!
*/
}
public static void linkTimeNonInherited() {
	SequenceNumber = 0;
/*
udanax-top.st:27300:HistoryCrum class methodsFor: 'smalltalk: initialization'!
linkTimeNonInherited
	SequenceNumber _ UInt32Zero!
*/
}
/**
 * Shepherds use a sequence number for their hash.  Return the next one
 * and increment.  This should actually do spread the hashes.
 */
public static int nextHistoryCrumSequenceNumber() {
	/* This actually needs to roll over the UInt32 limit. */
	SequenceNumber = SequenceNumber + 1 & 134217727
	/* 2^27-1 */
	;
	return SequenceNumber;
/*
udanax-top.st:27305:HistoryCrum class methodsFor: 'accessing'!
{UInt32} nextHistoryCrumSequenceNumber
	"Shepherds use a sequence number for their hash.  Return the next one
	 and increment.  This should actually do spread the hashes."
	"This actually needs to roll over the UInt32 limit."
	 
	 SequenceNumber _ SequenceNumber + 1 bitAnd: 134217727 "2^27-1".
	 ^SequenceNumber!
*/
}
}
