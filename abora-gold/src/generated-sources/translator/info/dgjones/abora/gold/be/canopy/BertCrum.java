/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.canopy;

import info.dgjones.abora.gold.be.canopy.BertCrum;
import info.dgjones.abora.gold.be.canopy.CanopyCache;
import info.dgjones.abora.gold.be.canopy.CanopyCrum;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.props.PropChange;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.spaces.cross.CrossRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import java.io.PrintWriter;

/**
 * This implementation tracks the endorsement information with
 * a strictly binary tree.  The tree gets heuristically balanced
 * upon insertion of new elements in such a way that the ocrums
 * pointing at a particular canopyCrum need not be updated.
 * Therefore we should not bother storing backpointers.  I''m
 * doing so currently in case we change algorithms.
 * Deletion may require backpointers to eliminate joins
 * with the deleted crums.
 */
public class BertCrum extends CanopyCrum {

/*
udanax-top.st:5114:
CanopyCrum subclass: #BertCrum
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Canopy'!
*/
/*
udanax-top.st:5118:
BertCrum comment:
'This implementation tracks the endorsement information with 
a strictly binary tree.  The tree gets heuristically balanced 
upon insertion of new elements in such a way that the ocrums 
pointing at a particular canopyCrum need not be updated.  
Therefore we should not bother storing backpointers.  I''m 
doing so currently in case we change algorithms.
Deletion may require backpointers to eliminate joins 
with the deleted crums.'!
*/
/*
udanax-top.st:5128:
(BertCrum getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:5231:
BertCrum class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:5234:
(BertCrum getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BertCrum.class).setAttributes( new Set().add("LOCKED").add("COPY").add("SHEPHERDPATRIARCH").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Make a canopyCrum for a root:  it has no children.
 */
public BertCrum() {
	super(0);
	newShepherd();
/*
udanax-top.st:5133:BertCrum methodsFor: 'private: creation'!
create
	"Make a canopyCrum for a root:  it has no children."
	super create: UInt32Zero.
	self newShepherd!
*/
}
/**
 * should have one per Ent
 */
public CanopyCache canopyCache() {
	return ((CanopyCache) CurrentBertCanopyCache.fluidGet());
/*
udanax-top.st:5141:BertCrum methodsFor: 'protected:'!
{CanopyCache wimpy} canopyCache
	"should have one per Ent"
	^CurrentBertCanopyCache fluidGet!
*/
}
public CanopyCrum makeNew() {
	return new BertCrum();
/*
udanax-top.st:5145:BertCrum methodsFor: 'protected:'!
{CanopyCrum} makeNew
	^BertCrum create!
*/
}
/**
 * BertCrum create verify2.
 */
public CanopyCrum another() {
	return new BertCrum();
/*
udanax-top.st:5150:BertCrum methodsFor: 'smalltalk:'!
{CanopyCrum} another
	"BertCrum create verify2."
	^BertCrum create!
*/
}
/*
udanax-top.st:5155:BertCrum methodsFor: 'smalltalk:'!
inspectHCrums
	| owners |
	owners _ self allOwners select: [ :each | each isKindOf: HistoryCrum].
	owners isEmpty ifTrue:
		[Transcript show: 'Nobody'; cr]
	ifFalse: [owners size = 1 ifTrue:
		[owners first inspect]
	ifFalse:
		[owners inspect]]!
*/
/*
udanax-top.st:5165:BertCrum methodsFor: 'smalltalk:'!
inspectMenuArray
	^#(
		('inspect hcrums'	inspectHCrums	'') )!
*/
public void printOn(PrintWriter aStream) {
	aStream.print(getAboraClass().name());
	aStream.print("(");
	aStream.print(children().size());
	aStream.print(")");
	/* child1 = NULL
		ifTrue: [aStream << (self flags printStringRadix: 2)]
		ifFalse: 
			[aStream nextPut: $(;
				 print: child1;
				 nextPut: $,;
				 print: child2;
				 nextPut: $)] */
/*
udanax-top.st:5169:BertCrum methodsFor: 'smalltalk:'!
printOn: aStream 
	aStream << self getCategory name << '(' << self children size << ')'.
	"child1 = NULL
		ifTrue: [aStream << (self flags printStringRadix: 2)]
		ifFalse: 
			[aStream nextPut: $(;
				 print: child1;
				 nextPut: $,;
				 print: child2;
				 nextPut: $)]"!
*/
}
public void showOn(PrintWriter oo) {
	oo.print(maxHeight());
	if ( ! (maxHeight() == minHeight())) {
		oo.print('-');
		oo.print(minHeight());
	}
	oo.print((Integer.toString(flags(), 2)));
/*
udanax-top.st:5180:BertCrum methodsFor: 'smalltalk:'!
showOn: oo
	oo print: self maxHeight.
	self maxHeight == self minHeight 
		ifFalse: [oo nextPut: $-; print: self minHeight].
	oo print: (self flags printStringRadix: 2)!
*/
}
public CanopyCrum makeNewParent(CanopyCrum first, CanopyCrum second) {
	AboraBlockSupport.enterConsistent(3);
	try {
		return new BertCrum(((BertCrum) first), ((BertCrum) second));
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:5188:BertCrum methodsFor: 'protected'!
{CanopyCrum} makeNewParent: first {CanopyCrum} with: second {CanopyCrum}
	DiskManager consistent: 3 with:
		[^BertCrum create: (first cast: BertCrum)
			with: (second cast: BertCrum)]!
*/
}
/**
 * Create a new parent for two BertCrums.
 * My client must bring my properties up to date.  This constructor just makes a new parent
 * whose properties are empty
 */
public BertCrum(BertCrum first, BertCrum second) {
	super(0, first, second);
	/* Have the super do the basic creation. */
	newShepherd();
	canopyCache().updateCacheForParent(fetchChild1(), this);
	canopyCache().updateCacheForParent(fetchChild2(), this);
/*
udanax-top.st:5196:BertCrum methodsFor: 'instance creation'!
create: first {BertCrum} with: second {BertCrum} 
	"Create a new parent for two BertCrums.
	My client must bring my properties up to date.  This constructor just makes a new parent whose properties are empty"
	| |
	"Have the super do the basic creation."
	super create: UInt32Zero with: first with: second.
	self newShepherd.
	self canopyCache updateCache: self fetchChild1 forParent: self.
	self canopyCache updateCache: self fetchChild2 forParent: self!
*/
}
public PropChange fullChange() {
	return PropChange.bertPropChange();
/*
udanax-top.st:5209:BertCrum methodsFor: 'smalltalk: suspended'!
{PropChange} fullChange
	^PropChange bertPropChange!
*/
}
public boolean isNotPartializable() {
	return (flags() & BertCrum.isNotPartializableFlag()) != 0;
/*
udanax-top.st:5214:BertCrum methodsFor: 'accessing'!
{BooleanVar} isNotPartializable
	^(self flags bitAnd: BertCrum isNotPartializableFlag) ~= UInt32Zero!
*/
}
public boolean isSensorWaiting() {
	return (flags() & BertCrum.isSensorWaitingFlag()) != 0;
/*
udanax-top.st:5218:BertCrum methodsFor: 'accessing'!
{BooleanVar} isSensorWaiting
	^(self flags bitAnd: BertCrum isSensorWaitingFlag) ~= UInt32Zero!
*/
}
public BertCrum(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:5224:BertCrum methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:5227:BertCrum methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public static void staticTimeNonInherited() {
	AboraSupport.defineFluid(CanopyCache.class, "CurrentBertCanopyCache", DiskManager.emulsion(), CanopyCache.make());
/*
udanax-top.st:5239:BertCrum class methodsFor: 'smalltalk: initialization'!
staticTimeNonInherited
	
	CanopyCache defineFluid: #CurrentBertCanopyCache with: DiskManager emulsion with: [CanopyCache make]!
*/
}
public static BertCrum make() {
	AboraBlockSupport.enterConsistent(1);
	try {
		return new BertCrum();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:5245:BertCrum class methodsFor: 'instance creation'!
make
	DiskManager consistent: 1 with: [
		^BertCrum create]!
*/
}
/**
 * The flag word corresponding to the given props
 */
public static int flagsFor(IDRegion permissions, CrossRegion endorsements, boolean isNotPartializable, boolean isSensorWaiting) {
	int result;
	result = 0;
	if (permissions != null) {
		result = result | (CanopyCrum.permissionsFlags(permissions));
	}
	if (endorsements != null) {
		result = result | (CanopyCrum.endorsementsFlags(endorsements));
	}
	if (isNotPartializable) {
		result = result | isNotPartializableFlag();
	}
	if (isSensorWaiting) {
		result = result | isSensorWaitingFlag();
	}
	return result;
/*
udanax-top.st:5251:BertCrum class methodsFor: 'flags'!
{UInt32} flagsFor: permissions {IDRegion | NULL}
	with: endorsements {CrossRegion | NULL}
	with: isNotPartializable {BooleanVar}
	with: isSensorWaiting {BooleanVar}
	"The flag word corresponding to the given props"
	
	| result {UInt32} |
	result := UInt32Zero.
	permissions ~~ NULL ifTrue:
		[result := result bitOr: (CanopyCrum permissionsFlags: permissions)].
	endorsements ~~ NULL ifTrue:
		[result := result bitOr: (CanopyCrum endorsementsFlags: endorsements)].
	isNotPartializable ifTrue:
		[result := result bitOr: self isNotPartializableFlag].
	isSensorWaiting ifTrue:
		[result := result bitOr: self isSensorWaitingFlag].
	^result!
*/
}
/**
 * Flag bit for active Editions
 */
public static int isNotPartializableFlag() {
	return 0x8000000;
/*
udanax-top.st:5269:BertCrum class methodsFor: 'flags'!
{UInt32 constFn} isNotPartializableFlag
	"Flag bit for active Editions" 
	^16r08000000!
*/
}
/**
 * Flag bit for active Editions
 */
public static int isSensorWaitingFlag() {
	return 0x4000000;
/*
udanax-top.st:5273:BertCrum class methodsFor: 'flags'!
{UInt32 constFn} isSensorWaitingFlag
	"Flag bit for active Editions" 
	^16r04000000!
*/
}
}
