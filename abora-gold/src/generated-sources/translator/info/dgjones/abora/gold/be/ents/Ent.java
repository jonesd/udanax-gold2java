/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.ents;

import info.dgjones.abora.gold.be.basic.BeGrandMap;
import info.dgjones.abora.gold.be.canopy.BertCrum;
import info.dgjones.abora.gold.be.ents.Ent;
import info.dgjones.abora.gold.be.ents.OrglRoot;
import info.dgjones.abora.gold.collection.tables.MuTable;
import info.dgjones.abora.gold.collection.tables.Pair;
import info.dgjones.abora.gold.collection.tables.ScruTable;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.smalltalk.RootHandle;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.unordered.HeaperAsPosition;
import info.dgjones.abora.gold.spaces.unordered.HeaperSpace;
import info.dgjones.abora.gold.traces.DagWood;
import info.dgjones.abora.gold.traces.TracePosition;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

public class Ent extends Abraham {

	protected MuTable oroots;
	protected DagWood fulltrace;
/*
udanax-top.st:6091:
Abraham subclass: #Ent
	instanceVariableNames: '
		oroots {MuTable NOCOPY smalltalk of: TracePosition and: OrglRoot}
		fulltrace {DagWood}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Ents'!
*/
/*
udanax-top.st:6097:
(Ent getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:6236:
Ent class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:6239:
(Ent getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Ent.class).setAttributes( new Set().add("LOCKED").add("COPY").add("SHEPHERDPATRIARCH").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public TracePosition newTrace() {
	return fulltrace.newPosition();
/*
udanax-top.st:6102:Ent methodsFor: 'orgl creation'!
{TracePosition} newTrace
	^fulltrace newPosition!
*/
}
public Ent() {
	super();
	oroots = MuTable.make(HeaperSpace.make());
	fulltrace = new DagWood();
	newShepherd();
	remember();
/*
udanax-top.st:6107:Ent methodsFor: 'instance creation'!
create
	super create.
	[oroots _ MuTable make: HeaperSpace make] smalltalkOnly.
	fulltrace _ DagWood create.
	self newShepherd.
	self remember!
*/
}
/*
udanax-top.st:6116:Ent methodsFor: 'smalltalk:'!
inspect
	Sensor leftShiftDown ifTrue: [self basicInspect]
		ifFalse: [self inspectFrom: fulltrace root]!
*/
/*
udanax-top.st:6120:Ent methodsFor: 'smalltalk:'!
inspectFrom: tracePos 
	| seen trace |
	seen _ Set new.
	EntView openOn: (TreeBarnacle new
			buildOn: (self makeHandleFor: tracePos)
			gettingChildren: 
				[:handle | 
				trace _ handle tracePos.
				(seen includes: trace)
					ifTrue: [OrderedCollection new]
					ifFalse: [seen add: trace.
							trace successors asOrderedCollection collect: [:tp | self makeHandleFor: tp]]]
			gettingImage: [:handle | handle displayString asDisplayText]
			at: 0 @ 0
			vertical: false
			separation: 10 @ 10)!
*/
/**
 * oroots at: (HeaperAsPosition make: root hCut) store: root
 */
public void installORoot(OrglRoot root) {
/*
udanax-top.st:6137:Ent methodsFor: 'smalltalk:'!
{void} installORoot: root {OrglRoot}
	"oroots at: (HeaperAsPosition make: root hCut) store: root"!
*/
}
/**
 * These traceHandles are to hold a place in the ent inspection view.
 * They are not used for ent behavior at all!!
 */
public RootHandle makeHandleFor(Object tracePos) {
	return RootHandle.tracePosEnt(tracePos, this);
/*
udanax-top.st:6140:Ent methodsFor: 'smalltalk:'!
makeHandleFor: tracePos 
	"These traceHandles are to hold a place in the ent inspection view.  
	They are not used for ent behavior at all!!"
	^RootHandle tracePos: tracePos ent: self!
*/
}
public OrglRoot oRootAt(TracePosition tpos) {
	return (OrglRoot) (oroots.fetch((HeaperAsPosition.make(tpos))));
/*
udanax-top.st:6146:Ent methodsFor: 'smalltalk:'!
{OrglRoot} oRootAt: tpos {TracePosition}
	^(oroots fetch: (HeaperAsPosition make: tpos)) cast: OrglRoot!
*/
}
public int contentsHash() {
	return super.contentsHash() ^ fulltrace.hashForEqual();
/*
udanax-top.st:6151:Ent methodsFor: 'testing'!
{UInt32} contentsHash
	^super contentsHash
		bitXor: fulltrace hashForEqual!
*/
}
/**
 * compute the join of the existing traces and bert crums in the table
 * @deprecated
 */
public Pair mapJoin(ScruTable table, BeGrandMap gm) {
	throw new PasseException();
/*
udanax-top.st:6158:Ent methodsFor: 'smalltalk: passe'!
{Pair of: TracePosition and: BertCrum} mapJoin: table {ScruTable of: (ID | ActualOrgl | IObject | PackOBits)} with: gm {BeGrandMap}
	"compute the join of the existing traces and bert crums in the table"
	"make new ones if there are none"
	self passe.
"	| n {IntegerVar} trace {TracePosition} crum {BertCrum} |
	[HistoryCrum] USES.
	n _ IntegerVar0.
	(table isKindOf: XnWordArray) ifFalse:
		[table stepper forEach: [ :each {Heaper} | | hroot {HRoot} |
			hroot _ NULL.
			(each isKindOf: ID) ifTrue:
				[hroot _ gm fetchIDHRoot: (each quickCast: ID)]
			ifFalse: [(each isKindOf: ActualOrgl) ifTrue:
				[hroot _ (each quickCast: ActualOrgl) stamp fetchHRoot]
			ifFalse: [(each isKindOf: IObject) ifTrue:
				[hroot _ (each quickCast: IObject) fetchHRoot]]].
			hroot ~~ NULL ifTrue:
				[ | newtrace {TracePosition} newcrum {BertCrum} |
				newtrace _ hroot hCrum hCut.
				newcrum _ hroot hCrum bertCrum.
				n = IntegerVar0 ifTrue:
					[trace _ newtrace.
					crum _ newcrum]
				ifFalse:
					[trace _ trace newSuccessorAfter: newtrace.
					crum _ (crum computeJoin: newcrum) cast: BertCrum].
				n _ n + 1]]].
	n = IntegerVar0 ifTrue: [^Pair make: fulltrace newPosition with: BertCrum make].
	n = 1 ifTrue: [^Pair make: trace newSuccessor with: crum].
	^Pair make: trace with: crum"!
*/
}
/**
 * map the elements in the table to just HRoots
 * @deprecated
 */
public ScruTable mapTable(ScruTable table, BeGrandMap gm) {
	throw new PasseException();
/*
udanax-top.st:6189:Ent methodsFor: 'smalltalk: passe'!
{ScruTable of: HRoot} mapTable: table {ScruTable of: (ID | ActualOrgl | IObject | PackOBits)} with: gm {BeGrandMap}
	"map the elements in the table to just HRoots"
	self passe.
	
"	| result {MuTable} stepper {TableStepper} |
self passe.	(table isKindOf: XnWordArray) ifTrue:
		[^ table].
	result _ MuTable make: table coordinateSpace.
	(stepper _ table stepper) forEach: [ :value {Heaper} |
		DiskManager consistent: 11 with:
			[result at: stepper key store: (gm getOrMakeHRoot: value)]].
	^ result"!
*/
}
/**
 * compute the join of the existing traces and bert crums in the table
 * @deprecated
 */
public OrglRoot newOrglRoot(ScruTable table, BeGrandMap gm) {
	throw new PasseException();
/*
udanax-top.st:6202:Ent methodsFor: 'smalltalk: passe'!
{OrglRoot} newOrglRoot: table {ScruTable of: FeRangeElement} with: gm {BeGrandMap}
	
	"compute the join of the existing traces and bert crums in the table"
	"make new ones if there are none"
	self passe.!
*/
}
/**
 * create a new partial orgl root on a region
 * @deprecated
 */
public OrglRoot newPartialOrglRoot(XnRegion region) {
	throw new PasseException();
/*
udanax-top.st:6208:Ent methodsFor: 'smalltalk: passe'!
{OrglRoot} newPartialOrglRoot: region {XnRegion} 
	"create a new partial orgl root on a region"
	
	self passe.
	CurrentTrace
		fluidBind: fulltrace newPosition
		during: 
			[| newCrum {BertCrum} |
			newCrum _ BertCrum create.
			CurrentBertCrum
				fluidBind: newCrum
				during: 
					[| newRoot {OrglRoot} |
					newRoot _ OrglRoot make.Region: region.
					"oroots at: (HeaperAsPosition make: newRoot hCut) introduce: newRoot."
					^newRoot]]!
*/
}
public Ent(Rcvr receiver) {
	super(receiver);
	fulltrace = (DagWood) receiver.receiveHeaper();
/*
udanax-top.st:6227:Ent methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	fulltrace _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(fulltrace);
/*
udanax-top.st:6231:Ent methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: fulltrace.!
*/
}
public static Ent make() {
	return new Ent();
/*
udanax-top.st:6244:Ent class methodsFor: 'instance creation'!
{Ent} make
	^ Ent create!
*/
}
public static void staticTimeNonInherited() {
	AboraSupport.defineFluid(TracePosition.class, "CurrentTrace", DiskManager.emulsion(), null);
	AboraSupport.defineFluid(BertCrum.class, "CurrentBertCrum", DiskManager.emulsion(), null);
/*
udanax-top.st:6249:Ent class methodsFor: 'smalltalk: initialization'!
staticTimeNonInherited
	TracePosition defineFluid: #CurrentTrace with: DiskManager emulsion with: [NULL].
	BertCrum defineFluid: #CurrentBertCrum with: DiskManager emulsion with: [NULL].!
*/
}
/**
 * When we are making an orgl out of a table, we break the table up into pieces which should
 * be no larger than this, so that they each fit into a snarf.
 */
public static int tableSegmentMaxSize() {
	return 16384;
/*
udanax-top.st:6256:Ent class methodsFor: 'magic numbers'!
{IntegerVar INLINE} tableSegmentMaxSize
	"When we are making an orgl out of a table, we break the table up into pieces which should be no larger than this, so that they each fit into a snarf."
	^16384!
*/
}
}
