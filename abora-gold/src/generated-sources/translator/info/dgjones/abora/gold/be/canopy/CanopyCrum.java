/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.canopy;

import info.dgjones.abora.gold.be.basic.BeGrandMap;
import info.dgjones.abora.gold.be.canopy.CanopyCache;
import info.dgjones.abora.gold.be.canopy.CanopyCrum;
import info.dgjones.abora.gold.be.canopy.prop.Prop;
import info.dgjones.abora.gold.canopy.Heaper2UInt32Cache;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.tables.Pair;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Array;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.props.PropChange;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.cross.CrossRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.turtle.AgendaItem;
import info.dgjones.abora.gold.turtle.PropChanger;
import info.dgjones.abora.gold.turtle.Sequencer;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;
import java.io.StringWriter;

/**
 * CanopyCrums form binary trees that acrete in a balanced fashion.  No rebalancing ever
 * happens.  Things are simply added to the tree up to the point thta the tree is balanced,
 * then the height of the tree gets extended at the root.
 * Essentially, when the join of two trees is asked for, if the two trees aren''t already
 * parts of a larger tree, the algorithm attempts to find a place in one tree into which the
 * other tree could completely fit without violating the depth constraint on the tree.  It
 * then returns the nearest root that contains both trees.  If it can''t put one tree into
 * the other, then it makes a new node that joins the two trees (probably with room to add
 * other stuff deeper down).
 * myRefCount is only the count of Loafs or HCrums that point at the CanopyCrum.  It doesn''t
 * include other CanopyCrums.
 * 12/2/92 Ravi
 * PropJoints have been suspended, and their function has been replaced by flag words in the
 * CanopyCrum. Any interesting Club or endorsement gets a bit, and there is a bit for "any
 * other Club" and "any other endorsement". Any criteria not given a bit of their own require
 * an exhaustive search. These flags are widded by ORing up the canopy. When we start using
 * more sophisticated hashing strategies, we will probably need to reanimate PropJoints.
 */
public class CanopyCrum extends Abraham {

	protected CanopyCrum child1;
	protected CanopyCrum child2;
	protected CanopyCrum parent;
	protected int minH;
	protected int maxH;
	protected int myOwnFlags;
	protected int myFlags;
	protected int myRefCount;
	protected static PtrArray FlagEndorsements;
	protected static IDRegion OtherClubs;
	protected static CrossRegion OtherEndorsements;
	protected static Heaper2UInt32Cache TheEFlagsCache;
	protected static Heaper2UInt32Cache ThePFlagsCache;
/*
udanax-top.st:4503:
Abraham subclass: #CanopyCrum
	instanceVariableNames: '
		child1 {CanopyCrum | NULL}
		child2 {CanopyCrum | NULL}
		parent {CanopyCrum | NULL}
		minH {IntegerVar}
		maxH {IntegerVar}
		myOwnFlags {UInt32}
		myFlags {UInt32}
		myRefCount {IntegerVar}'
	classVariableNames: '
		FlagEndorsements {PtrArray of: Position | XnRegion} 
		OtherClubs {IDRegion} 
		OtherEndorsements {CrossRegion} 
		TheEFlagsCache {Heaper2UInt32Cache} 
		ThePFlagsCache {Heaper2UInt32Cache} '
	poolDictionaries: ''
	category: 'Xanadu-Be-Canopy'!
*/
/*
udanax-top.st:4520:
CanopyCrum comment:
'CanopyCrums form binary trees that acrete in a balanced fashion.  No rebalancing ever happens.  Things are simply added to the tree up to the point thta the tree is balanced, then the height of the tree gets extended at the root.
Essentially, when the join of two trees is asked for, if the two trees aren''t already parts of a larger tree, the algorithm attempts to find a place in one tree into which the other tree could completely fit without violating the depth constraint on the tree.  It then returns the nearest root that contains both trees.  If it can''t put one tree into the other, then it makes a new node that joins the two trees (probably with room to add other stuff deeper down).
myRefCount is only the count of Loafs or HCrums that point at the CanopyCrum.  It doesn''t include other CanopyCrums.
12/2/92 Ravi
PropJoints have been suspended, and their function has been replaced by flag words in the CanopyCrum. Any interesting Club or endorsement gets a bit, and there is a bit for "any other Club" and "any other endorsement". Any criteria not given a bit of their own require an exhaustive search. These flags are widded by ORing up the canopy. When we start using more sophisticated hashing strategies, we will probably need to reanimate PropJoints.'!
*/
/*
udanax-top.st:4529:
(CanopyCrum getOrMakeCxxClassDescription)
	friends:
'friend class RecorderHoister;
';
	attributes: ((Set new) add: #DEFERRED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #DEFERRED.LOCKED; yourself)!
*/
/*
udanax-top.st:5001:
CanopyCrum class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:5004:
(CanopyCrum getOrMakeCxxClassDescription)
	friends:
'friend class RecorderHoister;
';
	attributes: ((Set new) add: #DEFERRED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #DEFERRED.LOCKED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(CanopyCrum.class).setAttributes( new Set().add("DEFERRED").add("COPY").add("SHEPHERDPATRIARCH").add("DEFERREDLOCKED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Find a canopyCrum that is an anscestor to
 * both the receiver and otherBCrum. otherBCrum
 * is added to the canopy in a pseudo-balanced fashion.
 * This demonstrates the beauty and power of caching
 * in object-oriented systems.
 */
public CanopyCrum computeJoin(CanopyCrum otherBCrum) {
	MuSet otherPath;
	CanopyCrum myRoot;
	CanopyCrum otherRoot;
	CanopyCache cache;
	if (isLE(otherBCrum)) {
		return this;
	}
	cache = canopyCache();
	otherPath = cache.pathFor(otherBCrum);
	otherRoot = cache.rootFor(otherBCrum);
	if (otherBCrum.isLE(this)) {
		return otherBCrum;
	}
	Stepper stomper = otherPath.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		CanopyCrum bCrum = (CanopyCrum) stomper.fetch();
		if (bCrum == null) {
			continue ;
		}
		if (bCrum.isLE(this)) {
			return bCrum;
		}
	}
	stomper.destroy();
	myRoot = cache.rootFor(this);
	if (myRoot.maxHeight() > otherRoot.maxHeight()) {
		return makeJoin(otherRoot);
	}
	else {
		return otherBCrum.makeJoin(myRoot);
	}
/*
udanax-top.st:4538:CanopyCrum methodsFor: 'canopy operations'!
{CanopyCrum} computeJoin: otherBCrum {CanopyCrum} 
	"Find a canopyCrum that is an anscestor to 
	 both the receiver and otherBCrum. otherBCrum 
	 is added to the canopy in a pseudo-balanced fashion. 
	 This demonstrates the beauty and power of caching
	 in object-oriented systems."
	| otherPath {MuSet of: CanopyCrum} myRoot {CanopyCrum} otherRoot {CanopyCrum} cache {CanopyCache} |
	(self isLE: otherBCrum) ifTrue: [^self].
	cache _ self canopyCache.
	otherPath _ cache pathFor: otherBCrum.
	otherRoot _ cache rootFor: otherBCrum.
	(otherBCrum isLE: self) ifTrue: [^otherBCrum].
	otherPath stepper forEach: 
		[:bCrum {CanopyCrum} | 
		(bCrum isLE: self) ifTrue: [^bCrum]].
	myRoot _ cache rootFor: self.
	myRoot maxHeight > otherRoot maxHeight
		ifTrue: [^self makeJoin: otherRoot]
		ifFalse: [^otherBCrum makeJoin: myRoot]!
*/
}
/**
 * split into two if possible, return the two leaves
 */
public Pair expand() {
	if (child1 != null && (child2 != null)) {
		return Pair.make(this, this);
	}
	if ( ! (child1 == null && (child2 == null))) {
		throw new AboraAssertionException("Must be both or niether");
	}
	AboraBlockSupport.enterConsistent(3);
	try {
		(child1 = makeNew()).setParent(this);
		(child2 = makeNew()).setParent(this);
		canopyCache().updateCacheForParent(child1, this);
		canopyCache().updateCacheForParent(child2, this);
		diskUpdate();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
	return Pair.make(child1, child2);
/*
udanax-top.st:4559:CanopyCrum methodsFor: 'canopy operations'!
{Pair of: CanopyCrum} expand
	"split into two if possible, return the two leaves"
	
	(child1 ~~ NULL and: [child2 ~~ NULL])
		ifTrue: [^Pair make: self with: self].
	(child1 == NULL and: [child2 == NULL]) assert: 'Must be both or niether'.
	DiskManager consistent: 3 with:
		[(child1 _ self makeNew) setParent: self.
		(child2 _ self makeNew) setParent: self.
		self canopyCache updateCache: child1 forParent: self.
		self canopyCache updateCache: child2 forParent: self.
		self diskUpdate].
	^Pair make: child1 with: child2!
*/
}
/**
 * Install otherCanopy at or below the receiver. If the otherCanopy fits in a lower branch,
 * put it there. Otherwise, replace the shortest child with a new child that contains the
 * shortest child and otherCanopy.
 */
public void includeCanopy(CanopyCrum otherCanopy) {
	/* This should be a friend or private function or something. */
	Someone.thingToDo();
	/* Propagate the children's props into their new parent */
	Someone.thingToDo();
	if ( ! (
	/* When we have non-props to propagate, do those, too.  i.e., height is currently handle by changeCanopy and will be moved out to HeightChanger momentarily. */
	child1 != null)) {
		throw new AboraAssertionException("shouldnt get here.");
	}
	if (child1.heightDiff() >= otherCanopy.maxHeight()) {
		child1.includeCanopy(otherCanopy);
	}
	else {
		if (child2.heightDiff() >= otherCanopy.maxHeight()) {
			child2.includeCanopy(otherCanopy);
		}
		else {
			AboraBlockSupport.enterConsistent();
			try {
				if (child1.maxHeight() > child2.maxHeight()) {
					(child2 = makeNewParent(child2, otherCanopy)).setParent(this);
				}
				else {
					(child1 = makeNewParent(child1, otherCanopy)).setParent(this);
				}
				/* Update the cache for the newly installed subTree
							 because of the new tree above it. */
				canopyCache().updateCacheFor(otherCanopy);
				(Sequencer.make((PropChanger.height(this)), (PropChanger.make(this)))).schedule();
			}
			finally {
				AboraBlockSupport.exitConsistent();
			}
		}
	}
/*
udanax-top.st:4573:CanopyCrum methodsFor: 'canopy operations'!
{void} includeCanopy: otherCanopy {CanopyCrum} 
	"Install otherCanopy at or below the receiver. If the otherCanopy fits in a lower branch, put it there. Otherwise, replace the shortest child with a new child that contains the shortest child and otherCanopy."
	"This should be a friend or private function or something."
	
	| |
	self thingToDo.
	"Propagate the children's props into their new parent"
	
	self thingToDo.	"When we have non-props to propagate, do those, too.  i.e., height is currently handle by changeCanopy and will be moved out to HeightChanger momentarily."
	child1 ~~ NULL assert: 'shouldnt get here.'.
	child1 heightDiff >= otherCanopy maxHeight
		ifTrue: [child1 includeCanopy: otherCanopy]
		ifFalse: [child2 heightDiff >= otherCanopy maxHeight
				ifTrue: [child2 includeCanopy: otherCanopy]
				ifFalse: 
					[DiskManager consistent:
						[child1 maxHeight > child2 maxHeight
							ifTrue: 
								[(child2 _ self makeNewParent: child2 with: otherCanopy) setParent: self]
							ifFalse: 
								[(child1 _ self makeNewParent: child1 with: otherCanopy) setParent: self].
						"Update the cache for the newly installed subTree
							 because of the new tree above it."
						self canopyCache updateCacheFor: otherCanopy.
						(Sequencer make: (PropChanger height: self)
							with: (PropChanger make: self)) schedule]]]!
*/
}
/**
 * Return true if other is equal to the receiver
 * or an anscestor (through the parent links).
 * Use caches for efficiency.
 */
public boolean isLE(CanopyCrum other) {
	return (canopyCache().pathFor(other)).hasMember(this);
/*
udanax-top.st:4602:CanopyCrum methodsFor: 'canopy operations'!
{Boolean} isLE: other {CanopyCrum}
	"Return true if other is equal to the receiver
	 or an anscestor (through the parent links). 
	 Use caches for efficiency."
	^(self canopyCache pathFor: other) hasMember: self!
*/
}
/**
 * Keep a refcount of diskful pointers to myself for disk space management.  (Maybe
 * backpointers later.)
 */
public void addPointer(Heaper ignored) {
	myRefCount = myRefCount + 1;
	if (myRefCount == 1) {
		remember();
	}
	diskUpdate();
/*
udanax-top.st:4611:CanopyCrum methodsFor: 'canopy accessing'!
{void} addPointer: ignored {Heaper unused}
	"Keep a refcount of diskful pointers to myself for disk space management.  (Maybe backpointers later.)"
	
	myRefCount _ myRefCount + 1.
	myRefCount == 1 ifTrue: [self remember].
	self diskUpdate!
*/
}
public CanopyCrum fetchParent() {
	return parent;
/*
udanax-top.st:4619:CanopyCrum methodsFor: 'canopy accessing'!
{CanopyCrum} fetchParent
	^parent!
*/
}
public int flags() {
	return myFlags;
/*
udanax-top.st:4622:CanopyCrum methodsFor: 'canopy accessing'!
{UInt32} flags
	^myFlags!
*/
}
public int heightDiff() {
	return maxH - minH;
/*
udanax-top.st:4625:CanopyCrum methodsFor: 'canopy accessing'!
{IntegerVar} heightDiff
	^maxH - minH!
*/
}
public boolean isLeaf() {
	return child1 == null && (child2 == null);
/*
udanax-top.st:4628:CanopyCrum methodsFor: 'canopy accessing'!
{BooleanVar} isLeaf
	^child1 == NULL and: [child2 == NULL]!
*/
}
public int maxHeight() {
	return maxH;
/*
udanax-top.st:4631:CanopyCrum methodsFor: 'canopy accessing'!
{IntegerVar}maxHeight
	^maxH!
*/
}
public int minHeight() {
	return minH;
/*
udanax-top.st:4634:CanopyCrum methodsFor: 'canopy accessing'!
{IntegerVar}minHeight
	^minH!
*/
}
/**
 * Keep a refcount of diskful pointers to myself for disk space management.  (Maybe
 * backpointers later.)
 * Forget the object if it goes to zero.
 */
public void removePointer(Heaper ignored) {
	Someone.thingToDo();
	/* Is calling destroy a bug? */
	myRefCount = myRefCount - 1;
	MarkM.knownBug();
	/* refCunt going to 0 with an outstanding AgendaItem. */
	/* (myRefCount == IntegerVar0 and: [parent == NULL])
		ifTrue: [self forget; destroy]
		ifFalse: [ */
	diskUpdate();
/*
udanax-top.st:4637:CanopyCrum methodsFor: 'canopy accessing'!
{void} removePointer: ignored {Heaper unused} 
	"Keep a refcount of diskful pointers to myself for disk space management.  (Maybe backpointers later.)
	 Forget the object if it goes to zero."
	
	self thingToDo.		"Is calling destroy a bug?"
	myRefCount _ myRefCount - 1.
	MarkM knownBug.   "refCunt going to 0 with an outstanding AgendaItem."
	"(myRefCount == IntegerVar0 and: [parent == NULL])
		ifTrue: [self forget; destroy]
		ifFalse: ["self diskUpdate!
*/
}
public void setParent(CanopyCrum p) {
	if (parent == null && (p != null)) {
		remember();
	}
	parent = p;
	if (myRefCount == 0 && (parent == null)) {
		destroy();
	}
	else {
		diskUpdate();
	}
/*
udanax-top.st:4649:CanopyCrum methodsFor: 'canopy accessing'!
{void} setParent: p {CanopyCrum | NULL} 
	(parent == NULL and: [p ~~ NULL])
		ifTrue: [self remember].
	parent _ p.
	(myRefCount == IntegerVar0 and: [parent == NULL])
		ifTrue: [self destroy]
		ifFalse: [self diskUpdate]!
*/
}
public CanopyCache canopyCache() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:4659:CanopyCrum methodsFor: 'protected:'!
{CanopyCache wimpy} canopyCache
	self subclassResponsibility!
*/
}
public void dismantle() {
	if ( ! (parent == null)) {
		throw new AboraAssertionException("We can only dismantle the canopy from the root on up.");
	}
	Someone.thingToDo();
	/* This first needs to remove all of myOwnProps from the canopy. */
	AboraBlockSupport.enterConsistent(3);
	try {
		if (child1 != null) {
			child1.setParent(null);
			child1 = null;
		}
		if (child2 != null) {
			child2.setParent(null);
			child2 = null;
		}
		super.dismantle();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:4662:CanopyCrum methodsFor: 'protected:'!
{void} dismantle
	parent == NULL assert: 'We can only dismantle the canopy from the root on up.'.
	self thingToDo.
	"This first needs to remove all of myOwnProps from the canopy."
	DiskManager consistent: 3 with: 
		[child1 ~~ NULL ifTrue: 
			[child1 setParent: NULL.
			child1 _ NULL].
		child2 ~~ NULL ifTrue: 
			[child2 setParent: NULL.
			child2 _ NULL].
		super dismantle]!
*/
}
public CanopyCrum fetchChild1() {
	return child1;
/*
udanax-top.st:4675:CanopyCrum methodsFor: 'protected:'!
{CanopyCrum} fetchChild1
	^child1!
*/
}
public CanopyCrum fetchChild2() {
	return child2;
/*
udanax-top.st:4678:CanopyCrum methodsFor: 'protected:'!
{CanopyCrum} fetchChild2
	^child2!
*/
}
public CanopyCrum makeNew() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:4681:CanopyCrum methodsFor: 'protected:'!
{CanopyCrum} makeNew
	self subclassResponsibility!
*/
}
public int ownFlags() {
	return myOwnFlags;
/*
udanax-top.st:4684:CanopyCrum methodsFor: 'protected:'!
{UInt32} ownFlags
	^myOwnFlags!
*/
}
public void setOwnFlags(int newFlags) {
	myOwnFlags = newFlags;
/*
udanax-top.st:4687:CanopyCrum methodsFor: 'protected:'!
{void} setOwnFlags: newFlags {UInt32}
	myOwnFlags _ newFlags.!
*/
}
/**
 * Make a canopyCrum for a root:  it has no children.
 */
public CanopyCrum(int flags) {
	super();
	minH = maxH = 1;
	child1 = child2 = parent = null;
	myOwnFlags = flags;
	myFlags = myOwnFlags;
	myRefCount = 0;
/*
udanax-top.st:4693:CanopyCrum methodsFor: 'create'!
create: flags {UInt32}
	"Make a canopyCrum for a root:  it has no children."
	super create.
	minH _ maxH _ 1.
	child1 _ child2 _ parent _ NULL.
	myOwnFlags _ flags.
	myFlags _ myOwnFlags.
	myRefCount _ IntegerVar0!
*/
}
/**
 * prop must be empty
 */
public CanopyCrum(int flags, CanopyCrum first, CanopyCrum second) {
	super();
	/* prop isEmpty assert: 'Must be empty'. */
	minH = maxH = 1;
	child1 = first;
	child1.setParent(this);
	child2 = second;
	child2.setParent(this);
	parent = null;
	myOwnFlags = flags;
	myFlags = (flags | child1.flags()) | child2.flags();
	myRefCount = 0;
/*
udanax-top.st:4702:CanopyCrum methodsFor: 'create'!
create: flags {UInt32} with: first {CanopyCrum} with: second {CanopyCrum}
	"prop must be empty"
	
	super create.
	"prop isEmpty assert: 'Must be empty'."
	minH _ maxH _ 1.
	child1 _ first.
	child1 setParent: self.
	child2 _ second.
	child2 setParent: self.
	parent _ NULL.
	myOwnFlags _ flags.
	myFlags _ (flags bitOr: child1 flags) bitOr: child2 flags.
	myRefCount _ IntegerVar0!
*/
}
/**
 * Return another instance of the same
 * class for testing purposes.
 */
public CanopyCrum another() {
	return new CanopyCrum();
/*
udanax-top.st:4720:CanopyCrum methodsFor: 'smalltalk: verification'!
{CanopyCrum} another
	"Return another instance of the same
	 class for testing purposes."
	^CanopyCrum create!
*/
}
public int refCount() {
	return myRefCount;
/*
udanax-top.st:4726:CanopyCrum methodsFor: 'smalltalk: verification'!
{IntegerVar} refCount
	^myRefCount!
*/
}
/**
 * BertCrum create verify1
 */
public CanopyCrum verify1() {
	for (int i = 0; i < 50; i ++ ) {
		computeJoin(another());
	}
	return this;
/*
udanax-top.st:4729:CanopyCrum methodsFor: 'smalltalk: verification'!
{CanopyCrum} verify1
	"BertCrum create verify1"
	50 timesRepeat: [self computeJoin: self another].
	^self!
*/
}
/**
 * BertCrum create verify2.
 */
public CanopyCrum verify2() {
	verifyHeight(5);
	computeJoin((another().verifyHeight(3)));
	return this;
/*
udanax-top.st:4735:CanopyCrum methodsFor: 'smalltalk: verification'!
{CanopyCrum} verify2
	"BertCrum create verify2."
	self verifyHeight: 5.
	self computeJoin: (self another verifyHeight: 3).
	^self!
*/
}
/**
 * Create a tree with maxHeight = height and minHeight = 2.
 */
public CanopyCrum verifyHeight(int height) {
	for (int i = 0; i < 
	/* BertCrum create verifyHeight: 4. */
	(AboraSupport.pow(2, height - 2)); i ++ ) {
		computeJoin(another());
	}
	return this;
/*
udanax-top.st:4742:CanopyCrum methodsFor: 'smalltalk: verification'!
{CanopyCrum} verifyHeight: height {IntegerVar}
	"Create a tree with maxHeight = height and minHeight = 2."
	"BertCrum create verifyHeight: 4."
	(2 raisedTo: height - 2) timesRepeat: [self computeJoin: self another].
	^self!
*/
}
public Array childArray() {
	return (child1 == null) ? new Array() : (child2 == null) ? Array.with(child1) : Array.with(child1, child2);
/*
udanax-top.st:4751:CanopyCrum methodsFor: 'smalltalk:'!
{Array of: CanopyCrum} childArray
	^child1 == NULL
		ifTrue: [#()]
		ifFalse: [child2 == NULL
				ifTrue: [Array with: child1]
				ifFalse: [Array with: child1 with: child2]]!
*/
}
public Array children() {
	return (child1 == null) ? new Array() : (child2 == null) ? Array.with(child1) : Array.with(child1, child2);
/*
udanax-top.st:4758:CanopyCrum methodsFor: 'smalltalk:'!
{Array of: CanopyCrum} children
	^child1 == NULL
		ifTrue: [#()]
		ifFalse: [child2 == NULL
				ifTrue: [Array with: child1]
				ifFalse: [Array with: child1 with: child2]]!
*/
}
public String displayString() {
	StringWriter stringWriter = new StringWriter();
	PrintWriter aStream = new PrintWriter(stringWriter);
	aStream.print(maxH);
	if ( ! (maxH == minH)) {
		aStream.print('-');
		aStream.print(minH);
	}
	return stringWriter.toString();
/*
udanax-top.st:4765:CanopyCrum methodsFor: 'smalltalk:'!
displayString
	^String
		streamContents: 
			[:aStream | 
			aStream print: maxH.
			maxH = minH ifFalse: [aStream nextPut: $-; print: minH]]!
*/
}
/*
udanax-top.st:4772:CanopyCrum methodsFor: 'smalltalk:'!
inspect
	Sensor leftShiftDown
		ifTrue: [self basicInspect]
		ifFalse: 
			[| cur {CanopyCrum} |
			cur _ self.
			[cur fetchParent == NULL]
				whileFalse: [cur _ cur fetchParent].
			cur inspectSubCanopy: self]!
*/
/*
udanax-top.st:4782:CanopyCrum methodsFor: 'smalltalk:'!
inspectSubCanopy: start 
	EntView openOn: (TreeBarnacle new
			buildOn: self
			gettingChildren: [:crum | crum childArray]
			gettingImage: [:crum | crum = start
					ifTrue: [crum displayString asText allBold asDisplayText]
					ifFalse: [crum displayString asDisplayText]]
			at: 0 @ 0
			vertical: true
			separation: 5 @ 10)!
*/
/**
 * Return an AgendaItem to propagate properties.
 * NOTE: The AgendaItem returned is not yet scheduled.  Doing so is up to my caller.
 */
public AgendaItem propChanger(PropChange change, Prop prop) {
	/* Atomically
		Update myOwnFlags but not myFlags (The latter includes the widded stuff)
		return a PropChanger which at each step will update myPropJoint and move to parent. */
	AboraBlockSupport.enterInsistent(3);
	try {
		myOwnFlags = myOwnFlags | prop.flags();
		diskUpdate();
		return PropChanger.make(this);
	}
	finally {
		AboraBlockSupport.exitInsistent();
	}
/*
udanax-top.st:4795:CanopyCrum methodsFor: 'props'!
{AgendaItem} propChanger: change {PropChange unused} with: prop {Prop}
	"Return an AgendaItem to propagate properties.
	
	NOTE: The AgendaItem returned is not yet scheduled.  Doing so is up to my caller."
	
	| |
	"Atomically
		Update myOwnFlags but not myFlags (The latter includes the widded stuff)
		return a PropChanger which at each step will update myPropJoint and move to parent."
	
	DiskManager insistent: 3 with:
		[myOwnFlags _ myOwnFlags bitOr: prop flags.
		self diskUpdate.
		^PropChanger make: self]!
*/
}
/**
 * This is only used by the TestPacker, so it includes all persistent state whether or not
 * it is semantically interesting--myRefCount is not semantically interesting.
 */
public int contentsHash() {
	return (((((((super.contentsHash() ^ child1.hashForEqual()) ^ child2.hashForEqual()) ^ parent.hashForEqual()) ^ (IntegerPos.integerHash(minH))) ^ (IntegerPos.integerHash(maxH))) ^ myFlags) ^ myOwnFlags) ^ (IntegerPos.integerHash(myRefCount));
/*
udanax-top.st:4813:CanopyCrum methodsFor: 'testing'!
{UInt32} contentsHash
	"This is only used by the TestPacker, so it includes all persistent state whether or not
	 it is semantically interesting--myRefCount is not semantically interesting."
	^(((((((super contentsHash
		bitXor: child1 hashForEqual)
		bitXor: child2 hashForEqual)
		bitXor: parent hashForEqual)
		bitXor: (IntegerPos integerHash: minH))
		bitXor: (IntegerPos integerHash: maxH))
		bitXor: myFlags)
		bitXor: myOwnFlags)
		bitXor: (IntegerPos integerHash: myRefCount)!
*/
}
/**
 * Figure out new props, etc. Return true if any changes may require further propagation
 */
public boolean changeCanopy() {
	/* At least one subclass adds behavior here by overriding and calling 'super changeCanopy:' */
	boolean result;
	/* If this is a leaf
		If any of my properties are changed
			Store the modification of the props.
	else
		save current flags
		recalculate the flags from myOwnFlags and the flags of the children
	If anything changed
		flag that the change must be written to disk
	return whether anything changed (which requires propagation rootward) */
	if (isLeaf()) {
		result = myFlags != myOwnFlags;
		myFlags = myOwnFlags;
	}
	else {
		int before;
		before = myFlags;
		myFlags = (myOwnFlags | child1.flags()) | child2.flags();
		result = before != myFlags;
	}
	if (result) {
		diskUpdate();
	}
	return result;
/*
udanax-top.st:4828:CanopyCrum methodsFor: 'protected'!
{BooleanVar} changeCanopy
	"Figure out new props, etc. Return true if any changes may require further propagation"
	"At least one subclass adds behavior here by overriding and calling 'super changeCanopy:'"
	
	| result {BooleanVar} |
	
	"If this is a leaf
		If any of my properties are changed
			Store the modification of the props.
	else
		save current flags
		recalculate the flags from myOwnFlags and the flags of the children
	If anything changed
		flag that the change must be written to disk
	return whether anything changed (which requires propagation rootward)"
	
	self isLeaf ifTrue:
		[result := myFlags ~= myOwnFlags.
		myFlags := myOwnFlags]
	ifFalse:
		[ | before {UInt32} |
		before := myFlags.
		myFlags := (myOwnFlags bitOr: child1 flags) bitOr: child2 flags.
		result := before ~= myFlags].
	result ifTrue:
		[self diskUpdate].
	^result!
*/
}
/**
 * Figure out new height. Return true if changes may require further propagation
 */
public boolean changeHeight() {
	int oldMin;
	int oldMax;
	/* If this is a leaf then it cannot have changed
	otherwise,
	recalculate the heights from the heights of the children
	If anything changed
		flag that the change must be written to disk
	return whether anything changed (which requires propagation rootward) */
	if (isLeaf()) {
		return false;
	}
	oldMin = minH;
	oldMax = maxH;
	if (child1.minHeight() > child2.minHeight()) {
		minH = child2.minHeight() + 1;
	}
	else {
		minH = child1.minHeight() + 1;
	}
	if (child1.maxHeight() > child2.maxHeight()) {
		maxH = child1.maxHeight() + 1;
	}
	else {
		maxH = child2.maxHeight() + 1;
	}
	if (oldMin != minH || (oldMax != maxH)) {
		diskUpdate();
		return true;
	}
	else {
		return false;
	}
/*
udanax-top.st:4856:CanopyCrum methodsFor: 'protected'!
{BooleanVar} changeHeight
	"Figure out new height. Return true if changes may require further propagation"
	
	| oldMin {IntegerVar} oldMax {IntegerVar} |
	
	"If this is a leaf then it cannot have changed
	otherwise,
	recalculate the heights from the heights of the children
	If anything changed
		flag that the change must be written to disk
	return whether anything changed (which requires propagation rootward)"
	
	self isLeaf ifTrue: [^false].
	oldMin := minH.
	oldMax := maxH.
	child1 minHeight > child2 minHeight
		ifTrue: [minH := child2 minHeight + 1]
		ifFalse: [minH := child1 minHeight + 1].
	child1 maxHeight > child2 maxHeight
		ifTrue: [maxH := child1 maxHeight + 1]
		ifFalse: [maxH := child2 maxHeight + 1].
	(oldMin ~= minH or: [oldMax ~= maxH]) ifTrue:
		[self diskUpdate.
		^true]
	ifFalse:
		[^false]!
*/
}
/**
 * Make a new crum that contains both first and second.
 * This method just makes a new parent whose properties are empty. My client must bring my
 * properties up to date
 */
public CanopyCrum makeNewParent(CanopyCrum first, CanopyCrum second) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:4883:CanopyCrum methodsFor: 'protected'!
{CanopyCrum} makeNewParent: first {CanopyCrum} with: second {CanopyCrum} 
	"Make a new crum that contains both first and second.
	This method just makes a new parent whose properties are empty. My client must bring my properties up to date"
	self subclassResponsibility!
*/
}
/**
 * Install otherCanopy as a subtree in the canopy containing the receiver. Look below
 * the receiver and then in successively higher branches for a branch that has
 * enough height difference to contain otherCanopy.
 */
public CanopyCrum makeJoin(CanopyCrum otherCanopy) {
	int height;
	CanopyCrum cur;
	/* TODO variable may not be initialized before being used */
	CanopyCrum prev = null;
	Someone.thingToDo();
	/* Propagate the children's props into their new parent */
	Someone.thingToDo();
	/* When we have non-props to propagate, do those, too.  i.e., height is currently handle by changeCanopy and will be moved out to HeightChanger momentarily. */
	height = otherCanopy.maxHeight();
	cur = this;
	while ( ! (cur == null || (cur.heightDiff() >= height))) {
		prev = cur;
		cur = cur.fetchParent();
	}
	if (cur == null) {
		/* join the trees at the top */
		cur = makeNewParent(prev, otherCanopy);
		canopyCache().updateCacheForParent(prev, cur);
		canopyCache().updateCacheForParent(otherCanopy, cur);
	}
	else {
		/* found a branch that can contain
				 otherCanopy. Place it in that branch. */
		cur.includeCanopy(otherCanopy);
	}
	/* Cur now contains the closest parent shared between self and otherCanopy. */
	return cur;
/*
udanax-top.st:4891:CanopyCrum methodsFor: 'private'!
{CanopyCrum} makeJoin: otherCanopy {CanopyCrum} 
	"Install otherCanopy as a subtree in the canopy containing the receiver. Look below 
	the receiver and then in successively higher branches for a branch that has 
	enough height difference to contain otherCanopy."
	| height {IntegerVar} cur {CanopyCrum} prev {CanopyCrum} |
	
	self thingToDo.
	"Propagate the children's props into their new parent"
	
	self thingToDo.	"When we have non-props to propagate, do those, too.  i.e., height is currently handle by changeCanopy and will be moved out to HeightChanger momentarily."
	height _ otherCanopy maxHeight.
	cur _ self.
	[cur == NULL or: [cur heightDiff >= height]]
		whileFalse: 
			[prev _ cur.
			cur _ cur fetchParent].
	cur == NULL
		ifTrue: ["join the trees at the top"
			cur _ self makeNewParent: prev with: otherCanopy.
			self canopyCache updateCache: prev forParent: cur.
			self canopyCache updateCache: otherCanopy forParent: cur.]
		ifFalse: ["found a branch that can contain
				 otherCanopy. Place it in that branch."
			cur includeCanopy: otherCanopy].
	"Cur now contains the closest parent shared between self and otherCanopy."
	^cur!
*/
}
/**
 * Figure out new height, props, etc. Return true if any changes may require further
 * propagation
 */
public boolean changeCanopy(PropChange change) {
	/* At least one subclass adds behavior here by overriding and calling 'super changeCanopy:' */
	boolean result;
	/* If this is a leaf
		If any of my properties are changed
			Store the modification of the props.
	else
		save current flags
		recalculate the flags from myOwnFlags and the flags of the children
		if we're changing all properties (kludge for when combining trees)
			recompute heights (min and max)
	If anything changed
		flag that the change must be written to disk
	return whether anything changed (which requires propagation rootward) */
	if (isLeaf()) {
		result = myFlags != myOwnFlags;
		if (result) {
			myFlags = myOwnFlags;
		}
	}
	else {
		int before;
		before = myFlags;
		myFlags = (myOwnFlags | child1.flags()) | child2.flags();
		if (change.isFull()) {
			int oldMin;
			int oldMax;
			Someone.thingToDo();
			/* Need to move height calculation into a different sort of PropChanger that propagates immediately. */
			oldMin = minH;
			oldMax = maxH;
			if (child1.minHeight() > child2.minHeight()) {
				minH = child2.minHeight() + 1;
			}
			else {
				minH = child1.minHeight() + 1;
			}
			if (child1.maxHeight() > child2.maxHeight()) {
				maxH = child1.maxHeight() + 1;
			}
			else {
				maxH = child2.maxHeight() + 1;
			}
			result = oldMin != minH || (oldMax != maxH);
		}
		else {
			result = false;
		}
		result = result || (before != myFlags);
	}
	if (result) {
		diskUpdate();
	}
	return result;
/*
udanax-top.st:4922:CanopyCrum methodsFor: 'smalltalk: suspended'!
{BooleanVar} changeCanopy: change {PropChange unused}
	"Figure out new height, props, etc. Return true if any changes may require further propagation"
	"At least one subclass adds behavior here by overriding and calling 'super changeCanopy:'"
	
	| result {BooleanVar} |
	
	"If this is a leaf
		If any of my properties are changed
			Store the modification of the props.
	else
		save current flags
		recalculate the flags from myOwnFlags and the flags of the children
		if we're changing all properties (kludge for when combining trees)
			recompute heights (min and max)
	If anything changed
		flag that the change must be written to disk
	return whether anything changed (which requires propagation rootward)"
	
	self isLeaf ifTrue:
		[result := myFlags ~= myOwnFlags.
		result ifTrue:
			[myFlags := myOwnFlags]]
	ifFalse:
		[ | before {UInt32} |
		before := myFlags.
		myFlags := (myOwnFlags bitOr: child1 flags) bitOr: child2 flags.
		change isFull ifTrue:
			[ | oldMin {IntegerVar} oldMax {IntegerVar} |
			self thingToDo.		"Need to move height calculation into a different sort of PropChanger that propagates immediately."
			oldMin := minH.
			oldMax := maxH.
			child1 minHeight > child2 minHeight
				ifTrue: [minH := child2 minHeight + 1]
				ifFalse: [minH := child1 minHeight + 1].
			child1 maxHeight > child2 maxHeight
				ifTrue: [maxH := child1 maxHeight + 1]
				ifFalse: [maxH := child2 maxHeight + 1].
			result := oldMin ~= minH or: [oldMax ~= maxH]]
		ifFalse:
			[result := false].
		result := result or: [before ~= myFlags]].
	result ifTrue:
		[self diskUpdate].
	^result!
*/
}
public PropChange fullChange() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:4967:CanopyCrum methodsFor: 'smalltalk: suspended'!
{PropChange} fullChange
	self subclassResponsibility!
*/
}
/*
udanax-top.st:4970:CanopyCrum methodsFor: 'smalltalk: suspended'!
{PropJoint} joint 
	"Return the abstracted information necessary to determine whether anything leafward may pass the filtering criteria."
	
	^myPropJoint!
*/
public CanopyCrum(Rcvr receiver) {
	super(receiver);
	child1 = (CanopyCrum) receiver.receiveHeaper();
	child2 = (CanopyCrum) receiver.receiveHeaper();
	parent = (CanopyCrum) receiver.receiveHeaper();
	minH = receiver.receiveIntegerVar();
	maxH = receiver.receiveIntegerVar();
	myOwnFlags = receiver.receiveUInt32();
	myFlags = receiver.receiveUInt32();
	myRefCount = receiver.receiveIntegerVar();
/*
udanax-top.st:4978:CanopyCrum methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	child1 _ receiver receiveHeaper.
	child2 _ receiver receiveHeaper.
	parent _ receiver receiveHeaper.
	minH _ receiver receiveIntegerVar.
	maxH _ receiver receiveIntegerVar.
	myOwnFlags _ receiver receiveUInt32.
	myFlags _ receiver receiveUInt32.
	myRefCount _ receiver receiveIntegerVar.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(child1);
	xmtr.sendHeaper(child2);
	xmtr.sendHeaper(parent);
	xmtr.sendIntegerVar(minH);
	xmtr.sendIntegerVar(maxH);
	xmtr.sendUInt32(myOwnFlags);
	xmtr.sendUInt32(myFlags);
	xmtr.sendIntegerVar(myRefCount);
/*
udanax-top.st:4989:CanopyCrum methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: child1.
	xmtr sendHeaper: child2.
	xmtr sendHeaper: parent.
	xmtr sendIntegerVar: minH.
	xmtr sendIntegerVar: maxH.
	xmtr sendUInt32: myOwnFlags.
	xmtr sendUInt32: myFlags.
	xmtr sendIntegerVar: myRefCount.!
*/
}
public static void initTimeNonInherited() {
	TheEFlagsCache = Heaper2UInt32Cache.make(50);
	ThePFlagsCache = Heaper2UInt32Cache.make(50);
/*
udanax-top.st:5013:CanopyCrum class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	self REQUIRES: Heaper2UInt32Cache.
	TheEFlagsCache := Heaper2UInt32Cache make: 50.
	ThePFlagsCache := Heaper2UInt32Cache make: 50.!
*/
}
public static void linkTimeNonInherited() {
	FlagEndorsements = null;
	OtherClubs = null;
	OtherEndorsements = null;
	TheEFlagsCache = null;
	ThePFlagsCache = null;
/*
udanax-top.st:5019:CanopyCrum class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	FlagEndorsements := NULL.
	OtherClubs := NULL.
	OtherEndorsements := NULL.
	TheEFlagsCache := NULL.
	ThePFlagsCache := NULL.!
*/
}
/**
 * Flag bits corresponding to endorsements
 */
public static int endorsementsFlags(CrossRegion endorsements) {
	int result;
	int f;
	result = TheEFlagsCache.fetch(endorsements);
	if (result != 0 || (endorsements.isEmpty())) {
		return result;
	}
	f = firstEndorsementsFlag();
	if ( ! (FlagEndorsements != null)) {
		throw new AboraAssertionException("Must be initialized");
	}
	for (int i = 0; i < FlagEndorsements.count(); i ++ ) {
		Heaper cast1 = (FlagEndorsements.get(i));
		if (cast1 instanceof Position) {
			Position p = (Position) cast1;
			if (endorsements.hasMember(p)) {
				result = result | f;
			}
		}
		else if (cast1 instanceof XnRegion) {
			XnRegion r = (XnRegion) cast1;
			if (endorsements.intersects(r)) {
				result = result | f;
			}
		}
		f = f << 1;
	}
	if (endorsements.intersects(OtherEndorsements)) {
		result = result | otherEndorsementsFlag();
	}
	TheEFlagsCache.cache(endorsements, result);
	return result;
/*
udanax-top.st:5029:CanopyCrum class methodsFor: 'protected: flags'!
{UInt32} endorsementsFlags: endorsements {CrossRegion}
	"Flag bits corresponding to endorsements"
	
	| result {UInt32} f {UInt32} |
	result := TheEFlagsCache fetch: endorsements.
	(result ~= UInt32Zero or: [endorsements isEmpty]) ifTrue:
		[^result].
	f := self firstEndorsementsFlag.
	FlagEndorsements ~~ NULL assert: 'Must be initialized'.
	UInt32Zero almostTo: FlagEndorsements count do: [ :i {UInt32} |
		(FlagEndorsements get: i)
			cast: Position into: [ :p |
				(endorsements hasMember: p) ifTrue:
					[result := result bitOr: f]]
			cast: XnRegion into: [ :r |
				(endorsements intersects: r) ifTrue:
					[result := result bitOr: f]].
		f := f bitShift: 1].
	(endorsements intersects: OtherEndorsements) ifTrue:
		[result := result bitOr: self otherEndorsementsFlag].
	TheEFlagsCache at: endorsements cache: result.
	^result!
*/
}
/**
 * Flag bits corresponding to permissions
 */
public static int permissionsFlags(IDRegion permissions) {
	int result;
	result = ThePFlagsCache.fetch(permissions);
	if (result != 0) {
		return result;
	}
	if (permissions.hasMember(((BeGrandMap) CurrentGrandMap.fluidGet()).publicClubID())) {
		result = result | publicClubFlag();
	}
	if (OtherClubs == null) {
		OtherClubs = (IDRegion) ((BeGrandMap) CurrentGrandMap.fluidGet()).publicClubID().asRegion().complement();
	}
	if (permissions.intersects(OtherClubs)) {
		result = result | otherClubsFlag();
	}
	ThePFlagsCache.cache(permissions, result);
	return result;
/*
udanax-top.st:5052:CanopyCrum class methodsFor: 'protected: flags'!
{UInt32} permissionsFlags: permissions {IDRegion}
	"Flag bits corresponding to permissions"
	
	| result {UInt32} |
	result := ThePFlagsCache fetch: permissions.
	result ~= UInt32Zero ifTrue:
		[^result].
	[BeGrandMap] USES.
	(permissions hasMember: CurrentGrandMap fluidGet publicClubID) ifTrue:
		[result := result bitOr: self publicClubFlag].
	OtherClubs == NULL ifTrue:
			[OtherClubs := CurrentGrandMap fluidGet publicClubID asRegion complement cast: IDRegion].
	(permissions intersects: OtherClubs) ifTrue:
		[result := result bitOr: self otherClubsFlag].
	ThePFlagsCache at: permissions cache: result.
	^result!
*/
}
/**
 * Max number of special endorsement flags
 */
public static int endorsementFlagLimit() {
	return 23;
/*
udanax-top.st:5071:CanopyCrum class methodsFor: 'private: flags'!
{Int32} endorsementFlagLimit
	"Max number of special endorsement flags"
	
	^23 "28 bits - 2 for permissions - 1 for all other endorsements - 2 reserved"!
*/
}
/**
 * Rightmost flag for interesting endorsements
 */
public static int firstEndorsementsFlag() {
	return 0x8;
/*
udanax-top.st:5076:CanopyCrum class methodsFor: 'private: flags'!
{UInt32} firstEndorsementsFlag
	"Rightmost flag for interesting endorsements"
	
	^16r00000008!
*/
}
/**
 * The flag for any other Clubs
 */
public static int otherClubsFlag() {
	return 0x2;
/*
udanax-top.st:5081:CanopyCrum class methodsFor: 'private: flags'!
{UInt32} otherClubsFlag
	"The flag for any other Clubs"
	
	^16r00000002!
*/
}
/**
 * Flag for all uninteresting endorsements
 */
public static int otherEndorsementsFlag() {
	return 0x4;
/*
udanax-top.st:5086:CanopyCrum class methodsFor: 'private: flags'!
{UInt32} otherEndorsementsFlag
	"Flag for all uninteresting endorsements"
	
	^16r00000004!
*/
}
/**
 * The flag for the Universal Public Club
 */
public static int publicClubFlag() {
	return 0x1;
/*
udanax-top.st:5091:CanopyCrum class methodsFor: 'private: flags'!
{UInt32} publicClubFlag
	"The flag for the Universal Public Club"
	
	^16r00000001!
*/
}
/**
 * Use a special flag to look for any of the these endorsements
 */
public static void useEndorsementFlags(PtrArray endorsements) {
	if ( ! (FlagEndorsements == null || (FlagEndorsements.contentsEqual(endorsements)))) {
		throw new AboraRuntimeException(AboraRuntimeException.INVALID_REQUEST);
	}
	/* Tried to initialize twice */
	if (endorsements.count() > endorsementFlagLimit()) {
		throw new AboraRuntimeException(AboraRuntimeException.INDEX_OUT_OF_BOUNDS);
	}
	FlagEndorsements = (PtrArray) endorsements.copy();
	OtherEndorsements = (CrossRegion) ((BeGrandMap) CurrentGrandMap.fluidGet()).endorsementSpace().fullRegion();
	for (int i = 0; i < FlagEndorsements.count(); i ++ ) {
		Heaper cast1 = (FlagEndorsements.get(i));
		if (cast1 instanceof Position) {
			Position p = (Position) cast1;
			OtherEndorsements = (CrossRegion) (OtherEndorsements.without(p));
		}
		else if (cast1 instanceof XnRegion) {
			XnRegion r = (XnRegion) cast1;
			OtherEndorsements = (CrossRegion) (OtherEndorsements.minus(r));
		}
	}
/*
udanax-top.st:5098:CanopyCrum class methodsFor: 'flag setup'!
{void} useEndorsementFlags: endorsements {PtrArray of: Position | XnRegion}
	"Use a special flag to look for any of the these endorsements"
	
	(FlagEndorsements == NULL or: [FlagEndorsements contentsEqual: endorsements]) ifFalse:
		[Heaper BLAST: #InvalidRequest]. "Tried to initialize twice"
	endorsements count > self endorsementFlagLimit ifTrue:
		[Heaper BLAST: #IndexOutOfBounds].
	FlagEndorsements := endorsements copy cast: PtrArray.
	OtherEndorsements := CurrentGrandMap fluidGet endorsementSpace fullRegion cast: CrossRegion.
	Int32Zero almostTo: FlagEndorsements count do: [ :i {Int32} |
		(FlagEndorsements get: i)
			cast: Position into: [ :p |
				OtherEndorsements := (OtherEndorsements without: p) cast: CrossRegion]
			cast: XnRegion into: [ :r |
				OtherEndorsements := (OtherEndorsements minus: r) cast: CrossRegion]].!
*/
}
public CanopyCrum() {
/*

Generated during transformation
*/
}
}
