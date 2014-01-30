/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.spaces.unordered;

import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.collection.sets.ScruSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.unordered.SetRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * How do you make regions for spaces whose positions
 * a) have no orderring (i.e., either no ordering can be imposed (as
 * in HeaperSpace) or it is undesirable to impose one (as curently
 * in IDSpace)); and
 * b) there is an inifinte supply of new positions, and you can only
 * name the positions you''ve encountered?
 * SetRegion is our answer to that.  To start with, a set region can simply be an enumeration
 * of the positions which are its members.  However, because the complement of an XuRegion
 * must be a valid XuRegion, and we have no other representation of the infinite set of
 * positions left over, we must also be able to represent the region consisting of all
 * positions except those explicitly enumerated.  Every SetRegion must either have a finite
 * number of positions, or it must cover all the space except for a finite number of
 * positions.
 * With regard to degrees of simplicity (see class comment in XuRegion), we currently only
 * have distinctions.  There are no non-distinctions, and therefore no non-simple SetRegions.
 * Interesting cases are:
 * 1) empty region
 * 2) full region
 * 3) singleton set (single member)
 * 4) singleton hole (single non-member)
 * 5) region with more than 1, but a finite number, of members
 * 6) region with more than 1, but a finite number, of non-members
 * Cases 1, 3, and 5 can be considered the "positive" regions, and cases 2, 4, and 6 the
 * "negative" ones.
 * Because we only have distinctions (which we are currently doing for an internal reason
 * which will probably go away), we forego the ability to use the generic XuRegion protocol
 * to decompose complex regions into simpler ones.  Instead we provide SetRegion specific
 * protocol ("positions" and "isComplement").
 * At a later time, we will probably have cases 1 thru 4 above be the only distinctions, case
 * 6 be a simple region but not a distinction, and have case 5 be a non-simple region.
 * (These choices are all consistent with the letter and spirit of the simplicity framework
 * documented in XuRegion.  Simple regions must be the *intersection* of distinctions,
 * therefore case 5 cannot be a simple non-distinction.)  Please try to write your software
 * so that it''ll be insensitive to this change.  Thanks.
 * SetRegion is an abstract superclass useful for defining regions for spaces which have the
 * constraints listed above.
 */
public class SetRegion extends XnRegion {

	protected ImmuSet myPositions;
	protected boolean myIsComplement;
/*
udanax-top.st:69930:
XnRegion subclass: #SetRegion
	instanceVariableNames: '
		myPositions {(ImmuSet of: Position) copy}
		myIsComplement {BooleanVar copy}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Unordered'!
*/
/*
udanax-top.st:69936:
SetRegion comment:
'How do you make regions for spaces whose positions
		a) have no orderring (i.e., either no ordering can be imposed (as 
			in HeaperSpace) or it is undesirable to impose one (as curently 
			in IDSpace)); and
		b) there is an inifinte supply of new positions, and you can only 
			name the positions you''ve encountered?
			
	SetRegion is our answer to that.  To start with, a set region can simply be an enumeration of the positions which are its members.  However, because the complement of an XuRegion must be a valid XuRegion, and we have no other representation of the infinite set of positions left over, we must also be able to represent the region consisting of all positions except those explicitly enumerated.  Every SetRegion must either have a finite number of positions, or it must cover all the space except for a finite number of positions.
	
	With regard to degrees of simplicity (see class comment in XuRegion), we currently only have distinctions.  There are no non-distinctions, and therefore no non-simple SetRegions.  Interesting cases are:
	
		1) empty region
		2) full region
		3) singleton set (single member)
		4) singleton hole (single non-member)
		5) region with more than 1, but a finite number, of members
		6) region with more than 1, but a finite number, of non-members
	
	Cases 1, 3, and 5 can be considered the "positive" regions, and cases 2, 4, and 6 the "negative" ones.
	
	Because we only have distinctions (which we are currently doing for an internal reason which will probably go away), we forego the ability to use the generic XuRegion protocol to decompose complex regions into simpler ones.  Instead we provide SetRegion specific protocol ("positions" and "isComplement").  
	
	At a later time, we will probably have cases 1 thru 4 above be the only distinctions, case 6 be a simple region but not a distinction, and have case 5 be a non-simple region.  (These choices are all consistent with the letter and spirit of the simplicity framework documented in XuRegion.  Simple regions must be the *intersection* of distinctions, therefore case 5 cannot be a simple non-distinction.)  Please try to write your software so that it''ll be insensitive to this change.  Thanks.
	
	SetRegion is an abstract superclass useful for defining regions for spaces which have the constraints listed above.'!
*/
/*
udanax-top.st:69962:
(SetRegion getOrMakeCxxClassDescription)
	friends:
'/- friends for class SetRegion -/
SPTR(SetRegion) setRegion (CoordinateSpace * cs, ImmuSet * aSet);
friend class SetRegionStepper;
';
	attributes: ((Set new) add: #DEFERRED; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SetRegion.class).setAttributes( new Set().add("DEFERRED").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public XnRegion asSimpleRegion() {
	return this;
/*
udanax-top.st:69972:SetRegion methodsFor: 'accessing'!
{XnRegion} asSimpleRegion
	^ self!
*/
}
public ScruSet distinctions() {
	return ImmuSet.make().with(this);
/*
udanax-top.st:69976:SetRegion methodsFor: 'accessing'!
{ScruSet of: XnRegion} distinctions
	^ImmuSet make with: self!
*/
}
/**
 * FALSE means that I'm a 'positive' region (see class comment).
 * TRUE means I'm a negative region.
 */
public boolean isComplement() {
	return myIsComplement;
/*
udanax-top.st:69980:SetRegion methodsFor: 'accessing'!
{BooleanVar INLINE} isComplement
	"FALSE means that I'm a 'positive' region (see class comment).  
	TRUE means I'm a negative region."
	
	^myIsComplement!
*/
}
/**
 * If I'm a positive region (see class comment and isComplement), then
 * this is a list of those positions I contain. If I'm negative, then it's
 * those positions I don't contain.
 */
public ImmuSet positions() {
	return myPositions;
/*
udanax-top.st:69986:SetRegion methodsFor: 'accessing'!
{ImmuSet wimpy INLINE of: Position} positions
	"If I'm a positive region (see class comment and isComplement), then 
	this is a list of those positions I contain. If I'm negative, then it's 
	those positions I don't contain."
	^myPositions!
*/
}
/**
 * Make up a singleton set containing the whole region
 */
public Stepper simpleRegions(OrderSpec order) {
	return Stepper.itemStepper(this);
/*
udanax-top.st:69993:SetRegion methodsFor: 'accessing'!
{Stepper} simpleRegions: order {OrderSpec unused default: NULL}
	"Make up a singleton set containing the whole region"
	^Stepper itemStepper: self!
*/
}
public int count() {
	if (myIsComplement) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_ENUMERABLE);
	}
	return myPositions.count();
/*
udanax-top.st:70000:SetRegion methodsFor: 'enumerating'!
{IntegerVar} count
	myIsComplement
		ifTrue: [Heaper BLAST: #NotEnumerable].
	^myPositions count!
*/
}
public boolean isEnumerable(OrderSpec order) {
	return ! myIsComplement;
/*
udanax-top.st:70005:SetRegion methodsFor: 'enumerating'!
{BooleanVar} isEnumerable: order {OrderSpec default: NULL}
	
	^myIsComplement not!
*/
}
public Position theOne() {
	if (count() != 1) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_ONE_ELEMENT);
	}
	return (Position) myPositions.theOne();
/*
udanax-top.st:70009:SetRegion methodsFor: 'enumerating'!
{Position} theOne
	self count ~~ 1 ifTrue:
		[ Heaper BLAST: #NotOneElement ].
	^ myPositions theOne cast: Position!
*/
}
public XnRegion complement() {
	return makeNew( ! myIsComplement, myPositions);
/*
udanax-top.st:70016:SetRegion methodsFor: 'operations'!
{XnRegion} complement
	^self makeNew: myIsComplement not with: myPositions!
*/
}
public XnRegion intersect(XnRegion region) {
	if (region.isEmpty()) {
		return region;
	}
	else {
		SetRegion other;
		other = (SetRegion) region;
		if (myIsComplement) {
			if (other.isComplement()) {
				return makeNew(true, (myPositions.unionWith(other.positions())));
			}
			else {
				return makeNew(false, (other.positions().minus(myPositions)));
			}
		}
		else {
			if (other.isComplement()) {
				return makeNew(false, (myPositions.minus(other.positions())));
			}
			else {
				return makeNew(false, (myPositions.intersect(other.positions())));
			}
		}
	}
/*
udanax-top.st:70020:SetRegion methodsFor: 'operations'!
{XnRegion} intersect: region {XnRegion}
	region isEmpty
		ifTrue: [ ^ region ]
		ifFalse:
			[| other {SetRegion wimpy} |
			other _ region cast: SetRegion.
			myIsComplement
				ifTrue: [other isComplement
					ifTrue: [^self makeNew: true with: (myPositions unionWith: other positions)]
					ifFalse: [^self makeNew: false with: (other positions minus: myPositions)]]
				ifFalse: [other isComplement
					ifTrue: [^self makeNew: false with: (myPositions minus: other positions)]
					ifFalse: [^self makeNew: false with: (myPositions intersect: other positions)]]]!
*/
}
public XnRegion minus(XnRegion other) {
	if (other.isEmpty()) {
		return this;
	}
	else {
		SetRegion set;
		set = (SetRegion) other;
		if (myIsComplement) {
			if (set.isComplement()) {
				return makeNew(false, (set.positions().minus(myPositions)));
			}
			else {
				return makeNew(true, (set.positions().unionWith(myPositions)));
			}
		}
		else {
			if (set.isComplement()) {
				return makeNew(false, (set.positions().intersect(myPositions)));
			}
			else {
				return makeNew(false, (myPositions.minus(set.positions())));
			}
		}
	}
/*
udanax-top.st:70034:SetRegion methodsFor: 'operations'!
{XnRegion} minus: other {XnRegion}
	other isEmpty
		ifTrue: [ ^ self ]
		ifFalse:
			[| set {SetRegion wimpy} |
			set _ other cast: SetRegion.
			myIsComplement
				ifTrue: [set isComplement
					ifTrue: [^self makeNew: false with: (set positions minus: myPositions)]
					ifFalse: [^self makeNew: true with: (set positions unionWith: myPositions)]]
				ifFalse: [set isComplement
					ifTrue: [^self makeNew: false with: (set positions intersect: myPositions)]
					ifFalse: [^self makeNew: false with: (myPositions minus: set positions)]]]!
*/
}
public XnRegion simpleUnion(XnRegion other) {
	return unionWith(other);
/*
udanax-top.st:70048:SetRegion methodsFor: 'operations'!
{XnRegion} simpleUnion: other {XnRegion}
	^self unionWith: other!
*/
}
public XnRegion unionWith(XnRegion region) {
	if (region.isEmpty()) {
		return this;
	}
	else {
		SetRegion other;
		other = (SetRegion) region;
		if (myIsComplement) {
			if (other.isComplement()) {
				return makeNew(true, (myPositions.intersect(other.positions())));
			}
			else {
				return makeNew(true, (myPositions.minus(other.positions())));
			}
		}
		else {
			if (other.isComplement()) {
				return makeNew(true, (other.positions().minus(myPositions)));
			}
			else {
				return makeNew(false, (myPositions.unionWith(other.positions())));
			}
		}
	}
/*
udanax-top.st:70051:SetRegion methodsFor: 'operations'!
{XnRegion} unionWith: region {XnRegion}
	region isEmpty
		ifTrue: [ ^ self ]
		ifFalse:
			[| other {SetRegion wimpy} |
			other _ region cast: SetRegion.
			myIsComplement
				ifTrue: [other isComplement
					ifTrue: [^self makeNew: true with: (myPositions intersect: other positions)]
					ifFalse: [^self makeNew: true with: (myPositions minus: other positions)]]
				ifFalse: [other isComplement
					ifTrue: [^self makeNew: true with: (other positions minus: myPositions)]
					ifFalse: [^self makeNew: false with: (myPositions unionWith: other positions)]]]!
*/
}
public XnRegion with(Position pos) {
	if (myIsComplement) {
		return makeNew(myIsComplement, (myPositions.without(pos)));
	}
	else {
		return makeNew(myIsComplement, (myPositions.with(pos)));
	}
/*
udanax-top.st:70065:SetRegion methodsFor: 'operations'!
{XnRegion} with: pos {Position}
	myIsComplement
		ifTrue: [^self makeNew: myIsComplement with: (myPositions without: pos)]
		ifFalse: [^self makeNew: myIsComplement with: (myPositions with: pos)]!
*/
}
public XnRegion without(Position pos) {
	if (myIsComplement) {
		return makeNew(myIsComplement, (myPositions.with(pos)));
	}
	else {
		return makeNew(myIsComplement, (myPositions.without(pos)));
	}
/*
udanax-top.st:70071:SetRegion methodsFor: 'operations'!
{XnRegion} without: pos {Position}
	myIsComplement
		ifTrue: [^self makeNew: myIsComplement with: (myPositions with: pos)]
		ifFalse: [^self makeNew: myIsComplement with: (myPositions without: pos)]!
*/
}
public int actualHashForEqual() {
	return (getCategory().hashForEqual() ^ myPositions.hashForEqual()) ^ ((myIsComplement) ? 15732 : 0);
/*
udanax-top.st:70079:SetRegion methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^(self getCategory hashForEqual
		bitXor: myPositions hashForEqual)
		bitXor: (myIsComplement ifTrue: [15732] ifFalse: [Int32Zero])!
*/
}
public boolean hasMember(Position atPos) {
	return (myPositions.hasMember(atPos)) != myIsComplement;
/*
udanax-top.st:70084:SetRegion methodsFor: 'testing'!
{BooleanVar} hasMember: atPos {Position}
	^(myPositions hasMember: atPos) ~~ myIsComplement!
*/
}
public boolean intersects(XnRegion region) {
	if (region.isEmpty()) {
		return false;
	}
	else {
		SetRegion other;
		other = (SetRegion) region;
		if (myIsComplement) {
			if (other.isComplement()) {
				return true;
			}
			else {
				return ! (other.positions().isSubsetOf(myPositions));
			}
		}
		else {
			if (other.isComplement()) {
				return ! (myPositions.isSubsetOf(other.positions()));
			}
			else {
				return other.positions().intersects(myPositions);
			}
		}
	}
/*
udanax-top.st:70087:SetRegion methodsFor: 'testing'!
{BooleanVar} intersects: region {XnRegion}
	region isEmpty
		ifTrue: [ ^ false ]
		ifFalse:
			[| other {SetRegion wimpy} |
			other _ region cast: SetRegion.
			myIsComplement
				ifTrue: [other isComplement
					ifTrue: [^true]
					ifFalse: [^(other positions isSubsetOf: myPositions) not]]
				ifFalse: [other isComplement
					ifTrue: [^(myPositions isSubsetOf: other positions) not]
					ifFalse: [^other positions intersects: myPositions]]]!
*/
}
public boolean isEmpty() {
	return ! myIsComplement && (myPositions.isEmpty());
/*
udanax-top.st:70101:SetRegion methodsFor: 'testing'!
{BooleanVar} isEmpty
	^myIsComplement not and: [myPositions isEmpty]!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof SetRegion) {
		SetRegion sr = (SetRegion) other;
		Someone.hack();
		return (other.isKindOf(getCategory())) && (sr.isComplement() == myIsComplement && (sr.positions().isEqual(myPositions)));
	}
	else {
		return false;
	}
/*
udanax-top.st:70104:SetRegion methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	other
		cast: SetRegion into: [:sr |
			self hack.
			^(other isKindOf: self getCategory)
			 and: [sr isComplement == myIsComplement
			 and: [sr positions isEqual: myPositions]]]
		others: [^false].
	^false "fodder"!
*/
}
public boolean isFinite() {
	return ! myIsComplement;
/*
udanax-top.st:70115:SetRegion methodsFor: 'testing'!
{BooleanVar} isFinite
	^myIsComplement not!
*/
}
public boolean isFull() {
	return myIsComplement && (myPositions.isEmpty());
/*
udanax-top.st:70118:SetRegion methodsFor: 'testing'!
{BooleanVar} isFull	
	^myIsComplement and: [myPositions isEmpty]!
*/
}
public boolean isSimple() {
	return true;
/*
udanax-top.st:70122:SetRegion methodsFor: 'testing'!
{BooleanVar} isSimple
	^true!
*/
}
public boolean isSubsetOf(XnRegion other) {
	if (other.isEmpty()) {
		return isEmpty();
	}
	else {
		SetRegion set;
		set = (SetRegion) other;
		if (myIsComplement) {
			return set.isComplement() && (set.positions().isSubsetOf(myPositions));
		}
		else {
			if (set.isComplement()) {
				return ! (set.positions().intersects(myPositions));
			}
			else {
				return myPositions.isSubsetOf(set.positions());
			}
		}
	}
/*
udanax-top.st:70125:SetRegion methodsFor: 'testing'!
{BooleanVar} isSubsetOf: other {XnRegion}
	other isEmpty
		ifTrue: [ ^ self isEmpty ]
		ifFalse:
			[| set {SetRegion wimpy} |
			set _ other cast: SetRegion.
			myIsComplement
				ifTrue: [^set isComplement and: [set positions isSubsetOf: myPositions]]
				ifFalse: [set isComplement
					ifTrue: [^(set positions intersects: myPositions) not]
					ifFalse: [^myPositions isSubsetOf: set positions]]]!
*/
}
/**
 * the set should be for my use alone
 */
public SetRegion(boolean cmp, ImmuSet set) {
	super();
	myIsComplement = cmp;
	myPositions = set;
/*
udanax-top.st:70139:SetRegion methodsFor: 'protected: creation'!
create: cmp {BooleanVar} with: set {ImmuSet of: Position}
	"the set should be for my use alone"
	super create.
	myIsComplement _ cmp.
	myPositions _ set!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	if (myIsComplement) {
		oo.print("~");
	}
	myPositions.printOnWithSimpleSyntax(oo, "{", ", ", "}");
/*
udanax-top.st:70147:SetRegion methodsFor: 'printing'!
{void} printOn: oo {ostream reference} 
	oo << self getCategory name.
	myIsComplement ifTrue: [oo << '~'].
	myPositions printOnWithSimpleSyntax: oo with: '{' with: ', ' with: '}'!
*/
}
public XnRegion makeNew(boolean isComplement, ImmuSet positions) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:70154:SetRegion methodsFor: 'protected: protected deferred'!
{XnRegion} makeNew: isComplement {BooleanVar} with: positions {ImmuSet of: Position}
	self subclassResponsibility!
*/
}
public CoordinateSpace coordinateSpace() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:70159:SetRegion methodsFor: 'deferred accessing'!
{CoordinateSpace} coordinateSpace
	self subclassResponsibility!
*/
}
public Stepper actualStepper(OrderSpec order) {
	return myPositions.stepper();
/*
udanax-top.st:70164:SetRegion methodsFor: 'protected: enumerating'!
{Stepper} actualStepper: order {OrderSpec unused}
	^myPositions stepper!
*/
}
public SetRegion(Rcvr receiver) {
	super(receiver);
	myPositions = (ImmuSet) receiver.receiveHeaper();
	myIsComplement = receiver.receiveBooleanVar();
/*
udanax-top.st:70170:SetRegion methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myPositions _ receiver receiveHeaper.
	myIsComplement _ receiver receiveBooleanVar.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myPositions);
	xmtr.sendBooleanVar(myIsComplement);
/*
udanax-top.st:70175:SetRegion methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myPositions.
	xmtr sendBooleanVar: myIsComplement.!
*/
}
public SetRegion() {
/*

Generated during transformation
*/
}
}
