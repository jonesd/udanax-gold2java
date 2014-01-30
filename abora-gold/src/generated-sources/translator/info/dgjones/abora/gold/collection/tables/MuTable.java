/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.tables;

import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.collection.tables.HashTable;
import info.dgjones.abora.gold.collection.tables.ImmuTable;
import info.dgjones.abora.gold.collection.tables.ImmuTableOnMu;
import info.dgjones.abora.gold.collection.tables.IntegerTable;
import info.dgjones.abora.gold.collection.tables.MuTable;
import info.dgjones.abora.gold.collection.tables.ScruTable;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.Signal;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.Dsp;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.spaces.integers.IntegerRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerSpace;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * MuTable represents the base class for all side-effectable tables.  It provides the basic
 * change protocol for tables.  See MuSet.
 */
public class MuTable extends ScruTable {

	protected static Signal AlreadyInTableSignal;
	protected static Signal NotInDomainSignal;
	protected static Signal NullInsertionSignal;
/*
udanax-top.st:47785:
ScruTable subclass: #MuTable
	instanceVariableNames: ''
	classVariableNames: '
		AlreadyInTableSignal {Signal smalltalk} 
		NotInDomainSignal {Signal smalltalk} 
		NullInsertionSignal {Signal smalltalk} '
	poolDictionaries: ''
	category: 'Xanadu-Collection-Tables'!
*/
/*
udanax-top.st:47792:
MuTable comment:
'MuTable represents the base class for all side-effectable tables.  It provides the basic change protocol for tables.  See MuSet.'!
*/
/*
udanax-top.st:47794:
(MuTable getOrMakeCxxClassDescription)
	friends:
'/- friends for class MuTable -/
friend class COWMuTable;';
	attributes: ((Set new) add: #EQ; add: #DEFERRED; add: #COPY; yourself)!
*/
/*
udanax-top.st:48100:
MuTable class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:48103:
(MuTable getOrMakeCxxClassDescription)
	friends:
'/- friends for class MuTable -/
friend class COWMuTable;';
	attributes: ((Set new) add: #EQ; add: #DEFERRED; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(MuTable.class).setAttributes( new Set().add("EQ").add("DEFERRED").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Associate key with value unless key is already associated with another value.  If so,
 * blast.
 */
public void introduce(Position key, Heaper value) {
	Heaper old;
	if ((old = store(key, value)) != null) {
		store(key, old);
		throw new AboraRuntimeException(AboraRuntimeException.ALREADY_IN_TABLE);
	}
/*
udanax-top.st:47802:MuTable methodsFor: 'accessing'!
{void} at: key {Position} introduce: value {Heaper} 
	"Associate key with value unless key is already associated with another value.  If so, blast."
	| old {Heaper} |
	((old _ self at: key store: value) ~~ NULL)
		ifTrue: [self at: key store: old.
			Heaper BLAST: #AlreadyInTable]!
*/
}
/**
 * Associate key with value only if key is already associated with a value.  Otherwise blast.
 */
public void replace(Position key, Heaper value) {
	if ((store(key, value)) == null) {
		wipe(key);
		/* restore table before blast */
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
	}
/*
udanax-top.st:47809:MuTable methodsFor: 'accessing'!
{void} at: key {Position} replace: value {Heaper} 
	"Associate key with value only if key is already associated with a value.  Otherwise blast."
	((self at: key store: value) == NULL)
		ifTrue: [self wipe: key. "restore table before blast"
			Heaper BLAST: #NotInTable]!
*/
}
/**
 * Associate value with key, whether or not there is a previous association.
 * Return the old range element if the position was previously occupied, NULL otherwise
 */
public Heaper store(Position key, Heaper value) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:47816:MuTable methodsFor: 'accessing'!
{Heaper} at: key {Position} store: value {Heaper} 
	"Associate value with key, whether or not there is a previous association.
	  Return the old range element if the position was previously occupied, NULL otherwise"
	self subclassResponsibility!
*/
}
public CoordinateSpace coordinateSpace() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:47822:MuTable methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	
	self subclassResponsibility!
*/
}
public int count() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:47826:MuTable methodsFor: 'accessing'!
{IntegerVar} count
	self subclassResponsibility!
*/
}
public XnRegion domain() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:47830:MuTable methodsFor: 'accessing'!
{XnRegion} domain
	self subclassResponsibility.!
*/
}
public Heaper fetch(Position key) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:47834:MuTable methodsFor: 'accessing'!
{Heaper} fetch: key {Position} 
	self subclassResponsibility!
*/
}
/**
 * Remove a key->value association from the table.
 * Blast if the key is not present.
 */
public void remove(Position anIdx) {
	if ( ! (wipe(anIdx))) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
	}
/*
udanax-top.st:47838:MuTable methodsFor: 'accessing'!
{void} remove: anIdx {Position}
	"Remove a key->value association from the table.
	 Blast if the key is not present."
	(self wipe: anIdx) ifFalse: [Heaper BLAST: #NotInTable]!
*/
}
public ScruTable subTable(XnRegion reg) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:47844:MuTable methodsFor: 'accessing'!
{ScruTable} subTable: reg {XnRegion} 
	self subclassResponsibility!
*/
}
/**
 * Remove a key->value association from the table.
 * Do not blast (or do anything else) if the key is not in my current domain.
 * Return TRUE if the association was present and removed,
 * Return FALSE if the association was not there
 */
public boolean wipe(Position anIdx) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:47848:MuTable methodsFor: 'accessing'!
{BooleanVar} wipe: anIdx {Position}
	"Remove a key->value association from the table.
	 Do not blast (or do anything else) if the key is not in my current domain.
	 Return TRUE if the association was present and removed,
	 Return FALSE if the association was not there"
	self subclassResponsibility!
*/
}
/**
 * 'MuTable::introduceAll is to 'MuTable::introduce' as 'MuTable::storeAll' is to
 * 'MuTable::store'.  See MuTable::storeAll.  In addition to the functionality
 * provided by MuTable::storeAll, I BLAST *if* all the associations I'm being
 * asked to store override existing associations of mine.  If I BLAST for this
 * reason, then I guarantee that I haven't changed myself at all.
 */
public void introduceAll(ScruTable table, Dsp dsp, XnRegion region) {
	/* Since this function checks the relavent regions, it can call the potentially 
	more efficient store: */
	if ( ! (table.coordinateSpace().isEqual(coordinateSpace()))) {
		throw new AboraRuntimeException(AboraRuntimeException.WRONG_COORD_SPACE);
	}
	if (dsp == null) {
		if (domain().intersects(table.domain())) {
			throw new AboraRuntimeException(AboraRuntimeException.ALREADY_IN_TABLE);
		}
	}
	else {
		if (region == null) {
			if (domain().intersects((dsp.ofAll(table.domain())))) {
				throw new AboraRuntimeException(AboraRuntimeException.ALREADY_IN_TABLE);
			}
		}
		else {
			if (domain().intersects((dsp.ofAll((table.domain().intersect(region)))))) {
				throw new AboraRuntimeException(AboraRuntimeException.ALREADY_IN_TABLE);
			}
		}
	}
	storeAll(table, dsp, region);
/*
udanax-top.st:47858:MuTable methodsFor: 'bulk operations'!
{void} introduceAll: table {ScruTable} 
	with: dsp {Dsp default: NULL} 
	with: region {XnRegion default: NULL}
	"'MuTable::introduceAll is to 'MuTable::introduce' as 'MuTable::storeAll' is to 
	'MuTable::store'.  See MuTable::storeAll.  In addition to the functionality 
	provided by MuTable::storeAll, I BLAST *if* all the associations I'm being
	asked to store override existing associations of mine.  If I BLAST for this
	reason, then I guarantee that I haven't changed myself at all."
	"Since this function checks the relavent regions, it can call the potentially 
	more efficient store:"
	(table coordinateSpace isEqual: self coordinateSpace) ifFalse:
		[ Heaper BLAST: #WrongCoordSpace ].
	dsp == NULL
		ifTrue:
			[(self domain intersects: table domain) ifTrue:
				[ Heaper BLAST: #AlreadyInTable ]]
		ifFalse:
			[region == NULL
				ifTrue:
					[(self domain intersects: (dsp ofAll: table domain)) ifTrue:
						[ Heaper BLAST: #AlreadyInTable ]]
				ifFalse:
					[(self domain intersects: (dsp ofAll: (table domain intersect: region))) ifTrue:
						[ Heaper BLAST: #AlreadyInTable ]]].
	self storeAll: table with: dsp with: region!
*/
}
/**
 * 'MuTable::replaceAll is to 'MuTable::replace' as 'MuTable::storeAll' is to
 * 'MuTable::store'.  See MuTable::storeAll.  In addition to the functionality
 * provided by MuTable::storeAll, I BLAST *unless* all the associations I'm being
 * asked to store override existing associations of mine.  If I BLAST for this
 * reason, then I guarantee that I haven't changed myself at all.
 */
public void replaceAll(ScruTable table, Dsp dsp, XnRegion region) {
	/* Since this function checks the relavent regions, it can call the potentially 
	more efficient store: */
	TableStepper stepper;
	if ( ! (table.coordinateSpace().isEqual(coordinateSpace()))) {
		throw new AboraRuntimeException(AboraRuntimeException.WRONG_COORD_SPACE);
	}
	if (dsp == null) {
		if ( ! (table.domain().isSubsetOf(domain()))) {
			throw new AboraRuntimeException(AboraRuntimeException.ALREADY_IN_TABLE);
		}
		Stepper stomper = (stepper = table.stepper());
		for (; stomper.hasValue(); stomper.step()) {
			Heaper e = (Heaper) stomper.fetch();
			if (e == null) {
				continue ;
			}
			store(stepper.position(), e);
		}
		stomper.destroy();
	}
	else {
		if (region == null) {
			if ( ! ((dsp.ofAll(table.domain())).isSubsetOf(domain()))) {
				throw new AboraRuntimeException(AboraRuntimeException.ALREADY_IN_TABLE);
			}
			Stepper stomper2 = (stepper = table.stepper());
			for (; stomper2.hasValue(); stomper2.step()) {
				Heaper x = (Heaper) stomper2.fetch();
				if (x == null) {
					continue ;
				}
				store((dsp.of(stepper.position())), x);
			}
			stomper2.destroy();
		}
		else {
			if ( ! ((dsp.ofAll((table.domain().intersect(region)))).isSubsetOf(domain()))) {
				throw new AboraRuntimeException(AboraRuntimeException.ALREADY_IN_TABLE);
			}
			Stepper stomper3 = (stepper = (table.subTable(region)).stepper());
			for (; stomper3.hasValue(); stomper3.step()) {
				Heaper y = (Heaper) stomper3.fetch();
				if (y == null) {
					continue ;
				}
				store((dsp.of(stepper.position())), y);
			}
			stomper3.destroy();
		}
	}
/*
udanax-top.st:47886:MuTable methodsFor: 'bulk operations'!
{void} replaceAll: table {ScruTable} 
	with: dsp {Dsp default: NULL} 
	with: region {XnRegion default: NULL}
	"'MuTable::replaceAll is to 'MuTable::replace' as 'MuTable::storeAll' is to 
	'MuTable::store'.  See MuTable::storeAll.  In addition to the functionality 
	provided by MuTable::storeAll, I BLAST *unless* all the associations I'm being
	asked to store override existing associations of mine.  If I BLAST for this
	reason, then I guarantee that I haven't changed myself at all."
	"Since this function checks the relavent regions, it can call the potentially 
	more efficient store:"
	
	| stepper {TableStepper} |
	(table coordinateSpace isEqual: self coordinateSpace) ifFalse:
		[ Heaper BLAST: #WrongCoordSpace ].
	dsp == NULL
		ifTrue:
			[(table domain isSubsetOf: self domain) ifFalse:
				[ Heaper BLAST: #AlreadyInTable ].
			(stepper _ table stepper) forEach:
				[ :e {Heaper} |
				self at: stepper position store: e]]
		ifFalse:
			[region == NULL
				ifTrue:
					[((dsp ofAll: table domain) isSubsetOf: self domain) ifFalse:
						[ Heaper BLAST: #AlreadyInTable ].
					(stepper _ table stepper) forEach:
						[ :x {Heaper} |
						self at: (dsp of: stepper position) store: x]]
				ifFalse:
					[((dsp ofAll: (table domain intersect: region)) isSubsetOf: self domain) ifFalse:
						[ Heaper BLAST: #AlreadyInTable ].
					(stepper _ (table subTable: region) stepper) forEach:
						[ :y {Heaper} |
						self at: (dsp of: stepper position) store: y]]]!
*/
}
/**
 * I 'store' into myself (see MuTable::store) all the associations from 'table'.
 * If 'region' is provided, then I only store those associations from 'table' whose
 * key is inside 'region'.  If 'dsp' is provided, then I transform the keys (from
 * the remaining associations) by dsp before storing into myself.
 */
public void storeAll(ScruTable table, Dsp dsp, XnRegion region) {
	TableStepper stepper;
	if ( ! (table.coordinateSpace().isEqual(coordinateSpace()))) {
		throw new AboraRuntimeException(AboraRuntimeException.WRONG_COORD_SPACE);
	}
	if (dsp == null) {
		Stepper stomper = (stepper = table.stepper());
		for (; stomper.hasValue(); stomper.step()) {
			Heaper e = (Heaper) stomper.fetch();
			if (e == null) {
				continue ;
			}
			store(stepper.position(), e);
		}
		stomper.destroy();
	}
	else {
		ScruTable localTable;
		if (region != null) {
			localTable = table.subTable(region);
		}
		else {
			localTable = table;
		}
		Stepper stomper2 = (stepper = localTable.stepper());
		for (; stomper2.hasValue(); stomper2.step()) {
			Heaper x = (Heaper) stomper2.fetch();
			if (x == null) {
				continue ;
			}
			store((dsp.of(stepper.position())), x);
		}
		stomper2.destroy();
	}
/*
udanax-top.st:47924:MuTable methodsFor: 'bulk operations'!
{void} storeAll: table {ScruTable} 
	with: dsp {Dsp default: NULL} 
	with: region {XnRegion default: NULL}
	"I 'store' into myself (see MuTable::store) all the associations from 'table'.
	If 'region' is provided, then I only store those associations from 'table' whose
	key is inside 'region'.  If 'dsp' is provided, then I transform the keys (from 
	the remaining associations) by dsp before storing into myself."
	
	| stepper {TableStepper} |
	(table coordinateSpace isEqual: self coordinateSpace) ifFalse:
		[ Heaper BLAST: #WrongCoordSpace ].
	dsp == NULL
		ifTrue:
			[(stepper _ table stepper) forEach:
				[ :e {Heaper} |
				self at: stepper position store: e]]
		ifFalse:
			[| localTable {ScruTable} |
			region ~~ NULL ifTrue: [ localTable _ table subTable: region ]
							ifFalse: [ localTable _ table ].
			(stepper _ localTable stepper) forEach:
				[ :x {Heaper} |
				self at: (dsp of: stepper position) store: x]]!
*/
}
/**
 * I 'wipe' from myself all associations whose key is in 'region'.  See MuTable::wipe
 */
public void wipeAll(XnRegion region) {
	if ( ! (region.coordinateSpace().isEqual(coordinateSpace()))) {
		throw new AboraRuntimeException(AboraRuntimeException.WRONG_COORD_SPACE);
	}
	Stepper stomper = region.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Position p = (Position) stomper.fetch();
		if (p == null) {
			continue ;
		}
		wipe(p);
	}
	stomper.destroy();
/*
udanax-top.st:47950:MuTable methodsFor: 'bulk operations'!
{void} wipeAll: region {XnRegion}
	"I 'wipe' from myself all associations whose key is in 'region'.  See MuTable::wipe"
	
	(region coordinateSpace isEqual: self coordinateSpace) ifFalse:
		[ Heaper BLAST: #WrongCoordSpace ].
	region stepper forEach: [ :p {Position} | self wipe: p]!
*/
}
public boolean includesKey(Position aKey) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:47960:MuTable methodsFor: 'testing'!
{BooleanVar} includesKey: aKey {Position}
	self subclassResponsibility!
*/
}
public boolean isEmpty() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:47963:MuTable methodsFor: 'testing'!
{BooleanVar} isEmpty
	self subclassResponsibility.!
*/
}
public TableStepper stepper(OrderSpec order) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:47968:MuTable methodsFor: 'enumerating'!
{TableStepper} stepper: order {OrderSpec default: NULL}
	self subclassResponsibility!
*/
}
public ImmuTable asImmuTable() {
	return new ImmuTableOnMu(((MuTable) copy()));
/*
udanax-top.st:47973:MuTable methodsFor: 'conversion'!
{ImmuTable} asImmuTable
        ^ImmuTableOnMu create: (self copy cast: MuTable)!
*/
}
/**
 * Note that muTable->asMuTable() returns a copy of the original.  The two
 * are now free to change independently.
 */
public MuTable asMuTable() {
	return (MuTable) copy();
/*
udanax-top.st:47977:MuTable methodsFor: 'conversion'!
{MuTable} asMuTable
	"Note that muTable->asMuTable() returns a copy of the original.  The two
	are now free to change independently."
	
	^self copy quickCast: MuTable!
*/
}
public XnRegion runAt(Position key) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:47985:MuTable methodsFor: 'runs'!
{XnRegion} runAt: key {Position} 
	self subclassResponsibility!
*/
}
public ScruTable copy() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:47991:MuTable methodsFor: 'creation'!
{ScruTable} copy
	self subclassResponsibility!
*/
}
public ScruTable emptySize(int size) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:47994:MuTable methodsFor: 'creation'!
{ScruTable} emptySize: size {IntegerVar}
	self subclassResponsibility!
*/
}
/**
 * Create a new table with an unspecified number of initial domain positions.
 */
public MuTable() {
	super();
/*
udanax-top.st:48000:MuTable methodsFor: 'protected: creation'!
create
	"Create a new table with an unspecified number of initial domain positions."
	super create.!
*/
}
/**
 * Unboxed version.  See class comment for XuInteger
 */
public void intIntroduce(int key, Heaper value) {
	introduce(IntegerPos.make(key), value);
/*
udanax-top.st:48006:MuTable methodsFor: 'overloads'!
{void} atInt: key {IntegerVar} introduce: value {Heaper} 
	"Unboxed version.  See class comment for XuInteger"
	self at: key integer introduce: value!
*/
}
/**
 * Unboxed version.  See class comment for XuInteger
 */
public void intReplace(int key, Heaper value) {
	replace(IntegerPos.make(key), value);
/*
udanax-top.st:48011:MuTable methodsFor: 'overloads'!
{void} atInt: key {IntegerVar} replace: value {Heaper} 
	"Unboxed version.  See class comment for XuInteger"
	self at: key integer replace: value!
*/
}
/**
 * Unboxed version.  See class comment for XuInteger
 */
public Heaper intStore(int aKey, Heaper anObject) {
	return store(IntegerPos.make(aKey), anObject);
/*
udanax-top.st:48016:MuTable methodsFor: 'overloads'!
{Heaper} atInt: aKey {IntegerVar} store: anObject {Heaper}
	"Unboxed version.  See class comment for XuInteger"
	^ self at: aKey integer store: anObject!
*/
}
public boolean includesIntKey(int aKey) {
	return includesKey(IntegerPos.make(aKey));
/*
udanax-top.st:48021:MuTable methodsFor: 'overloads'!
{BooleanVar} includesIntKey: aKey {IntegerVar}
	^self includesKey: aKey integer!
*/
}
public Heaper intFetch(int key) {
	return super.intFetch(key);
/*
udanax-top.st:48025:MuTable methodsFor: 'overloads'!
{Heaper} intFetch: key {IntegerVar} 
	^ super intFetch: key!
*/
}
/**
 * Unboxed version.  See class comment for XuInteger
 */
public void intRemove(int anIdx) {
	remove(IntegerPos.make(anIdx));
/*
udanax-top.st:48028:MuTable methodsFor: 'overloads'!
{void} intRemove: anIdx {IntegerVar}
	"Unboxed version.  See class comment for XuInteger"
	self remove: anIdx integer!
*/
}
/**
 * Unboxed version.  See class comment for XuInteger
 */
public boolean intWipe(int anIdx) {
	return wipe(IntegerPos.make(anIdx));
/*
udanax-top.st:48033:MuTable methodsFor: 'overloads'!
{BooleanVar} intWipe: anIdx {IntegerVar}
	"Unboxed version.  See class comment for XuInteger"
	^ self wipe: anIdx integer!
*/
}
public XnRegion runAtInt(int index) {
	return runAt((IntegerPos.make(index)));
/*
udanax-top.st:48038:MuTable methodsFor: 'overloads'!
{XnRegion} runAtInt: index {IntegerVar}
	^self runAt: (index integer)!
*/
}
public void introduceAll(ScruTable other) {
	introduceAll(other, null, null);
/*
udanax-top.st:48043:MuTable methodsFor: 'smalltalk: defaults'!
{void} introduceAll: other {ScruTable}
	self introduceAll: other with: NULL with: NULL!
*/
}
public void introduceAll(ScruTable table, Dsp dsp) {
	TableStepper stepper;
	if ( ! (table.coordinateSpace().isEqual(coordinateSpace()))) {
		throw new AboraRuntimeException(AboraRuntimeException.WRONG_COORD_SPACE);
	}
	if (domain().intersects((dsp.ofAll(table.domain())))) {
		throw new AboraRuntimeException(AboraRuntimeException.ALREADY_IN_TABLE);
	}
	if (dsp == null) {
		Stepper stomper = (stepper = table.stepper());
		for (; stomper.hasValue(); stomper.step()) {
			Heaper d = (Heaper) stomper.fetch();
			if (d == null) {
				continue ;
			}
			introduce(stepper.position(), d);
		}
		stomper.destroy();
	}
	else {
		Stepper stomper2 = (stepper = table.stepper());
		for (; stomper2.hasValue(); stomper2.step()) {
			Heaper e = (Heaper) stomper2.fetch();
			if (e == null) {
				continue ;
			}
			introduce((dsp.of(stepper.position())), e);
		}
		stomper2.destroy();
	}
/*
udanax-top.st:48046:MuTable methodsFor: 'smalltalk: defaults'!
{void} introduceAll: table {ScruTable} with: dsp {Dsp default: NULL} 
	| stepper {TableStepper} |
	(table coordinateSpace isEqual: self coordinateSpace)
		ifFalse: [Heaper BLAST: #WrongCoordSpace].
	(self domain intersects: (dsp ofAll: table domain))
		ifTrue: [Heaper BLAST: #AlreadyInTable].
	dsp == NULL
		ifTrue: [(stepper _ table stepper) forEach: [:d {Heaper} | 
				self at: stepper position introduce: d]]
		ifFalse: [(stepper _ table stepper) forEach: [:e {Heaper} | 
				self at: (dsp of: stepper position) introduce: e]]!
*/
}
public void removeAll(XnRegion region) {
	if ( ! (region.coordinateSpace().isEqual(coordinateSpace()))) {
		throw new AboraRuntimeException(AboraRuntimeException.WRONG_COORD_SPACE);
	}
	if ( ! (region.isSubsetOf(domain()))) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
	}
	Stepper stomper = region.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Position p = (Position) stomper.fetch();
		if (p == null) {
			continue ;
		}
		remove(p);
	}
	stomper.destroy();
/*
udanax-top.st:48058:MuTable methodsFor: 'smalltalk: defaults'!
{void} removeAll: region {XnRegion}
	(region coordinateSpace isEqual: self coordinateSpace) ifFalse:
		[ Heaper BLAST: #WrongCoordSpace ].
	(region isSubsetOf: self domain) ifFalse:
		[ Heaper BLAST: #NotInTable ].
		
	region stepper forEach: [ :p {Position} | self remove: p]!
*/
}
public void replaceAll(ScruTable other) {
	replaceAll(other, null, null);
/*
udanax-top.st:48068:MuTable methodsFor: 'smalltalk: defaults'!
{void} replaceAll: other {ScruTable}
	self replaceAll: other with: NULL with: NULL!
*/
}
public void replaceAll(ScruTable other, Dsp dsp) {
	replaceAll(other, dsp, null);
/*
udanax-top.st:48071:MuTable methodsFor: 'smalltalk: defaults'!
{void} replaceAll: other {ScruTable} with: dsp {Dsp}
	self replaceAll: other with: dsp with: NULL!
*/
}
public void storeAll(ScruTable other) {
	storeAll(other, null, null);
/*
udanax-top.st:48074:MuTable methodsFor: 'smalltalk: defaults'!
{void} storeAll: other {ScruTable}
	self storeAll: other with: NULL with: NULL!
*/
}
public void storeAll(ScruTable table, Dsp dsp) {
	TableStepper stepper;
	if ( ! (table.coordinateSpace().isEqual(coordinateSpace()))) {
		throw new AboraRuntimeException(AboraRuntimeException.WRONG_COORD_SPACE);
	}
	if (dsp == null) {
		Stepper stomper = (stepper = table.stepper());
		for (; stomper.hasValue(); stomper.step()) {
			Heaper e = (Heaper) stomper.fetch();
			if (e == null) {
				continue ;
			}
			store((dsp.of(stepper.position())), e);
		}
		stomper.destroy();
	}
	else {
		Stepper stomper2 = (stepper = table.stepper());
		for (; stomper2.hasValue(); stomper2.step()) {
			Heaper x = (Heaper) stomper2.fetch();
			if (x == null) {
				continue ;
			}
			store(stepper.position(), x);
		}
		stomper2.destroy();
	}
/*
udanax-top.st:48077:MuTable methodsFor: 'smalltalk: defaults'!
{void} storeAll: table {ScruTable} with: dsp {Dsp default: NULL} 
	| stepper {TableStepper} |
	(table coordinateSpace isEqual: self coordinateSpace)
		ifFalse: [Heaper BLAST: #WrongCoordSpace].
	dsp == NULL
		ifTrue: [(stepper _ table stepper) forEach: [:e {Heaper} | 
				self at: (dsp of: stepper position) store: e]]
		ifFalse: [(stepper _ table stepper) forEach: [:x {Heaper} | 
				self at: stepper position store: x]]!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:48089:MuTable methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public MuTable(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:48091:MuTable methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:48094:MuTable methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:48096:MuTable methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
/*
udanax-top.st:48111:MuTable class methodsFor: 'exceptions:'!
problems.AlreadyInTable
	^self signals: #(AlreadyInTable)!
*/
/*
udanax-top.st:48114:MuTable class methodsFor: 'exceptions:'!
problems.NullInsertion
	^self signals: #(NullInsertion)!
*/
/*
udanax-top.st:48119:MuTable class methodsFor: 'smalltalk: testing'!
test
	"Table test"
	| iTable |
	iTable _ IntegerTable make.
	iTable at: 0 introduce: #zero.
	iTable at: 1 introduce: #one.
	iTable at: 2 introduce: #two.
	iTable at: 3 introduce: #three.
	Transcript show: 'table printing:'; cr.
	Transcript print: iTable; cr; endEntry.!
*/
/**
 * A new empty MuTable whose domain space is 'cs'.
 */
public static MuTable make(CoordinateSpace cs) {
	if (cs.isEqual(IntegerSpace.make())) {
		return IntegerTable.make(10);
	}
	else {
		return HashTable.makeCoordinateSpace(cs);
	}
/*
udanax-top.st:48132:MuTable class methodsFor: 'pseudo constructors'!
{MuTable} make: cs {CoordinateSpace} 
	"A new empty MuTable whose domain space is 'cs'."
	
	(cs isEqual: IntegerSpace make) ifTrue:
		[^IntegerTable make: 10]
	ifFalse: [^HashTable make.CoordinateSpace: cs]!
*/
}
/**
 * Semantically identical to 'muTable(cs)'.  'reg' just provides a hint as to what
 * part of the domain space the new table should expect to be occupied.
 */
public static MuTable make(CoordinateSpace cs, XnRegion reg) {
	if (cs.isEqual(IntegerSpace.make())) {
		return IntegerTable.makeRegion(((IntegerRegion) reg));
	}
	else {
		return HashTable.makeCoordinateSpace(cs);
	}
/*
udanax-top.st:48139:MuTable class methodsFor: 'pseudo constructors'!
{MuTable} make: cs {CoordinateSpace} with: reg {XnRegion} 
	"Semantically identical to 'muTable(cs)'.  'reg' just provides a hint as to what
	part of the domain space the new table should expect to be occupied."
	
	(cs isEqual: IntegerSpace make) ifTrue:
		[^IntegerTable make.Region: (reg cast: IntegerRegion)]
	ifFalse: [^HashTable make.CoordinateSpace: cs]!
*/
}
public static void initTimeNonInherited() {
	/* Used in pseudoconstructor */
/*
udanax-top.st:48149:MuTable class methodsFor: 'smalltalk: initialization'!
initTimeNonInherited
	self REQUIRES: IntegerSpace. "Used in pseudoconstructor"
	self REQUIRES: IntegerTable.
	self REQUIRES: HashTable.!
*/
}
}
