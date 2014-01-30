/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.sets;

import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.collection.sets.ScruSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.OrderedCollection;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * X++ has three basic kinds of collection classes.  Tables, Sets and XuRegions.  XuRegions
 * are not-necessarily-discrete collections of positions, and are documented in the space
 * module.  Sets and Tables are both discrete and finite, and similar in many ways.  Both
 * originate in a three-way type distinction between:
 * ScruX  --  The protocol for examining one.  I.e., it is *Scru*table
 * ImmuX  --  The contract guarantees that the set or table you''re looking at won''t change
 * (though the things it contains may change)
 * MuX  --  Additional protocol for changing it.
 * Concrete classes may be a subclass of any of the above.  It makes sense to have a concrete
 * subclass of ScruX which isn''t a subclass of either MuX or ImmuX when, for example, it
 * represents a tracking, filtered view of some other set which is itself changing.  All
 * kinds of collection can be iterated over when appropriate using Steppers--our basic
 * iteration abstraction (see Stepper).
 * Immu''s are sort of like Stamps -- they represent a particular state a colection can have.
 * Mu''s are sort of like Berts -- they represent a continuing collection identity which can
 * change its current state.
 * Sets are pure collections--their contents are just a set of Heapers.  Sets (as opposed to
 * tables) do not provide any organization of these contents.
 */
public class ScruSet extends Heaper {

/*
udanax-top.st:45146:
Heaper subclass: #ScruSet
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Sets'!
*/
/*
udanax-top.st:45150:
ScruSet comment:
'X++ has three basic kinds of collection classes.  Tables, Sets and XuRegions.  XuRegions are not-necessarily-discrete collections of positions, and are documented in the space module.  Sets and Tables are both discrete and finite, and similar in many ways.  Both originate in a three-way type distinction between:
	
	ScruX  --  The protocol for examining one.  I.e., it is *Scru*table
		ImmuX  --  The contract guarantees that the set or table you''re looking at won''t change (though the things it contains may change)
		MuX  --  Additional protocol for changing it.
		
	Concrete classes may be a subclass of any of the above.  It makes sense to have a concrete subclass of ScruX which isn''t a subclass of either MuX or ImmuX when, for example, it represents a tracking, filtered view of some other set which is itself changing.  All kinds of collection can be iterated over when appropriate using Steppers--our basic iteration abstraction (see Stepper).
	
	Immu''s are sort of like Stamps -- they represent a particular state a colection can have.  Mu''s are sort of like Berts -- they represent a continuing collection identity which can change its current state.
	
	Sets are pure collections--their contents are just a set of Heapers.  Sets (as opposed to tables) do not provide any organization of these contents.'!
*/
/*
udanax-top.st:45162:
(ScruSet getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:45361:
ScruSet class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:45364:
(ScruSet getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ScruSet.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:45167:ScruSet methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
/**
 * Returns whether the two ScruSets have exactly the same set of elements at the moment.
 * 'a->contentsEqual(b)' is equivalent to
 * 'a->asImmuSet()->isEqual(b->asImmuSet())'.
 */
public boolean contentsEqual(ScruSet other) {
	if (other.count() != count()) {
		return false;
	}
	Stepper stomper = other.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Heaper each = (Heaper) stomper.fetch();
		if (each == null) {
			continue ;
		}
		if ( ! (hasMember(each))) {
			return false;
		}
	}
	stomper.destroy();
	return true;
/*
udanax-top.st:45170:ScruSet methodsFor: 'testing'!
{BooleanVar} contentsEqual: other {ScruSet}
	"Returns whether the two ScruSets have exactly the same set of elements at the moment.
	'a->contentsEqual(b)' is equivalent to 
	'a->asImmuSet()->isEqual(b->asImmuSet())'."
	
	other count ~= self count ifTrue: [^false].
	other stepper forEach: [ :each {Heaper} |
		(self hasMember: each) ifFalse: [^false]].
	^true!
*/
}
/**
 * Has the same relationship to contentsEqual that hashForEqual has to isEqual.
 * I.e., if 'a->contentsEqual (b)', then 'a->contentsHash() == b->contentsHash()'.
 * The same complex caveats apply as to the stability and portability of the
 * hash values as apply for hashForEqual.
 */
public int contentsHash() {
	int result;
	result = 0;
	Stepper stomper = stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Heaper each = (Heaper) stomper.fetch();
		if (each == null) {
			continue ;
		}
		result = result ^ each.hashForEqual();
	}
	stomper.destroy();
	return result;
/*
udanax-top.st:45180:ScruSet methodsFor: 'testing'!
{UInt32} contentsHash
	"Has the same relationship to contentsEqual that hashForEqual has to isEqual. 
	I.e., if 'a->contentsEqual (b)', then 'a->contentsHash() == b->contentsHash()'. 
	The same complex caveats apply as to the stability and portability of the 
	hash values as apply for hashForEqual."
	| result {UInt32} |
	result _ UInt32Zero.
	self stepper forEach: [ :each {Heaper} |
		result _ result bitXor: each hashForEqual].
	^result!
*/
}
/**
 * Is someone a member of the set now?
 */
public boolean hasMember(Heaper someone) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:45192:ScruSet methodsFor: 'testing'!
{BooleanVar} hasMember: someone {Heaper}
	"Is someone a member of the set now?"
	self subclassResponsibility!
*/
}
/**
 * tell whether they have any points in common
 */
public boolean intersects(ScruSet other) {
	/* subclasses can override for efficiency */
	if (other.isEmpty()) {
		return false;
	}
	if (count() > other.count()) {
		return other.intersects(this);
	}
	Stepper stomper = stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Heaper mem = (Heaper) stomper.fetch();
		if (mem == null) {
			continue ;
		}
		if (other.hasMember(mem)) {
			return true;
		}
	}
	stomper.destroy();
	return false;
/*
udanax-top.st:45196:ScruSet methodsFor: 'testing'!
{BooleanVar} intersects: other {ScruSet}
	"tell whether they have any points in common"
	"subclasses can override for efficiency"
	other isEmpty ifTrue: [ ^ false ].
	self count > other count
		ifTrue: [^other intersects: self].
	self stepper forEach: [:mem {Heaper} |
		(other hasMember: mem) ifTrue: [^true]].
	^false!
*/
}
/**
 * Whether it currently has any elements
 */
public boolean isEmpty() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:45207:ScruSet methodsFor: 'testing'!
{BooleanVar} isEmpty
	"Whether it currently has any elements"
	self subclassResponsibility!
*/
}
public boolean isEqual(Heaper other) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:45211:ScruSet methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	self subclassResponsibility!
*/
}
/**
 * Whether another currently has all my elements
 */
public boolean isSubsetOf(ScruSet another) {
	Stepper stomper = stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Heaper elem = (Heaper) stomper.fetch();
		if (elem == null) {
			continue ;
		}
		if ( ! (another.hasMember(elem))) {
			return false;
		}
	}
	stomper.destroy();
	return true;
/*
udanax-top.st:45215:ScruSet methodsFor: 'testing'!
{BooleanVar} isSubsetOf: another {ScruSet} 
	"Whether another currently has all my elements"
	self stepper forEach: 
		[:elem {Heaper} | 
		(another hasMember: elem) ifFalse: [^false]].
	^true!
*/
}
/**
 * A new one whose initial state is my current state, but that doesn't track
 * changes. Note that there is no implication that these can be 'destroy'ed
 * separately, because (for example) an ImmuSet just returns itself
 */
public ScruSet copy() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:45224:ScruSet methodsFor: 'creation'!
{ScruSet} copy
	"A new one whose initial state is my current state, but that doesn't track 
	changes. Note that there is no implication that these can be 'destroy'ed 
	separately, because (for example) an ImmuSet just returns itself"
	self subclassResponsibility!
*/
}
/**
 * The elements in the set in an array, in some random order
 */
public PtrArray asArray() {
	PtrArray result;
	Stepper mine;
	Someone.thingToDo();
	/* make this faster */
	result = PtrArray.nulls(count());
	mine = stepper();
	for (int index = 0; index < result.count(); index ++ ) {
		result.store(index, mine.fetch());
		mine.step();
	}
	mine.destroy();
	return result;
/*
udanax-top.st:45233:ScruSet methodsFor: 'conversion'!
{PtrArray} asArray
	"The elements in the set in an array, in some random order"
	
	| result {PtrArray} mine {Stepper} |
	self thingToDo. "make this faster"
	result := PtrArray nulls: self count DOTasLong.
	mine := self stepper.
	Int32Zero almostTo: result count do: [ :index {Int32} |
		result at: index store: mine fetch.
		mine step].
	mine destroy.
	^result!
*/
}
/**
 * Return an immu snapshot of my current state. Should probably be done with a
 * Converter rather than with a message (for the reasons listed in the Converter
 * class comment). In terms of the Stamp/Bert analogy mentioned in the class
 * comment, asImmuSet is like asking for the current Stamp.
 */
public ImmuSet asImmuSet() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:45246:ScruSet methodsFor: 'conversion'!
{ImmuSet} asImmuSet
	"Return an immu snapshot of my current state. Should probably be done with a 
	Converter rather than with a message (for the reasons listed in the Converter 
	class comment). In terms of the Stamp/Bert analogy mentioned in the class 
	comment, asImmuSet is like asking for the current Stamp."
	self subclassResponsibility!
*/
}
/**
 * Return a Mu whose initial state is the same as my current state, but which
 * will now deviate independently of me. In terms of the Stamp/Bert analogy
 * mentioned in the class comment, asMuSet is like asking for a new Bert starting
 * on the current Stamp.
 */
public MuSet asMuSet() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:45254:ScruSet methodsFor: 'conversion'!
{MuSet} asMuSet
	"Return a Mu whose initial state is the same as my current state, but which 
	will now deviate independently of me. In terms of the Stamp/Bert analogy 
	mentioned in the class comment, asMuSet is like asking for a new Bert starting 
	on the current Stamp."
	self subclassResponsibility!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	printOnWithSimpleSyntax(oo, "{", ", ", "}");
/*
udanax-top.st:45264:ScruSet methodsFor: 'printing'!
{void} printOn: oo {ostream reference} 
	oo << self getCategory name.
	self
		printOnWithSimpleSyntax: oo
		with: '{'
		with: ', '
		with: '}'!
*/
}
public void printOnWithSimpleSyntax(PrintWriter oo, String open, String sep, String close) {
	printOnWithSyntax(oo, open, sep, close, false);
/*
udanax-top.st:45272:ScruSet methodsFor: 'printing'!
{void} printOnWithSimpleSyntax: oo {ostream reference} 
	with: open {char star} 
	with: sep {char star} 
	with: close {char star} 
	
	self printOnWithSyntax: oo with: open with: sep with: close with: false!
*/
}
/**
 * For example, if we have the set '{a, b, c}' and we print it with
 * 'p->printOnWithSyntax(oo,
 */
public void printOnWithSyntax(PrintWriter oo, String open, String sep, String close, boolean fullPrint) {
	/* << */
	/* ,  */
	/* ;  */
	/* ,  */
	/* >> */
	/* );', we get '<<a; b; c>>'.  This is a convenient little hack
	for printing with all sorts of separators and brackets. */
	/* TODO variable may not be initialized before being used */
	Stepper dSet = null;
	int elemCount;
	boolean printMore;
	printMore = ! fullPrint;
	elemCount = 0;
	oo.print(open);
	if (isEmpty()) {
		oo.print("nullSet");
	}
	else {
		dSet = stepper();
		while (dSet.hasValue() && (printMore)) {
			oo.print(dSet.fetch());
			dSet.step();
			if (dSet.hasValue()) {
				oo.print(sep);
			}
			if (printMore && ((elemCount = elemCount + 1) > 200)) {
				printMore = false;
			}
		}
	}
	if ( ! printMore && (dSet.hasValue())) {
		oo.print("etc...");
	}
	oo.print(close);
/*
udanax-top.st:45279:ScruSet methodsFor: 'printing'!
{void} printOnWithSyntax: oo {ostream reference} 
	with: open {char star} 
	with: sep {char star} 
	with: close {char star}
	with: fullPrint {BooleanVar default: false}
	"For example, if we have the set '{a, b, c}' and we print it with 
	'p->printOnWithSyntax(oo, ""<<"", ""; "", "">>"");', we get '<<a; b; c>>'.  This is a convenient little hack
	for printing with all sorts of separators and brackets."
	
	| dSet {Stepper} elemCount {IntegerVar} printMore {BooleanVar} |
	printMore _ fullPrint not.
	elemCount _ IntegerVar0.
	oo << open.
	self isEmpty
		ifTrue: [oo << 'nullSet']
		ifFalse: 
			[dSet _ self stepper.
			[dSet hasValue and: [printMore]]
				whileTrue: 
					[oo << dSet fetch.
					dSet step.
					dSet hasValue ifTrue: [oo << sep].
					(printMore and: [(elemCount _ elemCount + 1) > 200])
						ifTrue: [printMore _ false]]].
	(printMore not and: [dSet hasValue])
		ifTrue: [oo << 'etc...'].
	oo << close!
*/
}
/**
 * How many elements are currently in the set.  Being a set, if the same element is put into
 * the set twice,
 * it is only in the set once.  'Same' above is according to 'isEqual'.
 */
public int count() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:45309:ScruSet methodsFor: 'enumerating'!
{IntegerVar} count
	"How many elements are currently in the set.  Being a set, if the same element is put into the set twice,
	it is only in the set once.  'Same' above is according to 'isEqual'."
	self subclassResponsibility!
*/
}
/**
 * Returns a stepper which will enumerate all the elements of the set in some unspecified
 * order
 */
public Stepper stepper() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:45314:ScruSet methodsFor: 'enumerating'!
{Stepper} stepper
	"Returns a stepper which will enumerate all the elements of the set in some unspecified order"
	self subclassResponsibility!
*/
}
/**
 * Iff I contain exactly one member, return it.  Otherwise BLAST.
 * The idea for this message is taken from the THE function of ONTIC
 * (reference McAllester)
 */
public Heaper theOne() {
	Stepper stepper;
	Heaper result;
	if (count() != 1) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_ONE_ELEMENT);
	}
	stepper = stepper();
	result = stepper.fetch();
	stepper.destroy();
	return result;
/*
udanax-top.st:45318:ScruSet methodsFor: 'enumerating'!
{Heaper} theOne
	"Iff I contain exactly one member, return it.  Otherwise BLAST.
	The idea for this message is taken from the THE function of ONTIC
	(reference McAllester)"
	| stepper {Stepper} result {Heaper} |
	self count ~= 1 ifTrue:
		[ Heaper BLAST: #NotOneElement ].
	stepper _ self stepper.
	result _ stepper fetch.
	stepper destroy.
	^ result!
*/
}
/*
udanax-top.st:45333:ScruSet methodsFor: 'private: smalltalk: private'!
{void} inspect
	^InspectorView open: (Sensor leftShiftDown ifTrue: [Inspector inspect: self] ifFalse: [SetInspector inspect: self])!
*/
/**
 * return all of my elements in an ordered collection for smalltalk MVC hacking
 */
public OrderedCollection asOrderedCollection() {
	OrderedCollection result;
	Stepper stomp;
	result = new OrderedCollection(count());
	stomp = stepper();
	while (stomp.hasValue()) {
		result.add(stomp.get());
		stomp.step();
	}
	return result;
/*
udanax-top.st:45338:ScruSet methodsFor: 'smalltalk: conversion'!
asOrderedCollection
	"return all of my elements in an ordered collection for smalltalk MVC hacking"
	| result {OrderedCollection} stomp {Stepper} |
	result _ OrderedCollection new: self count.
	stomp _ self stepper.
	[stomp hasValue]
		whileTrue: [result add: stomp get.
			stomp step].
	^result
	"| result {SortedCollection} stomp {Stepper} |
	result _ SortedCollection new: self count.
	stomp _ self stepper.
	[stomp hasValue]
		whileTrue: [result add: stomp get.
			stomp step].
	^result asOrderedCollection"!
*/
}
/*
udanax-top.st:45356:ScruSet methodsFor: 'smalltalk: conversion'!
{void} do: aBlock {BlockClosure of: Heaper}
	self stepper forEach: aBlock!
*/
/*
udanax-top.st:45369:ScruSet class methodsFor: 'exceptions: exceptions'!
problems.NotInSet
	^self signals: #(NotInSet)!
*/
public ScruSet() {
/*

Generated during transformation
*/
}
public ScruSet(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
