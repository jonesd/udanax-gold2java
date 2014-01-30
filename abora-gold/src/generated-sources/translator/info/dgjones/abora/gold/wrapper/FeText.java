/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.wrapper;

import info.dgjones.abora.gold.collection.basic.PrimArray;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.spaces.integers.IntegerMapping;
import info.dgjones.abora.gold.spaces.integers.IntegerRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerSpace;
import info.dgjones.abora.gold.wrapper.FeText;
import info.dgjones.abora.gold.wrapper.FeWrapper;
import info.dgjones.abora.gold.wrapper.FeWrapperSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import java.io.PrintWriter;

/**
 * Handles a integer-indexed, contiguous, zero-based Edition of RangeElements
 */
public class FeText extends FeWrapper {

	protected static FeWrapperSpec TheTextSpec;
/*
udanax-top.st:25378:
FeWrapper subclass: #FeText
	instanceVariableNames: ''
	classVariableNames: 'TheTextSpec {FeWrapperSpec} '
	poolDictionaries: ''
	category: 'Xanadu-wrapper'!
*/
/*
udanax-top.st:25382:
FeText comment:
'Handles a integer-indexed, contiguous, zero-based Edition of RangeElements'!
*/
/*
udanax-top.st:25384:
(FeText getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:25490:
FeText class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:25493:
(FeText getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeText.class).setAttributes( new Set().add("ONCLIENT").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * The Edition of the actual contents, without any style information. You should use this
 * instead of edition() when you want to get the Edition for comparisons, queries, etc.
 * Future styled text implementations will not store the contents as directly as we do now.
 */
public FeEdition contents() {
	return edition();
/*
udanax-top.st:25389:FeText methodsFor: 'text manipulation'!
{FeEdition CLIENT} contents
	"The Edition of the actual contents, without any style information. You should use this instead of edition() when you want to get the Edition for comparisons, queries, etc. Future styled text implementations will not store the contents as directly as we do now."
	
	^self edition!
*/
}
/**
 * The number of elements in the string
 */
public int count() {
	return edition().count();
/*
udanax-top.st:25394:FeText methodsFor: 'text manipulation'!
{IntegerVar CLIENT} count
	"The number of elements in the string"
	
	^self edition count!
*/
}
/**
 * All the text lying within the region, with the gaps compressed out.
 */
public FeText extract(IntegerRegion region) {
	return FeText.construct((edition().transformedBy(((IntegerRegion) (region.intersect(edition().domain()))).compactor())));
/*
udanax-top.st:25399:FeText methodsFor: 'text manipulation'!
{FeText CLIENT} extract: region {IntegerRegion}
	"All the text lying within the region, with the gaps compressed out."
	
	^FeText construct: (self edition
		transformedBy: ((region intersect: self edition domain) cast: IntegerRegion) compactor)!
*/
}
/**
 * Insert new information into the Edition at the given point, pushing everything after it
 * forward.
 */
public FeText insert(int position, FeText text) {
	validate(position);
	return FeText.construct(((text.edition().transformedBy((IntegerMapping.make(position)))).combine((edition().transformedBy(((IntegerMapping.make().restrict((IntegerRegion.before(position)))).combine(((IntegerMapping.make(text.count())).restrict((IntegerRegion.after(position)))))))))));
/*
udanax-top.st:25405:FeText methodsFor: 'text manipulation'!
{FeText CLIENT} insert: position {IntegerVar} with: text {FeText}
	"Insert new information into the Edition at the given point, pushing everything after it forward."
	
	self validate: position.
	^FeText construct: ((text edition
			transformedBy: (IntegerMapping make: position))
		combine: (self edition
			transformedBy: ((IntegerMapping make
					restrict: (IntegerRegion before: position))
				combine: ((IntegerMapping make: text count)
					restrict: (IntegerRegion after: position)))))!
*/
}
/**
 * Insert a virtual copy of the region of text before the given position, and remove it from
 * its current location. If the position is one past the last character, then it will be
 * inserted after the end. If the region is discontiguous, then the contiguous pieces are
 * concatenated together, in sequence, and inserted.
 */
public FeText move(int pos, IntegerRegion region) {
	IntegerRegion moved;
	IntegerRegion left;
	validate(pos);
	moved = (IntegerRegion) (edition().domain().intersect(region));
	left = (IntegerRegion) (edition().domain().minus(region));
	return FeText.construct((edition().transformedBy(((((IntegerRegion) (left.intersect((IntegerRegion.before(pos))))).compactor().combine((moved.compactor().transformedBy((IntegerMapping.make(pos)))))).combine((((IntegerRegion) (left.intersect((IntegerRegion.after(pos))))).compactor().transformedBy((IntegerMapping.make((moved.unionWith((IntegerRegion.make(0, pos)))).count())))))))));
/*
udanax-top.st:25417:FeText methodsFor: 'text manipulation'!
{FeText CLIENT} move: pos {IntegerVar} with: region {IntegerRegion}
	"Insert a virtual copy of the region of text before the given position, and remove it from its current location. If the position is one past the last character, then it will be inserted after the end. If the region is discontiguous, then the contiguous pieces are concatenated together, in sequence, and inserted."
	| moved {IntegerRegion} left {IntegerRegion} |
	self validate: pos.
	moved := (self edition domain intersect: region) cast: IntegerRegion.
	left := (self edition domain minus: region) cast: IntegerRegion.
	^FeText construct: (self edition
			transformedBy: ((((left intersect: (IntegerRegion before: pos)) cast: IntegerRegion)
					compactor
				combine: (moved compactor transformedBy: (IntegerMapping make: pos)))
				combine: (((left intersect: (IntegerRegion after: pos)) cast: IntegerRegion)
						compactor
					transformedBy: (IntegerMapping make: (moved
							unionWith: (IntegerRegion make: IntegerVar0 with: pos))
						count))))!
*/
}
/**
 * Replaces a region of text with a virtual copy of text from another document.
 * If the destination region lies to the left of the domain, inserts before the beginning; if
 * it intersects the domain, insert at the first common position; if it lies after the end,
 * insert after the end. Fails with
 * BLAST(AmbiguousReplacement) if the region is empty.
 * May be used to copy information within a single document.
 * This operation may not be particularly useful with non-simple destination regions.
 */
public FeText replace(IntegerRegion dest, FeText other) {
	int to;
	if ((IntegerRegion.before(0)).intersects(dest)) {
		to = 0;
	}
	else {
		if (dest.intersects(edition().domain())) {
			to = ((IntegerRegion) (dest.intersect(edition().domain()))).start();
		}
		else {
			if ((IntegerRegion.after(count())).intersects(dest)) {
				to = count();
			}
			else {
				throw new AboraRuntimeException(AboraRuntimeException.AMBIGUOUS_REPLACEMENT);
			}
		}
	}
	Someone.thingToDo();
	/* Do this all in one step */
	return (extract(((IntegerRegion) dest.complement()))).insert(to, other);
/*
udanax-top.st:25434:FeText methodsFor: 'text manipulation'!
{FeText CLIENT} replace: dest {IntegerRegion} with: other {FeText}
	"Replaces a region of text with a virtual copy of text from another document.
	If the destination region lies to the left of the domain, inserts before the beginning; if it intersects the domain, insert at the first common position; if it lies after the end, insert after the end. Fails with
		BLAST(AmbiguousReplacement) if the region is empty.
	May be used to copy information within a single document.
	This operation may not be particularly useful with non-simple destination regions."	
	
	| to {IntegerVar} |
	((IntegerRegion before: IntegerVar0) intersects: dest) ifTrue:
		[to := IntegerVar0]
	ifFalse: [(dest intersects: self edition domain) ifTrue:
		[to := ((dest intersect: self edition domain) cast: IntegerRegion) start]
	ifFalse: [((IntegerRegion after: self count) intersects: dest) ifTrue:
		[to := self count]
	ifFalse:
		[Heaper BLAST: #AmbiguousReplacement]]].
	
	self thingToDo. "Do this all in one step"
	^(self extract: (dest complement cast: IntegerRegion)) insert: to with: other!
*/
}
/**
 * Check that information can be inserted at the position. Blast if not.
 */
public void validate(int pos) {
	if ( ! (0 <= pos && (pos <= count()))) {
		throw new AboraRuntimeException(AboraRuntimeException.INVALID_TEXT_POSITION);
	}
/*
udanax-top.st:25456:FeText methodsFor: 'private:'!
{void} validate: pos {IntegerVar}
	"Check that information can be inserted at the position. Blast if not."
	(IntegerVar0 <= pos and: [pos <= self count]) ifFalse:
		[Heaper BLAST: #InvalidTextPosition]!
*/
}
public FeText(FeEdition edition, FeWrapperSpec spec) {
	super(edition, spec);
/*
udanax-top.st:25464:FeText methodsFor: 'protected: create'!
create: edition {FeEdition} with: spec {FeWrapperSpec}
	super create: edition with: spec!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	/* (self edition copy: (IntegerRegion before: 100)) retrieve forEach: [ :bundle {FeBundle} |
		bundle cast: FeArrayBundle into: [ :array |
			array array cast: UInt8Array into: [ :chars |
				oo << chars]
			others:
				[UInt32Zero almostTo: array array count do: [ :i {UInt32} |
					oo << (array get: i)]]]
		cast: FeElementBundle into: [ :element |
			]
		cast: FePlaceHolderBundle into: [ :places |
			]].
	(self edition isFinite not or: [self edition count > 100]) ifTrue:
		[oo << '...']. */
	oo.print(edition());
	/* for now */
	oo.print(")");
/*
udanax-top.st:25470:FeText methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '('.
	"(self edition copy: (IntegerRegion before: 100)) retrieve forEach: [ :bundle {FeBundle} |
		bundle cast: FeArrayBundle into: [ :array |
			array array cast: UInt8Array into: [ :chars |
				oo << chars]
			others:
				[UInt32Zero almostTo: array array count do: [ :i {UInt32} |
					oo << (array get: i)]]]
		cast: FeElementBundle into: [ :element |
			]
		cast: FePlaceHolderBundle into: [ :places |
			]].
	(self edition isFinite not or: [self edition count > 100]) ifTrue:
		[oo << '...']."
	oo << self edition. "for now"
	oo << ')'!
*/
}
public static boolean check(FeEdition edition) {
	return (IntegerSpace.make().isEqual(edition.coordinateSpace())) && (((IntegerRegion) edition.domain()).isCompacted());
/*
udanax-top.st:25498:FeText class methodsFor: 'private: wrapping'!
{BooleanVar} check: edition {FeEdition}
	^(IntegerSpace make isEqual: edition coordinateSpace)
		and: [(edition domain cast: IntegerRegion) isCompacted]!
*/
}
/**
 * Called from internal code to create and endorse new Editions. Does not check the contents;
 * assumes that it will only be called by trusted code.
 */
public static FeText construct(FeEdition edition) {
	spec().endorse(edition);
	return (FeText) (makeWrapper(edition));
/*
udanax-top.st:25503:FeText class methodsFor: 'private: wrapping'!
{FeText} construct: edition {FeEdition}
	"Called from internal code to create and endorse new Editions. Does not check the contents; assumes that it will only be called by trusted code."
	
	self spec endorse: edition.
	^(self makeWrapper: edition) cast: FeText!
*/
}
public static FeWrapper makeWrapper(FeEdition edition) {
	return new FeText(edition, spec());
/*
udanax-top.st:25509:FeText class methodsFor: 'private: wrapping'!
{FeWrapper} makeWrapper: edition {FeEdition}
	^self create: edition with: self spec!
*/
}
public static void setSpec(FeWrapperSpec wrap) {
	TheTextSpec = wrap;
/*
udanax-top.st:25512:FeText class methodsFor: 'private: wrapping'!
{void} setSpec: wrap {FeWrapperSpec}
	TheTextSpec := wrap.!
*/
}
public static void initTimeNonInherited() {
	FeWrapperSpec.DIRECTWRAPPER("Text", "Wrapper", FE_TEXT);
/*
udanax-top.st:25518:FeText class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	FeWrapperSpec DIRECTWRAPPER: 'Text' with: 'Wrapper' with: #FeText!
*/
}
public static void linkTimeNonInherited() {
	TheTextSpec = null;
/*
udanax-top.st:25522:FeText class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	TheTextSpec := NULL.!
*/
}
public static FeText make(PrimArray data) {
	if (data == null) {
		return construct((FeEdition.empty(IntegerSpace.make())));
	}
	else {
		return construct((FeEdition.fromArray(data)));
	}
/*
udanax-top.st:25528:FeText class methodsFor: 'pseudo constructors'!
{FeText CLIENT} make: data {PrimArray default: NULL}
	data == NULL
		ifTrue: [^self construct: (FeEdition empty: IntegerSpace make)]
		ifFalse: [^self construct: (FeEdition fromArray: data)]!
*/
}
public static FeWrapperSpec spec() {
	return TheTextSpec;
/*
udanax-top.st:25534:FeText class methodsFor: 'pseudo constructors'!
{FeWrapperSpec} spec
	^TheTextSpec!
*/
}
/**
 * {FeEdition CLIENT} contents
 * {IntegerVar CLIENT} count
 * {FeText CLIENT} extract: region {IntegerRegion}
 * {FeText CLIENT} insert: position {IntegerVar} with: text {FeText}
 * {FeText CLIENT} move: pos {IntegerVar} with: region {IntegerRegion}
 * {FeText CLIENT} replace: dest {IntegerRegion} with: other {FeText}
 */
public static void infostProtocol() {
/*
udanax-top.st:25540:FeText class methodsFor: 'smalltalk: system'!
info.stProtocol
"{FeEdition CLIENT} contents
{IntegerVar CLIENT} count
{FeText CLIENT} extract: region {IntegerRegion}
{FeText CLIENT} insert: position {IntegerVar} with: text {FeText}
{FeText CLIENT} move: pos {IntegerVar} with: region {IntegerRegion}
{FeText CLIENT} replace: dest {IntegerRegion} with: other {FeText}
"!
*/
}
public FeText() {
/*

Generated during transformation
*/
}
public FeText(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
