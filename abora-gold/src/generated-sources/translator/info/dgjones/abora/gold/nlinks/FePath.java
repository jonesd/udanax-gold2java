/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.nlinks;

import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.nkernel.FeLabel;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.nlinks.FePath;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.spaces.integers.IntegerRegion;
import info.dgjones.abora.gold.wrapper.FeWrapper;
import info.dgjones.abora.gold.wrapper.FeWrapperSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * A sequence of Labels, used for context information in a LinkEnd.
 */
public class FePath extends FeWrapper {

	protected static FeWrapperSpec ThePathSpec;
/*
udanax-top.st:25046:
FeWrapper subclass: #FePath
	instanceVariableNames: ''
	classVariableNames: 'ThePathSpec {FeWrapperSpec} '
	poolDictionaries: ''
	category: 'Xanadu-nlinks'!
*/
/*
udanax-top.st:25050:
FePath comment:
'A sequence of Labels, used for context information in a LinkEnd.'!
*/
/*
udanax-top.st:25052:
(FePath getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:25123:
FePath class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:25126:
(FePath getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FePath.class).setAttributes( new Set().add("ONCLIENT").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * The first label in the sequence
 * @deprecated
 */
public FeLabel first() {
	throw new PasseException();
/*
udanax-top.st:25057:FePath methodsFor: 'smalltalk: passe'!
{FeLabel} first
	"The first label in the sequence"
	self passe.
	^(self edition get: IntegerZero) cast: FeLabel!
*/
}
/**
 * Replace what is in the container at my path after index with contained.
 * @deprecated
 */
public FeEdition replace(FeEdition container, FeRangeElement contained, int index, int count) {
	throw new PasseException();
/*
udanax-top.st:25062:FePath methodsFor: 'smalltalk: passe'!
{FeEdition} replace: container {FeEdition}
	with: contained {FeRangeElement}
	with: index {IntegerVar}
	with: count {IntegerVar}
	"Replace what is in the container at my path after index with contained."
	
	| labels {XnRegion} |
	index = count ifTrue: [^contained cast: FeEdition].
	labels := container positionsLabelled: ((self edition get: index integer) cast: FeLabel).
	^container with: labels theOne
		with: (self replace: ((container get: labels theOne) cast: FeEdition)
			with: contained
			with: index + 1
			with: count)!
*/
}
/**
 * Replace whatever is at this path in the container with the newValue. Fail if at any point
 * there is not precisely one choice.
 * @deprecated
 */
public FeEdition replaceIn(FeEdition container, FeRangeElement value) {
	throw new PasseException();
/*
udanax-top.st:25077:FePath methodsFor: 'smalltalk: passe'!
{FeEdition} replaceIn: container {FeEdition} with: value {FeRangeElement}
	"Replace whatever is at this path in the container with the newValue. Fail if at any point there is not precisely one choice."
	
	self passe.
	^self replace: container with: value with: IntegerVarZero with: self edition count!
*/
}
/**
 * The remaining path after the first label in the sequence
 * @deprecated
 */
public FePath rest() {
	throw new PasseException();
/*
udanax-top.st:25083:FePath methodsFor: 'smalltalk: passe'!
{FePath} rest
	"The remaining path after the first label in the sequence"
	self passe.
	^(FePath construct: (self edition
		transformedBy: ((IntegerMapping make: -1)
			restrict: (IntegerRegion after: 1)))) cast: FePath!
*/
}
/**
 * Append it to the beginning of the path
 * @deprecated
 */
public FePath withFirst(FeLabel label) {
	throw new PasseException();
/*
udanax-top.st:25090:FePath methodsFor: 'smalltalk: passe'!
{FePath} withFirst: label {FeLabel}
	"Append it to the beginning of the path"
	self passe.
	^(FePath construct: ((self edition
			transformedBy: ((IntegerMapping make: 1)
				restrict: (IntegerRegion after: 1)))
		with: IntegerZero with: label)) cast: FePath!
*/
}
/**
 * Append it to the end of the path
 * @deprecated
 */
public FePath withLast(FeLabel label) {
	throw new PasseException();
/*
udanax-top.st:25098:FePath methodsFor: 'smalltalk: passe'!
{FePath} withLast: label {FeLabel}
	"Append it to the end of the path"
	self passe.
	^(FePath construct: (self edition
		with: self edition count with: label)) cast: FePath!
*/
}
/**
 * Follow a path down into an Edition and return what is at the end of the path. Fail if at
 * any point there is not precisely one choice.
 */
public FeRangeElement follow(FeEdition edition) {
	FeRangeElement result;
	FeLabel label;
	result = edition;
	for (int index = 0; index < edition().count(); index ++ ) {
		label = (FeLabel) (edition().get(IntegerPos.make(index)));
		result = ((FeEdition) result).get((((FeEdition) result).positionsLabelled(label)).theOne());
	}
	return result;
/*
udanax-top.st:25106:FePath methodsFor: 'operations'!
{FeRangeElement CLIENT} follow: edition {FeEdition}
	"Follow a path down into an Edition and return what is at the end of the path. Fail if at any point there is not precisely one choice."
	
	| result {FeRangeElement} label {FeLabel} |
	result := edition.
	IntegerVarZero almostTo: self edition count do: [ :index {IntegerVar} |
		label := (self edition get: index integer) cast: FeLabel.
		result := (result cast: FeEdition) get: ((result cast: FeEdition) positionsLabelled: label) theOne].
	^result!
*/
}
public FePath(FeEdition edition, FeWrapperSpec spec) {
	super(edition, spec);
/*
udanax-top.st:25118:FePath methodsFor: 'private: create'!
create: edition {FeEdition} with: spec {FeWrapperSpec}
	super create: edition with: spec!
*/
}
public static FePath make(PtrArray labels) {
	return (FePath) (spec().wrap((FeEdition.fromArray(labels))));
/*
udanax-top.st:25131:FePath class methodsFor: 'pseudo constructors'!
{FePath CLIENT} make: labels {PtrArray of: FeLabel}
	^(self spec wrap: (FeEdition fromArray: labels)) cast: FePath!
*/
}
public static FeWrapperSpec spec() {
	return ThePathSpec;
/*
udanax-top.st:25135:FePath class methodsFor: 'pseudo constructors'!
{FeWrapperSpec} spec
	^ThePathSpec!
*/
}
public static boolean check(FeEdition edition) {
	Ravi.thingToDo();
	/* check that there are only labels here */
	return (edition.domain().isKindOf(AboraSupport.findCategory(IntegerRegion.class))) && (((IntegerRegion) edition.domain()).isCompacted()
	/* and: [((edition zoneOf: FeLabel spec) domain
			isEqual: edition domain)] */
	);
/*
udanax-top.st:25141:FePath class methodsFor: 'private: wrapping'!
{BooleanVar} check: edition {FeEdition}
	
	Ravi thingToDo. "check that there are only labels here"
	^(edition domain isKindOf: IntegerRegion)
		and: [(edition domain cast: IntegerRegion) isCompacted
		"and: [((edition zoneOf: FeLabel spec) domain
			isEqual: edition domain)]"]!
*/
}
public static FePath construct(FeEdition edition) {
	spec().endorse(edition);
	return (FePath) (makeWrapper(edition));
/*
udanax-top.st:25149:FePath class methodsFor: 'private: wrapping'!
{FePath} construct: edition {FeEdition}
	
	self spec endorse: edition.
	^(self makeWrapper: edition) cast: FePath!
*/
}
public static FeWrapper makeWrapper(FeEdition edition) {
	return new FePath(edition, spec());
/*
udanax-top.st:25154:FePath class methodsFor: 'private: wrapping'!
{FeWrapper} makeWrapper: edition {FeEdition}
	
	^self create: edition with: self spec!
*/
}
public static void setSpec(FeWrapperSpec wrap) {
	ThePathSpec = wrap;
/*
udanax-top.st:25158:FePath class methodsFor: 'private: wrapping'!
{void} setSpec: wrap {FeWrapperSpec}
	ThePathSpec := wrap.!
*/
}
public static void initTimeNonInherited() {
	FeWrapperSpec.DIRECTWRAPPER("Path", "Wrapper", FE_PATH);
/*
udanax-top.st:25164:FePath class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	FeWrapperSpec DIRECTWRAPPER: 'Path'
		with: 'Wrapper'
		with: #FePath.!
*/
}
public static void linkTimeNonInherited() {
	ThePathSpec = null;
/*
udanax-top.st:25170:FePath class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	ThePathSpec := NULL.!
*/
}
/**
 * {FeRangeElement CLIENT} follow: edition {FeEdition}
 */
public static void infostProtocol() {
/*
udanax-top.st:25176:FePath class methodsFor: 'smalltalk: system'!
info.stProtocol
"{FeRangeElement CLIENT} follow: edition {FeEdition}
"!
*/
}
public FePath() {
/*

Generated during transformation
*/
}
public FePath(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
