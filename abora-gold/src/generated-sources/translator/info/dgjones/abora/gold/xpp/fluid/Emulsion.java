/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xpp.fluid;

import info.dgjones.abora.gold.java.AboraHeaper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.AboraClass;
import info.dgjones.abora.gold.java.missing.smalltalk.Array;
import info.dgjones.abora.gold.java.missing.smalltalk.OrderedCollection;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.fluid.Emulsion;
import info.dgjones.abora.gold.xpp.fluid.FluidVar;
import info.dgjones.abora.gold.xpp.fluid.GlobalEmulsion;
import java.io.PrintWriter;

public class Emulsion extends AboraHeaper {

	protected OrderedCollection myFluids;
	protected boolean myFluidsUsed;
	protected static Emulsion TheImageEmulsion;
/*
Xanadu-Xpp-fluid.st:0:
Object subclass: #Emulsion
	instanceVariableNames: '
		myFluids {UNKNOWN}
		myFluidsUsed {UNKNOWN}'
	classVariableNames: 'TheImageEmulsion {UNKNOWN} '
	poolDictionaries: ''
	category: 'Xanadu-Xpp-fluid'!
*/
/*
Xanadu-Xpp-fluid.st:6:
(Emulsion getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
/*
Xanadu-Xpp-fluid.st:60:
Emulsion class
	instanceVariableNames: ''!
*/
/*
Xanadu-Xpp-fluid.st:63:
(Emulsion getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Emulsion.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public Array fetchNewRawSpace(int size) {
	throw new SubclassResponsibilityException();
/*
Xanadu-Xpp-fluid.st:11:Emulsion methodsFor: 'accessing'!
{void star} fetchNewRawSpace: size {#size.U.t var}
	self subclassResponsibility!
*/
}
public Array fetchOldRawSpace() {
	throw new SubclassResponsibilityException();
/*
Xanadu-Xpp-fluid.st:15:Emulsion methodsFor: 'accessing'!
{void star} fetchOldRawSpace
	self subclassResponsibility!
*/
}
public Array fluidsSpace() {
	Array result;
	result = fetchOldRawSpace();
	if (result != null) {
		return result;
	}
	result = fetchNewRawSpace(myFluids.size());
	myFluidsUsed = true;
	for (int doIndex = 0; doIndex < myFluids.size(); doIndex ++ ) {
		FluidVar fluid = (FluidVar) myFluids.get(doIndex);
		fluid.init();
	}
	return result;
/*
Xanadu-Xpp-fluid.st:19:Emulsion methodsFor: 'accessing'!
{Array} fluidsSpace
	| result {Array} |
	result _ self fetchOldRawSpace.
	result ~~ NULL ifTrue:
		[ ^ result ].
	result _ self fetchNewRawSpace: myFluids size.
	myFluidsUsed _ true.
	myFluids do: [ :fluid | fluid init ].
	^ result.!
*/
}
public Emulsion() {
	super();
	myFluids = new OrderedCollection();
	myFluidsUsed = false;
/*
Xanadu-Xpp-fluid.st:31:Emulsion methodsFor: 'create'!
create
	super create.
	myFluids _ OrderedCollection new.
	myFluidsUsed _ false.!
*/
}
public void destructAll() {
	for (int doIndex = 0; doIndex < myFluids.size(); doIndex ++ ) {
		FluidVar f = (FluidVar) myFluids.get(doIndex);
		f.fluidSet(null);
	}
/*
Xanadu-Xpp-fluid.st:38:Emulsion methodsFor: 'finalization'!
destructAll
	myFluids do: [ :f | f fluidSet: nil ]!
*/
}
public void printOn(PrintWriter aStream) {
	aStream.print(Emulsion.class.getName());
	aStream.print("(");
	aStream.print(myFluids);
	aStream.print(")");
/*
Xanadu-Xpp-fluid.st:43:Emulsion methodsFor: 'printing'!
printOn: aStream
	aStream << self class name << '(' << myFluids << ')'!
*/
}
/**
 * add a new fluid to the emulsion, and return its index
 */
public int addFluid(FluidVar fluid) {
	if ( ! ( ! myFluidsUsed)) {
		throw new AboraAssertionException("Fluids may not be added to emulsion after init.");
	}
	myFluids.add(fluid);
	return myFluids.size();
/*
Xanadu-Xpp-fluid.st:48:Emulsion methodsFor: 'special'!
{Integer} addFluid: fluid {FluidVar}
	"add a new fluid to the emulsion, and return its index"
	myFluidsUsed not assert: 'Fluids may not be added to emulsion after init.'.
	myFluids add: fluid.
	^ myFluids size!
*/
}
public void reset() {
	if (myFluids != null) {
		for (int doIndex = 0; doIndex < myFluids.size(); doIndex ++ ) {
			FluidVar f = (FluidVar) myFluids.get(doIndex);
			f.goAway();
		}
	}
	myFluids = new OrderedCollection();
	myFluidsUsed = false;
/*
Xanadu-Xpp-fluid.st:54:Emulsion methodsFor: 'special'!
reset
	myFluids ~~ nil ifTrue: [myFluids do: [ :f | f goAway ]].
	myFluids _ OrderedCollection new.
	myFluidsUsed _ false.!
*/
}
public static Emulsion imageEmulsion() {
	return TheImageEmulsion;
/*
Xanadu-Xpp-fluid.st:68:Emulsion class methodsFor: 'accessing'!
imageEmulsion
	^ TheImageEmulsion!
*/
}
public static Emulsion globalEmulsion() {
	return GlobalEmulsion.make();
/*
Xanadu-Xpp-fluid.st:74:Emulsion class methodsFor: 'global: metamorphic creation'!
{Emulsion} globalEmulsion
	^ GlobalEmulsion make!
*/
}
public static void cleanupGarbage() {
	OrderedCollection subclasses = AboraSupport.subclasses(Emulsion.class);
	for (int doIndex = 0; doIndex < subclasses.size(); doIndex ++ ) {
		AboraClass c = (AboraClass) subclasses.get(doIndex);
		OrderedCollection doSource = c.allInstances();
		for (int doIndex1 = 0; doIndex1 < doSource.size(); doIndex1 ++ ) {
			Emulsion e = (Emulsion) doSource.get(doIndex1);
			if (e != TheImageEmulsion) {
				e.reset();
			}
		}
	}
/*
Xanadu-Xpp-fluid.st:80:Emulsion class methodsFor: 'cleanup'!
cleanupGarbage
	Emulsion subclassesDo: [:c | c allInstances do: [:e | e ~~ TheImageEmulsion ifTrue: [e reset]]]!
*/
}
/**
 * Emulsion initialize
 */
public static void initialize() {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
Xanadu-Xpp-fluid.st:85:Emulsion class methodsFor: 'initialize'!
initialize
	"Emulsion initialize"
	self cleanupGarbage.
	TheImageEmulsion ~~ nil ifTrue:
		[TheImageEmulsion reset.
		TheImageEmulsion _ nil].
	Smalltalk garbageCollect.
	TheImageEmulsion _ GlobalEmulsion new create.
	self initImageEmulsions: Object!
*/
}
public static void initImageEmulsions(AboraClass classx) {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
Xanadu-Xpp-fluid.st:96:Emulsion class methodsFor: 'initialize'!
initImageEmulsions: class
	class isMeta ifTrue: [^self].
	(class class includesSelector: #initImageEmulsion)
		ifTrue: [class initImageEmulsion].
	class subclassesDo: [:cl | self initImageEmulsions: cl]!
*/
}
public Emulsion(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
