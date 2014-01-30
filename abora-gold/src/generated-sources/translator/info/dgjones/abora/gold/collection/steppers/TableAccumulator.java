/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.steppers;

import info.dgjones.abora.gold.collection.steppers.Accumulator;
import info.dgjones.abora.gold.collection.steppers.TableAccumulator;
import info.dgjones.abora.gold.collection.tables.MuArray;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * Consider this class''s public status as obsolete.  Eventually This class will either be
 * private of get retired.
 */
public class TableAccumulator extends Accumulator {

/*
udanax-top.st:12379:
Accumulator subclass: #TableAccumulator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Steppers'!
*/
/*
udanax-top.st:12383:
TableAccumulator comment:
'Consider this class''s public status as obsolete.  Eventually This class will either be private of get retired.'!
*/
/*
udanax-top.st:12385:
(TableAccumulator getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:12410:
TableAccumulator class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:12413:
(TableAccumulator getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(TableAccumulator.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Add elem to the internal table.
 */
public void step(Heaper elem) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:12390:TableAccumulator methodsFor: 'deferred operations'!
{void} step: elem {Heaper}
	"Add elem to the internal table."
	self subclassResponsibility!
*/
}
/**
 * Return the accumulated table.
 */
public Heaper value() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:12394:TableAccumulator methodsFor: 'deferred operations'!
{Heaper} value
	"Return the accumulated table."
	self subclassResponsibility!
*/
}
/**
 * Should this copy the array?
 */
public Accumulator copy() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:12400:TableAccumulator methodsFor: 'deferred create'!
{Accumulator} copy
	"Should this copy the array?"
	self subclassResponsibility!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print(" on ");
	oo.print(value());
/*
udanax-top.st:12406:TableAccumulator methodsFor: 'printing'!
{void} printOn: oo {ostream reference} 
	oo << self getCategory name << ' on ' << self value!
*/
}
/**
 * Returns an Accumulator which will produce an MuArray of the elements
 * accumulated into it in order of accumulation. See MuArray. Equivalent to
 * 'arrayAccumulator()'. Eventually either he or I should be declared obsolete. INLINE
 */
public static TableAccumulator make() {
	return MuArray.arrayAccumulator();
/*
udanax-top.st:12418:TableAccumulator class methodsFor: 'pseudoConstructors'!
{TableAccumulator} make
	"Returns an Accumulator which will produce an MuArray of the elements 
	accumulated into it in order of accumulation. See MuArray. Equivalent to 
	'arrayAccumulator()'. Eventually either he or I should be declared obsolete. INLINE"
	^MuArray arrayAccumulator!
*/
}
public TableAccumulator() {
/*

Generated during transformation
*/
}
public TableAccumulator(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
