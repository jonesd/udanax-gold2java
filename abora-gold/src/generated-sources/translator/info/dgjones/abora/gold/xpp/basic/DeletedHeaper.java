/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xpp.basic;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class DeletedHeaper extends Heaper {

	protected Heaper myOldHeaper;
/*
Xanadu-Xpp-Basic.st:140:
Object subclass: #DeletedHeaper
	instanceVariableNames: 'myOldHeaper {Heaper}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xpp-Basic'!
*/
/*
Xanadu-Xpp-Basic.st:172:
DeletedHeaper class
	instanceVariableNames: ''!
*/
public Category getCategory() {
	return AboraSupport.findCategory(Heaper.class);
/*
Xanadu-Xpp-Basic.st:150:DeletedHeaper methodsFor: 'getcategory'!
getCategory
	^ Heaper!
*/
}
public boolean isKindOf(Object aClass) {
	return aClass == AboraSupport.findCategory(Heaper.class);
/*
Xanadu-Xpp-Basic.st:153:DeletedHeaper methodsFor: 'getcategory'!
isKindOf: aClass
	^ aClass == Heaper!
*/
}
/**
 * in preparation for a smalltalk become
 */
public DeletedHeaper() {
	myOldHeaper = this;
/*
Xanadu-Xpp-Basic.st:158:DeletedHeaper methodsFor: 'creation'!
create
	"in preparation for a smalltalk become"
	myOldHeaper _ self!
*/
}
public DeletedHeaper(Object a, Object b) {
	halt();
/*
Xanadu-Xpp-Basic.st:162:DeletedHeaper methodsFor: 'creation'!
create: a with: b
	self halt.!
*/
}
public void destroy() {
/*
Xanadu-Xpp-Basic.st:167:DeletedHeaper methodsFor: 'destroy'!
{void NOLOCK} destroy!
*/
}
/**
 * Since all Abrahm methods are nested inside a lock - operate -
 * unlock sequence, destroy can't deallocate. Therefore, the unlock
 * operation tests whether the shepherd has been destructed (by
 * checking its category) and deallocates if so.
 */
public static void unlockFunctionAvoidingDestroy(Abraham shep) {
	shep.destroy();
/*
Xanadu-Xpp-Basic.st:179:DeletedHeaper class methodsFor: 'safe unlocking'!
{void} unlockFunctionAvoidingDestroy: shep {Abraham} 
	"Since all Abrahm methods are nested inside a lock - operate - 
	unlock sequence, destroy can't deallocate. Therefore, the unlock 
	operation tests whether the shepherd has been destructed (by 
	checking its category) and deallocates if so."
	shep destroy!
*/
}
public DeletedHeaper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
